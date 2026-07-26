#!/usr/bin/env python3
# /// script
# dependencies = [
#     "jinja2>=3.1.6",
#     "jsonschema>=4.26.0",
#     "markdown>=3.10",
#     "pyyaml>=6.0.3",
# ]
# ///

import argparse
import dataclasses
import datetime
import fnmatch
import functools
import io
import json
import os
import shlex
import shutil
import subprocess
import sys
import tarfile
import tempfile
import time
from collections.abc import Callable, Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Literal

import jsonschema
import markdown
import yaml
from jinja2 import Environment, FileSystemLoader, select_autoescape

PROJECT_ROOT = Path(__file__).parent.resolve()
ARENA_REPO_URL = "https://github.com/leanprover/lean-kernel-arena"
LEAN4EXPORT_REPO_URL = "https://github.com/leanprover/lean4export"

INSTRUCTIONS_PER_SECOND = 6_000_000_000
MIN_COMPARABLE_CPU_TIME = 0.05
MIN_COMPARABLE_MAX_RSS = 200 * 1024 * 1024
MAX_TARBALL_ENTRY_SIZE = 10 * 1024 * 1024
MATHLIB_TEST = "mathlib"
OFFICIAL_CHECKER = "official"
UNIT_SPACE = "\N{NARROW NO-BREAK SPACE}"

Outcome = Literal["accept", "reject"]
Status = Literal["accepted", "rejected", "declined", "error"]
Correctness = Literal["correct", "incorrect", "declined", "error"]

VERBOSE = False


class BuildError(Exception):
    pass


def format_duration(seconds: float) -> str:
    if seconds >= 3600:
        return f"{seconds / 3600:.1f}{UNIT_SPACE}h"
    if seconds >= 60:
        return f"{seconds / 60:.1f}{UNIT_SPACE}m"
    if seconds >= 1:
        return f"{seconds:.1f}{UNIT_SPACE}s"
    return f"{seconds * 1000:.0f}{UNIT_SPACE}ms"


def format_memory(num_bytes: float) -> str:
    if num_bytes >= 1024**3:
        return f"{num_bytes / 1024**3:.1f}{UNIT_SPACE}GB"
    if num_bytes >= 1024**2:
        return f"{num_bytes / 1024**2:.1f}{UNIT_SPACE}MB"
    if num_bytes >= 1024:
        return f"{num_bytes / 1024:.1f}{UNIT_SPACE}KB"
    return f"{num_bytes:.0f}{UNIT_SPACE}B"


def format_unitless(count: float) -> str:
    if count >= 1e12:
        return f"{count / 1e12:.1f}{UNIT_SPACE}T"
    if count >= 1e9:
        return f"{count / 1e9:.1f}{UNIT_SPACE}G"
    if count >= 1e6:
        return f"{count / 1e6:.1f}{UNIT_SPACE}M"
    if count >= 1e3:
        return f"{count / 1e3:.1f}{UNIT_SPACE}k"
    return f"{count:.0f}"


def status_symbol(status: Status) -> str:
    match status:
        case "accepted":
            return "\N{THUMBS UP SIGN}"
        case "rejected":
            return "\N{RAISED HAND}"
        case "declined":
            return "\N{NO ENTRY SIGN}"
        case "error":
            return "\N{COLLISION SYMBOL}"


def correctness_symbol(correctness: Correctness) -> str:
    match correctness:
        case "correct":
            return "\N{WHITE HEAVY CHECK MARK}"
        case "incorrect":
            return "\N{CROSS MARK}"
        case "declined":
            return "\N{CIRCLED DIVISION SLASH}"
        case "error":
            return "\N{WARNING SIGN}\N{VARIATION SELECTOR-16}"


def render_markdown(text: str | None) -> str:
    if not text:
        return ""
    renderer = markdown.Markdown(extensions=["extra", "codehilite", "toc"])
    return renderer.convert(text.strip())


def indented(label: str, text: str) -> str:
    if not text.strip():
        return ""
    padding = "\n" + " " * (len(label) + 4)
    return f"  {label}: {text.strip().replace('\n', padding)}\n"


def as_dict(value: Any) -> dict[str, Any]:
    return value if isinstance(value, dict) else {}


@dataclass(frozen=True)
class Metrics:
    wall_time: float
    cpu_time: float
    max_rss: int
    instructions: int


@dataclass(frozen=True)
class CommandResult:
    returncode: int
    stdout: str
    stderr: str
    metrics: Metrics

    @property
    def output(self) -> str:
        return indented("stdout", self.stdout) + indented("stderr", self.stderr)


def probe(cmd: list[str]) -> bool:
    if shutil.which(cmd[0]) is None:
        return False
    try:
        return subprocess.run(cmd, capture_output=True, timeout=10).returncode == 0
    except (OSError, subprocess.TimeoutExpired):
        return False


@functools.cache
def perf_prefix() -> list[str]:
    if not probe(["perf", "stat", "-e", "instructions", "--", "true"]):
        return []
    return [
        "perf",
        "stat",
        "-j",
        "-o",
        "",
        "-e",
        "duration_time,task-clock,instructions",
        "--",
    ]


@functools.cache
def gnu_time_prefix() -> list[str]:
    if not probe(["time", "-f", "%M", "-o", "/dev/null", "--", "true"]):
        return []
    return ["time", "-f", "max_rss_kb=%M", "-o", "", "--"]


def argv(cmd: str | Sequence[str]) -> list[str]:
    return ["sh", "-c", cmd] if isinstance(cmd, str) else list(cmd)


def parse_perf_events(path: Path) -> dict[str, float]:
    events: dict[str, float] = {}
    for line in path.read_text().splitlines():
        if not line.strip():
            continue
        try:
            entry = as_dict(json.loads(line))
        except json.JSONDecodeError:
            continue
        event = entry.get("event")
        value = entry.get("counter-value")
        if not isinstance(event, str) or not isinstance(value, str):
            continue
        try:
            count = float(value)
        except ValueError:
            continue
        if event in {"duration_time", "task-clock"}:
            count *= 1e-3 if entry.get("unit") == "msec" else 1e-9
        events[event] = count
    return events


def parse_max_rss(path: Path) -> int:
    for line in path.read_text().splitlines():
        key, _, value = line.partition("=")
        if key.strip() == "max_rss_kb" and value.strip().isdigit():
            return int(value.strip()) * 1024
    return 0


def measure(
    cmd: str | Sequence[str],
    cwd: Path | None,
    env: dict[str, str] | None,
) -> CommandResult:
    with tempfile.TemporaryDirectory() as tmp:
        perf_out = Path(tmp) / "perf.json"
        time_out = Path(tmp) / "time.txt"
        prefix = [arg or str(perf_out) for arg in perf_prefix()]
        prefix += [arg or str(time_out) for arg in gnu_time_prefix()]
        measured_env = dict(os.environ if env is None else env) | {"LC_ALL": "C"}

        started = time.monotonic()
        completed = subprocess.run(
            prefix + argv(cmd),
            cwd=cwd,
            env=measured_env,
            capture_output=True,
            text=True,
        )
        elapsed = time.monotonic() - started

        events = parse_perf_events(perf_out) if perf_out.exists() else {}
        max_rss = parse_max_rss(time_out) if time_out.exists() else 0

    return CommandResult(
        returncode=completed.returncode,
        stdout=completed.stdout,
        stderr=completed.stderr,
        metrics=Metrics(
            wall_time=events.get("duration_time", elapsed),
            cpu_time=events.get("task-clock", 0.0),
            max_rss=max_rss,
            instructions=int(events.get("instructions", 0)),
        ),
    )


def run_plain(
    cmd: str | Sequence[str],
    cwd: Path | None,
    env: dict[str, str] | None,
) -> CommandResult:
    started = time.monotonic()
    completed = subprocess.run(
        argv(cmd),
        cwd=cwd,
        env=env,
        capture_output=True,
        text=True,
    )
    return CommandResult(
        returncode=completed.returncode,
        stdout=completed.stdout,
        stderr=completed.stderr,
        metrics=Metrics(time.monotonic() - started, 0.0, 0, 0),
    )


def report_command(result: CommandResult) -> None:
    outcome = "ok" if result.returncode == 0 else f"FAILED (exit {result.returncode})"
    metrics = result.metrics
    details = [f"wall: {format_duration(metrics.wall_time)}"]
    if metrics.cpu_time > 0:
        details.append(f"cpu: {format_duration(metrics.cpu_time)}")
    if metrics.max_rss > 0:
        details.append(f"rss: {format_memory(metrics.max_rss)}")
    if metrics.instructions > 0:
        details.append(f"inst: {metrics.instructions:,}")
    print(f"      -> {outcome} ({', '.join(details)})")
    print(result.output, end="")


def run_cmd(
    cmd: str | Sequence[str],
    cwd: Path | None = None,
    env: dict[str, str] | None = None,
    measure_perf: bool = False,
) -> CommandResult:
    if VERBOSE:
        shown = cmd if isinstance(cmd, str) else shlex.join(cmd)
        print(f"    $ {shown}{f' (in {cwd})' if cwd else ''}")

    measurable = measure_perf and (perf_prefix() or gnu_time_prefix())
    result = measure(cmd, cwd, env) if measurable else run_plain(cmd, cwd, env)

    if VERBOSE:
        report_command(result)
    return result


def run_checked(
    description: str,
    cmd: str | Sequence[str],
    cwd: Path | None = None,
    env: dict[str, str] | None = None,
) -> CommandResult:
    result = run_cmd(cmd, cwd=cwd, env=env)
    if result.returncode != 0:
        msg = f"{description} failed (exit {result.returncode})\n{result.output}"
        raise BuildError(msg.rstrip())
    return result


@dataclass(frozen=True)
class GitSource:
    url: str
    ref: str | None
    rev: str | None


@dataclass(frozen=True)
class LocalSource:
    path: str


Source = GitSource | LocalSource


@dataclass(frozen=True)
class StaticFile:
    path: str


@dataclass(frozen=True)
class LeanFileExport:
    path: str
    export_decls: tuple[str, ...]


@dataclass(frozen=True)
class ModuleExport:
    module: str
    export_decls: tuple[str, ...]


@dataclass(frozen=True)
class RunScript:
    command: str
    multiple: bool


TestBuild = StaticFile | LeanFileExport | ModuleExport | RunScript


def parse_source(data: dict[str, Any]) -> Source | None:
    if data.get("url"):
        return GitSource(url=data["url"], ref=data.get("ref"), rev=data.get("rev"))
    if data.get("dir"):
        return LocalSource(path=data["dir"])
    return None


def parse_build(name: str, data: dict[str, Any]) -> TestBuild:
    decls = tuple(data.get("export-decls", ()))
    if data.get("module"):
        return ModuleExport(module=data["module"], export_decls=decls)
    if data.get("run"):
        return RunScript(command=data["run"], multiple=bool(data.get("multiple")))
    if data.get("leanfile"):
        return LeanFileExport(path=data["leanfile"], export_decls=decls)
    if data.get("file"):
        return StaticFile(path=data["file"])
    msg = f"Test {name} has no 'module', 'run', 'leanfile' or 'file' field"
    raise BuildError(msg)


@dataclass(frozen=True)
class Test:
    name: str
    description: str | None
    outcome: Outcome | None
    compare_perf: bool
    skip_on_ci: bool
    source: Source | None
    pre_build: str | None
    build: TestBuild

    @property
    def yaml_file(self) -> str:
        return f"tests/{self.name}.yaml"

    @classmethod
    def parse(cls, name: str, data: dict[str, Any]) -> "Test":
        return cls(
            name=name,
            description=data.get("description"),
            outcome=data.get("outcome"),
            compare_perf=bool(data.get("compare-perf")),
            skip_on_ci=bool(data.get("skip-on-ci")),
            source=parse_source(data),
            pre_build=data.get("pre-build"),
            build=parse_build(name, data),
        )


def derived_version(data: dict[str, Any]) -> str | None:
    ref = (data.get("ref") or "").strip()
    rev = (data.get("rev") or "").strip()
    return f"{ref} ({rev[:7]})" if ref and rev else None


@dataclass(frozen=True)
class Checker:
    name: str
    version: str | None
    description: str | None
    source: Source | None
    build: str | None
    run: str
    disabled: bool
    serious: bool

    @classmethod
    def parse(cls, name: str, data: dict[str, Any]) -> "Checker":
        return cls(
            name=name,
            version=data.get("version") or derived_version(data),
            description=data.get("description"),
            source=parse_source(data),
            build=data.get("build"),
            run=data["run"],
            disabled=bool(data.get("disable")),
            serious=bool(data.get("serious", True)),
        )


def load_schema(schema_name: str) -> dict[str, Any]:
    return json.loads((PROJECT_ROOT / "schemas" / f"{schema_name}.json").read_text())


def validate_config(data: Any, schema_name: str, file_path: Path) -> None:
    try:
        jsonschema.validate(data, load_schema(schema_name))
    except jsonschema.ValidationError as e:
        location = " -> ".join(str(p) for p in e.absolute_path) or "root"
        print(f"Schema validation error in {file_path}:")
        print(f"  Path: {location}")
        print(f"  Error: {e.message}")
        print(f"  Expected: {e.validator_value}")
        print(f"  Found: {e.instance}")
        sys.exit(1)


def load_configs[T](
    directory: Path,
    schema_name: str,
    parse: Callable[[str, dict[str, Any]], T],
) -> list[T]:
    if not directory.exists():
        return []
    items = []
    for file in sorted(directory.rglob("*.yaml")):
        data = yaml.safe_load(file.read_text())
        validate_config(data, schema_name, file)
        items.append(parse(str(file.relative_to(directory).with_suffix("")), data))
    return items


def load_test_definitions() -> list[Test]:
    return load_configs(PROJECT_ROOT / "tests", "test", Test.parse)


def load_checkers() -> list[Checker]:
    checkers = load_configs(PROJECT_ROOT / "checkers", "checker", Checker.parse)
    return [checker for checker in checkers if not checker.disabled]


def matching[T](pattern: str, items: list[T], name: Callable[[T], str]) -> list[T]:
    if any(char in pattern for char in "*?[]"):
        return [item for item in items if fnmatch.fnmatch(name(item), pattern)]
    return [item for item in items if name(item) == pattern]


def prepare_source(source: Source | None, work_dir: Path, local_base: Path) -> Path:
    if work_dir.exists():
        shutil.rmtree(work_dir)
    work_dir.mkdir(parents=True)
    src_dir = work_dir / "src"

    match source:
        case GitSource(url=url, ref=ref, rev=rev):
            print(f"  Cloning {url}...")
            branch = ["--branch", ref] if ref else []
            run_checked("Clone", ["git", "clone", *branch, url, str(src_dir)])
            if rev:
                run_checked("Checkout", ["git", "checkout", rev], cwd=src_dir)
            return src_dir
        case LocalSource(path=path):
            source_dir = local_base / path
            if not source_dir.exists():
                msg = f"Source directory not found: {source_dir}"
                raise BuildError(msg)
            shutil.copytree(source_dir, src_dir)
            print(f"  Copied {source_dir} to {src_dir}")
            return src_dir
        case None:
            return work_dir


def scaffold_lean_module(lean_file: str, src_dir: Path) -> None:
    source_file = PROJECT_ROOT / lean_file
    if not source_file.exists():
        msg = f"Source file not found: {source_file}"
        raise BuildError(msg)

    toolchain = PROJECT_ROOT / "tests" / "lean-toolchain"
    if not toolchain.exists():
        msg = f"No lean-toolchain file found at {toolchain}"
        raise BuildError(msg)

    src_dir.mkdir(parents=True, exist_ok=True)
    shutil.copy(source_file, src_dir / "Test.lean")
    shutil.copy(toolchain, src_dir / "lean-toolchain")
    (src_dir / "lakefile.toml").write_text(
        'name = "test"\n\n[[lean_lib]]\nname = "Test"\n',
    )
    print(f"  Copied {source_file} to {src_dir / 'Test.lean'}")


def read_lean_toolchain(directory: Path) -> str:
    toolchain_file = directory / "lean-toolchain"
    if not toolchain_file.exists():
        msg = f"No lean-toolchain found in {directory}"
        raise BuildError(msg)
    return toolchain_file.read_text().strip()


def setup_lean4export(toolchain: str) -> Path:
    directory = toolchain.replace("/", "_").replace(":", "_")
    export_dir = PROJECT_ROOT / "_build" / "lean4export" / directory
    if export_dir.exists():
        return export_dir

    print(f"  Cloning lean4export for toolchain {toolchain}...")
    tmp_dir = export_dir.with_name(export_dir.name + ".tmp")
    if tmp_dir.exists():
        shutil.rmtree(tmp_dir)
    tmp_dir.mkdir(parents=True)

    run_checked(
        "Cloning lean4export",
        ["git", "clone", "--branch", "master", LEAN4EXPORT_REPO_URL, str(tmp_dir)],
    )
    (tmp_dir / "lean-toolchain").write_text(toolchain + "\n")

    print(f"  Building lean4export with toolchain {toolchain}...")
    run_checked("Building lean4export", "lake build", cwd=tmp_dir)

    tmp_dir.rename(export_dir)
    return export_dir


def run_lean4export(
    export_dir: Path,
    module: str,
    export_decls: tuple[str, ...],
    cwd: Path,
    out_file: Path,
) -> None:
    binary = export_dir / ".lake" / "build" / "bin" / "lean4export"
    if not binary.exists():
        msg = f"lean4export binary not found at {binary}"
        raise BuildError(msg)

    cmd = f"lake env {shlex.quote(str(binary))} {shlex.quote(module)}"
    if export_decls:
        cmd += " -- " + " ".join(shlex.quote(decl) for decl in export_decls)
    cmd += f" > {shlex.quote(str(out_file))}"
    run_checked("Export", cmd, cwd=cwd)


@dataclass(frozen=True)
class SourceLinks:
    declaration_url: str | None
    source_url: str | None


@dataclass(frozen=True)
class ExportInfo:
    lean4export_version: str | None
    lean_version: str | None
    lean_githash: str | None


@dataclass(frozen=True)
class BuiltTest:
    name: str
    file: Path
    outcome: Outcome | None
    size: int
    lines: int
    description: str | None
    compare_perf: bool
    skip_on_ci: bool
    yaml_file: str
    links: SourceLinks
    export: ExportInfo

    @property
    def size_str(self) -> str:
        return format_memory(self.size)

    @property
    def lines_str(self) -> str:
        return format_unitless(self.lines)

    @property
    def description_html(self) -> str:
        return render_markdown(self.description)

    def to_json(self) -> dict[str, Any]:
        data = dataclasses.asdict(self)
        del data["file"]
        return data

    @classmethod
    def from_json(cls, data: dict[str, Any], file: Path) -> "BuiltTest":
        return cls(
            name=data["name"],
            file=file,
            outcome=data["outcome"],
            size=data["size"],
            lines=data["lines"],
            description=data["description"],
            compare_perf=data["compare_perf"],
            skip_on_ci=data["skip_on_ci"],
            yaml_file=data["yaml_file"],
            links=SourceLinks(**data["links"]),
            export=ExportInfo(**data["export"]),
        )


def read_export_info(ndjson_file: Path) -> ExportInfo:
    with open(ndjson_file) as f:
        first_line = f.readline().strip()
    try:
        meta = as_dict(as_dict(json.loads(first_line)).get("meta"))
    except json.JSONDecodeError:
        return ExportInfo(None, None, None)
    lean = as_dict(meta.get("lean"))
    return ExportInfo(
        lean4export_version=as_dict(meta.get("exporter")).get("version"),
        lean_version=lean.get("version"),
        lean_githash=lean.get("githash"),
    )


def source_url(
    source: Source | None,
    lean_file: str | None,
    directory_prefix: str,
    git_revision: str,
) -> str | None:
    match source:
        case GitSource(url=url, rev=rev):
            repo = url.removesuffix(".git")
            return f"{repo}/tree/{rev}" if rev and "github.com" in repo else url
        case LocalSource(path=path):
            return f"{ARENA_REPO_URL}/tree/{git_revision}/{directory_prefix}{path}"
        case None:
            if lean_file:
                return f"{ARENA_REPO_URL}/blob/{git_revision}/{lean_file}"
            return None


def source_links(
    name: str,
    config_type: str,
    source: Source | None,
    lean_file: str | None,
    git_revision: str | None,
) -> SourceLinks:
    if git_revision is None:
        return SourceLinks(None, None)
    prefix = "checkers/" if config_type == "checkers" else ""
    return SourceLinks(
        declaration_url=f"{ARENA_REPO_URL}/blob/{git_revision}/{config_type}/{name}.yaml",
        source_url=source_url(source, lean_file, prefix, git_revision),
    )


def test_source_links(test: Test, git_revision: str | None) -> SourceLinks:
    lean_file = test.build.path if isinstance(test.build, LeanFileExport) else None
    return source_links(test.name, "tests", test.source, lean_file, git_revision)


@dataclass(frozen=True)
class BuildInfo:
    timestamp: str
    git_revision: str | None
    git_revision_short: str | None
    github_url: str | None
    github_run_id: str | None
    github_action_url: str | None


def github_commit_url(remote_url: str, git_revision: str) -> str | None:
    if "github.com" not in remote_url:
        return None
    repo = remote_url.removesuffix(".git")
    repo = (
        repo.split(":")[-1]
        if repo.startswith("git@")
        else repo.split("github.com/")[-1]
    )
    return f"https://github.com/{repo}/commit/{git_revision}"


@functools.cache
def get_build_metadata() -> BuildInfo:
    revision = run_cmd(["git", "rev-parse", "HEAD"])
    git_revision = revision.stdout.strip() if revision.returncode == 0 else None

    github_url = None
    if git_revision:
        remote = run_cmd(["git", "remote", "get-url", "origin"])
        if remote.returncode == 0:
            github_url = github_commit_url(remote.stdout.strip(), git_revision)

    server = os.environ.get("GITHUB_SERVER_URL")
    repository = os.environ.get("GITHUB_REPOSITORY")
    run_id = os.environ.get("GITHUB_RUN_ID")
    action_url = (
        f"{server}/{repository}/actions/runs/{run_id}"
        if server and repository and run_id
        else None
    )

    return BuildInfo(
        timestamp=datetime.datetime.now(datetime.UTC).strftime("%Y-%m-%d %H:%M:%S UTC"),
        git_revision=git_revision,
        git_revision_short=git_revision[:8] if git_revision else None,
        github_url=github_url,
        github_run_id=run_id if action_url else None,
        github_action_url=action_url,
    )


def prepare_test_work_dir(test: Test, work_base: Path) -> Path:
    src_dir = prepare_source(test.source, work_base / test.name, PROJECT_ROOT)
    if isinstance(test.build, LeanFileExport):
        scaffold_lean_module(test.build.path, src_dir)
    if test.pre_build:
        print(f"  Running pre-build: {test.pre_build}")
        run_checked("Pre-build", test.pre_build, cwd=src_dir)
    return src_dir


def export_test_module(
    test: Test,
    build: LeanFileExport | ModuleExport,
    work_base: Path,
    out_file: Path,
) -> None:
    src_dir = prepare_test_work_dir(test, work_base)
    export_dir = setup_lean4export(read_lean_toolchain(src_dir))
    module = "Test" if isinstance(build, LeanFileExport) else build.module

    print(f"  Building module {module}...")
    run_checked("Build", f"lake build {shlex.quote(module)}", cwd=src_dir)

    decls = f" ({', '.join(build.export_decls)})" if build.export_decls else ""
    print(f"  Exporting module {module}{decls}...")
    run_lean4export(export_dir, module, build.export_decls, src_dir, out_file)


def run_test_script(test: Test, command: str, work_base: Path, out: Path) -> None:
    src_dir = prepare_test_work_dir(test, work_base)
    print(f"  Running: {command}")
    run_checked("Script", command, cwd=src_dir, env=os.environ | {"OUT": str(out)})


def produce_ndjson(test: Test, work_base: Path, out_file: Path) -> None:
    match test.build:
        case StaticFile(path=path):
            source_file = PROJECT_ROOT / path
            if not source_file.exists():
                msg = f"Source file not found: {source_file}"
                raise BuildError(msg)
            shutil.copy(source_file, out_file)
            print(f"  Copied {source_file}")
        case LeanFileExport() | ModuleExport():
            export_test_module(test, test.build, work_base, out_file)
        case RunScript(command=command):
            run_test_script(test, command, work_base, out_file)


def built_test(
    test: Test,
    name: str,
    ndjson_file: Path,
    outcome: Outcome | None,
    description: str | None,
) -> BuiltTest:
    with open(ndjson_file) as f:
        lines = sum(1 for _ in f)
    return BuiltTest(
        name=name,
        file=ndjson_file,
        outcome=outcome,
        size=ndjson_file.stat().st_size,
        lines=lines,
        description=description,
        compare_perf=test.compare_perf,
        skip_on_ci=test.skip_on_ci,
        yaml_file=test.yaml_file,
        links=test_source_links(test, get_build_metadata().git_revision),
        export=read_export_info(ndjson_file),
    )


def write_json(path: Path, data: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2) + "\n")


def create_single_test(test: Test, output_dir: Path) -> None:
    output_file = output_dir / f"{test.name}.ndjson"
    output_file.parent.mkdir(parents=True, exist_ok=True)
    tmp_file = output_file.with_name(output_file.name + ".tmp")

    produce_ndjson(test, output_dir / "work", tmp_file)
    tmp_file.rename(output_file)

    stats = built_test(test, test.name, output_file, test.outcome, test.description)
    print(f"  Created {output_file} ({stats.size_str}, {stats.lines_str} lines)")
    write_json(output_dir / f"{test.name}.stats.json", stats.to_json())


def subtest_description(info_file: Path) -> str | None:
    if not info_file.exists():
        return None
    try:
        return as_dict(json.loads(info_file.read_text())).get("description")
    except json.JSONDecodeError as e:
        print(f"  Warning: Could not read {info_file}: {e}")
        return None


def create_multiple_tests(test: Test, command: str, output_dir: Path) -> None:
    final_dir = output_dir / test.name
    tmp_dir = output_dir / f"{test.name}.tmp"
    if tmp_dir.exists():
        shutil.rmtree(tmp_dir)
    tmp_dir.mkdir(parents=True)

    run_test_script(test, command, output_dir / "work", tmp_dir)

    outcomes: list[tuple[Path, Outcome]] = [
        (tmp_dir / "good", "accept"),
        (tmp_dir / "bad", "reject"),
    ]
    subtests = [
        (ndjson_file, outcome)
        for directory, outcome in outcomes
        if directory.exists()
        for ndjson_file in sorted(directory.glob("*.ndjson"))
    ]
    if not subtests:
        msg = f"No .ndjson files found in {tmp_dir}/good/ or {tmp_dir}/bad/"
        raise BuildError(msg)

    for ndjson_file, outcome in subtests:
        stats = built_test(
            test,
            f"{test.name}/{ndjson_file.stem}",
            ndjson_file,
            outcome,
            subtest_description(ndjson_file.with_suffix(".info.json")),
        )
        write_json(ndjson_file.with_suffix(".stats.json"), stats.to_json())

    if final_dir.exists():
        shutil.rmtree(final_dir)
    tmp_dir.rename(final_dir)
    print(f"  Created {len(subtests)} subtests in {final_dir}")


def create_test(test: Test, output_dir: Path) -> None:
    print(f"Creating test: {test.name} ({type(test.build).__name__})")
    output_dir.mkdir(parents=True, exist_ok=True)
    match test.build:
        case RunScript(command=command, multiple=True):
            create_multiple_tests(test, command, output_dir)
        case _:
            create_single_test(test, output_dir)


def report_totals(succeeded: int, failed: int) -> int:
    print(f"\nResults: {succeeded} succeeded, {failed} failed")
    return 0 if failed == 0 else 1


def cmd_build_test(args: argparse.Namespace) -> int:
    output_dir = PROJECT_ROOT / "_build" / "tests"
    tests = load_test_definitions()

    if args.name:
        tests = matching(args.name, tests, lambda test: test.name)
        if not tests:
            print(f"No tests found matching pattern: {args.name}")
            return 1

    if args.skip_ci:
        selected = [test for test in tests if not test.skip_on_ci]
        if len(selected) < len(tests):
            print(
                f"Skipping {len(tests) - len(selected)} test(s) due to --skip-ci flag"
            )
        tests = selected

    if not tests:
        print("No tests found.")
        return 0

    succeeded = 0
    failed = 0
    for test in tests:
        try:
            create_test(test, output_dir)
            succeeded += 1
        except BuildError as e:
            print(f"  Error: {e}")
            failed += 1
    return report_totals(succeeded, failed)


def checker_work_dir(checker: Checker, build_dir: Path) -> Path:
    root = build_dir / checker.name
    return root / "src" if checker.source else root


def build_checker(checker: Checker, build_dir: Path) -> None:
    print(f"Building checker: {checker.name} (version: {checker.version})")
    local_base = PROJECT_ROOT / "checkers"
    work_dir = prepare_source(checker.source, build_dir / checker.name, local_base)

    if checker.build:
        separator = "\n    " if "\n" in checker.build else " "
        print(
            f"  Building:{separator}{separator.join(checker.build.strip().splitlines())}"
        )
        run_checked("Build", checker.build, cwd=work_dir)

    print(f"  Checker {checker.name} built successfully")


def cmd_build_checker(args: argparse.Namespace) -> int:
    build_dir = PROJECT_ROOT / "_build" / "checkers"
    checkers = load_checkers()

    if args.name:
        checkers = matching(args.name, checkers, lambda checker: checker.name)
        if not checkers:
            print(f"No checkers found matching pattern: {args.name}")
            return 1

    if not checkers:
        print("No checkers found.")
        return 0

    succeeded = 0
    failed = 0
    for checker in checkers:
        try:
            build_checker(checker, build_dir)
            succeeded += 1
        except BuildError as e:
            print(f"  Error: {e}")
            failed += 1
    return report_totals(succeeded, failed)


@dataclass(frozen=True)
class Result:
    checker: str
    test: str
    status: Status
    correctness: Correctness
    exit_code: int
    wall_time: float
    cpu_time: float
    max_rss: int
    instructions: int
    stdout: str
    stderr: str

    @property
    def virtual_cpu_time(self) -> float:
        if self.instructions > 0:
            return self.instructions / INSTRUCTIONS_PER_SECOND
        return self.cpu_time

    @classmethod
    def from_json(cls, data: dict[str, Any]) -> "Result":
        return cls(
            checker=data["checker"],
            test=data["test"],
            status=data["status"],
            correctness=data["correctness"],
            exit_code=data["exit_code"],
            wall_time=data["wall_time"],
            cpu_time=data["cpu_time"],
            max_rss=data["max_rss"],
            instructions=data["instructions"],
            stdout=data["stdout"],
            stderr=data["stderr"],
        )


def exit_code_status(exit_code: int) -> Status:
    match exit_code:
        case 0:
            return "accepted"
        case 1:
            return "rejected"
        case 2:
            return "declined"
        case _:
            return "error"


def judge(status: Status, expected: Outcome | None) -> Correctness:
    match status:
        case "declined" | "error":
            return status
        case "accepted":
            return "correct" if expected == "accept" else "incorrect"
        case "rejected":
            return "correct" if expected == "reject" else "incorrect"


def run_checker_on_test(
    checker: Checker,
    test: BuiltTest,
    build_dir: Path,
    results_dir: Path,
) -> Result:
    if not test.file.exists():
        result = Result(
            checker=checker.name,
            test=test.name,
            status="error",
            correctness="error",
            exit_code=-1,
            wall_time=0.0,
            cpu_time=0.0,
            max_rss=0,
            instructions=0,
            stdout="",
            stderr=f"Test file not found: {test.file}",
        )
    else:
        work_dir = checker_work_dir(checker, build_dir)
        work_dir.mkdir(parents=True, exist_ok=True)
        command = run_cmd(
            checker.run,
            cwd=work_dir,
            env=os.environ | {"IN": str(test.file)},
            measure_perf=True,
        )
        status = exit_code_status(command.returncode)
        result = Result(
            checker=checker.name,
            test=test.name,
            status=status,
            correctness=judge(status, test.outcome),
            exit_code=command.returncode,
            wall_time=command.metrics.wall_time,
            cpu_time=command.metrics.cpu_time,
            max_rss=command.metrics.max_rss,
            instructions=command.metrics.instructions,
            stdout=command.stdout,
            stderr=command.stderr,
        )

    safe_test_name = test.name.replace("/", "_")
    write_json(
        results_dir / f"{checker.name}_{safe_test_name}.json",
        dataclasses.asdict(result),
    )
    return result


def load_built_tests() -> list[BuiltTest]:
    build_tests_dir = PROJECT_ROOT / "_build" / "tests"
    if not build_tests_dir.exists():
        return []

    tests = []
    for stats_file in sorted(build_tests_dir.rglob("*.stats.json")):
        ndjson_file = stats_file.with_name(
            stats_file.name.removesuffix(".stats.json") + ".ndjson",
        )
        if not ndjson_file.exists():
            print(f"Warning: No corresponding .ndjson file for {stats_file}")
            continue
        tests.append(
            BuiltTest.from_json(json.loads(stats_file.read_text()), ndjson_file),
        )

    tests.sort(key=lambda test: test.name)
    return tests


def load_results() -> dict[tuple[str, str], Result]:
    results_dir = PROJECT_ROOT / "_results"
    if not results_dir.exists():
        return {}
    results = {}
    for file in sorted(results_dir.glob("*.json")):
        result = Result.from_json(json.loads(file.read_text()))
        results[result.checker, result.test] = result
    return results


def cmd_run_checker(args: argparse.Namespace) -> int:
    build_dir = PROJECT_ROOT / "_build" / "checkers"
    results_dir = PROJECT_ROOT / "_results"
    results_dir.mkdir(parents=True, exist_ok=True)

    checkers = load_checkers()
    if args.checker:
        checkers = matching(args.checker, checkers, lambda checker: checker.name)
        if not checkers:
            print(f"No checkers found matching pattern: {args.checker}")
            return 1
    else:
        unbuilt = [c.name for c in checkers if not (build_dir / c.name).exists()]
        checkers = [c for c in checkers if (build_dir / c.name).exists()]
        if unbuilt:
            print(
                f"Skipping {len(unbuilt)} checker(s) that weren't built: "
                f"{', '.join(unbuilt)}",
            )

    tests = load_built_tests()
    if args.test:
        tests = matching(args.test, tests, lambda test: test.name)
        if not tests:
            print(f"No tests found matching pattern: {args.test}")
            return 1

    if not checkers:
        print("No built checkers found.")
        return 0
    if not tests:
        print("No built tests found.")
        return 0

    results = []
    for checker in checkers:
        for test in tests:
            print(
                f"Running {checker.name} on {test.name}...",
                end="\n" if VERBOSE else " ",
            )
            result = run_checker_on_test(checker, test, build_dir, results_dir)
            results.append(result)
            print(
                f"[{status_symbol(result.status)} "
                f"{correctness_symbol(result.correctness)} "
                f"{format_duration(result.wall_time)}]",
            )

    print("\n" + "=" * 60)
    print("Summary:")
    print("=" * 60)
    for correctness in ("correct", "incorrect", "declined", "error"):
        count = sum(1 for result in results if result.correctness == correctness)
        if count > 0:
            print(f"  {correctness}: {count} {correctness_symbol(correctness)}")

    return 0


@dataclass(frozen=True)
class CheckerStats:
    accept_correct: int
    accept_total: int
    reject_correct: int
    reject_total: int
    declined_count: int
    mathlib: Result | None

    @property
    def mathlib_instructions(self) -> int:
        return self.mathlib.instructions if self.mathlib else 0

    @property
    def mathlib_virtual_cpu_time(self) -> float | None:
        return self.mathlib.virtual_cpu_time if self.mathlib else None

    @property
    def mathlib_max_rss(self) -> int | None:
        return self.mathlib.max_rss if self.mathlib else None


@dataclass(frozen=True)
class CheckerReport:
    name: str
    version: str | None
    description_html: str
    serious: bool
    links: SourceLinks
    stats: CheckerStats


def percent_change(baseline: float, current: float, minimum: float) -> int | None:
    if baseline < minimum or current <= 0:
        return None
    return round((current - baseline) / baseline * 100)


@dataclass(frozen=True)
class ResultRow:
    test: BuiltTest
    result: Result
    official: Result | None

    @property
    def status_class(self) -> str:
        match self.result.correctness:
            case "correct":
                return ""
            case "incorrect":
                return "bg-error"
            case _:
                return "bg-warning"

    @property
    def baseline(self) -> Result | None:
        if self.test.outcome != "accept" or self.result.status != "accepted":
            return None
        if self.official is None or self.official.status != "accepted":
            return None
        return self.official

    @property
    def speed_delta(self) -> int | None:
        if self.baseline is None:
            return None
        return percent_change(
            self.baseline.virtual_cpu_time,
            self.result.virtual_cpu_time,
            MIN_COMPARABLE_CPU_TIME,
        )

    @property
    def memory_delta(self) -> int | None:
        if self.baseline is None:
            return None
        return percent_change(
            self.baseline.max_rss,
            self.result.max_rss,
            MIN_COMPARABLE_MAX_RSS,
        )

    @property
    def timed(self) -> bool:
        return (
            self.test.compare_perf
            and self.test.outcome == "accept"
            and self.result.status == "accepted"
        )


def compute_checker_stats(
    checker: Checker,
    tests: list[BuiltTest],
    results: dict[tuple[str, str], Result],
) -> CheckerStats:
    accept_correct = 0
    accept_total = 0
    reject_correct = 0
    reject_total = 0
    declined_count = 0
    mathlib = None

    for test in tests:
        result = results.get((checker.name, test.name))
        if result is None:
            continue

        if test.name == MATHLIB_TEST and result.status == "accepted":
            mathlib = result

        if result.status in {"declined", "error"}:
            declined_count += 1
        elif test.outcome == "accept":
            accept_total += 1
            accept_correct += result.status == "accepted"
        elif test.outcome == "reject":
            reject_total += 1
            reject_correct += result.status == "rejected"

    return CheckerStats(
        accept_correct=accept_correct,
        accept_total=accept_total,
        reject_correct=reject_correct,
        reject_total=reject_total,
        declined_count=declined_count,
        mathlib=mathlib,
    )


def checker_rank(report: CheckerReport) -> tuple[int, int, float, int]:
    stats = report.stats
    return (
        stats.reject_total - stats.reject_correct,
        stats.accept_total - stats.accept_correct,
        stats.mathlib_instructions or float("inf"),
        stats.declined_count,
    )


@dataclass(frozen=True)
class TarballInfo:
    tarball_size: int
    good_count: int
    bad_count: int


def create_test_tarball(tests: list[BuiltTest], output_dir: Path) -> TarballInfo:
    tarball_path = output_dir / "lean-arena-tests.tar.gz"
    good_count = 0
    bad_count = 0

    with tarfile.open(tarball_path, "w:gz") as tar:
        for test in tests:
            if test.size > MAX_TARBALL_ENTRY_SIZE or not test.file.exists():
                continue
            good = test.outcome == "accept"
            good_count += good
            bad_count += not good
            subdir = "good" if good else "bad"
            tar.add(test.file, arcname=f"{subdir}/{test.name}.ndjson")

    return TarballInfo(
        tarball_size=tarball_path.stat().st_size,
        good_count=good_count,
        bad_count=bad_count,
    )


def report_observed_rate(results: dict[tuple[str, str], Result]) -> None:
    measured = [r for r in results.values() if r.cpu_time > 0 and r.instructions > 0]
    if not measured:
        print("No instruction count measurements available for conversion rate")
        return
    rate = sum(r.instructions for r in measured) / sum(r.cpu_time for r in measured)
    print(
        f"Observed conversion rate: {format_unitless(rate)}inst/s "
        f"from {len(measured)} samples",
    )


def make_environment(templates_dir: Path, build_info: BuildInfo) -> Environment:
    env = Environment(
        loader=FileSystemLoader(templates_dir),
        autoescape=select_autoescape(),
    )
    template_globals: dict[str, Any] = env.globals
    template_globals.update(
        format_duration=format_duration,
        format_memory=format_memory,
        format_unitless=format_unitless,
        status_symbol=status_symbol,
        build_info=build_info,
        official_checker=OFFICIAL_CHECKER,
        min_comparable_cpu_time=MIN_COMPARABLE_CPU_TIME,
        instructions_per_second=INSTRUCTIONS_PER_SECOND,
    )
    return env


def render(env: Environment, template: str, output_file: Path, **data: Any) -> None:
    output_file.parent.mkdir(parents=True, exist_ok=True)
    env.get_template(template).stream(data).dump(str(output_file))
    print(f"Generated: {output_file}")


def cmd_build_site(args: argparse.Namespace) -> int:
    templates_dir = PROJECT_ROOT / "templates"
    if not templates_dir.exists():
        print(f"Templates directory not found: {templates_dir}")
        return 1

    output_dir = Path(args.outdir)
    output_dir.mkdir(parents=True, exist_ok=True)

    build_info = get_build_metadata()
    tests = load_built_tests()
    results = load_results()
    report_observed_rate(results)

    reports = sorted(
        (
            CheckerReport(
                name=checker.name,
                version=checker.version,
                description_html=render_markdown(checker.description),
                serious=checker.serious,
                links=source_links(
                    checker.name,
                    "checkers",
                    checker.source,
                    None,
                    build_info.git_revision,
                ),
                stats=compute_checker_stats(checker, tests, results),
            )
            for checker in load_checkers()
        ),
        key=checker_rank,
    )

    rows = {
        (report.name, test.name): ResultRow(
            test=test,
            result=result,
            official=results.get((OFFICIAL_CHECKER, test.name)),
        )
        for report in reports
        for test in tests
        if (result := results.get((report.name, test.name))) is not None
    }

    columns = sorted(
        (report for report in reports if report.serious),
        key=lambda report: report.name != OFFICIAL_CHECKER,
    )

    env = make_environment(templates_dir, build_info)
    render(
        env,
        "index.html",
        output_dir / "index.html",
        tests=tests,
        checkers=reports,
        columns=columns,
        rows=rows,
        tarball_info=create_test_tarball(tests, output_dir),
    )

    for report in reports:
        render(
            env,
            "checker.html",
            output_dir / "checker" / report.name / "index.html",
            checker=report,
            rows=[
                rows[key] for test in tests if (key := (report.name, test.name)) in rows
            ],
        )

    for test in tests:
        render(
            env,
            "test.html",
            output_dir / "test" / test.name / "index.html",
            test=test,
            root_path="../" * (test.name.count("/") + 2),
            rows=[
                rows[key]
                for report in reports
                if (key := (report.name, test.name)) in rows
            ],
        )

    static_dir = templates_dir / "static"
    if static_dir.exists():
        shutil.copytree(static_dir, output_dir / "static", dirs_exist_ok=True)
        print(f"Copied static files to: {output_dir / 'static'}")

    print(f"\nSite built successfully in: {output_dir}")
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="lka",
        description="Lean Kernel Arena - Tool for managing Lean kernel tests and checkers",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Print commands being executed and their stats",
    )
    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    build_test_parser = subparsers.add_parser(
        "build-test",
        help="Build test files from test definitions",
    )
    build_test_parser.add_argument(
        "name",
        nargs="?",
        help="Name or glob pattern of the test to build (default: all tests)",
    )
    build_test_parser.add_argument(
        "--skip-ci",
        action="store_true",
        help="Skip tests marked with 'skip-on-ci: true' in YAML",
    )

    build_checker_parser = subparsers.add_parser(
        "build-checker",
        help="Build checkers from checker definitions",
    )
    build_checker_parser.add_argument(
        "name",
        nargs="?",
        help="Name or glob pattern of the checker to build (default: all checkers)",
    )

    run_checker_parser = subparsers.add_parser("run", help="Run checkers on tests")
    run_checker_parser.add_argument(
        "--checker",
        help="Name or glob pattern of the checker to run (default: all checkers)",
    )
    run_checker_parser.add_argument(
        "--test",
        help="Name or glob pattern of the test to run (default: all tests)",
    )
    build_site_parser = subparsers.add_parser("build-site", help="Build the website")
    build_site_parser.add_argument(
        "--outdir",
        default="_out",
        help="Output directory for the website (default: _out)",
    )

    return parser


def main() -> int:
    global VERBOSE

    for stream in (sys.stdout, sys.stderr):
        if isinstance(stream, io.TextIOWrapper):
            stream.reconfigure(line_buffering=True, write_through=True)

    parser = build_parser()
    args = parser.parse_args()
    VERBOSE = args.verbose

    match args.command:
        case "build-test":
            return cmd_build_test(args)
        case "build-checker":
            return cmd_build_checker(args)
        case "run":
            return cmd_run_checker(args)
        case "build-site":
            return cmd_build_site(args)
        case _:
            parser.print_help()
            return 0 if args.command is None else 1


if __name__ == "__main__":
    sys.exit(main())
