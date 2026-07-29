# Lean Kernel Arena Developer Guide

## Architecture Overview

This is a **benchmarking framework for Lean kernel implementations** that tests proof checkers against standardized test cases and generates comparative reports. Everything is one Lake package.

### Core Components
- **`Arena/`** + **`Main.lean`**: the `lka` CLI — builds tests, builds checkers, runs them, assembles site data
- **`ArenaSite/`** + **`ArenaSite.lean`**: the Verso site (`arena-site` executable) rendering the results
- **`ArenaSite/Descriptions.lean`**: prose for every test and checker, authored in Verso markup
- **Test definitions** (`Tests/*.toml`): where the export data comes from, and the expected outcome
- **Checker definitions** (`checkers/*.toml`): how to build and run a proof checker
- **Nix environment**: reproducible toolchain via `flake.nix`

### Data Flow
1. **Tests** → `.ndjson` export files plus `.stats.json` metadata in `_build/tests/`
2. **Checkers** → built in `_build/checkers/`
3. **Results** → one JSON per (checker, test) in `_results/`
4. **Site data** → `site-data/arena.json`, read by `ArenaSite` at elaboration time
5. **Website** → static site in `_out/`

## Essential Workflows

If `direnv` is not active, prepend commands with `nix develop -c`.

```bash
lake exe lka build-test [names...]     # Generate test exports
lake exe lka build-checker [names...]  # Build checkers
lake exe lka run                       # Execute checkers on tests
lake exe lka site-data                 # Refresh site-data/arena.json only
lake exe lka build-site                # Regenerate data, site, tutorial viewer, tarball
```

`build-test` and `build-checker` accept any number of names or globs; `run` narrows with the comma-separated `--checker` and `--test`. `-v` traces commands and measurements, and goes after the subcommand. The command tree is built with [lean4-cli](https://github.com/leanprover/lean4-cli), so `--help` works at every level.

### Key Conventions
- **Exit codes determine status**: 0=accepted, 1=rejected, 2=declined, anything else=error
- **TOML-driven configuration**: one file per test and per checker
- **Name derivation**: the path under `Tests/` or `checkers/` without the extension is the name, so `Tests/Perf/AppLam.toml` is the test `perf/app-lam`. The Lean file it points at is a module of the `Tests/` Lake project and is PascalCase — test identity and module name are deliberately decoupled
- **Descriptions live in Lean**, not in the config files: add an entry to `ArenaSite/Descriptions.lean` and to the lookup at the bottom of that file
- **Config validity is a type**: `Arena.Source` and `Arena.Production` encode which field combinations are legal; the TOML decoder in `Arena/Config.lean` rejects the rest

### Result Data Structure
Results stored as `_results/{checker}_{test}.json`:
```json
{
  "checker": "checker-name",
  "test": "test-name",
  "status": "accepted|rejected|declined|error",
  "correctness": "correct|incorrect|declined|error",
  "exit_code": 0,
  "wall_time": 1.23,
  "cpu_time": 1.18,
  "max_rss": 1048576,
  "instructions": 0,
  "stdout": "...",
  "stderr": "..."
}
```

### Site Architecture
- `Arena/SiteData.lean` writes `site-data/arena.json`; `ArenaSite/Data.lean` reads it in `TermElabM`
- `ArenaSite/Pages.lean` declares one top-level `Part Page` per page via the `generate_arena_pages` command, then `checker_pages%` / `test_pages%` reference those constants. Splicing the page contents inline instead overflows the code generator.
- Times cross the JSON boundary as integer nanoseconds; `Float` has no `Quote` instance
- URL structure: `checker/{name}/`, `test/{name}/`, with intermediate `test/{group}/` index pages for nested test names

## Critical Integration Points

### Lean4Export Dependency
Tests using `module` or `leanfile` auto-clone and build `lean4export`. Builds are cached per toolchain in `_build/lean4export/{toolchain}/`, so tests on different Lean versions coexist.

### File System Layout
```
_build/tests/{name}.ndjson        # Generated test data
_build/tests/{name}.stats.json    # Size, line count, exporter metadata
_build/lean4export/{toolchain}/   # Per-toolchain lean4export builds
_build/checkers/{name}/           # Checker build directories
_build/work/tests/{name}/         # Scratch clones and checkouts
_results/{checker}_{test}.json    # Individual run results
site-data/arena.json              # Site input
_out/                             # Generated website
```

### GitHub Actions Integration
- Manual trigger workflow: `build-and-deploy.yml`
- Nix environment setup via `cachix/install-nix-action`
- GitHub Pages deployment from `_out/`

## Project-Specific Patterns

### Verbose Mode
`Arena.verboseRef` gates command tracing in `Arena.run`, including perf and GNU time measurements. Each subcommand declares its own `-v` flag: lean4-cli does not give a subcommand access to its parent's parsed flags.

### Error Handling Strategy
- Failures are thrown as `IO.Error`; `forEachReporting` in `Main.lean` catches per item and reports a summary count
- A missing test file yields a recorded result with status `error`
