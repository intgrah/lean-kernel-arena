# Lean Kernel Arena

![Lean Kernel Arena Banner](https://raw.githubusercontent.com/leanprover/lean-kernel-arena/refs/heads/master/static/kernel-arena-banner.jpg)

A benchmarking framework for Lean kernel implementations that tests proof checkers against standardized test cases and generates comparative reports.

**<https://arena.lean-lang.org>**

## Overview

The Lean Kernel Arena provides a systematic way to:

- **Advertise** different Lean kernel implementations
- **Test** them for completeness and soundness
- **Benchmark** their performance on real-world proofs
- **Identify** edge cases and potential bugs in proof checkers
- **Facilitate** new kernel development, by providing a sequence of more interesting test cases

## Architecture

Everything lives in one Lake package.

- **Test definitions** (`Tests/*.toml`): where the Lean export data comes from, and whether it should be accepted or rejected
- **Checker definitions** (`checkers/*.toml`): how to build and run a proof checker
- **`Arena/`**: the `lka` command-line tool that builds tests, builds checkers, runs them, and assembles the site data
- **`ArenaSite/`**: the [Verso](https://github.com/leanprover/verso) site that renders the results
- **`ArenaSite/Descriptions.lean`**: the prose for every test and checker, authored in Verso markup

## Getting Started

### Development Environment

Building the arena needs:

- `elan`, to build the Lean code
- `rustc` and `cargo`, to build the Rust checkers
- GNU `time` and `perf`, to measure checker runs
- `git` and `tar`

Using Nix, `nix develop` gives you a shell with all of them.

### Running locally

```bash
# Build all tests
lake exe lka build-test

# Build all checkers
lake exe lka build-checker

# Run all checkers on all tests
lake exe lka run

# Generate the website into _out/
lake exe lka build-site

# View the result
python3 -m http.server 8880 --directory _out
```

`build-test` and `build-checker` take any number of names or globs, and `run` narrows with `--checker` and `--test`, each a comma-separated list of names or globs:

```bash
lake exe lka build-test 'perf/*' mathlib
lake exe lka run --checker nanoda,sokonanoda --test mathlib
```

`build-site` regenerates `site-data/arena.json` from `_build/` and `_results/`, elaborates the Verso site against it, renders the tutorial viewer, and packs the test tarball. To refresh only the data, use `lka site-data`.

Every command takes `-v` for the commands being run and their measurements, and `--help` for its own usage.

## Contributing

Contributions are welcome! We especially encourage:

### Contributing Tests

**We need more tests with tricky corner cases!** Tests that expose bugs or edge cases in existing checkers are particularly valuable.

To contribute a test, create a TOML file in `Tests/`, and add its description to `ArenaSite/Descriptions.lean`. Tests can be defined in several ways.

#### Module-based test (from a Lean repository)

```toml
url = "https://github.com/user/lean-project"
ref = "main"                      # git branch or tag
rev = "deadbeef"                  # git revision
module = "MyModule"               # module to export
outcome = "accept"                # or "reject" for tests that should fail
export_decls = ["myTheorem"]      # optional: export only these declarations and their dependencies
```

#### Single file test

When a full lake project is overkill and a single file suffices, use `leanfile`. The file is a module of the Lake project rooted at `Tests/`, so it is named in PascalCase and added to the `roots` list in `lakefile.lean`:

```toml
leanfile = "Tests/MyTest.lean"
outcome = "accept"
export_decls = ["myTheorem"]      # optional
```

The test's own name comes from the `.toml` path, so `tests/proj-of-prop.toml` stays the test `proj-of-prop` whatever its Lean file is called.

#### Static export file

For a hand-crafted export file, use `file`:

```toml
file = "Tests/my-export.ndjson"
outcome = "reject"
```

#### Multiple test generation

To generate many test cases from a single source, use `multiple`. This is only valid with `run`, and produces `.ndjson` files organised into `good/` and `bad/` subdirectories:

```toml
dir = "my-test-project"           # or url/ref/rev for git repos
multiple = true
run = """
lake clean
lake build MyProject
"""
```

Your `run` command writes into the directory `$OUT`, as either `good/<name>.ndjson` or `bad/<name>.ndjson`. A `<name>.info.json` file next to it with `{"description": "…"}` supplies that subtest's description.

This is useful for systematic testing across many related scenarios, or for tutorial-style suites.

### Contributing Checkers

We welcome more alternative kernel implementations, including incomplete ones, especially if they explore a particular corner of the design space (e.g. trimmed for performance, simplicity, verifiability, using a particular term representation, type checking or reduction strategy or a different host language).

The following resources may be useful:

* The thesis [The Type Theory of Lean](https://github.com/digama0/lean-type-theory/releases) by Mario Carneiro is a thorough description of Lean's theory.
* The book [Type Checking in Lean4](https://ammkrn.github.io/type_checking_in_lean4/) by Chris Bailey has good advice on writing a Lean kernel.
* On the [arena website](https://arena.lean-lang.org/) you can download a zipfile with the arena tests (excluding large ones).
* The [source of the tutorial tests](https://github.com/leanprover/lean-kernel-arena/blob/master/tutorial/Tutorial.lean) suggests a sequence in which to implement tests.

To add a new checker:

1. Create a TOML file in `checkers/`.
2. Add its description to `ArenaSite/Descriptions.lean`.

Example:

```toml
version = "1.0.0"
url = "https://github.com/user/my-checker"
ref = "main"
rev = "deadbeef"
build = "cargo build --release"
run = "./target/release/my-checker < $IN"
```

Optional keys: `dir` (a directory under `checkers/`, instead of `url`), `disable`, `serious` (set to `false` to keep the checker out of the main comparison tables), and `threads`.

The `run` command receives the test file path via the `$IN` environment variable, in the NDJSON-based format created by [`lean4export`](https://github.com/leanprover/lean4export). (At the time of writing, the [format is still in flux](https://github.com/leanprover/lean4export/issues/3).)

**Exit codes:**

- `0`: Proof accepted (valid)
- `1`: Proof rejected (invalid)
- `2`: Declined (checker cannot handle this proof)

  A declined test is simply ignored for the purpose of completeness and correctness. For example, a checker that does not support `native_decide` can decline to process a proof involving the `Lean.trustCompiler` axiom. This is different from rejecting the proof (you are not claiming that the proof is not valid) or erroring out (which indicates a bug in the checker).

- anything else: an error in the checker

If it is already known that a checker cannot handle a test, and running it would just waste time, the checker YAML can list that test in the `declines` field (a test name or list of test names). Such tests are recorded as declined without running the checker at all.

The arena does not automatically update the checkers; please submit new releases manually.

## Fair Play

Checkers are not run in a sandbox. We assume good faith from all contributors. The goal is to collaboratively improve Lean kernel implementations, not to exploit the test environment. Malicious submissions will be rejected.

## On `Init.Prelude`

The official Lean kernel assumes that `Init.Prelude` is **the** prelude shipped with Lean, and does not support other declarations here. Therefore the tests in the arena satisfy that declarations from `Init.Prelude` are either completely absent, or come from an official release or release candidate. The lean version in the test header can be used to recognize the version, should that be useful to some checker. Checkers are free to do additional checks here, but are not expected to accept or reject declarations that are not part of an official release.

Some checkers perform extra checks here. If there is interest in testing this functionality, we can label such tests and let the official kernel decline handling them.

## Questions?

Open an issue or discussion on GitHub, or [contact Joachim Breitner on zulip](https://leanprover.zulipchat.com/#narrow/dm/470149-Joachim-Breitner).
