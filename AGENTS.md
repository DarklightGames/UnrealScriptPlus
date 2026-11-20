# AGENTS.md

Guidance for AI coding agents working on this repository.

## Project

usagi is a Cargo workspace for parsing Unreal Engine 2 UnrealScript, running
static analysis on it, building mods with UCC, and reading package files:

| Crate | Contents |
|---|---|
| `crates/usagi-core` | The library (`usagi_core`): parser, AST, symbol table and lints. |
| `crates/usagi-pkg` | The package reader (`usagi_pkg`). See [Packages](#packages). |
| `crates/usagi-cli` | The `usagi` binary: `parse`, `lint`, `rule`, `build` (in `src/build/`), `pkg` (in `src/pkg.rs`) and configuration files. Its tests are in `tests/cli.rs`. |

Paths below are relative to `crates/usagi-core`.

| Path | Contents |
|---|---|
| `src/UnrealScript.pest` | The grammar (pest). |
| `src/parser.rs` | Builds the AST from the grammar's parse tree; `parse_program`/`parse_program_with`. |
| `src/ast.rs` | The AST. |
| `src/symbols.rs` | The per-file symbol table: declarations, scopes and resolved references. |
| `src/visitor.rs` | The lint checks. |
| `src/lint.rs` | The lint registry (codes, names, categories, severities) and `LintSelection`. |
| `tests/` | Integration tests. `tests/lints.rs` covers the lints and `tests/symbols.rs` the symbol table. |

`defaultproperties` blocks aren't parsed by the grammar. They are parsed by the
`t3d` crate (on crates.io).

## Commands

```sh
cargo build --workspace --all-targets   # should finish with no warnings
cargo test --workspace      # all tests
cargo fmt
cargo llvm-cov --summary-only   # test coverage (needs cargo-llvm-cov)
```

`src/parser.rs` and `src/visitor.rs` have 100% line, region and function
coverage from the tests alone (not counting the corpus). Keep it that way: test
every new branch, and when the grammar makes a branch impossible, restructure
the code (e.g. an infallible helper, or `expect` stating the guarantee) rather
than leaving a branch that no test can reach.

Validate parser and lint changes against a large body of real UnrealScript
(such as a UE2 game's source tree):

```sh
cargo build --release -p usagi-cli
cd <root>
usagi parse --no-config --format quiet --statistics .   # every file should parse
usagi lint --no-config --format tsv . > lints.tsv       # then diff against a previous run
```

Every file should still parse. Shipped engine code is treated as read-only:
if it doesn't parse, fix the parser, not the code. Source files may be
single-byte (Latin-1, as UCC reads them) or UTF-16, not UTF-8; see
`crates/usagi-core/src/source.rs`.

## Permissive parsing

UE2 accepts code that a strict reading of UnrealScript rejects, and legacy code
relies on it. [`docs/PERMISSIVE_PARSING.md`](docs/PERMISSIVE_PARSING.md) lists
every such rule. It covers both recovered problems, which strict mode rejects,
and quirks that are accepted silently, and it explains how to add a new rule.
**Keep it up to date:** whenever the parser starts accepting or recovering
from something new, add it to that document in the same change, together with
a regression test in `tests/programs.rs`.

## The CLI

**Keep [`docs/CLI.md`](docs/CLI.md) up to date.** It documents every command,
flag, output format, exit code and configuration setting of `usagi`. Update it
in the same change as the CLI, and add a test to
`crates/usagi-cli/tests/cli.rs`.

### `usagi build`

`src/build/` holds the build command: `mod.rs` (the steps, in order, for a
mod or for the game itself without one, and the `build.locked` checks),
`ini.rs` (UE2 `.ini` files and how a mod's `Default.ini` changes the game's),
`ucc.rs` (running UCC), `manifest.rs` (the `.usagi-build.toml` record) and
`version.rs` (the generated version class). Its CLI tests run a fake
`UCC.exe`, a shell script run by `sh` in place of Wine, that imitates what the
real one does to the file system with `-mod=` and with `-ini=`, including its
quirks (see `FAKE_UCC` in `tests/cli.rs`). Unlike the real one, it matches
package file names case-sensitively. When UCC turns out to behave differently, change the fake to
match and add a test.

A build changes the game directory: it deletes and moves packages, rewrites
`<Mod>.ini`, writes the version class, and without a mod, recompiles packages
in the game's own `System` folder. Never try out a build on a real game
install; use a scratch copy (copy `System/Default.ini`, the mod's `System`
folder and the `.ini` files, and symlink the source package folders).

Releases are built by `.github/workflows/release.yml` when a `v*` tag is
pushed. It also publishes `usagi-core`, `usagi-pkg` and then `usagi-cli` to
crates.io, so before tagging, set `version` in the root `Cargo.toml` to match
the tag, and the `usagi-core` and `usagi-pkg` dependencies' `version` in
`crates/usagi-cli/Cargo.toml` to the same.

## Packages

`crates/usagi-pkg` reads UE2 package files. **Keep
[`docs/PACKAGE_FORMAT.md`](docs/PACKAGE_FORMAT.md) up to date:** it documents
the binary format, with references to the engine source, and the crate
follows it. When the crate reads something new, or turns out to differ from the
document, update the document in the same change.

| Path | Contents |
|---|---|
| `src/summary.rs` | The package summary (`Summary`), its GUID and generations. |
| `src/tables.rs` | The name, import and export tables, object references (`ObjectRef`), and the checks the engine would fail on. |
| `src/flags.rs` | Package flags and object flags. |
| `src/uz2.rs` | Decompressing and compressing `.uz2` files. |
| `src/edit.rs` | Changing package files in place (`set_package_flags`). |
| `src/package.rs` | `Package`: reads a file (decompressing a `.uz2`), and looks up names, paths and outers. |
| `src/reader.rs` | The byte cursor: integers, compact indices and strings. |
| `src/error.rs` | `Error`. |
| `tests/` | Integration tests; `tests/common/mod.rs` writes packages (summary, tables and data) and `.uz2` files. |

Read the package exactly as the engine does (`ULinkerLoad` in
`Core/Src/UnLinker.cpp`), including what it accepts that looks wrong, and
reject only what it would fail on. The crate has 100% line, region and
function coverage from its own tests (`cargo llvm-cov --summary-only -p
usagi-pkg --all-features`); keep it that way, as for the parser.

Objects are addressed UnrealScript-style, as the SDK and UnrealScript do:
`Class'Package.Group.Name'` (e.g. `Texture'MyPkg.Walls.Brick'`). Use this
form wherever the CLI prints an object reference. Where it takes one, the
class is optional (`MyPkg.Walls.Brick`), since the path alone identifies the
object.

Tests write their packages as bytes: there are no game files in the
repository. Check changes against a real game's packages (every one should
read) on a local install, which is read-only: never write to it.

## Static analysis

**Keep [`docs/STATIC_ANALYSIS.md`](docs/STATIC_ANALYSIS.md) up to date.** It lists every
kind of static analysis warning: code, name, severity and what it reports,
including notable exemptions. Update it in the same change whenever a lint is
added, removed or renamed, or when what it reports or exempts changes. The
`every_lint_is_documented` test in `tests/lints.rs` fails if a lint is missing
from the list, or if the list has a code that no longer exists.

To add a lint:

1. Add it to the `lints!` table in `src/lint.rs` with the next free code in
   its category (`C` correctness, `S` suspicious, `P` performance,
   `R` redundancy, `N` naming, `F` formatting), a kebab-case name, a severity
   whether its problems have a fix (`Always`, `Sometimes` or `Never`) and a
   one-line summary. Codes are stable, so never reuse or renumber one.
2. Report it from `src/visitor.rs` with `visitor.report(Lint::..., message, span)`,
   or `visitor.report_with_fix(...)` with a `Fix` (see `src/fix.rs`). A safe
   fix must never change what the code does, or lose or orphan a comment:
   use `visitor.deletion(...)`, which offers no fix when a comment is on the
   deleted lines, and `visitor.has_comment_in(...)`; test it with `assert_fix` in
   `tests/lints.rs`, and check it on a copy of the corpus with `usagi lint
   --diff` and `--fix`, never on the corpus itself.
   Make the span cover only the offending text. For anything that depends on
   names, use the symbol table (`visitor.symbols`). Names the table can't
   resolve, such as inherited members, are unknown, not errors.
3. Add tests to `tests/lints.rs` for code that should and shouldn't trigger the lint.
4. Run the corpus with `--lints`, read through the hits, and add exemptions
   for false positives (with a test for each).
5. Add the lint to `docs/STATIC_ANALYSIS.md`.
