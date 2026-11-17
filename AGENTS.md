# AGENTS.md

Guidance for AI coding agents working on this repository.

## Project

UnrealScriptPlus is a Cargo workspace for parsing Unreal Engine 2
UnrealScript and running static analysis on it:

| Crate | Contents |
|---|---|
| `crates/usagi-core` | The library (`usagi_core`): parser, AST, symbol table and lints. |

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
| `examples/parse_file.rs` | Parses one file and prints its problems. |
| `examples/parse_corpus.rs` | Parses every `.uc` file under a directory and writes a TSV report. |

`defaultproperties` blocks aren't parsed by the grammar. They are parsed by the
`t3d` crate (a git dependency pinned by `rev` in `Cargo.toml`).

## Commands

```sh
cargo build --workspace --all-targets   # should finish with no warnings
cargo test --workspace      # all tests
cargo test -p usagi-core --example parse_corpus   # the corpus tool's own tests (not run by `cargo test`)
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
cargo run --release --example parse_corpus -- [--strict] [--lints lints.tsv] [--include <pattern>]... <root> report.tsv
```

Every file should still parse. Shipped engine code is treated as read-only:
if it doesn't parse, fix the parser, not the code. Source files may be
Windows-1252 rather than UTF-8.

## Permissive parsing

UE2 accepts code that a strict reading of UnrealScript rejects, and legacy code
relies on it. [`docs/PERMISSIVE_PARSING.md`](docs/PERMISSIVE_PARSING.md) lists
every such rule. It covers both recovered problems, which strict mode rejects,
and quirks that are accepted silently, and it explains how to add a new rule.
**Keep it up to date:** whenever the parser starts accepting or recovering
from something new, add it to that document in the same change, together with
a regression test in `tests/programs.rs`.

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
   and a one-line summary. Codes are stable, so never reuse or renumber one.
2. Report it from `src/visitor.rs` with `visitor.report(Lint::..., message, span)`.
   Make the span cover only the offending text. For anything that depends on
   names, use the symbol table (`visitor.symbols`). Names the table can't
   resolve, such as inherited members, are unknown, not errors.
3. Add tests to `tests/lints.rs` for code that should and shouldn't trigger the lint.
4. Run the corpus with `--lints`, read through the hits, and add exemptions
   for false positives (with a test for each).
5. Add the lint to `docs/STATIC_ANALYSIS.md`.
