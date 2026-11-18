# The `usagi` command-line tool

`usagi` parses and lints Unreal Engine 2 UnrealScript. It is built from the
`usagi-cli` crate.

```sh
cargo install --path crates/usagi-cli   # or download a binary from the Releases page
usagi lint                               # lint every .uc file under the current directory
```

## Commands

Every command that takes paths accepts files and directories. Directories are
searched for `.uc` files (in any letter case). With no paths, the current
directory is used. A file named directly is always checked, even if
`include`/`exclude` would skip it.

### `usagi lint [PATHS]...`

Parses each file permissively and reports lint problems (see
[STATIC_ANALYSIS.md](STATIC_ANALYSIS.md)). Problems that permissive parsing
recovered from are also shown, under the code `parse`.

| Flag | Meaning |
|---|---|
| `--select LINT,...` | Report these lints, replacing the configured list. |
| `--extend-select LINT,...` | Report these lints as well. |
| `--ignore LINT,...` | Don't report these lints, in addition to the configured ones. |
| `--format FORMAT` | How to print problems (see below). |
| `--statistics` | Print the number of problems and files for each lint. |
| `--fix` | Apply safe fixes to the files, then report the problems that are left. |
| `--diff` | Print the changes `--fix` would make, as a unified diff, without changing any file. |
| `--strict` | Treat recovered problems as errors, as `parse --strict` does. |
| `--include GLOB,...` / `--exclude GLOB,...` | Choose files (see below), replacing the configured lists. |

A lint selector is `ALL`, a code (`S003`), a code prefix (`S`, `S00`), a
lint name (`self-assignment`) or a category name (`suspicious`), all
case-insensitive.

#### Fixes

Problems that `--fix` can fix are marked `[*]` in the concise format, and
have a "fixable with --fix" note in the full format. Only *safe* fixes, which
never change what the code does, are applied; see
[STATIC_ANALYSIS.md](STATIC_ANALYSIS.md#fixes) for which lints have them. Only
selected lints are fixed, so `usagi lint --fix --select F001` fixes nothing
else.

`--fix` re-lints each fixed file and fixes again, up to 10 times, so that
fixes that overlapped one another are all applied. Files are written back in
the encoding they were read in, with their line endings unchanged. A fix whose
text that encoding can't store (e.g. a character above U+00FF in a Latin-1
file) is not applied.

Use `--diff` as a dry run: it prints what would change and writes nothing.

### `usagi parse [PATHS]...`

Parses each file and reports syntax errors and recovered problems.

| Flag | Meaning |
|---|---|
| `--strict` | Fail files that permissive parsing would recover (see [PERMISSIVE_PARSING.md](PERMISSIVE_PARSING.md)). |
| `--ast` | Print each file's syntax tree. |
| `--statistics` | Print how many files had each outcome: `ok`, `recovered`, `strict_error`, `syntax_error`, `panic` or `io_error`. |
| `--format`, `--include`, `--exclude` | As for `lint`. |

### `usagi rule <CODE|NAME>` / `usagi rule --all`

Explains a lint, or lists every lint.

### `usagi format`

Not implemented yet.

### Global flags

| Flag | Meaning |
|---|---|
| `--config FILE` | Use this configuration file. |
| `--no-config` | Don't read a configuration file. |
| `--color auto\|always\|never` | When to use colour. `auto` uses colour on a terminal, unless `NO_COLOR` is set. |

## Output formats

| Format | Output |
|---|---|
| `full` (default) | Each problem with the source it points at, like rustc. |
| `concise` | `path:line:column: CODE message` |
| `json` | An array of `{path, line, column, end_line, end_column, code, name, severity, message, fix}`, where `fix` is the title of the safe fix, or `null`. |
| `tsv` | `code`, `name`, `path`, `line` and `message`, separated by tabs. |
| `quiet` | Nothing. Use it with `--statistics`, or to rely on the exit code alone. |

Paths are shown relative to the current directory. Problems are sorted by
path, then by position.

Besides lint codes, a problem's code can be `parse` (a recovered problem),
`syntax` (a syntax error), `panic` (an internal error in the parser) or `io`
(the file couldn't be read).

## Exit codes

| Code | Meaning |
|---|---|
| 0 | Nothing to report. Recovered problems alone don't count, unless `--strict` is used. |
| 1 | `lint`: a lint problem, or a file that failed to parse; with `--fix`, a problem left after fixing; with `--diff`, a change to show. `parse`: a file that failed to parse. |
| 2 | A usage error, a configuration error or a missing path. |

## Configuration

`usagi` reads the first `usagi.toml` or `.usagi.toml` it finds in the current
directory or one of its parents. Every setting is optional.

```toml
# Only check files matching these globs. Default: every .uc file.
include = ["*/Classes"]
# Skip files matching these globs.
exclude = ["Engine", "UnrealGame/**/Old*.uc"]
# Treat recovered problems as errors.
strict = false

[lint]
select = ["ALL"]          # default: ALL
extend-select = []
ignore = ["F003", "naming"]

# Lint settings for some files. Every matching entry applies, in order, so a
# later entry wins over an earlier one.
[[lint.per-path]]
paths = ["Engine", "UnrealGame"]
ignore = ["ALL"]

[[lint.per-path]]
paths = ["UnrealGame/Classes/MyClass.uc"]
extend-select = ["C"]
```

Globs in a configuration file are relative to the file's directory. Globs
given as flags are relative to the current directory. They match without
regard to case, and `*` doesn't cross a `/` but `**` does. A glob that
matches a directory matches everything inside it, so `Engine` means the whole
`Engine` directory.

Flags take precedence over the file:

- `--select`, `--include` and `--exclude` replace the configured lists.
- `--extend-select` and `--ignore` add to them.
- `--strict` overrides `strict`.

An unknown key, an invalid glob or an unknown lint selector is an error
(exit code 2), reported with the file and the key.
