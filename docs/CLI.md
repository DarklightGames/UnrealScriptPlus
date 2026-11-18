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

### `usagi build`

Compiles a mod's out-of-date packages with UCC (`Editor.MakeCommandlet`).
On systems other than Windows, UCC is run with Wine.

The game directory (`root`) contains `System/UCC.exe`. The mod is the
folder `<root>/<Mod>`, and its packages are the `+EditPackages` in
`<root>/<Mod>/System/Default.ini`. Each package's sources are in
`<root>/<Package>/Classes`.

A build does the following:

1. Writes the version class, if the mod's `usagi.toml` sets
   `build.version-class`. It isn't rewritten if it's unchanged.
2. Removes the mod's packages from `<root>/System`. UCC loads them from
   the mod's `System` folder, so a copy left in `<root>/System` by an
   interrupted build would be loaded first.
3. If `Default.ini` changed since the last build (or there was no build yet),
   or `--clean` is given, rebuilds every package and exports localization. In
   `<root>/<Mod>/System/<Mod>.ini`, the arrays that `Default.ini` changes with
   `+Key=`/`-Key=` (`EditPackages`, `ServerPackages`, `Paths`, ...) are
   recomputed from the game's `System/Default.ini`. Every other setting in the
   file is kept. If the game has no `System/Default.ini`, `<Mod>.ini` is
   removed, and UCC makes a new one.
4. Fails if two files with the same name (in any letter case) are in the
   mod's `Paths`.
5. Finds the out-of-date packages, in `EditPackages` order. Packages that are
   already compiled in `<root>/System` (the game's own) are skipped. A
   package is out of date when its sources changed: a `.uc` file was added,
   removed, renamed or edited. Every package after an out-of-date package is
   rebuilt too, since it may depend on it, unless `--ignore-dependencies` is
   given.
6. Parses the sources of the packages it will compile, and stops if a file
   doesn't parse, unless `--no-check` is given. `strict` applies.
7. Removes the out-of-date packages and runs UCC. UCC's progress messages
   are hidden unless `--verbose` is given.
8. Moves the compiled packages to `<root>/<Mod>/System`.
9. With `--dumpint`, exports the localization (`.int` files) of the
   packages whose sources changed. The exports run in parallel; their
   output is shown only for the ones that fail, or with `--verbose`. An
   export succeeds when it writes the `.int` file and UCC reports no errors,
   except one: DumpInt treats its own name on the command line as a package to
   export, so it always reports `No packages found matching
   Editor.DumpIntCommandlet!`. That error is ignored and not shown. The
   `.int` files are moved to `<root>/<Mod>/System`. `System/UCC.log` is
   restored to the log of the build, for editors that read errors from it.
10. Records each compiled package's sources in
    `<root>/<Mod>/.usagi-build.toml`. A package that failed to compile isn't
    recorded, so it's rebuilt next time.

| Flag | Meaning |
|---|---|
| `--mod NAME` | The mod to build. Overrides `build.mod`. |
| `--root DIR` | The game directory. Overrides `build.root`. |
| `--clean` | Rebuild every package and export localization. |
| `--dumpint` | Export localization for the packages that changed. |
| `--debug` | Compile debug packages, for UDebugger. |
| `--ignore-dependencies` | Don't rebuild the packages after one that changed. |
| `--no-check` | Don't parse the sources before compiling. |
| `-v`, `--verbose` | Show all of UCC's output. |

It ends with `Build: N succeeded, N failed, 0 skipped, N up-to-date`.

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
| 1 | `build`: a package failed to compile, or a source file failed to parse. |
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

# Settings for `usagi build`.
[build]
mod = "MyMod"             # the mod to build when --mod isn't given
root = "."                # the game directory. Default: this file's directory
wine = "wine"             # runs UCC.exe, except on Windows. Default: wine
```

Globs in a configuration file are relative to the file's directory (except in a
mod's file; see above). Globs
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

### Mod configuration

Each mod can have its own configuration file, `<root>/<Mod>/usagi.toml` (or
`.usagi.toml`). It's used together with the configuration found as usual (the
game directory's): by `usagi build` for the mod being built, and by every
other command for the mod named in `build.mod`. It takes every setting except
`build.mod` and `build.root`:

```toml
include = ["MyMod_*"]
strict = false

[lint]
ignore = ["F003"]

[build]
version = "1.2.3-beta"    # MAJOR.MINOR.PATCH[-PRERELEASE]
# Generate a class with `version` and the Git commit and branch at this path,
# relative to the game directory. Requires `version`.
version-class = "MyPackage/Classes/MyBuildManifest.uc"
wine = "wine"
```

The game directory's file then needs only:

```toml
[build]
mod = "MyMod"
```

`include`, `exclude`, `strict`, `lint.select` and `build.wine` in the mod's
file replace the game directory's; `lint.extend-select`, `lint.ignore` and
`lint.per-path` are added to them. Globs in both files are relative to the
game directory. `version` and `version-class` can only be set in a mod's
file, and `mod` and `root` only in the game directory's; either mistake is an
error. Run `usagi` from the game directory or below it, outside the mod
folder, so that the game directory's configuration is the one found.
