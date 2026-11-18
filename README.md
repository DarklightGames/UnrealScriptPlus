![build status](https://github.com/DarklightGames/usagi/actions/workflows/rust.yml/badge.svg)

# usagi

`usagi` is a fast linter and toolkit for [**U**nreal**S**cript](https://unreal.fandom.com/wiki/UnrealScript), the scripting
language of Unreal Engine 2.

# Components

| Component | Crate | Responsibilities |
|-----------|-------|------------------|
| Library | [`usagi-core`](crates/usagi-core) | Reads UnrealScript source files and turns them into a syntax tree, then analyses that tree to find problems and suggest fixes. Other tools can build on it. |
| CLI | [`usagi-cli`](crates/usagi-cli) | The `usagi` command. Finds your files, reads `usagi.toml`, runs the library over them, prints the problems it finds and applies fixes when you ask it to. |

> usagi is still young. Linting and parsing work today; a code formatter and
> other tools are planned.

## Installation

Download `usagi` for Linux or Windows from the
[Releases](https://github.com/DarklightGames/usagi/releases) page, or build it
from source with [Rust](https://rustup.rs):

```sh
cargo install --path crates/usagi-cli
```

## Getting started

Run usagi from the root of your source tree:

```sh
usagi lint              # check every .uc file under the current directory
usagi lint --fix        # apply safe fixes, then report what's left
usagi lint --diff       # preview the fixes without changing any file
usagi rule S003         # explain what a lint means
usagi rule --all        # list every lint
```

To keep your settings with your project, add a `usagi.toml`:

```toml
exclude = ["Engine", "UnrealGame"]

[lint]
ignore = ["naming"]
```

## Documentation

- [CLI reference](docs/CLI.md) — every command, flag and setting
- [Lints](docs/STATIC_ANALYSIS.md) — what usagi checks and how to choose which lints run
- [Legacy code](docs/PERMISSIVE_PARSING.md) — how usagi handles code that relies on old compiler leniency

## License

See [LICENSE](LICENSE).
