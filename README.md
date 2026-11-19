![build status](https://github.com/DarklightGames/usagi/actions/workflows/rust.yml/badge.svg)

# usagi

`usagi` is a fast linter, build tool and toolkit for [**U**nreal**S**cript](https://unreal.fandom.com/wiki/UnrealScript), the
scripting language of Unreal Engine 2.

# Components

| Component | Crate | Responsibilities |
|-----------|-------|------------------|
| Library | [`usagi-core`](crates/usagi-core) | Reads UnrealScript source files and turns them into a syntax tree, then analyses that tree to find problems and suggest fixes. Other tools can build on it. |
| CLI | [`usagi-cli`](crates/usagi-cli) | The `usagi` command. Finds your files, reads `usagi.toml`, runs the library over them, prints the problems it finds and applies fixes when you ask it to. Also builds packages, for a mod or the game itself: it works out which are out of date and compiles them with UCC. |

> usagi is still young. Linting, parsing and building work today; a code
> formatter and other tools are planned.

## Installation

Download `usagi` for Linux or Windows from the
[Releases](https://github.com/DarklightGames/usagi/releases) page, or install
it from [crates.io](https://crates.io/crates/usagi-cli) with
[Rust](https://rustup.rs):

```sh
cargo install usagi-cli
```

The parser and lints are also available as a library,
[`usagi-core`](https://crates.io/crates/usagi-core).

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

## Building

`usagi build` compiles packages with UCC, the UE2 command-line tool (run
under [Wine](https://www.winehq.org) on Linux). It only rebuilds the packages
whose sources changed and the packages after them, and it parses the sources
first, so syntax errors are caught before UCC runs. The packages are the
`EditPackages` in the INI that UCC reads.

To build the game's own packages, name the INI, and lock the packages that
must never be rebuilt:

```toml
[build]
ini = "MyGame.ini"
locked = ["Core", "Engine"]
```

To build a mod instead, point the game directory's `usagi.toml` at it. Locking
everything keeps the game's packages in `System` safe; the mod's own packages
still build:

```toml
[build]
mod = "MyMod"
locked = ["*"]
```

and keep the mod's own settings in `MyMod/usagi.toml`:

```toml
include = ["MyMod_*"]

[build]
version = "1.0.0"
version-class = "MyMod_Engine/Classes/MyModVersion.uc"   # optional
```

The compiled packages and localization files go into the mod's `System`
folder. Then, from the game directory:

```sh
usagi build             # compile what changed
usagi build --dumpint   # also export localization (.int) files
usagi build --clean     # rebuild everything
```

## Documentation

- [CLI reference](docs/CLI.md) — every command, flag and setting, including
  [`usagi build`](docs/CLI.md#usagi-build), [locked packages](docs/CLI.md#locked-packages)
  and [mod configuration](docs/CLI.md#mod-configuration)
- [Lints](docs/STATIC_ANALYSIS.md) — what usagi checks and how to choose which lints run
- [Legacy code](docs/PERMISSIVE_PARSING.md) — how usagi handles code that relies on old compiler leniency

## License

See [LICENSE](LICENSE).
