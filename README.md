![example event parameter](https://github.com/DarklightGames/usagi/actions/workflows/rust.yml/badge.svg)

# usagi

usagi is intended to provide an extension to the version of the version of UnrealScript shipped with Unreal Engine 2.

The end-goal is to allow for the addition of new language features and to allow the creation of static code-analysis tools.

This project is in the very early stages of development. More information will be added as the project develops.

## Installation

Download `usagi` for Linux or Windows from the
[Releases](https://github.com/DarklightGames/usagi/releases) page, or
build it from source:

```sh
cargo install --path crates/usagi-cli
```

## Usage

```sh
usagi lint                          # lint every .uc file under the current directory
usagi lint --select C,S MyPackage   # only correctness and suspicious lints
usagi parse --statistics .          # check that everything parses
usagi rule S003                     # explain a lint
```

Settings can be kept in a `usagi.toml`:

```toml
exclude = ["Engine", "UnrealGame"]

[lint]
ignore = ["naming"]
```

See [docs/CLI.md](docs/CLI.md) for every command, flag and setting.

## Static analysis

usagi reports likely bugs, redundant code and style problems. See
[docs/STATIC_ANALYSIS.md](docs/STATIC_ANALYSIS.md) for the list of warnings and how to
choose which ones are reported.

## Legacy code

Shipped UE2 code relies on leniency in the original compiler. See
[docs/PERMISSIVE_PARSING.md](docs/PERMISSIVE_PARSING.md) for the rules the
parser follows to accept it unchanged.
