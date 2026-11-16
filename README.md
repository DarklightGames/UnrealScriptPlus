![example event parameter](https://github.com/DarklightGames/UnrealScriptPlus/actions/workflows/rust.yml/badge.svg)

# UnrealScriptPlus

UnrealScriptPlus is intended to provide an extension to the version of the version of UnrealScript shipped with Unreal Engine 2.

The end-goal is to allow for the addition of new language features and to allow the creation of static code-analysis tools.

This project is in the very early stages of development. More information will be added as the project develops.

## Static analysis

UnrealScriptPlus reports likely bugs, redundant code and style problems. See
[docs/STATIC_ANALYSIS.md](docs/STATIC_ANALYSIS.md) for the list of warnings and how to
choose which ones are reported.

## Legacy code

Shipped UE2 code relies on leniency in the original compiler. See
[docs/PERMISSIVE_PARSING.md](docs/PERMISSIVE_PARSING.md) for the rules the
parser follows to accept it unchanged.
