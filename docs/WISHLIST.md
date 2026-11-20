# Wishlist

Ideas that aren't implemented yet. For the lints that exist, see
[STATIC_ANALYSIS.md](STATIC_ANALYSIS.md).

## Static analysis

### Per file

* Non-exhaustive `switch` on an `enum` value, for enums declared in the file.
* `[]` instead of `()` as the array index in a `defaultproperties` target.
* Statements (`#exec`, `const`) before the class declaration. Parsing this
  is an accepted quirk (see [PERMISSIVE_PARSING.md](PERMISSIVE_PARSING.md)), so
  this would be a lint, not a grammar change.
* More than one `defaultproperties` block. UE2 accepts it, but the behaviour is
  unreliable.
* More than one `default:` in a `switch`. UE2 accepts it, and default cases
  anywhere in the switch.
* The wrong form of `return`: `return X;` in a function with no return type, or
  a bare `return;` in a function that has one.
* A `goto` to a label that isn't declared in the state.
* Flow-sensitive "value assigned is never read" (R010 counts any read anywhere).
* Unused parameters of `private` and functions.

### Needs a project-wide symbol table

* Redundant `defaultproperties` values: a value that a subclass sets to the
  same value its parent already has.
* A non-`Actor` `Object` that holds references to `Actor`s.
* Exhaustive `switch` checking for enums declared in other classes.
* A `config` variable in a class that isn't `config` anywhere in its hierarchy.
* A `defaultproperties` key that isn't declared anywhere in the hierarchy.
* `simulated` outside an `Actor` subclass.
* Argument types checked against function signatures, e.g. a string passed to
  `GotoState`.

## CLI

* Inline suppression comments, e.g. `// usagi: ignore[S008]`.
* `usagi format`.
* Unsafe fixes for an editor to offer.
* SARIF output, for GitHub code scanning.

## Testing against the engine

* Check what UnrealScript actually does by running it in the engine. usagi
  would generate UnrealScript test classes (e.g. to pin down operator
  precedence, or whether a lint's fix changes behavior), then either run them
  with UCC itself or hand them to the user to run, and read the results back
  from the log.

## Packages

* Write a byte whose property is an enum as the enum's value
  (`LODSet=LODSET_Interface`), as UnrealEd does: the declaration now gives
  the enum.
* Fill in properties that aren't stored from the class's defaults, which are
  a chain: a class's defaults are only those that differ from its
  superclass's (see [ASSET_PIPELINE.md](ASSET_PIPELINE.md)).

## A compiler

* Compile UnrealScript to packages without UCC: see
  [COMPILER.md](COMPILER.md).

## A persistent runtime

* A usagi runtime that loads a game's packages (and, potentially, its source)
  into memory once, up front, and keeps indexes and caches: classes and their
  defaults, where each object is, who refers to what. Commands now read every
  file they need each time they run; exporting with class defaults and
  cross-package references (see [ASSET_PIPELINE.md](ASSET_PIPELINE.md)) would
  read many. The same runtime could serve an interactive mode, a TUI, and a
  language server.

## Build

* Clean up after an interrupted build: on Ctrl-C, stop UCC and remove the
  half-built packages from `System/` (the next build removes them now).
* Work when run from inside a mod folder, by finding the game directory above it.
* Run UCC on Windows at a lower priority too.

## Extended Language Specification (UnrealScriptPlus?)

I would also like to add more modern language features that can alleviate some
of the annoyances of UnrealScript. For example:

* Add ternary operators.
* Add inline `local` varable declaration & assignment.

This would require a transpiler step, but investigation needs to be done as to how
feasible this is given the odd environment that `UCC` operates in.

With usagi's own compiler ([COMPILER.md](COMPILER.md#extending-the-language)),
no transpiler is needed: new syntax compiles straight to the engine's
bytecode, as long as it can be expressed in it.
