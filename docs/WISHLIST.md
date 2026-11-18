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
* Fixes for more lints: F003 and N006 (safe), and unsafe fixes for an editor to offer.
* SARIF output, for GitHub code scanning.

## Extended Language Specification (UnrealScriptPlus?)

I would also like to add more modern language features that can alleviate some
of the annoyances of UnrealScript. For example:

* Add ternary operators.
* Add inline `local` varable declaration & assignment.

This would require a transpiler step, but investigation needs to be done as to how
feasible this is given the odd environment that `UCC` operates in.
