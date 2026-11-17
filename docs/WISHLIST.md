# Wishlist

Ideas that aren't implemented yet. For the lints that exist, see
[STATIC_ANALYSIS.md](STATIC_ANALYSIS.md).

## Static analysis

### Per file

* Redundant empty `return;` as the last statement of a function.
* Non-exhaustive `switch` on an `enum` value, for enums declared in the file.
* `[]` instead of `()` as the array index in a `defaultproperties` target.
* Statements (`#exec`, `const`) before the class declaration. Parsing this
  is an accepted quirk (see [PERMISSIVE_PARSING.md](PERMISSIVE_PARSING.md)), so
  this would be a lint, not a grammar change.
* More than one `defaultproperties` block. UE2 accepts it, but the behaviour is
  unreliable.

### Needs a project-wide symbol table

* Redundant `defaultproperties` values: a value that a subclass sets to the
  same value its parent already has.
* A non-`Actor` `Object` that holds references to `Actor`s.
* Exhaustive `switch` checking for enums declared in other classes.

## Syntax sugar

### Initializers on local declarations

```
local int Foo = 42;
local string Bar;
local int Baz = Foo;
```

would be expanded to:

```
local int Foo;
local string Bar;
local int Baz;

Foo = 42;
Baz = Foo;
```

### Ternary operator

```
Foo = Bar ? 42 : Baz;
```

would be expanded to:

```
if (Bar)
{
    Foo = 42;
}
else
{
    Foo = Baz;
}
```
