# Warnings
* Redundant function type specifiers (e.g. having both an `event` and `function` specifier, only one is required.)
* Disallow empty `defaultproperties` blocks.
* Do not allow empty `replication` blocks.
* Disallow unnecessary trailing empty call arguments
* Disallow consecutive `float` arguments in functions with `exec` modifier.
* Enforce `SCREAMING_SNAKE_CASE` names for `const` names.
 `defaultproperties` values should not accept `name_literal` tokens as values, as they are broken when assigning to variables of `name` type.

# Static Analysis Wishlist
* [ ] Check if switch cases are exhaustive for `enum` types
* [ ] Enforce ordering of function and variable modifiers (alphabetical)
* [ ] Redundant values in `defaulproperties` (where the values are defined identically in child classes.)
* [ ] Redundant parentheses around expressions (e.g. `Foo(Bar, (Baz))`.)
* [ ] Enforce the use of `(` and `)` as target index specifiers in `defaultproperties` assignment targets.
* [ ] Do not allow statements prior to the `class_declaration` (also simplifies the `program` rule.)
* [ ] Do not allow multiple `defaultproperties` blocks (allowed by native UnrealScript, but is thoroughly broken)
* [ ] Redundant `self` qualifiers.
* [ ] Redundant empty `return` statements as last statement in function.
* [ ] Non-`Actor` derived `Object`s holding references to `Actor`s. 

# Syntax Sugar
## `local_declaration` should allow assignments on the same line
### Example
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
Foo = Baz;
```

# Ternary Operators
Ternary operators  
```Foo = Bar ? 42 : Baz;```
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

# Formatted Strings (f-strings)
Strings that allow formatting within the string instead of chaining concatenation operators.

Since these are not string literals, they are classed as expressions

```
f"This is a {Adjective} string!";
```

This would only be allowable inside of code statements, and cannot be used inside `defaultproperties`, `const` or anything like that.

This would simply be expanding the string out to the following:

```
"This is a " $ (Adjective) $ " string";
```