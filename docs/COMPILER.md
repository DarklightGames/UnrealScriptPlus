# An UnrealScript compiler (design)

A design for compiling UnrealScript to UE2 packages (`.u` files) without UCC.
**Nothing here is implemented yet.** It builds on what usagi already has:
the parser, AST and symbol table in `usagi-core`, and in `usagi-pkg`, the
native data of classes, states, functions, structs and property declarations,
and their bytecode, read and written by one function per type (see
[PACKAGE_FORMAT.md](PACKAGE_FORMAT.md#native-data) and
[ASSET_PIPELINE.md](ASSET_PIPELINE.md)).

## Why

- Building without Wine, and much faster than UCC.
- Better error messages than UCC's, from the same front end as the linter.
- A compiler checked against UCC is the strongest test of usagi's
  understanding of the language and the format.

## What compiling produces

A `.u` package that the engine loads as if UCC had made it:

- For each class, its exports: the class, and its fields (its properties,
  functions with their parameters and locals, states, structs, enums and
  constants), each a `UField` linked to the next through `Next`, the first
  being the class's `Children`; and its source as a `TextBuffer`.
- Each function's and state's bytecode, with jump and skip offsets in
  memory, where object references and names take 4 bytes
  ([Bytecode](PACKAGE_FORMAT.md#bytecode)).
- Each class's default properties, as tagged properties: only those that
  differ from the superclass's defaults.
- The name, import and export tables and the summary, laid out as
  `UObject::SavePackage` writes them.

## The parts

### 1. Writing packages

`usagi-pkg` can write native data (`Saver`), but not whole packages: the
summary, name table, import and export tables, and tagged properties. This
comes first, and can be tested on its own: reading every package in a game
and writing it again must give the same bytes (or, where the engine's
choices aren't reproducible, a package that reads the same).

### 2. Default properties

Parsing `defaultproperties` (with the `t3d` crate), checking each value
against its property's declaration, and writing the values that differ from
the superclass's defaults. This needs the inherited defaults (see
[WISHLIST.md](WISHLIST.md#packages)), but no bytecode. It's useful on its own:
a class whose code doesn't change could be rebuilt with new defaults.

### 3. Declarations

Classes, structs, enums, constants, properties and function signatures:
their flags (`CPF_`, `FUNC_`, `CLASS_`, `STRUCT_`), `ArrayDim`, `Category`,
replication offsets, `ClassWithin`, `ClassConfigName`, `HideCategories`,
`Dependencies` and `PackageImports`. Declarations from other packages come
from their `.u` files, through `Packages`, as they do for reading properties.

### 4. Semantic analysis

Most of the work, and what the linter's symbol table grows into:

- Types: every expression's type, and the implicit conversions UCC inserts
  as `EX_PrimitiveCast` (`ECastToken`).
- Operators: each is a native function chosen by its operands' types
  (overloads, and their precedence and the conversions they allow), called
  by its number (`iNative`).
- Functions: whether a call is virtual (`EX_VirtualFunction`, by name),
  final (`EX_FinalFunction`, by reference), global, a delegate, or native by
  number; optional and `out` parameters; `super` and `Class.static`.
- Contexts: `a.b` (`EX_Context`, with its skip size and the size to zero),
  `default.` and `static.` (`EX_ClassContext`, `EX_DefaultVariable`).
- Variables: local, instance, default, bool (`EX_BoolVariable` and its
  mask), struct members, static and dynamic array elements, and array
  methods.

### 5. Code generation

The bytecode for each statement and expression: assignments (`EX_Let`,
`EX_LetBool`, `EX_LetDelegate`), control flow (`EX_JumpIfNot`, `EX_Jump`,
`switch` as `EX_Switch` and a chain of `EX_Case`), `foreach`
(`EX_Iterator`), `assert`, state code with its labels (`EX_LabelTable`,
4-byte aligned, and `LabelTableOffset`), `goto`, `stop`, `return`, and
`EX_EndOfScript` at the end. Offsets are in memory, so code is laid out
with each expression's size in memory, which `Script` already works out.

### 6. Matching UCC

Where the engine depends on UCC's choices, the compiler must make the same
ones: names and flags, the order of fields, `FriendlyName`, `ClassGuid`,
the dependencies' `ScriptTextCRC`, and when `EX_DebugInfo` is written.
Where it doesn't, the compiler can differ, but the closer it is, the easier
it is to test.

## Testing

The round-trip checks give an exact reference: compile a game's source and
compare each package with UCC's, first by structure (the same exports,
fields, flags and default properties), then by bytecode, then byte for byte.
UT2004's, Red Orchestra's and Darkest Hour's sources and `.u` files are the
corpus. As for the parser, every class should compile, and a difference from
UCC is a bug until shown otherwise.

## Extending the language

With its own compiler, usagi isn't limited to the language UCC accepts. The
engine only runs bytecode, so any syntax that can be lowered to the existing
tokens works in the shipped game, with no engine changes and no transpiling
to UnrealScript first. For example:

- Expressions: the ternary operator (`a ? b : c`, as `EX_JumpIfNot` and
  `EX_Jump`), `??`-style defaults for `None`, string interpolation (as
  concatenation), compound assignments that UCC lacks.
- Declarations: `local` anywhere in a function, declared and assigned at
  once, with its type inferred (`local x = 5;`), all hoisted to the
  function's locals as UCC requires.
- Control flow: `for` over a dynamic array's elements, early `continue` and
  `break` with labels, `switch` on names and strings, pattern-like `case`s.
- Types: enums and constants used across classes without the workarounds,
  struct literals in code (not only in `defaultproperties`), `const`
  locals, and stricter checks (nullability of object references, exhaustive
  `switch`) that the linter can share.
- Sugar for common engine patterns: iterators, timers, replication blocks,
  `default.` access.

Some limits come from the engine, not the compiler: anything that needs new
tokens, new native functions or a different object layout (generics with
runtime types, closures that capture locals, new property types) can't run
on the shipped engine. Lambdas without captures could be lowered to
delegates on generated functions.

The extended syntax should be opt-in (a file extension, or a directive at
the top of the file) so that plain UnrealScript still compiles exactly as
UCC compiles it, and the linter and formatter need to understand both.

## Limits

Native functions and classes are C++: their code comes from the shipped
packages and DLLs. Script calls them by the numbers their declarations give,
so a compiler can compile calls to them, and classes that extend them, but
not the natives themselves. A package of native classes (`Engine.u`) can be
recompiled only as far as its script.

## Order

1. A package writer, tested by rewriting existing packages.
2. Default properties only.
3. Declarations: compiling a class with no code.
4. Function bodies, one construct at a time, each checked against UCC.
5. States and state code.
6. Replacing UCC in `usagi build`, behind a flag, then by default.
7. Extended syntax, once plain UnrealScript compiles as UCC compiles it.
