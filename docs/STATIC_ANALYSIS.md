# Static analysis

UnrealScriptPlus checks UnrealScript code for likely bugs, redundant code and
style problems. Every problem found by a lint carries the lint's **code** and
**name** (e.g. `S003 self-assignment`) on `ProgramError::lint`, alongside a
severity (`Error` or `Warning`).

The registry of lints is in [`src/lint.rs`](../src/lint.rs). The checks are in
[`src/visitor.rs`](../src/visitor.rs), and some of them use the per-file symbol
table in [`src/symbols.rs`](../src/symbols.rs).

## Categories

A lint code is its category's letter followed by three digits.

| Prefix | Category | What it finds |
|---|---|---|
| `C` | correctness | Code that is wrong: it won't compile, or won't do what it says. |
| `S` | suspicious | Code that is very likely a mistake. |
| `P` | performance | Code that is needlessly slow. |
| `R` | redundancy | Code that does nothing and can be removed. |
| `N` | naming | Naming conventions. |
| `F` | formatting | Formatting and ordering conventions. |

## Choosing lints

Every lint is reported by default. `LintSelection` chooses which to keep, like
Ruff's `select` and `ignore`. A selector is `ALL`, a code or code prefix (`S`,
`S00`, `S003`), a lint name (`self-assignment`) or a category name
(`suspicious`), all case-insensitive. A lint is reported if a `select` selector
matches it and no `ignore` selector does.

```rust
let mut result = unrealscript_parser::parse_program(source)?;
LintSelection::new(["C", "S"], ["S008"]).retain(&mut result.errors);
```

The examples accept the same selectors:

```sh
cargo run --example parse_file -- --select=C,S --ignore=S008 Foo.uc
cargo run --release --example parse_corpus -- --lints lints.tsv <root> report.tsv
```

`parse_corpus --lints` writes every problem to a TSV file and prints the
number of problems and files for each lint.

## Lints

### Correctness

| Code | Name | Severity | Reports |
|---|---|---|---|
| C001 | `multiple-function-types` | Error | A function that declares more than one of `function`, `event`, `delegate`, and so on. |
| C002 | `exec-consecutive-float-arguments` | Error | An `exec` function with consecutive `float` arguments, which the console can't parse. |
| C003 | `defaultproperties-name-literal` | Error | A `defaultproperties` value that is a name literal. These don't work, so use a string. |
| C004 | `break-outside-loop` | Error | `break` outside a loop or `switch`. |
| C005 | `continue-outside-loop` | Error | `continue` outside a loop. |
| C006 | `division-by-zero` | Warning | Division or modulo (`/`, `%`, `/=`) by a literal zero. |
| C007 | `array-index-out-of-bounds` | Warning | A literal index outside a static array declared in the file, in code or in `defaultproperties`. |
| C008 | `latent-call-outside-state-code` | Warning | A latent function (`Sleep`, `FinishAnim`, `MoveTo`, ...) called from a function rather than from state code. |
| C009 | `conflicting-access-modifiers` | Warning | More than one of `private`, `protected` and `public`. |
| C010 | `byte-value-out-of-range` | Warning | A literal outside [0..255] assigned to a `byte`, in code or in `defaultproperties`. |
| C011 | `duplicate-declaration` | Error | A name declared more than once in the same scope. Operator overloads, and the same value in different enums, are allowed. |
| C012 | `assignment-to-constant` | Error | An assignment (including `+=` and `++`) to a const, an enum value or a function. Assigning a function to a delegate is allowed. |
| C014 | `undeclared-replicated-member` | Error | A member in the replication block that isn't declared in the class itself. UE2 only lets a class replicate its own members. |
| C015 | `return-form-mismatch` | Error | `return X;` in a function with no return type, or a bare `return;` in a function that has one. |

### Suspicious

| Code | Name | Severity | Reports |
|---|---|---|---|
| S001 | `empty-body` | Warning | An `if`, `else`, loop or `foreach` whose body is a lone `;`. Not reported for a `for` loop with a post clause, or a `while` loop whose condition has side effects, because there the header does the work. |
| S002 | `useless-expression` | Warning | An expression statement that has no effect, e.g. `A == B;`. |
| S003 | `self-assignment` | Warning | A variable assigned to itself. `for (i = i; ...)` is allowed. |
| S004 | `identical-operands` | Warning | Both operands of a comparison, logical or arithmetic operator are the same. `X != X`, the NaN check, is allowed. |
| S005 | `constant-condition` | Warning | An `if` or `while` condition that is a literal. `while (true)` is allowed. |
| S006 | `unreachable-code` | Warning | A statement after `return`, `break`, `continue`, `goto` or `stop`. A `break` straight after a `return` is allowed. |
| S007 | `duplicate-switch-case` | Warning | The same `case` value more than once in a `switch`. |
| S008 | `switch-case-fallthrough` | Warning | A non-empty `case` that falls through to the next one. |
| S009 | `negated-comparison-operand` | Warning | `!A == B`, which means `(!A) == B`. |
| S010 | `duplicate-default-property` | Warning | The same property assigned more than once in a `defaultproperties` block. Only the last value is used. |
| S011 | `rotator-component-out-of-range` | Warning | A rotator literal component outside the 16-bit range. |
| S012 | `rotator-component-not-integer` | Warning | A rotator literal component that isn't an integer. |
| S013 | `default-property-type-mismatch` | Warning | A `defaultproperties` value whose literal type doesn't match the variable's type. |
| S014 | `duplicate-replicated-member` | Warning | A member listed more than once in the replication block. |
| S015 | `shadowed-variable` | Warning | A local or argument with the same name as a variable or const of the class (or state). |
| S016 | `missing-return` | Warning | A function with a return type where some path reaches the end without `return`; UCC returns zero or `None` there. Understands `if`/`else`, `switch` with `default` (including fallthrough into a case that returns) and `while (true)`/`for (;;)` loops without `break`. |

### Performance

| Code | Name | Severity | Reports |
|---|---|---|---|
| P001 | `actor-iterator-in-per-frame-function` | Warning | `foreach AllActors`, `DynamicActors` or `AllObjects` in a function that runs every frame (`Tick`, `PlayerTick`, `PostRender`, `DrawHUD`, `RenderOverlays`). |

### Redundancy

| Code | Name | Severity | Reports |
|---|---|---|---|
| R001 | `empty-statement` | Warning | A lone `;`. |
| R002 | `redundant-parentheses` | Warning | Parentheses that don't change the meaning of an expression. |
| R003 | `duplicate-modifier` | Warning | The same modifier more than once on a class, variable or function. |
| R004 | `redundant-default-value` | Warning | A `defaultproperties` value equal to the type's zero value. |
| R005 | `empty-defaultproperties` | Warning | An empty `defaultproperties` block. |
| R006 | `empty-replication-block` | Warning | An empty `replication` block. |
| R007 | `duplicate-replication-condition` | Warning | Two replication statements with the same reliability and condition. |
| R008 | `trailing-empty-arguments` | Warning | Empty arguments at the end of a call, e.g. `Foo(A,,)`. |
| R009 | `unused-local` | Warning | A local variable that is never used. |
| R010 | `local-set-but-never-used` | Warning | A local variable that is only ever assigned, incremented or compound-assigned. Passing it to a function, `X.Y = ...` and `X[i] = ...` count as uses. |
| R011 | `unused-private-member` | Warning | A `private` variable or function that nothing in the file uses. Replication and `defaultproperties` count as uses. Not reported for native classes or members, events, delegates or `exec` functions. |
| R012 | `redundant-self` | Warning | A `Self.` qualifier that doesn't change what a name refers to: no argument, local or const of the enclosing function hides the member. |
| R013 | `redundant-return` | Warning | A bare `return;` as the last statement of a function. A body that is only `return;` is allowed, since it deliberately stubs out the parent's version. |

### Naming

| Code | Name | Severity | Reports |
|---|---|---|---|
| N001 | `const-name-case` | Warning | A `const` name that isn't SCREAMING_SNAKE_CASE. |
| N002 | `function-name-case` | Warning | A function name that doesn't begin with a capital letter. Operators are exempt. |
| N003 | `enum-name-prefix` | Warning | An enum name that doesn't begin with `E`. |
| N004 | `bool-name-prefix` | Warning | A `bool` variable name that doesn't begin with `b`. |
| N005 | `object-literal-type-case` | Warning | An object literal type that doesn't begin with a capital letter, e.g. `texture'Foo'`. |
| N006 | `identifier-case-mismatch` | Warning | A name written with different capitalization than its declaration in the same file. |

### Formatting

| Code | Name | Severity | Reports |
|---|---|---|---|
| F001 | `boolean-literal-case` | Warning | A boolean literal that isn't lowercase. |
| F002 | `object-literal-keyword-case` | Warning | A `class'...'` or `enum'...'` keyword that isn't lowercase. |
| F003 | `unsorted-modifiers` | Warning | Variable or function modifiers that aren't in alphabetical order. |

## Problems that aren't lints

Problems that permissive parsing recovers from have no lint code
(`ProgramError::lint` is `None`) and can't be deselected. Examples are
discarded `defaultproperties` lines and malformed float literals like `0.0.5`.
In strict mode they are returned as `ParsingError::Strict` instead.
