# Static analysis

usagi checks UnrealScript code for likely bugs, redundant code and
style problems. Every problem found by a lint carries the lint's **code** and
**name** (e.g. `S003 self-assignment`) on `ProgramError::lint`, alongside a
severity (`Error` or `Warning`).

The registry of lints is in [`src/lint.rs`](../crates/usagi-core/src/lint.rs). The checks are in
[`src/visitor.rs`](../crates/usagi-core/src/visitor.rs), and some of them use the per-file symbol
table in [`src/symbols.rs`](../crates/usagi-core/src/symbols.rs).

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
let mut result = usagi_core::parse_program(source)?;
LintSelection::new(["C", "S"], ["S008"]).retain(&mut result.errors);
```

`usagi lint` accepts the same selectors, on the command line or in a
`usagi.toml` (see [CLI.md](CLI.md)):

```sh
usagi lint --select C,S --ignore S008 Foo.uc
usagi lint --format tsv --statistics <root> > lints.tsv
```

`--statistics` prints the number of problems and files for each lint.

## Fixes

Some problems come with a suggested fix (`ProgramError::fix`): a list of text
edits. The **Fix** column below says whether a lint's problems have one:
**Yes** for every problem, **Sometimes** for some of them.

A fix is never offered if it would delete or move a comment, or leave one
describing code that's gone: a fix that deletes a statement or block isn't
offered if a comment is on any of its lines (`X=0 // off for now`), and
removing parentheses or trailing commas isn't offered if a comment is among
them. Comments are found by `usagi_core::source::comment_ranges`, which skips
string and name literals, so `"http://..."` isn't a comment.

A fix is either *safe* (it never changes what the code does) or *unsafe*.
`usagi lint --fix` applies safe fixes, and `usagi lint --diff` shows what they
would change without writing anything (see [CLI.md](CLI.md)). Unsafe fixes are
never applied from the command line; they are for an editor to offer.

## Lints

### Correctness

| Code | Name | Severity | Fix | Reports |
|---|---|---|---|---|
| C001 | `multiple-function-types` | Error |  | A function that declares more than one of `function`, `event`, `delegate`, and so on. |
| C002 | `exec-consecutive-float-arguments` | Error |  | An `exec` function with consecutive `float` arguments, which the console can't parse. |
| C003 | `defaultproperties-name-literal` | Error |  | A `defaultproperties` value that is a name literal. These don't work, so use a string. |
| C004 | `break-outside-loop` | Error |  | `break` outside a loop or `switch`. |
| C005 | `continue-outside-loop` | Error |  | `continue` outside a loop. |
| C006 | `division-by-zero` | Warning |  | Division or modulo (`/`, `%`, `/=`) by a literal zero. |
| C007 | `array-index-out-of-bounds` | Warning |  | A literal index outside a static array declared in the file, in code or in `defaultproperties`. |
| C008 | `latent-call-outside-state-code` | Warning |  | A latent function (`Sleep`, `FinishAnim`, `MoveTo`, ...) called from a function rather than from state code. |
| C009 | `conflicting-access-modifiers` | Warning |  | More than one of `private`, `protected` and `public`. |
| C010 | `byte-value-out-of-range` | Warning |  | A literal outside [0..255] assigned to a `byte`, in code or in `defaultproperties`. |
| C011 | `duplicate-declaration` | Error |  | A name declared more than once in the same scope. Operator overloads, and the same value in different enums, are allowed. |
| C012 | `assignment-to-constant` | Error |  | An assignment (including `+=` and `++`) to a const, an enum value or a function. Assigning a function to a delegate is allowed. |
| C014 | `undeclared-replicated-member` | Error |  | A member in the replication block that isn't declared in the class itself. UE2 only lets a class replicate its own members. |
| C015 | `return-form-mismatch` | Error |  | `return X;` in a function with no return type, or a bare `return;` in a function that has one. |

### Suspicious

| Code | Name | Severity | Fix | Reports |
|---|---|---|---|---|
| S001 | `empty-body` | Warning |  | An `if`, `else`, loop or `foreach` whose body is a lone `;`. Not reported for a `for` loop with a post clause, or a `while` loop whose condition has side effects, because there the header does the work. |
| S002 | `useless-expression` | Warning |  | An expression statement that has no effect, e.g. `A == B;`. |
| S003 | `self-assignment` | Warning |  | A variable assigned to itself. `for (i = i; ...)` is allowed. |
| S004 | `identical-operands` | Warning |  | Both operands of a comparison, logical or arithmetic operator are the same. `X != X`, the NaN check, is allowed. |
| S005 | `constant-condition` | Warning |  | An `if` or `while` condition that is a literal. `while (true)` is allowed. |
| S006 | `unreachable-code` | Warning |  | A statement after `return`, `break`, `continue`, `goto` or `stop`. A `break` straight after a `return` is allowed. |
| S007 | `duplicate-switch-case` | Warning |  | The same `case` value more than once in a `switch`. |
| S008 | `switch-case-fallthrough` | Warning |  | A non-empty `case` that falls through to the next one. |
| S009 | `negated-comparison-operand` | Warning |  | `!A == B`, which means `(!A) == B`. |
| S010 | `duplicate-default-property` | Warning |  | The same property assigned more than once in a `defaultproperties` block. Only the last value is used. |
| S011 | `rotator-component-out-of-range` | Warning |  | A rotator literal component outside the 16-bit range. |
| S012 | `rotator-component-not-integer` | Warning |  | A rotator literal component that isn't an integer. |
| S013 | `default-property-type-mismatch` | Warning |  | A `defaultproperties` value whose literal type doesn't match the variable's type. |
| S014 | `duplicate-replicated-member` | Warning |  | A member listed more than once in the replication block. |
| S015 | `shadowed-variable` | Warning |  | A local or argument with the same name as a variable or const of the class (or state). In a `static` function only consts count, since instance variables aren't accessible there. |
| S016 | `missing-return` | Warning |  | A function with a return type where some path reaches the end without `return`; UCC returns zero or `None` there. Understands `if`/`else`, `switch` with `default` (including fallthrough into a case that returns) and `while (true)`/`for (;;)` loops without `break`. |

### Performance

| Code | Name | Severity | Fix | Reports |
|---|---|---|---|---|
| P001 | `actor-iterator-in-per-frame-function` | Warning |  | `foreach AllActors`, `DynamicActors` or `AllObjects` in a function that runs every frame (`Tick`, `PlayerTick`, `PostRender`, `DrawHUD`, `RenderOverlays`). |

### Redundancy

| Code | Name | Severity | Fix | Reports |
|---|---|---|---|---|
| R001 | `empty-statement` | Warning | Sometimes | A lone `;`. |
| R002 | `redundant-parentheses` | Warning | Sometimes | Parentheses that don't change the meaning of an expression. |
| R003 | `duplicate-modifier` | Warning | Yes | The same modifier more than once on a class, variable or function. |
| R004 | `redundant-default-value` | Warning | Sometimes | A `defaultproperties` value equal to the type's zero value. |
| R005 | `empty-defaultproperties` | Warning | Sometimes | An empty `defaultproperties` block. |
| R006 | `empty-replication-block` | Warning | Sometimes | An empty `replication` block. |
| R007 | `duplicate-replication-condition` | Warning |  | Two replication statements with the same reliability and condition. |
| R008 | `trailing-empty-arguments` | Warning | Sometimes | Empty arguments at the end of a call, e.g. `Foo(A,,)`. |
| R009 | `unused-local` | Warning |  | A local variable that is never used. |
| R010 | `local-set-but-never-used` | Warning |  | A local variable that is only ever assigned, incremented or compound-assigned. Passing it to a function, `X.Y = ...` and `X[i] = ...` count as uses. |
| R011 | `unused-private-member` | Warning |  | A `private` variable or function that nothing in the file uses. Replication and `defaultproperties` count as uses. Not reported for native classes or members, events, delegates or `exec` functions. |
| R012 | `redundant-self` | Warning | Yes | A `Self.` qualifier that doesn't change what a name refers to: no argument, local or const of the enclosing function hides the member. |
| R013 | `redundant-return` | Warning | Sometimes | A bare `return;` as the last statement of a function. A body that is only `return;` is allowed, since it deliberately stubs out the parent's version. |
| R014 | `redundant-case-before-default` | Warning | Sometimes | An empty `case` label directly before `default:`, which runs the same code without it. Labels whose value has side effects, such as a function call, are allowed. |

### Naming

| Code | Name | Severity | Fix | Reports |
|---|---|---|---|---|
| N001 | `const-name-case` | Warning |  | A `const` name that isn't SCREAMING_SNAKE_CASE. |
| N002 | `function-name-case` | Warning |  | A function name that doesn't begin with a capital letter. Operators are exempt. |
| N003 | `enum-name-prefix` | Warning |  | An enum name that doesn't begin with `E`. |
| N004 | `bool-name-prefix` | Warning |  | A `bool` variable name that doesn't begin with `b`. |
| N005 | `object-literal-type-case` | Warning |  | An object literal type that doesn't begin with a capital letter, e.g. `texture'Foo'`. |
| N006 | `identifier-case-mismatch` | Warning |  | A name written with different capitalization than its declaration in the same file. |

### Formatting

| Code | Name | Severity | Fix | Reports |
|---|---|---|---|---|
| F001 | `boolean-literal-case` | Warning | Yes | A boolean literal that isn't lowercase. |
| F002 | `object-literal-keyword-case` | Warning | Yes | A `class'...'` or `enum'...'` keyword that isn't written `Class` or `Enum`, e.g. `class'Foo'` or `ENUM'Bar'`. |
| F003 | `unsorted-modifiers` | Warning |  | Variable or function modifiers that aren't in alphabetical order. |

## Problems that aren't lints

Problems that permissive parsing recovers from have no lint code
(`ProgramError::lint` is `None`) and can't be deselected. Examples are
discarded `defaultproperties` lines and malformed float literals like `0.0.5`.
In strict mode they are returned as `ParsingError::Strict` instead.
