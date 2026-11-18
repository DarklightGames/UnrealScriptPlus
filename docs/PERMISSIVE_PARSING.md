# Permissive parsing of legacy code

UE2's UnrealScript compiler, and the importer it uses for `defaultproperties`
blocks, accept more than a strict reading of the language allows. Shipped code
(the engine's own classes and mods built on them) relies on that leniency, and
it has to parse unchanged: legacy source is treated as read-only.

The parser handles this leniency in two ways:

1. **Recovered problems.** These are malformed input that UE2 repairs or
   skips. The parser does the same and reports each problem. In permissive
   mode (the default) the problems are warnings. In strict mode they are
   errors.
2. **Accepted quirks.** These are things UE2's compiler accepts without
   complaint. The parser accepts them silently in both modes.

Regression tests for these rules are in [`tests/programs.rs`](../crates/usagi-core/tests/programs.rs).
Most of them reproduce failures found by parsing the Red Orchestra / Darkest
Hour codebase (7,633 files) with `usagi parse`.

## Parse modes

```rust
use usagi_core::{parse_program, parse_program_with, ParseMode, ParseOptions};

// Permissive (the default): recovered problems are warnings in
// `ProgramResult::errors`, with `lint: None`.
let result = parse_program(source)?;

// Strict: any recovered problem fails the parse with
// `ParsingError::Strict(problems)`.
let result = parse_program_with(source, &ParseOptions { mode: ParseMode::Strict });
```

The two modes produce the same AST. Recovered problems have no lint code, so
`LintSelection` can't filter them out (see [STATIC_ANALYSIS.md](STATIC_ANALYSIS.md)).
`usagi parse --statistics` counts a file with recovered problems as
`recovered`, or as `strict_error` when run with `--strict` (see [CLI.md](CLI.md)).

`parse_program` and `parse_program_with` take a `&str`. To parse a file's
bytes the way UCC reads them, use `parse_bytes`, which decodes the file (see
[Encodings](#encodings)) and reports encoding problems as recovered problems.

## Recovered problems

### Malformed float literals

UE2's compiler accepts a number with more than one decimal point, anywhere a
number can appear. The parser reads the longest valid prefix.

| Source | Read as | Message |
|---|---|---|
| `sb_Main.TopPadding=0.0.5;` (XVoting/MapVoteGameConfigPage.uc) | `0.0` | ``Malformed numeric literal "0.0.5" was read as "0.0"`` |
| `-1.25.` | `-1.25` | ``Malformed numeric literal "-1.25." was read as "-1.25"`` |

### Encodings

UCC loads a `.uc` file with `appLoadFileToString` (`Core/Src/UnMisc.cpp` in
the UE2 source):

- A file that starts with the UTF-16 byte order mark `FF FE` (little-endian)
  or `FE FF` (big-endian), and has an even size, is read as UTF-16.
- Any other file is read one byte per character, with byte `n` becoming
  U+00`nn`. That is Latin-1, not Windows-1252: bytes `0x80`-`0x9F` become
  control characters, not `€`, `…` and so on.

UTF-8 is never recognized, so a UTF-8 file's non-ASCII characters turn into
several Latin-1 characters each. `usagi_core::decode_source` reads files the
same way, except that it decodes a UTF-8 file as UTF-8 so that its text is
what the author meant. `parse_bytes` then reports the problem:

| Problem | Example | Message |
|---|---|---|
| A UTF-8 file with non-ASCII characters | `// Günther` | ``The file is UTF-8, so UCC reads "ü" as "Ã¼"; ...`` |
| U+FFFD in a UTF-8 file: a character lost when the file was converted to UTF-8 | `GermanNames(0)="G�nther Liebing"` (DH_Engine/Classes/DarkestHourGame.uc) | ``"�" contains U+FFFD, the Unicode replacement character: ...`` |
| A UTF-8 byte order mark | | ``The file starts with a UTF-8 byte order mark, which UCC reads as the characters "ï»¿"`` |

A file is only treated as UTF-8 if it is valid UTF-8 and has a non-ASCII
character or a UTF-8 byte order mark. ASCII files are the same in every
encoding.

### `defaultproperties`

A `defaultproperties` block isn't UnrealScript. It's T3D-style property text
that UE2 imports line by line, skipping lines it can't read. The grammar only
finds where the block ends. Its contents are parsed by the
[`t3d`](https://github.com/DarklightGames/t3d-rs) crate, which recovers from
problems the same way UE2 does. Messages are prefixed with `defaultproperties: `.

| Problem | Examples from shipped code | What happens |
|---|---|---|
| A line that can't be parsed | `0.905471`<br>`Texture'InterfaceArt_tex.Menu.changeme_texture'`<br>`/ Moving fire anims`<br>`WinHeight1.0`<br>`RemoteRole==ROLE_None`<br>`DefaultStyleNames(35)="GUI2K4.STY2CheckBox` (unterminated string) | The line is discarded, and the lines around it are kept. ``invalid line was ignored: `...` `` |
| Junk after a complete statement | `PitchDownLimit=50000/`<br>`WinWidth= 0.43000                           .`<br>`Grenades(0)=(Item=class'Foo.Bar',Amount=2)        NebelGranate39Weapon`<br>`Digits=(A=(X1=1),B=(X1=2)))`<br>`BindingLabel(35)="Toggle "BehindView""` | The statement is kept and the rest of the line is ignored. ``unexpected text after statement was ignored: `...` `` An empty assignment followed by junk (`Foo= ???`) is discarded instead, as an invalid line. |
| `Begin Object` with no `End Object` | | The object is closed at the end of the block. ``... has no matching `End Object` `` |
| `End Object` with no `Begin Object`, or `End` of a different type | | Reported. A mismatched `End` still closes the open object. |
| A block with no closing `}` | | The block runs to the end of the file. `defaultproperties block is not closed before the end of the file` |

## Accepted quirks

These are accepted silently in both modes.

### Lexical

| Quirk | Example |
|---|---|
| Keywords are case-insensitive. | `Class A Extends Object;`, `VAR Int X;`, `RETURN;` |
| Carriage returns are ignored, so CRLF line endings work. | |
| Block comments nest. | `/* a /* b */ c */` |
| A float may have no leading digit, and may end in `f`. | `.45`, `-.25`, `1.0f` |
| A name literal may contain spaces. | `'Some Name'` |
| `return` needs no space before its value. | `return"";` |

### Declarations

| Quirk | Example |
|---|---|
| `#exec` directives and `const`s may come before the class declaration. | `#exec OBJ LOAD FILE=Foo.utx`<br>`class A extends Object;` |
| Stray semicolons at the top level, after a declaration, and between local declarations. | `var int X;;`<br>`local int A;; local int B;` |
| More than one `defaultproperties` block. UE2 accepts this, but its behaviour is unreliable. | |

### Statements

| Quirk | Example |
|---|---|
| The `;` after `do ... until (...)` is optional. | `do { I++; } until (I > 3)` |
| An `if` with no body at all, usually because the body was commented out. This is reported by lint S001 `empty-body`. | `if (A > 0)`<br>`// DoSomething();`<br>`}` |
| More than one `default:` in a `switch`, and `default:` before other cases. Each is parsed as a default case. | `default: break; default: break;`<br>`default: A = 1; case 1: break;` |
| A `goto` label may be quoted or not. | `goto 'Begin';`, `goto Begin;` |

### `defaultproperties`

| Quirk | Example |
|---|---|
| Comments and quoted text are skipped when finding the closing `}`. A quote ends at the end of its line, because shipped code contains unterminated strings. | `// }`, `A="}"` |
| Array indices may use `()` or `[]`. | `A(0)=1`, `A[0]=1` |
| Any number of `;` may end a statement. | `A=1;;` |
| A value may be empty. | `A=` |
| A struct may have a trailing comma, and an array may have empty elements. | `D=(X=1,Y=2,)`, `E=(1,,3)` |
| Whitespace is allowed between an object reference's type and its quote. | `B=Texture 'Pkg.Tex'` |
| An unquoted value may contain unusual characters, or be a dotted object path. | `idle-empty`, `20_seconds`, `Package.Class.SubObject` |
| `Begin`/`End` are case-insensitive. | `begin object class=Foo name=Bar` |

## Adding a rule

When shipped code fails to parse:

1. Work out whether UE2's compiler accepts the code as it is (an accepted
   quirk), or whether UE2 repairs or skips something malformed (a recovered
   problem).
2. For an accepted quirk, change the grammar so that the code parses silently.
   For a recovered problem, recover from it the way UE2 does, and report it as
   a recovered problem in `recovered_problems` in
   [`src/parser.rs`](../crates/usagi-core/src/parser.rs) so that strict mode rejects it. Problems
   inside `defaultproperties` belong in the `t3d` crate.
3. Add a regression test to `tests/programs.rs`.
4. Add the rule to this document.
