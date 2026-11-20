# The `usagi` command-line tool

`usagi` parses and lints Unreal Engine 2 UnrealScript, builds mods with
UCC, and inspects package files. It is built from the `usagi-cli` crate.

```sh
cargo install usagi-cli                  # or download a binary from the Releases page
usagi lint                               # lint every .uc file under the current directory
usagi build                              # compile the mod's out-of-date packages
usagi pkg list System/Engine.u --all     # show a package's summary, imports and exports
usagi pkg obj Textures/UI.utx "Texture'UI.HUD.*'" --property Format,LODSet
```

## Commands

| Command | Does |
|---|---|
| [`lint`](#usagi-lint-paths) | Parse files and report lint problems; apply fixes. |
| [`parse`](#usagi-parse-paths) | Parse files and report syntax errors. |
| [`rule`](#usagi-rule-codename--usagi-rule---all) | Explain a lint, or list every lint. |
| [`build`](#usagi-build) | Compile a mod's out-of-date packages with UCC. |
| [`pkg list`](#usagi-pkg-list-path) | Show a package file's summary, dependencies, imports and exports. |
| [`pkg flags`](#usagi-pkg-flags-paths) | Show or change package files' flags. |
| [`pkg obj`](#usagi-pkg-obj-path-objects) | Show objects' properties, as T3D or as a table. |
| [`format`](#usagi-format) | Not implemented yet. |

Every command that takes paths accepts files and directories. Directories are
searched for `.uc` files (in any letter case). With no paths, the current
directory is used. A file named directly is always checked, even if
`include`/`exclude` would skip it.

### `usagi lint [PATHS]...`

Parses each file permissively and reports lint problems (see
[STATIC_ANALYSIS.md](STATIC_ANALYSIS.md)). Problems that permissive parsing
recovered from are also shown, under the code `parse`.

| Flag | Meaning |
|---|---|
| `--select LINT,...` | Report these lints, replacing the configured list. |
| `--extend-select LINT,...` | Report these lints as well. |
| `--ignore LINT,...` | Don't report these lints, in addition to the configured ones. |
| `--format FORMAT` | How to print problems (see below). |
| `--statistics` | Print the number of problems and files for each lint. |
| `--fix` | Apply safe fixes to the files, then report the problems that are left. |
| `--diff` | Print the changes `--fix` would make, as a unified diff, without changing any file. |
| `--strict` | Treat recovered problems as errors, as `parse --strict` does. |
| `--include GLOB,...` / `--exclude GLOB,...` | Choose files (see below), replacing the configured lists. |

A lint selector is `ALL`, a code (`S003`), a code prefix (`S`, `S00`), a
lint name (`self-assignment`) or a category name (`suspicious`), all
case-insensitive.

#### Fixes

Problems that `--fix` can fix are marked `[*]` in the concise format, and
have a "fixable with --fix" note in the full format. Only *safe* fixes, which
never change what the code does, are applied; see
[STATIC_ANALYSIS.md](STATIC_ANALYSIS.md#fixes) for which lints have them. Only
selected lints are fixed, so `usagi lint --fix --select F001` fixes nothing
else.

`--fix` re-lints each fixed file and fixes again, up to 10 times, so that
fixes that overlapped one another are all applied. Files are written back in
the encoding they were read in, with their line endings unchanged. A fix whose
text that encoding can't store (e.g. a character above U+00FF in a Latin-1
file) is not applied.

Use `--diff` as a dry run: it prints what would change and writes nothing.

### `usagi parse [PATHS]...`

Parses each file and reports syntax errors and recovered problems.

| Flag | Meaning |
|---|---|
| `--strict` | Fail files that permissive parsing would recover (see [PERMISSIVE_PARSING.md](PERMISSIVE_PARSING.md)). |
| `--ast` | Print each file's syntax tree. |
| `--statistics` | Print how many files had each outcome: `ok`, `recovered`, `strict_error`, `syntax_error`, `panic` or `io_error`. |
| `--format`, `--include`, `--exclude` | As for `lint`. |

### `usagi rule <CODE|NAME>` / `usagi rule --all`

Explains a lint, or lists every lint.

### `usagi format`

Not implemented yet.

### `usagi build`

Compiles out-of-date packages with UCC (`Editor.MakeCommandlet`). On
systems other than Windows, UCC is run with Wine.

The game directory (`root`) contains `System/UCC.exe`. The packages to build
are the `EditPackages` of the INI file that UCC reads, in order, and each
package's sources are in `<root>/<Package>/Classes`. A build is one of two
kinds:

- **A mod** (`--mod NAME` or `build.mod`): the mod is the folder
  `<root>/<Mod>`. The INI is the mod's `System/<Mod>.ini`, which UCC makes
  from the game's and the mod's `System/Default.ini`. Packages compiled
  already in `<root>/System` are the game's, and are skipped. The compiled
  packages and localization files are moved to `<root>/<Mod>/System`.
- **The game itself** (no mod, or `--no-mod`): the INI is `build.ini` in
  `<root>/System`, e.g. `RedOrchestra.ini`, and it's passed to UCC with
  `-ini=`. Without `build.ini`, it's the only INI in `System` with
  `EditPackages` (apart from `Default.ini`, `DefUser.ini` and `User.ini`);
  if there are several, it's an error. Compiled packages stay in
  `<root>/System`.

A build does the following:

1. Writes the version class, if `build.version-class` is set. It isn't
   rewritten if it's unchanged.
2. With a mod, removes the mod's packages from `<root>/System`, even if
   they match `build.locked`. UCC loads them from the mod's `System` folder,
   so a copy left in `<root>/System` by an interrupted build would be loaded
   first.
3. With a mod, if `Default.ini` changed since the last build (or there was
   no build yet), or `--clean` is given, rebuilds every package and exports
   localization. In `<root>/<Mod>/System/<Mod>.ini`, the arrays that
   `Default.ini` changes with `+Key=`/`-Key=` (`EditPackages`,
   `ServerPackages`, `Paths`, ...) are recomputed from the game's
   `System/Default.ini`. Every other setting in the file is kept. If the game
   has no `System/Default.ini`, `<Mod>.ini` is removed, and UCC makes a new
   one. Without a mod, `--clean` rebuilds every package and exports
   localization.
4. Fails if two files with the same name (in any letter case) are in the
   INI's `Paths`.
5. Finds the out-of-date packages, in `EditPackages` order. A package is out
   of date when its sources changed since it was last built (a `.uc` file was
   added, removed, renamed or edited), or it hasn't been built by usagi yet.
   Every package after an out-of-date package is rebuilt too, since it may
   depend on it, unless `--ignore-dependencies` is given. A package without
   sources is never rebuilt; if it isn't compiled either, that's an error.
6. Checks the locked packages (see below).
7. Parses the sources of the packages it will compile, and stops if a file
   doesn't parse, unless `--no-check` is given. `strict` applies.
8. Removes the out-of-date packages and runs UCC. UCC's progress messages
   are hidden unless `--verbose` is given.
9. With a mod, moves the compiled packages to `<root>/<Mod>/System`.
10. With `--dumpint`, exports the localization (`.int` files) of the
    packages whose sources changed. The exports run in parallel; their
    output is shown only for the ones that fail, or with `--verbose`. An
    export succeeds when it writes the `.int` file and UCC reports no errors,
    except one: DumpInt treats its own name on the command line as a package
    to export, so it always reports `No packages found matching
    Editor.DumpIntCommandlet!`. That error is ignored and not shown. With a
    mod, the `.int` files are moved to `<root>/<Mod>/System`.
    `System/UCC.log` is restored to the log of the build, for editors that
    read errors from it.
11. Records each compiled package's sources in `.usagi-build.toml`, in the
    mod folder or the game directory. A package that failed to compile isn't
    recorded, so it's rebuilt next time.

#### Locked packages

`build.locked` lists packages that are never deleted or rebuilt, as globs
that match package names in any letter case (`*` matches any characters,
`?` one), e.g. the game's own:

```toml
[build]
locked = ["Core", "Engine", "RO*", "X*"]
```

The first build takes a locked package as it is and records its sources.
After that, the build stops before changing anything (exit code 2) if a
locked package:

- has sources that changed since they were recorded;
- comes after a package that's being rebuilt, and so may depend on it
  (`--ignore-dependencies` leaves it as it is);
- isn't compiled, so UCC would compile it;
- would be rebuilt by `--clean` (so `--clean` can't be used without a mod
  while anything is locked).

In a mod build, locks don't apply to the mod's own packages (the
`+EditPackages` of its `Default.ini`), which are built in its `System` folder,
so `locked = ["*"]` protects everything in `<root>/System` without stopping
the mod from building. A mod's own package left in `<root>/System` is always
removed (step 2), even if it matches `locked`. Nothing is locked by default.

| Flag | Meaning |
|---|---|
| `--mod NAME` | The mod to build. Overrides `build.mod`. |
| `--no-mod` | Build the game itself, even if `build.mod` is set. |
| `--root DIR` | The game directory. Overrides `build.root`. |
| `--clean` | Rebuild every package and export localization. |
| `--dumpint` | Export localization for the packages that changed. |
| `--debug` | Compile debug packages, for UDebugger. |
| `--ignore-dependencies` | Don't rebuild the packages after one that changed. |
| `--no-check` | Don't parse the sources before compiling. |
| `-v`, `--verbose` | Show all of UCC's output. |

It ends with `Build: N succeeded, N failed, 0 skipped, N up-to-date`.

### `usagi pkg list <PATH>`

Prints the summary (header) of a package file (`.u`, `.utx`, `.usx`, `.ukx`,
`.uax`, `.ut2`, ...): its version, package flags, GUID, the size and
position of its name, import and export tables, and its generations. Then
it lists the packages this one depends on. With `--imports` and `--exports`,
it lists instead the objects the package refers to in other packages, and
the objects in it. A file whose extension is `.uz2`
(in any case) is decompressed first, as the engine does. The format is
described in [PACKAGE_FORMAT.md](PACKAGE_FORMAT.md).

`PATH` is a path to the file; looking packages up by name isn't supported
yet. The command doesn't read the configuration file.

| Flag | Meaning |
|---|---|
| `--dependencies[=packages\|objects]` | List the packages this one depends on; with `=objects`, also the objects referenced in each, by group. This is the default when none of these five flags is given. |
| `--groups` | List this package's groups (subpackages). |
| `--imports` | List the imports. |
| `--exports` | List the exports. |
| `--all` | List the dependencies (packages only, unless `--dependencies=objects` is given too), the groups, the imports and the exports. |
| `--no-summary` | Leave out the summary. |
| `--sort index\|path\|class\|size\|offset` | How to sort the tables. Default: `index` (table order). |
| `--reverse` | Reverse the sort order. |
| `--humanize` | Show sizes in units (`2.6 MB`) and counts with thousands separators (`15,530`). Text format only. |
| `--format text\|concise\|json` | How to print the package. Default: `text`. |

`text` prints the file's path, then the summary, one field per line:

```text
System/Engine.u
  Tag          0x9E2A83C1
  Version      128 (licensee 29)
  Flags        0x00000020 (Official)
  GUID         17158B3B4FF9A24F520996A03FACE2BE
  Names        8773 at 0x2A0
  Imports      103 at 0x259C18
  Exports      15530 at 0x259FB2
  Size         2722417 bytes
  Generations  77
     1   9629 exports  6132 names
     2   9668 exports  6158 names
     ...
```

`Tag` adds `(Red Orchestra, Killing Floor)` for packages with Tripwire's
tag, `0x9E2A83C2`. Table positions are file offsets. Offsets are always
hexadecimal in the text format (and decimal numbers in the JSON). `Flags` names each flag that is set, and any bits that aren't a
known flag as `unknown`. A `.uz2` adds its compressed size to `Size`:
`13400320 bytes (4625215 compressed)`. A package older than version 68 has a
heritage table instead of a GUID and generations; it adds a `Heritage` line
(`2 GUIDs at 0x2C`), its GUID is the last one in that table, and it has
one generation, made from its counts, as the engine does.

Then the tables, with a header row and one line per package or object. The
tables are listed in this order, whatever the order of the flags:
dependencies, groups, imports, exports. For example:

```text
Dependencies (2)
  Package  Imports  Classes
  Core           1  Class (1)
  Engine         3  Class (2), Texture (1)

Imports (7)
   #  Class           Path
  -1  Core.Package    Core
  -2  Core.Package    Engine
  -3  Core.Class      Engine.Texture
  -4  Core.Class      Core.Package
  -5  Core.Class      Engine.Actor
  -6  Core.Package    Engine.Group
  -7  Engine.Texture  Engine.Group.Rock

Exports (4)
  #  Class    Path                 Size  Offset  Super         Flags
  1  Class    MyPkg.MyClass          40    0xCA  Engine.Actor  Public,LoadForClient,LoadForServer,LoadForEdit,Standalone,Native
  2  Package  MyPkg.MyGroup           1    0xF2                Public,LoadForClient,LoadForServer,LoadForEdit
  3  Texture  MyPkg.MyGroup.MyTex     5    0xF3                Public,LoadForClient,LoadForServer,LoadForEdit
  4  Class    MyPkg.None              0
```

A dependency is a top-level package that the package imports from: a file
the engine loads with it. `Imports` is the number of objects imported from
it, in it or in its groups, and `Classes` their classes, with how many of
each, the most common first. Groups are left out: they're part of the
package, not separate files. A package that imports nothing has an empty
`Dependencies (0)` table.

With `--dependencies=objects`, each package is followed by the objects
referenced in it, as a tree under their groups. Each object is a row of the
table, with its class in the `Classes` column. Groups and objects are sorted
by name, ignoring case:

```text
Dependencies (2)
  Package     Imports  Classes
  Core              1  Class (1)
  └─ Package           Class
  Engine            3  Class (2), Texture (1)
  ├─ Actor             Class
  ├─ Group
  │  └─ Rock           Texture
  └─ Texture           Class
```

`--groups` lists this package's groups: its exports of class `Core.Package`,
with the number of objects directly in each (not counting those in its
subgroups, or the subgroups themselves) and their classes:

```text
Groups (1)
  Group          Objects  Classes
  MyPkg.MyGroup        1  Texture (1)
```

`#` is the object's number as a reference to it is stored in the file:
imports are numbered -1, -2, -3, ... and exports 1, 2, 3, ...

- An import shows its class (`ClassPackage.ClassName`) and its full path.
- An export shows its class, and its full path, which starts with the
  package's name (taken from the file name, as the engine does). Then the
  size and file offset of its data, the struct it extends (`Super`, for
  classes, structs, states and functions), and its object flags. An export
  with no data has a size of 0 and no offset; these are placeholders left by
  `ucc conform`. A column with nothing in it, such as `Super` in a texture
  package, is left out.

The class, super and outer references, as stored, are only in the JSON.

`--sort` orders the tables, and the JSON's arrays:

| Sort | Order |
|---|---|
| `index` | Table order (the default). |
| `path` | Full path (a dependency's name), ignoring case. |
| `class` | Class name, ignoring case. Dependencies and groups keep table order. |
| `size` | Size of the export's data, or the number of objects in a dependency or group; largest first. Imports keep table order. |
| `offset` | File offset of the export's data, with exports that have no data last. Imports, dependencies and groups keep table order. |

Ties keep table order. `--reverse` reverses any of these, e.g. `--sort size
--reverse` puts the smallest first. To find the largest objects in a package:
`usagi pkg list Foo.utx --exports --no-summary --sort size | head`.

With `--humanize`, sizes are in binary units with one decimal (`40 B`,
`1.3 KB`, `2.6 MB`; 1 KB is 1024 bytes) and counts have thousands separators
(`15,530`), in the summary, the table headings and the tables:

```text
  Exports      15,530 at 0x259FB2
  Size         2.6 MB
...
Exports (15,530)
     #  Class       Path                                  Size    Offset  ...
  8721  TextBuffer  Engine.PlayerController.ScriptText  201.6 KB   0xEE462  ...
```

The text is coloured on a terminal (see `--color`). Lines can be long: the
path column is as wide as the longest path.

`concise` prints names only, one per line, without the summary, headings or
colour: the dependencies' names (each followed, with `=objects`, by its
objects' full paths), then the groups', imports' and exports' full paths, for
the tables asked for. For example, `usagi pkg list Foo.u --format
concise` lists the packages `Foo.u` depends on.

`json` prints an object:

```json
{
  "path": "MyPkg.u",
  "size": 351,
  "compressed_size": null,
  "version": 128,
  "licensee_version": 29,
  "summary": {
    "tag": 2653586369,
    "file_version": 1900672,
    "package_flags": { "bits": 33, "names": ["AllowDownload", "Official"], "unknown": 0 },
    "name_count": 12,
    "name_offset": 64,
    "export_count": 4,
    "export_offset": 297,
    "import_count": 7,
    "import_offset": 248,
    "guid": "0123456789ABCDEFDEADBEEF0000002A",
    "generations": [{ "export_count": 4, "name_count": 10 }],
    "heritage": null
  },
  "dependencies": [
    {
      "name": "Engine",
      "index": -2,
      "import_count": 3,
      "classes": [{ "name": "Class", "count": 2 }, { "name": "Texture", "count": 1 }]
    },
    ...
  ],
  "imports": [
    {
      "index": -3,
      "class_package": "Core",
      "class_name": "Class",
      "outer_index": -2,
      "object_name": "Texture",
      "path": "Engine.Texture"
    },
    ...
  ],
  "exports": [
    {
      "index": 1,
      "class_index": 0,
      "class_name": "Class",
      "super_index": -5,
      "super_path": "Engine.Actor",
      "outer_index": 0,
      "object_name": "MyClass",
      "path": "MyPkg.MyClass",
      "flags": { "bits": 68091908, "names": ["Public", "LoadForClient", "LoadForServer", "LoadForEdit", "Standalone", "Native"], "unknown": 0 },
      "serial_size": 40,
      "serial_offset": 202
    },
    ...
  ]
}
```

- `size`, `compressed_size`, `version`, `licensee_version` and `summary` are
  left out with `--no-summary`; `dependencies`, `imports` and `exports` are
  only there for the tables asked for.
- A dependency's `index` is its top-level import's. With
  `--dependencies=objects`, each dependency also has `objects`: an array of
  `{index, path, class_package, class_name}`, sorted by path.
- `groups` is an array of `{index, path, object_count, classes}`.
- `compressed_size` is the size of the `.uz2` file, or `null`.
- `tag` is `2653586369` (`0x9E2A83C1`), or `2653586370` (`0x9E2A83C2`) in Red
  Orchestra and Killing Floor.
- `file_version` is the version as stored: `version` in the low 16 bits and
  `licensee_version` in the high 16.
- `heritage` is `{ "count", "offset" }` before version 68, otherwise `null`.
- The `*_index` fields are object references as stored: 0 for none (for
  `class_index`, the export is a class; for `outer_index`, the object is
  directly in its package), `n` for export `n`, and `-n` for import `-n`.
  `super_path` is `null` when there's no super, and `serial_offset` when
  there's no data.

It fails (exit code 2) if the file can't be read, isn't a package (doesn't
start with the tag `0x9E2A83C1` or `0x9E2A83C2`), or ends early, or if it
has something the engine would fail on: a negative count in the summary, a
name or object reference that's out of range, an import inside an export,
objects inside each other in a loop, or export data that isn't in the file.

### `usagi pkg flags <PATHS>...`

Shows package files' flags, or with `--set` and `--clear`, changes them:

```sh
usagi pkg flags MyMod.u                                   # show
usagi pkg flags MyMod.u --clear ServerSideOnly            # change
usagi pkg flags MyMod.u --set ClientOptional,ServerSideOnly --clear AllowDownload
usagi pkg flags System/*.u --set ServerSideOnly --dry-run
```

| Flag | Meaning |
|---|---|
| `--set FLAG,...` | Flags to set. Can be given more than once. |
| `--clear FLAG,...` | Flags to clear. Can be given more than once. |
| `--dry-run` | Show what would change without changing any file. |
| `--force` | Change packages with the `Official` flag too. |

Flags are named as in [PACKAGE_FORMAT.md](PACKAGE_FORMAT.md#package-flags),
without the `PKG_` prefix, in any case: `AllowDownload`, `ClientOptional`,
`ServerSideOnly`, `BrokenLinks`, `Unsecure`, `Official`,
`ExtraDownloadFile` and `Need`. A flag can't be both set and cleared.

Each file gets one line: its flags, or the change.

```text
MyMod.u: 0x00000005 (AllowDownload, ServerSideOnly) -> 0x00000001 (AllowDownload)
Other.u: 0x00000001 (AllowDownload) (unchanged)
```

Only the 4 bytes of the flags change. A `.uz2` file is decompressed,
changed and compressed again (with the same blocks as `ucc compress`).

A package with the `Official` flag isn't changed without `--force`.
Changing a package's flags changes its MD5 (the engine's "QuickMD5" covers
the summary), so an official package would no longer match the stock file,
and servers that check packages' MD5s would reject it.

Each file is changed, or fails, on its own. If any fails (it can't be read,
isn't a package, or is official), the command reports each failure and exits
with code 2.

### `usagi pkg obj <PATH> [OBJECTS]...`

Prints the properties of objects in a package file: the script-visible
properties stored with each object (a texture's `Format`, `USize`, `LODSet`,
...), read as described in
[PACKAGE_FORMAT.md](PACKAGE_FORMAT.md#tagged-properties).

```sh
usagi pkg obj MyTex.utx "Texture'MyTex.Walls.Brick'"           # one object, as T3D
usagi pkg obj MyTex.utx MyTex.Walls.Brick                      # the class is optional
usagi pkg obj MyTex.utx "Texture'MyTex.UI.*'" --property LODSet   # a table, for an audit
usagi pkg obj MyTex.utx --class Shader --format json
```

Objects are named as UnrealScript names them, `Class'Package.Group.Name'`,
or without the class, `Package.Group.Name`. Names are matched ignoring case.
In the path, `*` matches any characters within a name, `**` any characters
(including `.`), and `?` one character within a name: `MyTex.UI.*` is every
object directly in the group `UI`, and `MyTex.UI.**` every object in it and
its groups. With no objects, every object in the package is shown. Objects
are shown in the order of the export table, once each.

It's an error (exit code 2) if an object without wildcards isn't in the
package, if its class isn't the one given (`Shader'MyTex.Brick'` when it's a
texture: subclasses don't count), or if a pattern with wildcards matches
nothing. For an object that the package only refers to, in another
package, the error says which package it's in; `pkg obj` doesn't read other
packages. The class in a pattern with wildcards only filters the objects.

| Flag | Meaning |
|---|---|
| `--class CLASS,...` | Only objects of these classes: by name (`Texture`) or with their package (`Engine.Texture`), in any case. Subclasses don't match. Can be given more than once. |
| `--property NAME,...` | Only these properties, in any case. In the text format, the objects are shown as a table. Can be given more than once. |
| `--raw` | Show where each property is in the object's data, its type, its size and its value's bytes. Text format only. |
| `--format text\|concise\|json` | How to print the properties. Default: `text`. |

`text` prints each object as T3D, as UnrealEd exports objects, after a
comment with its reference and how much native data follows its
properties:

```text
// Texture'MyTex.UI.Stamina' (32,786 bytes of native data)
Begin Object Class=Texture Name=Stamina
    InternalTime=2027224930
    InternalTime(1)=1082066665
    bAlphaTexture=True
    LODSet=5
    Format=8
    UClampMode=1
    UBits=7
    USize=128
    UClamp=128
End Object
```

Values are written as UnrealEd writes them: floats with six decimals, objects
as `Class'Package.Group.Name'` (or `None`), strings in double quotes, names
bare, bools as `True` and `False`, structs as `(Field=...,...)`, and arrays
of structs as `((...),(...))`. `Foo(2)=` is element 2 of a static array.
Only the properties stored in the file are shown: a property whose value is
the class's default isn't stored. Some values can't be read from the file
alone, and are written in angle brackets, which isn't T3D:

- A byte is its number: which enum it belongs to isn't stored, so
  `LODSet=5`, not `LODSet=LODSET_Interface`.
- An array whose elements aren't structs: its length and size, since its
  element type isn't stored (`<3 elements, 12 bytes>`).
- A struct stored in binary (in packages before licensee version 28), or a
  value that doesn't fit its type: its bytes (`<4 bytes: 01 02 03 04>`, up to
  16).

A class's default properties come after its script, which isn't read, so a
class is shown as a comment saying so.

With `--property`, the text format is a table instead: a row for each object,
and a column for each property, in the order given. A property that isn't
stored is `-`; a static array is shown as `(0)=... (2)=...`. Use it to check
that a setting is the same everywhere, such as every texture in a UI group
having `LODSet=5`:

```text
  Object                      Format  LODSet  USize
  Texture'MyTex.UI.Stamina'   8       5       128
  Texture'MyTex.UI.Needle'    8       -       128
```

`concise` prints one line per property, after its object's reference, for
`grep` and the like:

```text
Texture'MyTex.UI.Stamina' LODSet=5
Texture'MyTex.UI.Stamina' Format=8
```

`--raw` prints a table for each object of where each property's tag starts in
the object's data, its type (with a struct's name), its array index, its size
and its value's bytes (the first 32), then the `None` that ends the list and
the native data after it. An object with a state frame (`RF_HasStack`, such
as an actor in a map) shows its size first. A struct whose tag has the wrong
size (see [PACKAGE_FORMAT.md](PACKAGE_FORMAT.md#structs)) shows both:
`7 (tag: 6)`.

`json` prints an array with an object for each:

```json
{
  "index": 2,
  "reference": "Texture'MyTex.UI.Stamina'",
  "class": "Texture",
  "path": "MyTex.UI.Stamina",
  "offset": 73246,
  "size": 32845,
  "properties": [
    { "name": "bAlphaTexture", "type": "BoolProperty", "index": 0, "offset": 13, "size": 0, "tag_size": 0, "value": true },
    { "name": "LODSet", "type": "ByteProperty", "index": 0, "offset": 16, "size": 1, "tag_size": 1, "value": 5 }
  ],
  "native_size": 32786,
  "error": null
}
```

`index` is the export's number, `offset` and `size` its data's place in the
file, and each property's `offset` is where its tag starts in the object's
data. `type` is the engine's name for the type, and `struct` a struct's name.
A value is a number, a bool, a string (a name, a string, or an object's
reference), `null` (no object), an object of fields (a vector, rotator or
colour, or a delegate's `object` and `function`), an array of properties (a
struct), an array of those (an array of structs), or `{"length": 3, "bytes":
"..."}` for any other array and `{"bytes": "..."}` for a value that can't be
read, with every byte in hex. `properties` is `null` for a class, and
`native_size` is `null` when the properties couldn't all be read.

If an object's data is malformed (a tag that runs past its end, or a name
out of range), the properties before the problem are shown, then the
problem: as a comment in T3D, a line in `--raw`, `error` in JSON, and `?` in a
table for a property that wasn't found. Each such problem is also reported on
standard error, and the command exits with code 1.

### Global flags

| Flag | Meaning |
|---|---|
| `--config FILE` | Use this configuration file. |
| `--no-config` | Don't read a configuration file. |
| `--color auto\|always\|never` | When to use colour. `auto` uses colour on a terminal, unless `NO_COLOR` is set. |
| `-j N`, `--jobs N` | How many files to parse, or UCC processes to run (`build --dumpint`), at once. Overrides `jobs`. Default: half the logical CPUs. |

The worker threads, and the UCC processes they start, run at a lower priority
(nice 10) on Linux and macOS, so that a big run doesn't make the rest of the
machine unresponsive.

## Output formats

These are the formats of `lint` and `parse`. For `pkg list` and `pkg obj`,
see [their](#usagi-pkg-list-path) [sections](#usagi-pkg-obj-path-objects).

| Format | Output |
|---|---|
| `full` (default) | Each problem with the source it points at, like rustc. |
| `concise` | `path:line:column: CODE message` |
| `json` | An array of `{path, line, column, end_line, end_column, code, name, severity, message, fix}`, where `fix` is the title of the safe fix, or `null`. |
| `tsv` | `code`, `name`, `path`, `line` and `message`, separated by tabs. |
| `quiet` | Nothing. Use it with `--statistics`, or to rely on the exit code alone. |

Paths are shown relative to the current directory. Problems are sorted by
path, then by position.

Besides lint codes, a problem's code can be `parse` (a recovered problem),
`syntax` (a syntax error), `panic` (an internal error in the parser) or `io`
(the file couldn't be read).

## Exit codes

| Code | Meaning |
|---|---|
| 0 | Nothing to report. Recovered problems alone don't count, unless `--strict` is used. |
| 1 | `lint`: a lint problem, or a file that failed to parse; with `--fix`, a problem left after fixing; with `--diff`, a change to show. `parse`: a file that failed to parse. |
| 1 | `build`: a package failed to compile, or a source file failed to parse. |
| 1 | `pkg obj`: an object whose data is malformed. |
| 2 | A usage error, a configuration error or a missing path. `pkg list`, `pkg flags`, `pkg obj`: a file that can't be read or isn't a valid package; `pkg flags`: an official package without `--force`; `pkg obj`: an object that isn't in the package or isn't of the class given, or an invalid object reference. `build`: a setup problem, such as a missing folder, or a locked package that would have to be rebuilt. |

## Configuration

`usagi` reads the first `usagi.toml` or `.usagi.toml` it finds in the current
directory or one of its parents. Every setting is optional.

```toml
# Only check files matching these globs. Default: every .uc file.
include = ["*/Classes"]
# Skip files matching these globs.
exclude = ["Engine", "UnrealGame/**/Old*.uc"]
# Treat recovered problems as errors.
strict = false
# How many files to parse at once. Default: half the logical CPUs.
jobs = 6

[lint]
select = ["ALL"]          # default: ALL
extend-select = []
ignore = ["F003", "naming"]

# Lint settings for some files. Every matching entry applies, in order, so a
# later entry wins over an earlier one.
[[lint.per-path]]
paths = ["Engine", "UnrealGame"]
ignore = ["ALL"]

[[lint.per-path]]
paths = ["UnrealGame/Classes/MyClass.uc"]
extend-select = ["C"]

# Settings for `usagi build`.
[build]
mod = "MyMod"             # the mod to build when --mod isn't given. Default: none
root = "."                # the game directory. Default: this file's directory
wine = "wine"             # runs UCC.exe, except on Windows. Default: wine
ini = "MyGame.ini"        # without a mod: the INI in System whose EditPackages are built
locked = ["Core", "Engine"]  # globs of packages never to delete or rebuild; ["*"] for all
# Without a mod, the version class is set here (with a mod, in the mod's file):
# version = "1.2.3"
# version-class = "MyPackage/Classes/MyBuildManifest.uc"
```

Globs in a configuration file are relative to the file's directory (except in a
mod's file; see above). Globs
given as flags are relative to the current directory. They match without
regard to case, and `*` doesn't cross a `/` but `**` does. A glob that
matches a directory matches everything inside it, so `Engine` means the whole
`Engine` directory.

Flags take precedence over the file:

- `--select`, `--include` and `--exclude` replace the configured lists.
- `--extend-select` and `--ignore` add to them.
- `--strict` overrides `strict`.

An unknown key, an invalid glob or an unknown lint selector is an error
(exit code 2), reported with the file and the key.

### Mod configuration

Each mod can have its own configuration file, `<root>/<Mod>/usagi.toml` (or
`.usagi.toml`). It's used together with the configuration found as usual (the
game directory's): by `usagi build` for the mod being built, and by every
other command for the mod named in `build.mod`. It takes every setting except
`build.mod`, `build.root` and `build.ini`:

```toml
include = ["MyMod_*"]
strict = false

[lint]
ignore = ["F003"]

[build]
version = "1.2.3-beta"    # MAJOR.MINOR.PATCH[-PRERELEASE]
# Generate a class with `version` and the Git commit and branch at this path,
# relative to the game directory. Requires `version`.
version-class = "MyPackage/Classes/MyBuildManifest.uc"
wine = "wine"
```

The game directory's file then needs only:

```toml
[build]
mod = "MyMod"
```

`include`, `exclude`, `strict`, `jobs`, `lint.select` and `build.wine` in the
mod's file replace the game directory's; `lint.extend-select`, `lint.ignore`,
`lint.per-path` and `build.locked` are added to them. Globs in both files are
relative to the game directory. When building a mod, `version` and
`version-class` can only be set in its file, and `mod`, `root` and `ini` only
in the game directory's; either mistake is an error. Run `usagi` from the game
directory or below it, outside the mod folder, so that the game directory's
configuration is the one found.
