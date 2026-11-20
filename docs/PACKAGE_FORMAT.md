# The Unreal Engine 2 package format

This document describes the binary layout of UE2 packages (`.u`, `.utx`,
`.usx`, `.ukx`, `.uax`, `.ut2`, ...) as written by UT2004 build 3369, which is
`PACKAGE_FILE_VERSION` 128, licensee version 29. Every extension is the same
format: the extension only says what is usually inside.

Red Orchestra, Killing Floor and their mods (such as Darkest Hour) are built
on Tripwire's version of the engine. Their packages have the same container,
also at version 128 and licensee version 29, but a different
[tag](#summary-fpackagefilesummary). This was checked on 4,122 packages from
those games' installs and the Darkest Hour source tree.

The [`usagi-pkg`](../crates/usagi-pkg) crate reads this format (so far, the
summary, the name, import and export tables, objects' tagged properties,
classes and the other native data marked below, and `.uz2` files), and finds
the packages a package refers to. `usagi pkg list` prints a package's
summary, imports and exports, and `usagi pkg obj` objects' properties.

It covers the *container*: the summary, the name, import and export tables, and
how they reference each other. Of what's inside an export's data, it covers
the [tagged properties](#tagged-properties) at its start and, for some
classes, the [native data](#native-data) after them: classes, states,
functions and structs (with their [bytecode](#bytecode)), property
declarations, enums, constants and text buffers, which usagi reads and writes
back; and textures and palettes, which it doesn't read yet.

The references are to the engine source (`Core/...`) and were checked by
reading all 847 packages of a UT2004 install, versions 68 to 128, with a
throwaway reader. Every one of them matched the layout below.

| Source | Contents |
|---|---|
| `Core/Inc/UnObjVer.h` | Tag and version constants. |
| `Core/Inc/UnLinker.h`, `Core/Src/UnLinker.cpp` | The summary, import and export records, and the loader (`ULinkerLoad`). |
| `Core/Src/UnObj.cpp` | `UObject::SavePackage` (the writer) and `FCompactIndex`. |
| `Core/Src/UnName.cpp` | Name table entries. |
| `Core/Src/UnMisc.cpp` | `FString`. |
| `Core/Src/UnCoreNet.cpp` | `UPackageMap`, which uses generations. |
| `Core/Inc/FFileManagerGeneric.h` | `.uz2` compression. |
| `Core/Src/UnClass.cpp` | `FPropertyTag` and `UStruct::SerializeTaggedProperties`: tagged properties. `UField`, `UStruct`, `UState`, `UClass`, `UFunction` and `UConst`'s `Serialize`, and `UStruct::SerializeExpr`: bytecode. |
| `Core/Inc/UnStack.h` | The bytecode's tokens (`EExprToken`). |
| `Core/Src/UnProp.cpp` | Each property type's `Serialize` (its declaration), `SerializeItem` (its value) and `ExportTextItem` (its T3D text). |
| `Core/Src/UnMisc.cpp` | `appFindPackageFile`: finding a package's file by its name. |
| `Engine/Inc/UnTex.h`, `Engine/Src/UnTex.cpp` | `UTexture`, `FMipmap` and `UPalette`: textures' native data. |
| `Engine/Inc/UnRenderResource.h` | `ETextureFormat` and `FColor`. |

## Primitive types

All multi-byte values are **little-endian** (the engine byte-swaps on
big-endian platforms).

| Type | Size | Encoding |
|---|---|---|
| `u8` | 1 | Byte. |
| `i32`, `u32` | 4 | Little-endian integer. The engine's `INT` and `DWORD`. |
| `index` | 1–5 | Compact index, a signed variable-length integer. See below. |
| `guid` | 16 | Four `u32`s: `A`, `B`, `C`, `D`. |
| `string` | variable | `FString`. See below. |
| `name` | 1–5 | An `index` into the package's name table. |
| `object` | 1–5 | An `index` that is an object reference. See [Object references](#object-references). |

### Compact index (`FCompactIndex`)

`Core/Src/UnObj.cpp`, `operator<<(FArchive&, FCompactIndex&)`. Written with
the `AR_INDEX(x)` macro.

A sign-and-magnitude integer, least significant bits first:

| Byte | Bits |
|---|---|
| 0 | bit 7: sign (1 = negative). bit 6: another byte follows. bits 0–5: value bits 0–5. |
| 1–3 | bit 7: another byte follows. bits 0–6: the next 7 value bits. |
| 4 | All 8 bits are the next value bits (only reached after bytes 1–3 all had bit 7 set). |

```text
fn read_index(r) -> i32:
    b0 = r.u8()
    value = b0 & 0x3f
    if b0 & 0x40:
        shift = 6
        for i in 1..=4:
            b = r.u8()
            if i == 4:
                value |= b << shift          # the 5th byte has no continuation bit
                break
            value |= (b & 0x7f) << shift
            shift += 7
            if b & 0x80 == 0: break
    return if b0 & 0x80 { -value } else { value }
```

Values below 64 in magnitude take one byte. The writer takes `Abs(value)`, so
`i32::MIN` can't be written correctly, and a negative zero (`0x80`) reads as 0.

### String (`FString`)

`Core/Src/UnMisc.cpp`, `operator<<(FArchive&, FString&)`.

```text
count: index
if count >= 0: count bytes, Latin-1
else:          -count u16s, UTF-16LE
```

`count` **includes the null terminator**, which is stored. An empty string is
written as `count = 0` with nothing after it; a string that reads as just the
terminator (`count = 1`) is also empty. The writer uses the single-byte form
when every character is at most `0xFF`.

## File layout

```text
offset 0                     Summary
Summary.NameOffset           Name table     (NameCount entries)
                             Export data    (each export's serialized object)
Summary.ImportOffset         Import table   (ImportCount entries)
Summary.ExportOffset         Export table   (ExportCount entries)
                             end of file
```

That's the order `UObject::SavePackage` writes the sections in: the summary
(with placeholder values), the names, each export's data in export-table
order, the imports, the exports, and then it seeks back to 0 and writes the
summary again with the real counts and offsets. Every package in the UT2004
install has exactly this order, with the export data contiguous and the export
table ending at the end of the file.

A reader shouldn't rely on the order, only on the offsets in the summary:
the loader seeks to each table's offset and reads it, and seeks to each
export's `SerialOffset` to load it. Nothing else in the container is located
by position, but see [Absolute offsets inside export data](#absolute-offsets-inside-export-data).

## Summary (`FPackageFileSummary`)

`Core/Src/UnLinker.cpp`, `operator<<(FArchive&, FPackageFileSummary&)`.

| Field | Type | Notes |
|---|---|---|
| `Tag` | `u32` | `0x9E2A83C1` (`PACKAGE_FILE_TAG`), bytes `C1 83 2A 9E`. Anything else isn't a package; the engine stops reading the summary there. Tripwire's engine (Red Orchestra, Killing Floor) writes `0x9E2A83C2` (bytes `C2 83 2A 9E`) instead; UEViewer accepts both. |
| `FileVersion` | `u32` | Low 16 bits: engine version (128). High 16 bits: licensee version (29 for UT2004; 0 in older files). |
| `PackageFlags` | `u32` | `PKG_*`; see below. |
| `NameCount` | `i32` | |
| `NameOffset` | `i32` | Absolute file offset. |
| `ExportCount` | `i32` | |
| `ExportOffset` | `i32` | |
| `ImportCount` | `i32` | |
| `ImportOffset` | `i32` | |
| `Guid` | `guid` | Version ≥ 68. Identifies the package; see [Generations](#guid-and-generations). |
| `GenerationCount` | `i32` | Version ≥ 68. A plain `i32`, not an `index`. |
| `Generations` | `GenerationCount` × (`ExportCount: i32`, `NameCount: i32`) | Version ≥ 68. |

Before version 68 the `Guid` and generations are replaced by
`HeritageCount: i32, HeritageOffset: i32`, pointing at a table of
`HeritageCount` GUIDs elsewhere in the file; the last one is the package's
GUID, and the engine makes up a single generation from `ExportCount` and
`NameCount`. `PACKAGE_MIN_VERSION` is 60: older files make the engine ask
before loading. Name table entries changed at version 64 (see below).

When saving, the engine always writes the current version, `128 | 29 << 16`
(the licensee half only when `GSys->LicenseeMode` is set).

### Package flags

`Core/Inc/UnObjBas.h`, `EPackageFlags`.

| Flag | Value | Meaning |
|---|---|---|
| `PKG_AllowDownload` | `0x0001` | Clients may download it. `SavePackage` always sets this. |
| `PKG_ClientOptional` | `0x0002` | Optional for clients. |
| `PKG_ServerSideOnly` | `0x0004` | Only needed on the server. |
| `PKG_BrokenLinks` | `0x0008` | Set in memory when imports failed to resolve; not meaningful on disk. |
| `PKG_Unsecure` | `0x0010` | Not trusted. |
| `PKG_Official` | `0x0020` | Official package: no downloads or download replacements. Set when saving if `[Packages] SavePackagesAsOfficial` is true in `Official.ini`. |
| `PKG_ExtraDownloadFile` | `0x4000` | Not a real package; a file to download (e.g. `.ogg`). |
| `PKG_Need` | `0x8000` | The client needs to download it. |

In the UT2004 install the values seen are `0x21`, `0x01`, `0x20`, `0x05`,
`0x24` and `0x25`.

## Name table

`Core/Src/UnName.cpp`, `operator<<(FArchive&, FNameEntry&)`. `NameCount`
entries starting at `NameOffset`:

| Field | Type | Notes |
|---|---|---|
| `Name` | `string` | Version ≥ 64. Before 64, a null-terminated Latin-1 string with no length. The engine truncates to 63 characters (`NAME_SIZE` is 64). |
| `Flags` | `u32` | Object flags (`RF_*`, below). |

Every `name` in the file is an index into this table, and every name is
case-insensitive, as names are everywhere in Unreal.

The loader only keeps a name if `Flags` has one of the `RF_LoadForClient`,
`RF_LoadForServer` or `RF_LoadForEdit` bits for the current context; others
become `None`. That's how editor-only names are dropped from a game client.
The other bits are whatever the in-memory name had when the package was saved
(e.g. `RF_TagExp`, `0x10`, is left on by the save). Common values in UT2004:
`0x00070010` (all three contexts), `0x00040010` (editor only) and `0` (no
context).

The writer sorts names by how often they're used, most used first, so `None`
is usually, but not necessarily, index 0. (With a conformal save, see below,
the previous generation's names keep their indices and only the new ones are
sorted.)

## Object references

This is the indexing scheme the import and export tables, and object data,
use for "an object", in this package or another. It's an `i32` (an `index`
on disk, except where noted) that is one of:

| Value | Refers to |
|---|---|
| `0` | Nothing (`None`/null). In a few fields it has a specific meaning instead; see below. |
| `n > 0` | Export `n - 1`. |
| `n < 0` | Import `-n - 1`. |

So exports are numbered 1, 2, 3, ... and imports −1, −2, −3, ...
(`ULinkerLoad::IndexToObject`). The same scheme is used for object references
inside export data (`ULinkerLoad::operator<<(UObject*&)`).

Note that the indices are only meaningful **within one package**. Another
package is never referred to by index: an import names the object it wants,
and the loader resolves it by name when it loads the other package (see
[Resolving imports](#resolving-imports)).

## Import table

`Core/Src/UnLinker.cpp`, `operator<<(FArchive&, FObjectImport&)`.
`ImportCount` entries starting at `ImportOffset`:

| Field | Type | Notes |
|---|---|---|
| `ClassPackage` | `name` | The package that defines the object's class, e.g. `Core`, `Engine`. |
| `ClassName` | `name` | The object's class, e.g. `Package`, `Class`, `Texture`. |
| `PackageIndex` | **`i32`** | The import's outer, as an object reference. **A plain 4-byte `i32`, not an `index`.** |
| `ObjectName` | `name` | |

An import's outer is always another import (`PackageIndex < 0`), or `0` for a
top-level package. A top-level import must be `Core.Package` (`ClassPackage =
Core`, `ClassName = Package`); its `ObjectName` is the package to load, found
by file name on the search paths. Groups within another package are imports of
class `Core.Package` whose outer is another import. For example, from
`DM-Rankin.ut2`:

```text
-2   Core.Package   outer 0    HumanoidArchitecture
-3   Core.Package   outer -2   Bases                  → HumanoidArchitecture.Bases
-4   Engine.Texture outer -3   bas02HAb               → HumanoidArchitecture.Bases.bas02HAb
```

An import's full path is its outer chain, joined with `.`, and its full name
is `ClassName Path` (`ULinker::GetImportFullName`).

Since the class package and name are both names, the class of an import needs
no import of its own. (It does get one if the package refers to the class
object itself, e.g. as some export's class.)

## Export table

`Core/Src/UnLinker.cpp`, `operator<<(FArchive&, FObjectExport&)`.
`ExportCount` entries starting at `ExportOffset`:

| Field | Type | Notes |
|---|---|---|
| `ClassIndex` | `index` | The object's class, as an object reference. **`0` means the export is itself a class** (its class is `Core.Class`). |
| `SuperIndex` | `index` | For structs (classes, structs, states and functions): the parent struct (`UStruct::SuperField`), as an object reference. `0` for none, and for everything else. |
| `PackageIndex` | **`i32`** | The export's outer, as an object reference. **A plain 4-byte `i32`, not an `index`.** `0` means the package itself. |
| `ObjectName` | `name` | |
| `ObjectFlags` | `u32` | `RF_*`, masked to `RF_Load` (the flags that are saved). |
| `SerialSize` | `index` | Size in bytes of the export's data. |
| `SerialOffset` | `index` | Absolute file offset of the export's data. **Only present if `SerialSize != 0`.** |

An export's outer is always another export (`PackageIndex > 0`) or the
package (`0`); the writer asserts this. Groups are exports whose class is
`Core.Package`. An export's full name is `ClassName Package.Group.Object`
(`ULinker::GetExportFullName`).

The package the export's class comes from (`GetExportClassPackage`) is:

- `ClassIndex < 0`: the class is an import; its class package is the `ObjectName` of
  the import's outer (which must be an import).
- `ClassIndex > 0`: the class is defined in this package.
- `ClassIndex == 0`: `Core` (the class is `Core.Class`).

When loading, the engine reads exactly `SerialSize` bytes at `SerialOffset` by
calling the class's `Serialize`, and fails if it reads a different amount.

**Empty exports.** An export with `SerialSize = 0` has no data. In the UT2004
install these are all placeholders: `ClassIndex = 0`, `ObjectName = None`,
`ObjectFlags = 0`. A conformal save (below) writes one for each export of the
previous generation that no longer exists, to keep the other exports'
indices.

### Object flags

`Core/Inc/UnObjBas.h`, `EObjectFlags`. Only the `RF_Load` flags are saved
with exports:

| Flag | Value | Meaning |
|---|---|---|
| `RF_Transactional` | `0x00000001` | Supports undo/redo in the editor. |
| `RF_Public` | `0x00000004` | Visible outside the package. **Only public exports can be imported** by another package. |
| `RF_SourceModified` | `0x00000020` | Modified relative to source files. |
| `RF_Final` | `0x00000080` | Not visible outside the class. |
| `RF_PerObjectLocalized` | `0x00000100` | Localized by instance name, not by class. |
| `RF_Protected` | `0x00000800` | Protected property. |
| `RF_LoadForClient` | `0x00010000` | Load in a game client. |
| `RF_LoadForServer` | `0x00020000` | Load in a server. |
| `RF_LoadForEdit` | `0x00040000` | Load in the editor. |
| `RF_Standalone` | `0x00080000` | Keep even if unreferenced. |
| `RF_NotForClient` | `0x00100000` | Don't load in a game client. |
| `RF_NotForServer` | `0x00200000` | Don't load in a server. |
| `RF_NotForEdit` | `0x00400000` | Don't load in the editor. |
| `RF_HasStack` | `0x02000000` | Has an execution stack (state code). |
| `RF_Native` | `0x04000000` | Native (classes). |

The loader only creates an export if its `ObjectFlags` has one of the
`RF_LoadFor*` bits for the current context, like names.

## Resolving imports

`ULinkerLoad::VerifyImport`. This is how references between packages work,
and why there's no cross-package index to keep consistent:

1. A top-level import (`PackageIndex = 0`) loads the package file named by its
   `ObjectName`.
2. Any other import first resolves its outer, and uses the same package
   (the one at the top of its outer chain).
3. In that package, it looks for an export whose `ObjectName`, class name
   and class package (from `GetExportClassName`/`GetExportClassPackage`)
   all equal the import's. The comparison is by name, case-insensitively.
4. The export's outer must match the import's outer: if the import's outer is
   the top-level package, the export's `PackageIndex` must be 0; if it
   resolved to export `j`, the export's `PackageIndex` must be `j + 1`.
   **Quirk:** an export with `PackageIndex = 0` is also accepted when the outer
   doesn't match. So an import of `Pkg.Group.Obj` can resolve to a top-level
   `Pkg.Obj`.
5. The export must be `RF_Public`. Otherwise loading fails (or the import is
   left unresolved, with `LOAD_Forgiving`).
6. If nothing matches, a public, native, transient object of that class
   and name already in memory is accepted. Otherwise the import fails.

Legacy quirk: an import of class `Mesh` that isn't found is retried as
`LodMesh`.

So rebuilding a package that others import from doesn't break them as long
as the names, classes, outers and `RF_Public` flags they import stay the same.
The indices can change freely.

## Finding packages

A package is found by its name (`appFindPackageFile`, `Core/Src/UnMisc.cpp`):
the engine tries each of the game's `Paths` (`[Core.System]` in its ini) in
turn. A path is a folder and a file pattern, `../Textures/*.utx`, relative to
the game's `System` folder: the engine looks for the file named the package's
name and the pattern's extension (what follows the `*`) in the folder (what
comes before it), and the first that exists is the package.

A mod's `Paths` (with UCC's `-mod=`) come from its configuration, which UCC
makes from the game's `Default.ini` changed by the mod's (`+Paths=` adds,
`-Paths=` removes). They're still relative to the game's `System` folder, so
a mod's own folders are written `../MyMod/System/*.u`.

## GUID and generations

Each package has a GUID, generated when it is first saved, and a list of
*generations*, each recording the `ExportCount` and `NameCount` of one save.
These exist for network play, not for loading.

Over the network, objects and names are sent as indices into the concatenated
export and name tables of every package in the `UPackageMap` (`Core/Src/UnCoreNet.cpp`). A client and
server with different builds of a package (same GUID) agree on a generation,
the older of the two, and only use the exports and names that existed in it.
That only works if a newer build keeps the older one's indices, which is what
a **conformal save** does (`SavePackage` with a `Conform` linker, used by
`ucc conform`): it copies the old GUID and generations, keeps the old names at
their old indices, keeps old exports at their old indices (with empty
placeholders for deleted ones), puts anything new after them, and appends a
new generation. A normal save makes a new GUID and a single generation.

Most packages in UT2004 have one generation; 39 have more (`Engine.u` has 77).

## Absolute offsets inside export data

The container has no other absolute offsets, but some export data does:
`TLazyArray` (`Core/Inc/UnTemplate.h`), used for large arrays such as texture
mipmaps, is written as an `i32` absolute file offset of the end of the array,
followed by the array. The loader uses it to skip the array and load it later.
So **moving an export's data within the file breaks it** unless those offsets
are fixed up, which needs knowledge of the object's format. Copying export data
verbatim is only safe at the same offset.

## Tagged properties

Most objects' data starts with their *tagged properties*: the script-visible
properties whose values differ from their class's defaults (`UObject::Serialize`
in `Core/Src/UnObj.cpp`, then `UStruct::SerializeTaggedProperties` in
`Core/Src/UnClass.cpp`). Each is a tag followed by its value, and the list
ends with the name `None`. What follows is the object's native data, if its
class has any: a texture's mipmaps, a mesh's vertices, a function's bytecode.

usagi reads them as described here; `usagi pkg obj` prints them. This was
checked on the 1,703 packages of a Red Orchestra install with Darkest Hour
(textures, meshes, sounds, scripts and maps): every object's properties read
to their `None`.

### Where they are

- An object with `RF_HasStack` (such as an actor in a map) starts with its
  state frame (`FStateFrame`): `Node` (an object reference), `StateNode` (an
  object reference), `ProbeMask` (`u64`), `LatentAction` (`i32`) and, if
  `Node` isn't 0, the offset in its code (a compact index). The properties
  follow it.
- A class (`ClassIndex` 0) has no tagged properties at the start: its data is
  its `UStruct`, `UState` and `UClass` fields (including its bytecode; see
  [Classes](#classes-states-functions-and-structs)), and its default
  properties come at the end, as tagged properties.
- Every other object starts with its properties. Objects of native classes
  with no script properties (functions, properties, enums, ...) start with
  `None` alone.

### The tag (`FPropertyTag`)

| Field | Type | Notes |
|---|---|---|
| `Name` | compact index | A name. `None` ends the list, and the tag has nothing else. |
| `Info` | `u8` | Bits 0-3: the type. Bits 4-6: the size. Bit 7: an array index follows, or for a bool, its value. |
| `StructName` | compact index | A name. Only for a struct (type 10). |
| `Size` | `u8`, `u16` or `i32` | Only if bits 4-6 are 5, 6 or 7. |
| `ArrayIndex` | 1, 2 or 4 bytes | Only if bit 7 is set and the type isn't bool. |

Bits 4-6 of `Info` are the value's size: 0 to 4 for 1, 2, 4, 12 and 16
bytes, and 5, 6 and 7 for a size in a `u8`, `u16` or `i32` after the tag.

The array index is the element of a static array (`var int Foo[4]`). If the
first byte's top bit is clear, it's the index; if the top two bits are `10`,
the index is the other 6 bits and the next byte (big-endian, 14 bits);
if they're `11`, it's the other 6 bits and the next three bytes (30 bits).

The types (`Core/Inc/UnNames.h`), and their values (`SerializeItem`):

| Type | Name | Value |
|---|---|---|
| 1 | `ByteProperty` | `u8`. An enum's value is its number: which enum isn't stored. |
| 2 | `IntProperty` | `i32` |
| 3 | `BoolProperty` | None: the value is bit 7 of `Info`, and the size is 0. |
| 4 | `FloatProperty` | `f32` |
| 5 | `ObjectProperty` | An object reference (compact index). |
| 6 | `NameProperty` | A name (compact index). |
| 7 | `DelegateProperty` | An object reference, then a name: the function. |
| 8 | `ClassProperty` | An object reference. |
| 9 | `ArrayProperty` | A dynamic array: see [Arrays](#arrays). |
| 10 | `StructProperty` | A struct: see [Structs](#structs). |
| 11 | `VectorProperty` | 3 `f32`s. Not in Red Orchestra's packages: a vector is a struct. |
| 12 | `RotatorProperty` | 3 `i32`s. Not in Red Orchestra's packages: a rotator is a struct. |
| 13 | `StrProperty` | An `FString`. |
| 14 | `MapProperty` | Not in Red Orchestra's packages. |
| 15 | `FixedArrayProperty` | Not in Red Orchestra's packages, but for two tags in one Darkest Hour map, whose name (`None,Tree_round_Lowpoly3,1All_Trees_veg`) isn't a property's. The engine skips them by their size. |

When the engine loads an object, it looks each tag's name up in the class.
A property it doesn't find, or whose type or struct name doesn't match, is
skipped by its size, with a warning. So a reader without the classes can skip
any property by its size, except, from licensee version 28, a struct
(below).

### Structs

From licensee version 28 (`0x1C`; UT2004 and Red Orchestra are 29), a
struct's value is itself a list of tagged properties ending in `None`, except
for `Vector`, `Rotator` and `Color`, which are stored in binary
(`UStructProperty::SerializeItem`). Before it, every struct is stored in
binary: its fields in order, with no tags, so reading one needs its
definition.

In binary, `Vector` is `X`, `Y`, `Z` (`f32`s), `Rotator` is `Pitch`, `Yaw`,
`Roll` (`i32`s), and `Color` is `B`, `G`, `R`, `A` (bytes), in the order the
struct declares them in `Object.uc`.

**The size in a tagged struct's tag can be wrong**, and the loader ignores
it: from licensee version 28, it reads the struct by its tags, and doesn't
seek to where the size says it ends. The size is wrong because of how the
engine works it out when it saves: it serializes the value to a counting
archive (`FArchiveCountSize`), and to build each tag inside the struct, it
counts that property's value with a second counting archive whose
`MapObject` and `MapName` are the first's, which aren't overridden and return
0. So every object and name inside the struct is counted as one byte. When
the real index takes 3 bytes or more (an index of 8,192 or more), the
property's real size is 3 or more, so its tag gets a size byte (bits 4-6 are
5), which the struct's size didn't count. A struct's size is short by one
byte for each such property in it. This happens in big maps, where export
and name indices are large: an actor's `Region`, whose `Zone` is an object.

So a tagged struct must be read by its tags, as usagi does.

### Arrays

A dynamic array's value is its length (a compact index), then its elements,
each as `SerializeItem` writes the element type. The tag doesn't say what the
element type is, so the elements can't be read without the class, except
that an array of tagged structs is self-describing.

The size of an array of tagged structs is wrong in the same way as a
struct's (Materials of a static mesh in a big map, for example), and in some
other ways too: Darkest Hour's class defaults have arrays of structs whose
size is 2 bytes short, with no large indices involved. The loader doesn't
look at it ([below](#reading-by-the-declarations)).

Without the declarations, usagi takes an array's elements to be tagged
structs if they read as such (each a non-empty list of properties of known
types ending in `None`), and either end where the tag's size says, or a tag
follows them and no property's tag is where the size says. A `None` where
the size says doesn't count: in a short array, the size can end at the `None`
that ends its last element. Otherwise it skips the array by its size. This is
a guess, and it's wrong for some of Darkest Hour's arrays: an array of names
whose bytes happen to read as structs, followed by properties. With the
declarations, nothing is guessed.

### Reading by the declarations

The loader reads each property by its declaration, which it finds by the
tag's name in the class (or, for a struct's properties, the struct) and its
superclasses (`UStruct::SerializeTaggedProperties`):

1. If there's no such property, the value is skipped, by the tag's size.
2. Old packages: a static array saved before the property became a dynamic
   array (the tag's type is the array's element type) is read as one element
   at the tag's array index; and a byte saved before the property became a
   float is read as a byte.
3. If the tag's type isn't the property's (`GetID`, the index of its class's
   name: a `ClassProperty`'s is `ObjectProperty`'s), the array index is
   beyond a static array's size, a struct's name isn't the property's struct's,
   or the property isn't stored (`CPF_Native`, or `CPF_Transient`), the value is
   skipped, by its size.
4. Otherwise, it's read as the declaration's type reads it (`SerializeItem`),
   whatever the tag's size says. From licensee version 28, that's where it
   ends. Before it, the loader then seeks to where the tag's size says it
   ends, if that's elsewhere (for a struct that has changed).

`SerializeItem` reads a byte, int, float or pointer as such; a bool inside
an array or struct as a byte (a property's own bool is in its tag); an
object or name as a compact index; a string as an `FString`; a delegate as an
object and a name; a dynamic array as its length (a compact index) and its
elements; a static array (`FixedArrayProperty`) as its elements; a struct as
[above](#structs), and in binary by its declaration (its own properties, then
its superclass's, each element of a static array in turn, except those that
aren't stored); and a map (`MapProperty`) as nothing at all.

usagi reads tagged properties this way when it can find the declarations: in
the package or the packages it imports, which it finds as
[the engine does](#finding-packages). A class's declarations are its
`Children`, a chain of fields through each one's `Next`, and its superclass is
its `SuperField`. For a property whose declaration can't be found, because a
package or a superclass can't be read, it guesses as above. This reads every
class's defaults in UT2004's, Red Orchestra's and Darkest Hour's `.u` files
(15,147 classes) to their last byte.

## Native data

After an object's tagged properties (after their `None`) comes its class's
native data, written by the class's C++ `Serialize` after `Super::Serialize`
has written the properties. Its layout depends on the class, and isn't
described anywhere in the data: a reader has to know the class. `usagi pkg
export` writes it, unread, to an object's `.bin` file.

### Classes, states, functions and structs

Each serializes its parent's data, then its own (`Core/Src/UnClass.cpp`). A
class's data comes right after its state frame, if it has one; a state's,
function's or struct's, after its tagged properties (`None` alone).

`UField`:

| Field | Type | Notes |
|---|---|---|
| `SuperField` | object | The superclass, parent state, or the function or struct this one overrides or extends. |
| `Next` | object | The next field of the struct this one is in. |

`UStruct`, after `UField`:

| Field | Type | Notes |
|---|---|---|
| `ScriptText` | object | The source code, a `TextBuffer`. |
| `Children` | object | The first of its fields. |
| `FriendlyName` | name | |
| `StructFlags` | `u32` | From licensee version 26 (`0x1A`). |
| `Line`, `TextPos` | `i32`, `i32` | Where it is in the source. |
| `ScriptSize` | `i32` | The size of the [bytecode](#bytecode) in memory. |
| `Script` | | The bytecode. |

`UState`, after `UStruct`: `ProbeMask` (`u64`), `IgnoreMask` (`u64`),
`LabelTableOffset` (`u16`) and `StateFlags` (`u32`).

`UClass`, after `UState`, then its default properties as tagged properties:

| Field | Type | Notes |
|---|---|---|
| `OldClassRecordSize` | `i32` | Up to version 61. |
| `ClassFlags` | `u32` | |
| `ClassGuid` | GUID | |
| `Dependencies` | array | Each a class (object), `Deep` (`u32`) and `ScriptTextCRC` (`u32`) (`FDependency`). |
| `PackageImports` | array of names | |
| `ClassWithin` | object | From version 62. |
| `ClassConfigName` | name | From version 62. |
| `HideCategories` | array of names | From version 99. |

An array is a compact index count, then the items.

`UFunction`, after `UStruct`:

| Field | Type | Notes |
|---|---|---|
| `ParmsSize` | `u16` | Up to version 63. |
| `iNative` | `u16` | Its native function number, or 0. |
| `NumParms` | `u8` | Up to version 63. |
| `OperPrecedence` | `u8` | |
| `ReturnValueOffset` | `u16` | Up to version 63. |
| `FunctionFlags` | `u32` | |
| `RepOffset` | `u16` | Only if replicated (`FUNC_Net`, `0x40`). |

A struct (`Core.Struct`) is `UStruct` alone.

### Bytecode

The `ScriptSize` before a struct's code is its size **in memory**, which
isn't its size in the file: in memory, an object reference or a name takes 4
bytes, and in the file it's a compact index of 1 to 5. So the code can't be
skipped by its size. The loader reads it expression by expression
(`UStruct::SerializeExpr`), keeping count of the size in memory, until that
reaches `ScriptSize`; it fails if it doesn't match exactly. Jump offsets in
the code are offsets in memory too.

An expression is a token (`EExprToken`, `Core/Inc/UnStack.h`), a byte, then
what that token reads: values, and other expressions. A token from `0x70`
calls the native final function with that number; from `0x60`, the native
function `(token - 0x60) << 8 | next byte`. Both are followed by their
arguments, expressions up to `EX_EndFunctionParms` (`0x16`). So are
`EX_VirtualFunction` and `EX_GlobalFunction` (after a name),
`EX_FinalFunction` (after an object). A function call that's followed by an
`EX_DebugInfo` (`0x42`) whose version is 100 reads it as part of the call.
Tokens the engine doesn't read (`0x03`, `0x15`, `0x2B`, `0x35`, `0x3A` to
`0x3F`, `0x46`, `0x48` to `0x5F`) are an error.

The expressions are as `SerializeExpr` reads them, which isn't always what
they mean: `EX_BoolVariable` (`0x2D`) has nothing after it, since the
variable it's about is read as an expression of its own. An `EX_LabelTable`
(`0x0C`) must start 4-byte aligned in memory; its entries are a name and an
`i32` each (8 bytes in memory), up to the one named `None`.

Every class, state, function and struct in UT2004's and Red Orchestra's
and Darkest Hour's `.u` files reads this way to the last byte, and is
written back the same, byte for byte.

### Property declarations

A property declaration (`IntProperty`, `ArrayProperty`, ...) is a field of a
class, struct or function: after its tagged properties, `UField`, then
`UProperty::Serialize` (`Core/Src/UnProp.cpp`), then its class's:

| Field | Type | Notes |
|---|---|---|
| `ArrayDim` | `i32` | A static array's size; 1 otherwise. |
| `PropertyFlags` | `u32` | `CPF_` flags. |
| `Category` | name | |
| `RepOffset` | `u16` | Only if replicated (`CPF_Net`, `0x20`). |
| `CommentString` | `FString` | Only with `CPF_CommentString` (`0x02000000`). |

then, by its class: `ByteProperty`, its enum (object, or 0);
`ObjectProperty`, its class; `ClassProperty`, its class and metaclass;
`DelegateProperty`, its function; `StructProperty`, its struct;
`ArrayProperty`, its element, `Inner` (a property, an object reference);
`FixedArrayProperty`, `Inner` and its count (`i32`); `MapProperty`, its key
and value (objects). `IntProperty`, `BoolProperty`, `FloatProperty`,
`NameProperty`, `StrProperty` and `PointerProperty` add nothing.

### Enums, constants and text buffers

After `UField`, an enum (`UEnum`) has its values, an array of names; a
constant (`UConst`), its value as source text, an `FString`. A text buffer
(`UTextBuffer`, a class's source code), after its tagged properties, has
`Pos` and `Top` (`i32`s) and `Text` (an `FString`).

### Textures (`Engine.Texture`)

`UTexture::Serialize` (`Engine/Src/UnTex.cpp`) writes only the mipmaps, with
`SerializeMips`; `UMaterial` and `UBitmapMaterial` write nothing of their own.
The mipmaps are an array of `FMipmap` (`Engine/Inc/UnTex.h`, its
`operator<<`):

| Field | Type | Notes |
|---|---|---|
| Count | compact index | The number of mipmaps, largest first. |

then for each mipmap:

| Field | Type | Notes |
|---|---|---|
| `SeekPos` | `i32` | The absolute offset in the file of the end of `Data` (see [Absolute offsets inside export data](#absolute-offsets-inside-export-data)). It's only there to skip the data; it's not needed to read it. |
| `Data` | compact index count, then that many bytes | The pixels, in the texture's `Format` (a `TLazyArray<BYTE>`, `Core/Inc/UnTemplate.h`). |
| `USize`, `VSize` | `i32`, `i32` | The mipmap's width and height. |
| `UBits`, `VBits` | `u8`, `u8` | Their base-2 logarithms: `USize` is `1 << UBits`. |

The texture's format isn't in the native data: it's the tagged property
`Format` (`ETextureFormat`, `Engine/Inc/UnRenderResource.h`), and when it
isn't stored, it's the default, 0 (`TEXF_P8`). How each format stores a
mipmap's pixels, with the number of textures of each in UT2004's
`Textures/*.utx`:

| `Format` | Name | Pixels | Textures |
|---|---|---|---|
| 0 | `TEXF_P8` | 1 byte per pixel: an index into the texture's `Palette` (a tagged property, a reference to a `Palette` object). | 559 |
| 1 | `TEXF_RGBA7` | Not found in UT2004's textures, so not described here. | 0 |
| 2 | `TEXF_RGB16` | Not found in UT2004's textures, so not described here. | 0 |
| 3 | `TEXF_DXT1` | BC1: 8 bytes per 4×4 block. | 2,992 |
| 4 | `TEXF_RGB8` | Not found in UT2004's textures, so not described here. | 0 |
| 5 | `TEXF_RGBA8` | 4 bytes per pixel, in the order **B, G, R, A** (`FColor` in memory on Intel; UModel reads it as BGRA8). | 663 |
| 6 | `TEXF_NODATA` | Not found in UT2004's textures, so not described here. | 0 |
| 7 | `TEXF_DXT3` | BC2: 16 bytes per 4×4 block. | 1,511 |
| 8 | `TEXF_DXT5` | BC3: 16 bytes per 4×4 block. | 3,860 |
| 9 | `TEXF_L8` | 1 byte per pixel, grey. | 0 |
| 10 | `TEXF_G16` | 2 bytes per pixel, grey. | 3 |
| 11 | `TEXF_RRRGGGBBB` | Not found in UT2004's textures, so not described here. | 0 |

A compressed mipmap smaller than a block (a 2×2 or 1×1 DXT mipmap) still
takes a whole block. Rows run top to bottom.

Every one of those 9,588 textures (objects of class `Texture` exactly, not
its subclasses) read this way to the last byte of its data, with each
mipmap's size matching its format and dimensions and its `UBits` and `VBits`
matching its size.

Things to allow for, from the engine and UModel (`Unreal/UnrealMaterial/
UnTexture2.cpp`):

- A mipmap's data can be empty; UModel uses the first mipmap that has data
  (it cites `DemoPlayerSkins.utx`).
- A procedural texture (`bParametric`, such as `FireTexture`) has its data
  emptied when it's saved: it's made at run time.
- Before version 84, a texture with `bHasComp` has two arrays of mipmaps,
  and the first is thrown away.

UnrealEd exports DXT1, DXT3 and DXT5 textures as DDS (`UTextureExporterDDS`
in `Editor/Src/UnEdFact.cpp`): a DDS header, then every mipmap's data as it
is. It exports `TEXF_RGBA8` as TGA (`UTextureExporterTGA`), the rows of the
first mipmap bottom to top, the bytes as they are, since TGA is BGRA too.

### Palettes (`Engine.Palette`)

`UPalette::Serialize` writes its colours, a `TArray<FColor>`: a compact index
count (256 in every palette in UT2004's textures), then 4 bytes each, in the
order **R, G, B, A** (`FColor`'s `operator<<`). Before version 66, the alpha
isn't stored right: the engine sets it to 255 when loading.

## Compressed packages (`.uz2`)

Packages for redirect downloads are compressed with `ucc compress` into
`<file>.uz2` (`Core/Inc/FFileManagerGeneric.h`, `FILECOPY_Compress`). The
file is a sequence of blocks, one per 32 KiB (`COPYBLOCKSIZE`) of the
original:

| Field | Type | Notes |
|---|---|---|
| `CompressedSize` | `u32` | At most 33096 (`MAXCOMPSIZE`). |
| `UncompressedSize` | `u32` | At most 32768. |
| `Data` | `CompressedSize` bytes | A zlib stream (zlib's `compress`). |

until the end of the file. Concatenating the decompressed blocks gives the
original package.

## Reading a package

1. Read the summary; check the tag; take the version from the low 16 bits.
2. Seek to `NameOffset` and read `NameCount` names.
3. Seek to `ImportOffset` and read `ImportCount` imports (resolving their
   names).
4. Seek to `ExportOffset` and read `ExportCount` exports.
5. For an export's data, read `SerialSize` bytes at `SerialOffset`.

Checks worth making, since the engine makes them: name indices in range,
object references in range, import outers that are imports, export outers
that are exports, top-level imports of class `Core.Package`, and export data
within the file.
