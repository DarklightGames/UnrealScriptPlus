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
summary, the name, import and export tables, and `.uz2` files), and
`usagi pkg list` prints a package's summary, imports and exports.

It covers the *container*: the summary, the name, import and export tables, and
how they reference each other. It doesn't cover what's inside an export's data
(properties, bytecode, textures and so on).

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
