# What happens to references when you rename or delete

This explains why renaming or deleting something in a UE2 package (UT2004
build 3369) doesn't update the packages that refer to it, and what those
packages do the next time they load. It builds on
[`PACKAGE_FORMAT.md`](PACKAGE_FORMAT.md); the references are to the engine
source.

## The short version

A package refers to an object in another package **by name**, not by pointer
or ID. The reference is an import: the class package, the class name, the
object's name and its outer chain (package and groups), all stored as text in
the referencing package. Nothing in the referenced package records who refers
to it, and the engine never searches for referencers on disk.

So a rename or delete only changes the package it happens in, plus any
package that is **loaded in memory at the time and then saved**. Every other
referencing package still has the old name and finds out when it next loads.

```mermaid
flowchart LR
    subgraph A["HumanoidArchitecture.utx (on disk)"]
        AE["export: Texture Bases.bas02HAb"]
    end
    subgraph B["DM-Rankin.ut2 (on disk)"]
        BI1["import -2: Core.Package HumanoidArchitecture"]
        BI2["import -3: Core.Package Bases, outer -2"]
        BI3["import -4: Engine.Texture bas02HAb, outer -3"]
        BX["export data: ... object ref -4 ..."]
        BX --> BI3 --> BI2 --> BI1
    end
    BI1 -. "found by file name on Paths" .-> A
    BI3 -. "matched by name + class + outer at load time" .-> AE
```

There is no arrow from A to B: A doesn't know B exists.

## Renaming an object in the editor

The editor's rename (`OBJ RENAME`, `Editor/Src/UnEdSrv.cpp`) calls
`UObject::Rename` (`Core/Src/UnObj.cpp`), which only changes the in-memory
object's name and outer. It marks the old and new packages dirty and nothing
else.

```mermaid
flowchart TD
    R["OBJ RENAME OLDNAME=... NEWNAME=..."] --> RN["UObject::Rename: change the in-memory name and outer"]
    RN --> D["Mark the old and new packages dirty"]
    D --> Q{"Is a referencing package loaded in memory right now?"}
    Q -- "Yes" --> MEM["Its references are pointers to the object,<br/>so they now point at the renamed object"]
    MEM --> SAVEQ{"Do you save that package?"}
    SAVEQ -- "Yes" --> NEWIMP["SavePackage rebuilds its import table from the pointers:<br/>the import gets the NEW name. Fixed."]
    SAVEQ -- "No (or reload it from disk)" --> STALE
    Q -- "No (it's only on disk)" --> STALE["Its import still has the OLD name. Stale."]
    D --> SAVEA["Save the renamed object's package:<br/>its export now has the NEW name"]
    SAVEA --> GONE["Nothing in it has the old name any more"]
    STALE --> LOAD["Next time it loads: see 'Loading a stale reference'"]
    GONE --> LOAD
```

`SavePackage` doesn't keep imports from the previous file: it builds the
import table each time from the objects the package's data points at
(`FArchiveSaveTagImports`). That's why loading a referencer before renaming
and saving it afterwards fixes it, and why nothing else does.

## Deleting an object in the editor

The editor's delete (`DELETE CLASS=... OBJECT=...`, `UnrealEd/Src/EdHook.cpp`)
refuses if the object is referenced, but it uses `UObject::IsReferenced`,
which only looks at objects **in memory**. Packages on disk are not checked.

```mermaid
flowchart TD
    DEL["DELETE CLASS=... OBJECT=..."] --> REF{"IsReferenced: does anything in memory refer to it?"}
    REF -- "Yes" --> INUSE["'... is in use': not deleted"]
    REF -- "No" --> GO["Delete it and mark its package dirty"]
    GO --> SAVE["Save the package: the export is gone"]
    SAVE --> DISK{"Do packages that aren't loaded refer to it?"}
    DISK -- "Yes (never checked)" --> STALE["Their imports name an object that no longer exists"]
    DISK -- "No" --> OK["Done"]
    STALE --> LOAD["Next time they load: see 'Loading a stale reference'"]
```

## Renaming or deleting a package file

A top-level import (outer 0) is a `Core.Package` import whose name is the
package's **file name**, without the extension. The loader finds the file with
`appFindPackageFile` (`Core/Src/UnMisc.cpp`), trying the name against each
`Paths=` entry in the `[Core.System]` section of the game's `.ini`. Renaming
the file renames the package as far as everyone else is concerned. And unlike
a missing object, a missing package is fatal for whatever is loading.

```mermaid
flowchart TD
    L["Load a package that imports Foo.Bar"] --> TOP["Resolve the top-level import 'Foo'"]
    TOP --> MEMQ{"Is Foo already loaded?"}
    MEMQ -- "Yes" --> USE["Use it"]
    MEMQ -- "No" --> FIND["appFindPackageFile('Foo'): try Foo with each Paths= pattern"]
    FIND -- "Found" --> USE
    FIND -- "Not found (renamed, deleted or not on Paths)" --> ERR["Throw 'Can't find file for package Foo'"]
    ERR --> FAIL["The whole load fails: the map, mod or package doesn't load"]
    USE --> NEXT["Resolve the rest of the import: see below"]
```

Renaming a package *inside* the editor (moving objects with `NEWPACKAGE=`)
is an object rename like any other: the objects move to a new package in
memory, and only loaded, re-saved referencers follow them.

## Loading a stale reference

This is `ULinkerLoad::VerifyImport` (`Core/Src/UnLinker.cpp`), which runs for
every import when a package loads. The package file was found; now the loader
looks for the object in it.

```mermaid
flowchart TD
    I["Import: ClassPackage.ClassName ObjectName, outer chain"] --> H["Look through the source package's exports for one with<br/>the same name, class name and class package"]
    H --> M{"Found, with a matching outer?<br/>(an export at the top level also matches)"}
    M -- "Yes" --> PUB{"Is it RF_Public?"}
    PUB -- "Yes" --> OK["Resolved: the reference works"]
    PUB -- "No" --> PRIV["Error: 'Can't import private object'.<br/>The load fails."]
    M -- "No" --> CLS{"Is the import's class known?<br/>(its class package is loaded and has the class)"}
    CLS -- "Yes" --> NAT{"Is there a public, native, transient<br/>object with that name in memory?"}
    NAT -- "Yes" --> OK
    NAT -- "No" --> MISS["Warning: 'Missing Texture Foo.Group.Bar'<br/>(in the editor, also the load errors dialog)"]
    MISS --> NONE["The import stays unresolved:<br/>every reference to it loads as None"]
    CLS -- "No" --> FI["Error: 'Can't find ... in file ...'.<br/>The load fails."]
```

The usual result of a rename or delete is the **Missing** path: the object's
class (`Engine.Texture`, `Engine.StaticMesh`, `Core.Class`...) is loaded, so
the loader only warns, and the reference becomes `None`. The package loads
fine, with the texture, mesh or sound gone. One case is worse: if the missing
import is the **class** of one of the package's exports (e.g. a renamed
UnrealScript class whose actors are placed in a map), the loader has no class
to create that export with, and loading typically ends in a serialization
error.

### The None gets saved

If you then save the referencing package, the dead reference is saved as
`None`, and since nothing points at the import any more, the import isn't
written. The old name is gone from the file, so you can't find or fix the
reference afterwards, even by restoring the old name.

```mermaid
flowchart LR
    A["Stale import, old name"] --> B["Load: 'Missing ...' warning, reference = None"]
    B --> C["Save the package"]
    C --> D["Reference written as 0 (None),<br/>import dropped from the table"]
    D --> E["Link permanently lost"]
```

## `ucc make`

`ucc make` (`Editor/Src/UMakeCommandlet.cpp`) goes through `EditPackages` in
order and only compiles a package if **loading its `.u` file fails**, which
usually means the file doesn't exist. A dependent package's `.u` that still
loads isn't recompiled, even if a class it uses was renamed or deleted.

```mermaid
flowchart TD
    S["For each EditPackages entry, in order"] --> LP{"Does Pkg.u load?"}
    LP -- "Yes" --> SKIP["Keep it as it is, stale imports included"]
    LP -- "No (missing, or an import fails fatally)" --> COMP["Compile Pkg from Pkg/Classes/*.uc and save it"]
    COMP --> NEWREF["Its imports are made from the current names:<br/>a renamed class is a compile error in its .uc files"]
    SKIP --> WARN["Stale references show up as 'Missing ...' warnings<br/>(or fail) when something loads it"]
```

So after renaming or deleting a class, delete the `.u` files of every package
that depends on it, not just its own, so they are recompiled against the new
names. (A stale import can fail the load fatally, in which case `ucc make`
rebuilds the package anyway, but the common "Missing" case loads fine and is
skipped.)

## Renaming safely

- **In the editor:** load every package that refers to the object (open the
  maps, load the packages) *before* renaming it, then save all of them along
  with the renamed object's package. Their imports are rebuilt from memory
  with the new name.
- **In script packages:** delete the `.u` of every dependent package and
  rebuild them.
- **Package files:** don't rename one that other packages already import from.
  Every referencer would need to be loaded and re-saved against the new name,
  which the editor can't do because the old file name is how it finds the
  package in the first place.
- **Check afterwards:** load the referencing packages and look for "Missing"
  warnings in the log (or the editor's load errors dialog) *before* saving
  them, because saving turns them into permanent `None`s.
