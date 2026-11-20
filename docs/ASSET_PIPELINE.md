# Asset pipeline (design)

A design for exporting objects' native data to usable formats (textures to
DDS and PNG, sounds to WAV, meshes and animations to glTF, ...), and later for
importing those formats back into package data. **Nothing here is
implemented yet** except the fallback: `usagi pkg export` writes every
object's tagged properties as T3D and its native data, unread, as `.bin`
(see [CLI.md](CLI.md#usagi-pkg-export-paths---output-dir)). The native data
formats known so far are in
[PACKAGE_FORMAT.md](PACKAGE_FORMAT.md#native-data).

## Goals

- Export any kind of object, not just textures, without each new kind
  meaning changes all over the code.
- No pairing of every class with every file format: N readers and M writers
  should be N + M pieces of code, not N × M.
- Anything without a reader still exports, as `.t3d` and `.bin`.
- The same pieces work the other way round: a file format read into package
  data.

## Layers

```
 package bytes ──► [1. native data] ──► [2. adapter] ──► asset model ──► [3. writer] ──► file
               ◄──                  ◄──              ◄──             ◄── [3. reader]  ◄──
                          ▲
                [0. object context]
```

### 0. Object context (`usagi-pkg`)

What every reader needs, from one place:

- The object's class chain, most derived first.
- Its properties, typed (`props.get::<u8>("Format")`), **with class
  defaults**: a property equal to its default isn't stored (a texture
  without `Format` is `TEXF_P8`).
- A resolver for the objects it refers to (a texture's `Palette`, a mesh's
  materials), in the same package or another one.
- The package's version and licensee version.

Class chains and defaults come from the game's `.u` files, not from a table
in usagi: a package only names its classes (`Engine.Texture`), and the
superclass and `defaultproperties` are in the package that declares the
class. This needs:

- Reading a class export's data: its `UStruct`, `UState` and `UClass`
  fields, then its default properties (tagged properties, after the script).
  `usagi pkg obj` can't read them yet.
- Finding a class's package from the game directory (`--game DIR`, or the
  root in `usagi.toml`).

### 1. Native data (`usagi-pkg`)

Types that mirror the engine's C++ `Serialize` functions one for one:
`FMipmap`, `TLazyArray`, `FColor`, and later meshes' and sounds' structures.
Each is serialized by a single function that both reads and writes, as the
engine's `FArchive` and `operator<<` do, so reading and writing can't
disagree. `TLazyArray`'s absolute `SeekPos` is handled once, in its own
type, and fixed up on writing.

**We intend to generate these functions with a derive macro, in a
proc-macro crate of its own**, once a few have been written by hand (texture,
palette, sound and a mesh) and the pattern is clear. What matters most is
that it's bidirectional, so one declaration gives both reading and writing,
and self-documenting, so the declaration *is* the layout:

```rust
#[derive(Serialize)]
struct Mipmap {
    data: LazyArray<u8>,   // SeekPos, then the array
    u_size: i32,
    v_size: i32,
    u_bits: u8,
    v_bits: u8,
}
```

It would also give, from the same declaration:

- Round-trip tests (read, write, compare) over every object in a corpus.
- A visitor over the fields, for dumps of any native data (names, offsets,
  values) and error messages with field paths (`Mips[3].Data: ...`).
- Version differences as attributes (`#[since(84)]`, `#[licensee(...)]`)
  rather than conditions at each call site.

Layouts that don't fit (the two mipmap arrays before version 84, sizes that
depend on a property) implement the same trait by hand. Until the macro
exists, every type is written by hand against a bidirectional `Archive`
trait, in the shape the macro will generate, so that moving to it changes
nothing built on top.

Readers are registered by class path (`Engine.Texture`). Lookup walks the
class chain to the nearest registered class: a subclass's data starts with
its parent's (`Super::Serialize`), so a `Cubemap` gets its `Texture` part
read and only the rest stays unread.

### 2. Adapters

Small functions per class between native data and a game-independent asset
model: `Texture` ↔ `Image`, `Sound` ↔ `Audio`, `StaticMesh` and
`SkeletalMesh` ↔ `Mesh`, `MeshAnimation` ↔ `Animation`. The models are few:
`Image` (format, mipmaps, palette), `Audio`, `Mesh`, `Animation`.

### 3. Writers and readers of file formats (`usagi-io`)

Each declares the asset model it takes and the format it writes: `Image` →
DDS, PNG, TGA; `Audio` → WAV; `Mesh` → glTF, PSK; `Animation` → glTF, PSA.
A writer may refuse an asset it can't hold without loss (DDS and a `P8`
image). Readers of formats, for importing, go the other way.

They're in a crate of their own, `usagi-io`, so that `usagi-pkg` stays a
reader of the package format without image or audio dependencies.

## CLI

`usagi pkg export --format <FORMAT>` picks the writer, with a default for
each kind of asset (DDS for DXT textures, PNG for other images, ...).
`--raw` writes `.bin` as well. Objects without a reader or a writer are
written as `.t3d` and `.bin`, as now.

## Open questions

- When to move this onto a persistent runtime (see
  [WISHLIST.md](WISHLIST.md#a-persistent-runtime)), since resolving
  references and class defaults means reading many packages.
