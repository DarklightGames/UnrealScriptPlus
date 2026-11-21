//! Reads Unreal Engine 2 package files (`.u`, `.utx`, `.usx`, `.ukx`,
//! `.uax`, `.ut2`, ...), as written by UT2004 and other UE2 games, and their
//! `.uz2` compressed form.
//!
//! The format is described in `docs/PACKAGE_FORMAT.md` in the repository.
//! The summary and the name, import and export tables are read, and in each
//! export's data, its tagged properties ([`Package::properties`]) and, for
//! classes, states, functions, structs, property declarations and the like,
//! its native data ([`Package::class`], [`Package::read_native`]), and
//! textures' and sounds' ([`Package::texture`], [`Texture`], [`Sound`]).
//!
//! Native data is read and written by one function per type, as the engine
//! does ([`archive::Serialize`]), so what's read can be written back the
//! same. [`Packages`] finds the packages a package refers to, and reads
//! tagged properties by their declarations ([`Packages::properties`]).
//! [`diff`] compares two versions of a package.
//!
//! ```no_run
//! let package = usagi_pkg::Package::read("System/Engine.u".as_ref())?;
//! println!("version {}", package.summary.version());
//! # Ok::<(), usagi_pkg::Error>(())
//! ```

pub mod archive;
mod class;
pub mod diff;
mod edit;
mod error;
mod flags;
mod media;
mod package;
mod packages;
mod pattern;
mod properties;
mod reader;
pub mod script;
mod summary;
mod tables;
pub mod uz2;
pub mod volatile;

pub use class::{
    Class, Const, Dependency, Enum, Field, Function, PropertyDecl, PropertyKind, State, StateFrame,
    Struct, TextBuffer,
};
pub use edit::set_package_flags;
pub use error::{Error, Result};
pub use flags::{ObjectFlags, PackageFlags};
pub use media::{
    Mipmap, SOUND_GROUP_SOUNDS_LICENSEE_VERSION, Sound, SoundGroup, TEXTURE_MIPS_VERSION, Texture,
};
pub use package::{Package, package_name};
pub use packages::{Decl, Lookup, Packages, Scope, SearchPath, Type};
pub use pattern::ObjectPattern;
pub use properties::{Properties, Property, PropertyType, TAGGED_STRUCTS_LICENSEE_VERSION, Value};
pub use summary::{
    Generation, Guid, Heritage, PACKAGE_FILE_TAG, Summary, TRIPWIRE_PACKAGE_FILE_TAG,
};
pub use tables::{Export, Import, NameEntry, ObjectRef};
