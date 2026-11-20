//! Reads Unreal Engine 2 package files (`.u`, `.utx`, `.usx`, `.ukx`,
//! `.uax`, `.ut2`, ...), as written by UT2004 and other UE2 games, and their
//! `.uz2` compressed form.
//!
//! The format is described in `docs/PACKAGE_FORMAT.md` in the repository.
//! The summary and the name, import and export tables are read, and in each
//! export's data, its tagged properties ([`Package::properties`]).
//!
//! ```no_run
//! let package = usagi_pkg::Package::read("System/Engine.u".as_ref())?;
//! println!("version {}", package.summary.version());
//! # Ok::<(), usagi_pkg::Error>(())
//! ```

mod edit;
mod error;
mod flags;
mod package;
mod properties;
mod reader;
mod summary;
mod tables;
pub mod uz2;

pub use edit::set_package_flags;
pub use error::{Error, Result};
pub use flags::{ObjectFlags, PackageFlags};
pub use package::{Package, package_name};
pub use properties::{Properties, Property, PropertyType, TAGGED_STRUCTS_LICENSEE_VERSION, Value};
pub use summary::{
    Generation, Guid, Heritage, PACKAGE_FILE_TAG, Summary, TRIPWIRE_PACKAGE_FILE_TAG,
};
pub use tables::{Export, Import, NameEntry, ObjectRef};
