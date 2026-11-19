//! Reads Unreal Engine 2 package files (`.u`, `.utx`, `.usx`, `.ukx`,
//! `.uax`, `.ut2`, ...), as written by UT2004 and other UE2 games, and their
//! `.uz2` compressed form.
//!
//! The format is described in `docs/PACKAGE_FORMAT.md` in the repository.
//! The summary and the name, import and export tables are read; what's in
//! each export's data isn't.
//!
//! ```no_run
//! let package = usagi_pkg::Package::read("System/Engine.u".as_ref())?;
//! println!("version {}", package.summary.version());
//! # Ok::<(), usagi_pkg::Error>(())
//! ```

mod error;
mod flags;
mod package;
mod reader;
mod summary;
mod tables;
pub mod uz2;

pub use error::{Error, Result};
pub use flags::{ObjectFlags, PackageFlags};
pub use package::Package;
pub use summary::{
    Generation, Guid, Heritage, PACKAGE_FILE_TAG, Summary, TRIPWIRE_PACKAGE_FILE_TAG,
};
pub use tables::{Export, Import, NameEntry, ObjectRef};
