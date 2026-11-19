//! The package file summary (`FPackageFileSummary`), at the start of every
//! package. See `operator<<(FArchive&, FPackageFileSummary&)` in the
//! engine's `Core/Src/UnLinker.cpp`.

use std::fmt;

#[cfg(feature = "serde")]
use serde::{Serialize, Serializer};

use crate::reader::Reader;
use crate::{Error, PackageFlags, Result};

/// The first four bytes of a package (`PACKAGE_FILE_TAG`).
pub const PACKAGE_FILE_TAG: u32 = 0x9E2A83C1;

/// The tag of packages written by Tripwire's engine: Red Orchestra, Killing
/// Floor and their mods. The rest of the file is the same as with
/// [`PACKAGE_FILE_TAG`].
pub const TRIPWIRE_PACKAGE_FILE_TAG: u32 = 0x9E2A83C2;

/// The first version with a GUID and generations. Older files have a
/// heritage table instead.
const GENERATIONS_VERSION: u16 = 68;

/// The package file summary.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Summary {
    /// [`PACKAGE_FILE_TAG`], or [`TRIPWIRE_PACKAGE_FILE_TAG`] in Red
    /// Orchestra and Killing Floor.
    pub tag: u32,
    /// The file version as stored: the engine version in the low 16 bits and
    /// the licensee version in the high 16. See [`Summary::version`] and
    /// [`Summary::licensee_version`].
    pub file_version: u32,
    pub package_flags: PackageFlags,
    pub name_count: u32,
    /// The absolute file offset of the name table.
    pub name_offset: u32,
    pub export_count: u32,
    pub export_offset: u32,
    pub import_count: u32,
    pub import_offset: u32,
    /// The package's GUID. Before version 68, the last GUID of the heritage
    /// table, or all zeros if it's empty.
    pub guid: Guid,
    /// The export and name counts of each conformal save, oldest first.
    /// Before version 68, a single generation made from the summary's
    /// counts, as the engine does.
    pub generations: Vec<Generation>,
    /// The heritage table, before version 68.
    pub heritage: Option<Heritage>,
}

impl Summary {
    /// Reads the summary at the start of `bytes`.
    ///
    /// Like the engine, it doesn't check that the tables are in the file;
    /// [`Package`](crate::Package) does when it reads them.
    pub fn read(bytes: &[u8]) -> Result<Summary> {
        let mut r = Reader::new(bytes);
        let tag = r.u32("tag")?;
        if tag != PACKAGE_FILE_TAG && tag != TRIPWIRE_PACKAGE_FILE_TAG {
            return Err(Error::NotAPackage { tag });
        }
        let file_version = r.u32("file version")?;
        let package_flags = PackageFlags(r.u32("package flags")?);
        let name_count = count(&mut r, "name count")?;
        let name_offset = count(&mut r, "name offset")?;
        let export_count = count(&mut r, "export count")?;
        let export_offset = count(&mut r, "export offset")?;
        let import_count = count(&mut r, "import count")?;
        let import_offset = count(&mut r, "import offset")?;

        let (guid, generations, heritage) = if file_version as u16 >= GENERATIONS_VERSION {
            let guid = guid(&mut r, "GUID")?;
            let generation_count = count(&mut r, "generation count")?;
            let mut generations = Vec::new();
            for _ in 0..generation_count {
                generations.push(Generation {
                    export_count: count(&mut r, "generation export count")?,
                    name_count: count(&mut r, "generation name count")?,
                });
            }
            (guid, generations, None)
        } else {
            let heritage = Heritage {
                count: count(&mut r, "heritage count")?,
                offset: count(&mut r, "heritage offset")?,
            };
            // The engine reads every GUID of the table into the same
            // variable, so the last one wins.
            let mut guid = Guid::default();
            r.seek(heritage.offset as usize);
            for _ in 0..heritage.count {
                guid = self::guid(&mut r, "heritage table")?;
            }
            let generation = Generation {
                export_count,
                name_count,
            };
            (guid, vec![generation], Some(heritage))
        };

        Ok(Summary {
            tag,
            file_version,
            package_flags,
            name_count,
            name_offset,
            export_count,
            export_offset,
            import_count,
            import_offset,
            guid,
            generations,
            heritage,
        })
    }

    /// The engine version (`PACKAGE_FILE_VERSION`): 128 for UT2004.
    pub fn version(&self) -> u16 {
        self.file_version as u16
    }

    /// The licensee version: 29 for UT2004, and 0 in many older files.
    pub fn licensee_version(&self) -> u16 {
        (self.file_version >> 16) as u16
    }
}

/// Reads an `i32` that can't be negative: a count or an offset.
fn count(r: &mut Reader, what: &'static str) -> Result<u32> {
    let value = r.i32(what)?;
    u32::try_from(value).map_err(|_| Error::InvalidSummary(format!("the {} is {}", what, value)))
}

fn guid(r: &mut Reader, what: &'static str) -> Result<Guid> {
    Ok(Guid {
        a: r.u32(what)?,
        b: r.u32(what)?,
        c: r.u32(what)?,
        d: r.u32(what)?,
    })
}

/// A GUID (`FGuid`): four `u32`s.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Guid {
    pub a: u32,
    pub b: u32,
    pub c: u32,
    pub d: u32,
}

/// Formats the GUID as the engine does (`FGuid::String`): 32 upper-case
/// hex digits.
impl fmt::Display for Guid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:08X}{:08X}{:08X}{:08X}",
            self.a, self.b, self.c, self.d
        )
    }
}

/// Serialized as its string form.
#[cfg(feature = "serde")]
impl Serialize for Guid {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        serializer.collect_str(self)
    }
}

/// The export and name counts of one save of the package.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Generation {
    pub export_count: u32,
    pub name_count: u32,
}

/// The heritage table of a package older than version 68: `count` GUIDs at
/// `offset`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Heritage {
    pub count: u32,
    pub offset: u32,
}
