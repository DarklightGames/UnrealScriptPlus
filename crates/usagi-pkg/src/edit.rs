//! Changing packages in place.

use std::fs::OpenOptions;
use std::io::{Seek, SeekFrom, Write};
use std::path::Path;

use crate::{PackageFlags, Result, Summary, uz2};

/// Where the package flags are in the summary.
const PACKAGE_FLAGS_OFFSET: usize = 8;

/// Whether a path is a `.uz2` file, by its extension (in any case), as the
/// engine decides.
pub(crate) fn is_uz2(path: &Path) -> bool {
    path.extension()
        .is_some_and(|e| e.eq_ignore_ascii_case("uz2"))
}

/// Sets a package file's flags. Only the flags change.
///
/// An uncompressed package has its 4 flag bytes rewritten in place. A
/// `.uz2` file is decompressed, changed and compressed again, and the file
/// is rewritten. The file must start with a package summary; its tables
/// aren't read.
pub fn set_package_flags(path: &Path, flags: PackageFlags) -> Result<()> {
    let bytes = std::fs::read(path)?;
    if is_uz2(path) {
        let mut package = uz2::decompress(&bytes)?;
        Summary::read(&package)?;
        package[PACKAGE_FLAGS_OFFSET..PACKAGE_FLAGS_OFFSET + 4]
            .copy_from_slice(&flags.0.to_le_bytes());
        std::fs::write(path, uz2::compress(&package))?;
    } else {
        Summary::read(&bytes)?;
        OpenOptions::new()
            .write(true)
            .open(path)
            .and_then(|mut file| {
                file.seek(SeekFrom::Start(PACKAGE_FLAGS_OFFSET as u64))
                    .and_then(|_| file.write_all(&flags.0.to_le_bytes()))
            })?;
    }
    Ok(())
}
