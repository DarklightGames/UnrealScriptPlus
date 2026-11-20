use std::path::Path;

use crate::tables::{self, Tables};
use crate::{Export, Import, NameEntry, ObjectRef, Result, Summary, uz2};

/// A package file: its summary and its name, import and export tables.
///
/// The tables are checked when the package is read: every name and object
/// reference is in range, and no object is inside itself. So the methods
/// that look things up can't fail.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Package {
    pub summary: Summary,
    /// The size of the package in bytes (once decompressed, for a `.uz2`).
    pub size: usize,
    /// The size of the `.uz2` file, if the package was compressed.
    pub compressed_size: Option<usize>,
    tables: Tables,
}

impl Package {
    /// Reads a package file. A file whose extension is `.uz2` (in any case)
    /// is decompressed first: like the engine, it's the extension that says
    /// whether a file is compressed.
    pub fn read(path: &Path) -> Result<Package> {
        let bytes = std::fs::read(path)?;
        if crate::edit::is_uz2(path) {
            Package::from_uz2_bytes(&bytes)
        } else {
            Package::from_bytes(&bytes)
        }
    }

    /// Reads a package from the bytes of the whole file.
    pub fn from_bytes(bytes: &[u8]) -> Result<Package> {
        let summary = Summary::read(bytes)?;
        let tables = tables::read(bytes, &summary)?;
        Ok(Package {
            summary,
            size: bytes.len(),
            compressed_size: None,
            tables,
        })
    }

    /// Reads a package from the bytes of a whole `.uz2` file.
    pub fn from_uz2_bytes(bytes: &[u8]) -> Result<Package> {
        let package = Package::from_bytes(&uz2::decompress(bytes)?)?;
        Ok(Package {
            compressed_size: Some(bytes.len()),
            ..package
        })
    }

    /// The name table.
    pub fn names(&self) -> &[NameEntry] {
        &self.tables.names
    }

    /// The import table. Import `i` is [`ObjectRef::Import(i)`], stored on
    /// disk as `-i - 1`.
    pub fn imports(&self) -> &[Import] {
        &self.tables.imports
    }

    /// The export table. Export `i` is [`ObjectRef::Export(i)`], stored on
    /// disk as `i + 1`.
    pub fn exports(&self) -> &[Export] {
        &self.tables.exports
    }

    /// The name with this index. Every name index in an [`Import`] or
    /// [`Export`] of this package is valid.
    ///
    /// # Panics
    ///
    /// If `index` is out of range.
    pub fn name(&self, index: usize) -> &str {
        &self.tables.names[index].name
    }

    /// An object's name; `None` for [`ObjectRef::Null`].
    pub fn object_name(&self, object: ObjectRef) -> Option<&str> {
        let index = match object {
            ObjectRef::Null => return None,
            ObjectRef::Export(i) => self.tables.exports[i].object_name,
            ObjectRef::Import(i) => self.tables.imports[i].object_name,
        };
        Some(self.name(index))
    }

    /// An object's path: its outers' names and its own, outermost first,
    /// joined with `.`; `None` for [`ObjectRef::Null`].
    ///
    /// An import's path starts with the package it's in. An export's path
    /// doesn't include this package's own name, which isn't stored in the
    /// file (the engine takes it from the file name).
    pub fn path(&self, object: ObjectRef) -> Option<String> {
        let mut parts = Vec::new();
        let mut current = object;
        while let Some(name) = self.object_name(current) {
            parts.push(name);
            current = self.outer(current);
        }
        if parts.is_empty() {
            return None;
        }
        parts.reverse();
        Some(parts.join("."))
    }

    /// An object's outer: the object it's inside. [`ObjectRef::Null`] for a
    /// top-level import (a package), an export directly in this package, or
    /// [`ObjectRef::Null`] itself.
    pub fn outer(&self, object: ObjectRef) -> ObjectRef {
        self.tables.outer(object)
    }

    /// The outermost object an object is in: for an import, the package
    /// it comes from (a top-level `Core.Package` import, which is a file);
    /// for an export directly in this package, the export itself. The object
    /// itself if it has no outer; [`ObjectRef::Null`] for
    /// [`ObjectRef::Null`].
    pub fn top_level(&self, object: ObjectRef) -> ObjectRef {
        let mut current = object;
        loop {
            match self.outer(current) {
                ObjectRef::Null => return current,
                outer => current = outer,
            }
        }
    }

    /// The name of an export's class: `Class` if the export is itself a
    /// class (`ClassIndex` is 0).
    pub fn class_name(&self, export: &Export) -> &str {
        self.object_name(export.class).unwrap_or("Class")
    }
}
