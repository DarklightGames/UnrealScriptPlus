//! The name, import and export tables. See `ULinkerLoad::ULinkerLoad`,
//! `operator<<(FArchive&, FObjectImport&)` and
//! `operator<<(FArchive&, FObjectExport&)` in the engine's
//! `Core/Src/UnLinker.cpp`, and `operator<<(FArchive&, FNameEntry&)` in
//! `Core/Src/UnName.cpp`.

use crate::reader::Reader;
use crate::{Error, ObjectFlags, Result, Summary};

/// The first version whose names are `FString`s. Older names are
/// null-terminated.
const STRING_NAMES_VERSION: u16 = 64;

/// The longest name the engine keeps (`NAME_SIZE - 1`); longer ones are
/// cut short.
const MAX_NAME_LEN: usize = 63;

/// A reference to an object: none, an export or an import. On disk it's an
/// `i32`: 0, export `n - 1` for `n > 0`, or import `-n - 1` for `n < 0`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ObjectRef {
    Null,
    /// An index into [`Package::exports`](crate::Package::exports).
    Export(usize),
    /// An index into [`Package::imports`](crate::Package::imports).
    Import(usize),
}

impl ObjectRef {
    /// The reference stored on disk as `raw`.
    pub fn from_raw(raw: i32) -> ObjectRef {
        match raw {
            0 => ObjectRef::Null,
            n if n > 0 => ObjectRef::Export(n as usize - 1),
            n => ObjectRef::Import((n + 1).unsigned_abs() as usize),
        }
    }

    /// The reference as it's stored on disk: exports are numbered 1, 2,
    /// 3, ... and imports -1, -2, -3, ...
    pub fn raw(self) -> i32 {
        match self {
            ObjectRef::Null => 0,
            ObjectRef::Export(i) => i as i32 + 1,
            ObjectRef::Import(i) => -(i as i32) - 1,
        }
    }
}

/// An entry of the name table.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameEntry {
    pub name: String,
    /// Object flags (`RF_*`); the engine only loads a name whose flags have
    /// one of the `LoadFor*` bits of the current context.
    pub flags: u32,
}

/// An object in another package that this one refers to (`FObjectImport`).
/// Names are indices into [`Package::names`](crate::Package::names).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    /// The package that defines the object's class.
    pub class_package: usize,
    pub class_name: usize,
    /// The import's outer: another import, or [`ObjectRef::Null`] for a
    /// top-level package.
    pub outer: ObjectRef,
    pub object_name: usize,
}

/// An object in this package (`FObjectExport`). Names are indices into
/// [`Package::names`](crate::Package::names).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Export {
    /// The object's class; [`ObjectRef::Null`] if the object is itself a
    /// class (its class is `Core.Class`).
    pub class: ObjectRef,
    /// For classes, structs, states and functions, the struct they extend
    /// (`SuperField`); otherwise [`ObjectRef::Null`].
    pub super_struct: ObjectRef,
    /// The export's outer; [`ObjectRef::Null`] for the package itself.
    pub outer: ObjectRef,
    pub object_name: usize,
    pub flags: ObjectFlags,
    /// The size of the object's data.
    pub serial_size: u32,
    /// Where the object's data is; `None` if it has none.
    pub serial_offset: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tables {
    pub names: Vec<NameEntry>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}

/// Reads and checks the tables of the package `bytes`, whose summary is
/// `summary`.
pub fn read(bytes: &[u8], summary: &Summary) -> Result<Tables> {
    let mut r = Reader::new(bytes);

    r.seek(summary.name_offset as usize);
    let mut names = Vec::new();
    for _ in 0..summary.name_count {
        let name = if summary.version() >= STRING_NAMES_VERSION {
            r.string("name table")?
        } else {
            r.c_string("name table")?
        };
        names.push(NameEntry {
            name: name.chars().take(MAX_NAME_LEN).collect(),
            flags: r.u32("name table")?,
        });
    }
    let name_count = names.len();
    let name = |r: &mut Reader, object: &dyn Fn() -> String, field: &str| {
        let index = r.index("import or export table")?;
        usize::try_from(index)
            .ok()
            .filter(|&i| i < name_count)
            .ok_or_else(|| {
                invalid(format!(
                    "{}: the {} is name {}, but there are {} names",
                    object(),
                    field,
                    index,
                    name_count
                ))
            })
    };

    r.seek(summary.import_offset as usize);
    let mut imports = Vec::new();
    for i in 0..summary.import_count as usize {
        let object = || describe(ObjectRef::Import(i));
        imports.push(Import {
            class_package: name(&mut r, &object, "class package")?,
            class_name: name(&mut r, &object, "class name")?,
            outer: ObjectRef::from_raw(r.i32("import table")?),
            object_name: name(&mut r, &object, "object name")?,
        });
    }

    r.seek(summary.export_offset as usize);
    let mut exports = Vec::new();
    for i in 0..summary.export_count as usize {
        let object = || describe(ObjectRef::Export(i));
        let class = ObjectRef::from_raw(r.index("export table")?);
        let super_struct = ObjectRef::from_raw(r.index("export table")?);
        let outer = ObjectRef::from_raw(r.i32("export table")?);
        let object_name = name(&mut r, &object, "object name")?;
        let flags = ObjectFlags(r.u32("export table")?);
        let serial_size = r.index("export table")?;
        let serial_offset = if serial_size != 0 {
            Some(r.index("export table")?)
        } else {
            None
        };
        let serial_size = u32::try_from(serial_size)
            .map_err(|_| invalid(format!("{}: its size is {}", object(), serial_size)))?;
        let serial_offset = serial_offset
            .map(|offset| {
                u32::try_from(offset)
                    .ok()
                    .filter(|&o| u64::from(o) + u64::from(serial_size) <= bytes.len() as u64)
                    .ok_or_else(|| {
                        invalid(format!(
                            "{}: its data ({} bytes at offset {}) isn't in the file ({} bytes)",
                            object(),
                            serial_size,
                            offset,
                            bytes.len()
                        ))
                    })
            })
            .transpose()?;
        exports.push(Export {
            class,
            super_struct,
            outer,
            object_name,
            flags,
            serial_size,
            serial_offset,
        });
    }

    let tables = Tables {
        names,
        imports,
        exports,
    };
    tables.check_references()?;
    Ok(tables)
}

impl Tables {
    /// Checks that every object reference is in range, that imports are
    /// only inside imports (the engine asserts it), and that no object is
    /// inside itself.
    fn check_references(&self) -> Result<()> {
        for (i, import) in self.imports.iter().enumerate() {
            let object = ObjectRef::Import(i);
            if let ObjectRef::Export(_) = import.outer {
                return Err(invalid(format!(
                    "{}: its outer is export {}; an import must be in an import",
                    describe(object),
                    import.outer.raw()
                )));
            }
            self.check_reference(object, "outer", import.outer)?;
        }
        for (i, export) in self.exports.iter().enumerate() {
            let object = ObjectRef::Export(i);
            self.check_reference(object, "class", export.class)?;
            self.check_reference(object, "super", export.super_struct)?;
            self.check_reference(object, "outer", export.outer)?;
        }
        // Now that every outer is in range, look for loops.
        let objects = self.imports.len() + self.exports.len();
        let starts = (0..self.imports.len())
            .map(ObjectRef::Import)
            .chain((0..self.exports.len()).map(ObjectRef::Export));
        for start in starts {
            let mut current = start;
            for _ in 0..=objects {
                current = self.outer(current);
                if current == ObjectRef::Null {
                    break;
                }
            }
            if current != ObjectRef::Null {
                return Err(invalid(format!(
                    "{}: its outers form a loop",
                    describe(start)
                )));
            }
        }
        Ok(())
    }

    fn check_reference(&self, object: ObjectRef, field: &str, target: ObjectRef) -> Result<()> {
        let in_range = match target {
            ObjectRef::Null => true,
            ObjectRef::Export(i) => i < self.exports.len(),
            ObjectRef::Import(i) => i < self.imports.len(),
        };
        if in_range {
            return Ok(());
        }
        Err(invalid(format!(
            "{}: its {} is object {}, but there are {} imports and {} exports",
            describe(object),
            field,
            target.raw(),
            self.imports.len(),
            self.exports.len()
        )))
    }

    /// The outer of an object; `Null` for `Null`.
    pub fn outer(&self, object: ObjectRef) -> ObjectRef {
        match object {
            ObjectRef::Null => ObjectRef::Null,
            ObjectRef::Export(i) => self.exports[i].outer,
            ObjectRef::Import(i) => self.imports[i].outer,
        }
    }
}

/// "import -3" or "export 5".
fn describe(object: ObjectRef) -> String {
    match object {
        ObjectRef::Import(_) => format!("import {}", object.raw()),
        _ => format!("export {}", object.raw()),
    }
}

fn invalid(message: String) -> Error {
    Error::InvalidTable(message)
}
