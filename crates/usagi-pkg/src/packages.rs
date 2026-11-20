//! A set of packages that refer to each other: finding a package by name,
//! resolving imports to the exports they name (`ULinkerLoad::VerifyImport`
//! in `Core/Src/UnLinker.cpp`), and finding the declarations of a class's
//! or struct's properties, through its superclasses, which may be in other
//! packages.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::class::PropertyKind;
use crate::{ObjectRef, Package, Result};

/// `RF_Public`: other packages can import the object.
const RF_PUBLIC: u32 = 0x0000_0004;

/// `CPF_Native`: the property isn't stored; C++ serializes it.
pub(crate) const CPF_NATIVE: u32 = 0x0000_1000;

/// `CPF_Transient`: the property isn't stored in packages.
pub(crate) const CPF_TRANSIENT: u32 = 0x0000_2000;

/// How many superclasses or outers are followed before giving up, in case
/// a package refers to itself in a loop.
const MAX_CHAIN: usize = 64;

/// Where to look for packages: files named `<Package><extension>` in a
/// folder. A game's `Paths` setting (`../Textures/*.utx`) is a list of them
/// (`appFindPackageFile`, `Core/Src/UnMisc.cpp`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SearchPath {
    pub dir: PathBuf,
    /// Including the dot: `.utx`.
    pub extension: String,
}

impl SearchPath {
    /// A `Paths` entry, relative to `base` (the game's `System` folder): the
    /// folder is what comes before the `*`, the extension what comes after.
    pub fn from_pattern(base: &Path, pattern: &str) -> SearchPath {
        let pattern = pattern.replace('\\', "/");
        let (dir, extension) = pattern.split_once('*').unwrap_or((&pattern, ""));
        SearchPath {
            dir: base.join(dir),
            extension: extension.to_string(),
        }
    }
}

/// A struct, class, state or function: an export of a package.
#[derive(Clone)]
pub struct Scope {
    pub package: Rc<Package>,
    pub export: usize,
}

impl std::fmt::Debug for Scope {
    /// The object's reference, not the whole package.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.package.reference(ObjectRef::Export(self.export))
        )
    }
}

impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.package, &other.package) && self.export == other.export
    }
}

/// A property's type, with what it refers to resolved.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// A byte, and its enum if it's one.
    Byte(Option<Scope>),
    Int,
    Bool,
    Float,
    /// An object or a class.
    Object,
    Name,
    Str,
    Delegate,
    Pointer,
    Map,
    Array(Box<Type>),
    FixedArray(Box<Type>, i32),
    /// A struct, by name, and its declaration if it was found.
    Struct(String, Option<Scope>),
    /// A type whose declaration couldn't be read.
    Unknown,
}

impl Type {
    /// The type's ID in a property tag (`UProperty::GetID`, the index of its
    /// class's name): a class property's is an object property's.
    pub fn id(&self) -> u8 {
        match self {
            Type::Byte(_) => 1,
            Type::Int => 2,
            Type::Bool => 3,
            Type::Float => 4,
            Type::Object => 5,
            Type::Name => 6,
            Type::Delegate => 7,
            Type::Array(_) => 9,
            Type::Struct(..) => 10,
            Type::Str => 13,
            Type::Map => 14,
            Type::FixedArray(..) => 15,
            Type::Pointer => 16,
            Type::Unknown => 0,
        }
    }
}

/// A property's declaration, with its type resolved.
#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    pub name: String,
    pub array_dim: i32,
    /// `CPF_` flags.
    pub flags: u32,
    pub ty: Type,
}

impl Decl {
    /// Whether the engine reads the property from a package
    /// (`UProperty::ShouldSerializeValue`): it isn't native or transient.
    pub fn is_stored(&self) -> bool {
        self.flags & (CPF_NATIVE | CPF_TRANSIENT) == 0
    }
}

/// What looking for a property's declaration found.
#[derive(Debug, Clone, PartialEq)]
pub enum Lookup {
    Found(Rc<Decl>),
    /// The struct and its superclasses were all read, and none declares it.
    Missing,
    /// It isn't in what could be read, but a superclass couldn't be read.
    Unknown,
}

/// A struct's own properties, and its superclass.
#[derive(Debug)]
struct StructInfo {
    properties: Vec<Rc<Decl>>,
    /// `Some(None)` if it has no superclass; `None` if it has one that
    /// couldn't be found.
    super_scope: Option<Option<Scope>>,
}

/// Packages that refer to each other, loaded as they're needed from
/// directories, and declarations read from them, cached.
#[derive(Debug, Default)]
pub struct Packages {
    paths: Vec<SearchPath>,
    /// The files in each folder of `paths`, by lowercase name.
    dirs: RefCell<HashMap<PathBuf, HashMap<String, PathBuf>>>,
    /// Packages by lowercase name; `None` if it couldn't be read.
    loaded: RefCell<HashMap<String, Option<Rc<Package>>>>,
    structs: RefCell<HashMap<(String, usize), Rc<StructInfo>>>,
    resolved: RefCell<HashMap<(String, usize), Option<Scope>>>,
}

impl Packages {
    /// A set that looks for packages in `paths`, in order: the first file
    /// that's there, by name in any case, is the package.
    pub fn new(paths: Vec<SearchPath>) -> Packages {
        Packages {
            paths,
            ..Packages::default()
        }
    }

    /// Adds a package that has been read, in place of any of the same name.
    pub fn add(&self, package: Package) -> Rc<Package> {
        let package = Rc::new(package);
        self.loaded
            .borrow_mut()
            .insert(package.name.to_lowercase(), Some(Rc::clone(&package)));
        package
    }

    /// The package named `name` (in any case): one added, or the file in
    /// the directories. `None` if there's none, or it can't be read.
    pub fn get(&self, name: &str) -> Option<Rc<Package>> {
        let key = name.to_lowercase();
        if let Some(package) = self.loaded.borrow().get(&key) {
            return package.clone();
        }
        let path = self.paths.iter().find_map(|search| {
            let file = format!("{}{}", key, search.extension.to_lowercase());
            self.find_file(&search.dir, &file)
        });
        let package = path.and_then(|path| Package::read(&path).ok()).map(Rc::new);
        self.loaded.borrow_mut().insert(key, package.clone());
        package
    }

    /// The file in `dir` named `name` (lowercase), in any case. Each folder
    /// is listed once.
    fn find_file(&self, dir: &Path, name: &str) -> Option<PathBuf> {
        let mut dirs = self.dirs.borrow_mut();
        let files = dirs.entry(dir.to_path_buf()).or_insert_with(|| {
            std::fs::read_dir(dir)
                .into_iter()
                .flatten()
                .flatten()
                .map(|entry| {
                    let name = entry.file_name().to_string_lossy().to_lowercase();
                    (name, entry.path())
                })
                .collect()
        });
        files.get(name).cloned()
    }

    /// The export an object reference in `package` is: itself if it's an
    /// export, or the export an import resolves to. `None` for `None`, for
    /// a package, or for an import that doesn't resolve.
    pub fn resolve(&self, package: &Rc<Package>, object: ObjectRef) -> Option<Scope> {
        match object {
            ObjectRef::Null => None,
            ObjectRef::Export(export) => Some(Scope {
                package: Rc::clone(package),
                export,
            }),
            ObjectRef::Import(import) => {
                let key = (package.name.to_lowercase(), import);
                if let Some(scope) = self.resolved.borrow().get(&key) {
                    return scope.clone();
                }
                let scope = self.resolve_import(package, import);
                self.resolved.borrow_mut().insert(key, scope.clone());
                scope
            }
        }
    }

    /// `ULinkerLoad::VerifyImport`, for an import that isn't a package. An
    /// import's outers can't loop: [`Package::read`] checks.
    fn resolve_import(&self, package: &Rc<Package>, import: usize) -> Option<Scope> {
        let imp = &package.imports()[import];
        let ObjectRef::Import(outer) = imp.outer else {
            // A package: not an export.
            return None;
        };
        // The package it's in, and the export its outer resolved to.
        let (target, outer_export) = if package.imports()[outer].outer == ObjectRef::Null {
            let name = package.name(package.imports()[outer].object_name);
            (self.get(name)?, None)
        } else {
            let outer = self.resolve_import(package, outer)?;
            (outer.package, Some(outer.export))
        };
        let name = package.name(imp.object_name);
        let class_name = package.name(imp.class_name);
        let class_package = package.name(imp.class_package);
        let export = (0..target.exports().len()).find(|&i| {
            let export = &target.exports()[i];
            let outer_matches = match (export.outer, outer_export) {
                (ObjectRef::Null, _) => true,
                (ObjectRef::Export(o), Some(wanted)) => o == wanted,
                _ => false,
            };
            outer_matches
                && export.flags.0 & RF_PUBLIC != 0
                && target.name(export.object_name).eq_ignore_ascii_case(name)
                && target.class_name(export).eq_ignore_ascii_case(class_name)
                && class_package_name(&target, i).eq_ignore_ascii_case(class_package)
        })?;
        Some(Scope {
            package: target,
            export,
        })
    }

    /// The scope whose properties an export's tagged properties are: a
    /// class's own (its defaults), or otherwise its class's. `None` if the
    /// class can't be found.
    pub fn scope_of(&self, package: &Rc<Package>, export: usize) -> Option<Scope> {
        match package.exports()[export].class {
            ObjectRef::Null => Some(Scope {
                package: Rc::clone(package),
                export,
            }),
            class => self.resolve(package, class),
        }
    }

    /// The declaration of the property named `name` (in any case) in
    /// `scope` or its superclasses, nearest first.
    pub fn find_property(&self, scope: &Scope, name: &str) -> Lookup {
        let mut scope = scope.clone();
        for _ in 0..MAX_CHAIN {
            let Some(info) = self.struct_info(&scope) else {
                return Lookup::Unknown;
            };
            if let Some(decl) = info
                .properties
                .iter()
                .find(|d| d.name.eq_ignore_ascii_case(name))
            {
                return Lookup::Found(Rc::clone(decl));
            }
            match &info.super_scope {
                Some(Some(parent)) => scope = parent.clone(),
                Some(None) => return Lookup::Missing,
                None => return Lookup::Unknown,
            }
        }
        Lookup::Unknown
    }

    /// The properties of `scope` and its superclasses, in the order
    /// `UStruct::SerializeBin` writes them: the struct's own, then its
    /// superclass's. `None` if one of them can't be read.
    pub fn struct_properties(&self, scope: &Scope) -> Option<Vec<Rc<Decl>>> {
        let mut properties = Vec::new();
        let mut scope = scope.clone();
        for _ in 0..MAX_CHAIN {
            let info = self.struct_info(&scope)?;
            properties.extend(info.properties.iter().cloned());
            match &info.super_scope {
                Some(Some(parent)) => scope = parent.clone(),
                Some(None) => return Some(properties),
                None => return None,
            }
        }
        None
    }

    fn struct_info(&self, scope: &Scope) -> Option<Rc<StructInfo>> {
        let key = (scope.package.name.to_lowercase(), scope.export);
        if let Some(info) = self.structs.borrow().get(&key) {
            return Some(Rc::clone(info));
        }
        let info = Rc::new(self.read_struct_info(scope).ok()??);
        self.structs.borrow_mut().insert(key, Rc::clone(&info));
        Some(info)
    }

    fn read_struct_info(&self, scope: &Scope) -> Result<Option<StructInfo>> {
        let package = &scope.package;
        let Some(structure) = package.structure(scope.export)? else {
            return Ok(None);
        };
        let super_scope = match structure.field.super_field {
            ObjectRef::Null => Some(None),
            parent => self.resolve(package, parent).map(Some),
        };
        let mut properties = Vec::new();
        let mut child = structure.children;
        for _ in 0..package.exports().len() {
            let ObjectRef::Export(export) = child else {
                break;
            };
            let class = package.class_name(&package.exports()[export]);
            if PropertyKind::of_class(class).is_some() {
                let decl = package.property_decl(export)?;
                properties.push(Rc::new(Decl {
                    name: package
                        .name(package.exports()[export].object_name)
                        .to_string(),
                    array_dim: decl.array_dim,
                    flags: decl.flags,
                    ty: self.type_of(package, &decl.kind, 0),
                }));
                child = decl.field.next;
            } else {
                child = package.next_field(export)?;
            }
        }
        Ok(Some(StructInfo {
            properties,
            super_scope,
        }))
    }

    /// A declaration's type, with its inner properties and struct resolved.
    fn type_of(&self, package: &Rc<Package>, kind: &PropertyKind, depth: usize) -> Type {
        let inner = |inner: ObjectRef| -> Type {
            if depth > MAX_CHAIN {
                return Type::Unknown;
            }
            let Some(scope) = self.resolve(package, inner) else {
                return Type::Unknown;
            };
            match scope.package.property_decl(scope.export) {
                Ok(decl) => self.type_of(&scope.package, &decl.kind, depth + 1),
                Err(_) => Type::Unknown,
            }
        };
        match kind {
            PropertyKind::Byte { enumeration } => Type::Byte(self.resolve(package, *enumeration)),
            PropertyKind::Int => Type::Int,
            PropertyKind::Bool => Type::Bool,
            PropertyKind::Float => Type::Float,
            PropertyKind::Object { .. } | PropertyKind::Class { .. } => Type::Object,
            PropertyKind::Name => Type::Name,
            PropertyKind::Str => Type::Str,
            PropertyKind::Delegate { .. } => Type::Delegate,
            PropertyKind::Pointer => Type::Pointer,
            PropertyKind::Map { .. } => Type::Map,
            PropertyKind::Array { inner: i } => Type::Array(Box::new(inner(*i))),
            PropertyKind::FixedArray { inner: i, count } => {
                Type::FixedArray(Box::new(inner(*i)), *count)
            }
            PropertyKind::Struct { structure } => Type::Struct(
                package
                    .object_name(*structure)
                    .unwrap_or_default()
                    .to_string(),
                self.resolve(package, *structure),
            ),
        }
    }
}

/// The name of the package that defines an export's class
/// (`GetExportClassPackage`): `Core` for a class, whose class is
/// `Core.Class`.
fn class_package_name(package: &Package, export: usize) -> &str {
    match package.exports()[export].class {
        ObjectRef::Null => "Core",
        ObjectRef::Export(_) => &package.name,
        class @ ObjectRef::Import(_) => {
            let top = package.top_level(class);
            package.object_name(top).expect("an import")
        }
    }
}
