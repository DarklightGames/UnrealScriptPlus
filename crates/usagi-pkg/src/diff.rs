//! Comparing two versions of a package: what was added, removed or
//! changed, by what each object is rather than where it is in the file.
//!
//! Objects are matched by their class and path, not their index, so
//! reordered tables aren't a change. Property values are compared as T3D
//! text, in which objects are named by their paths, so a reference to the
//! same object compares equal even when its index changed. Some things
//! change every time a package is saved, even when nothing else does (the
//! GUID, the generations, where the tables and each object's data are, and
//! each export's index): they're compared too, and marked as volatile.

use std::collections::HashMap;
use std::rc::Rc;

use crate::archive::NameRef;
use crate::volatile::VOLATILE_PROPERTIES;
use crate::{
    Error, Generation, Guid, Heritage, ObjectFlags, ObjectRef, Package, PackageFlags, Packages,
    Sound, SoundGroup, Texture,
};

/// A value that differs between the old package and the new.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Change<T> {
    pub old: T,
    pub new: T,
}

impl<T: PartialEq> Change<T> {
    /// The change from `old` to `new`; `None` if they're equal.
    fn of(old: T, new: T) -> Option<Change<T>> {
        (old != new).then_some(Change { old, new })
    }
}

/// One of the packages compared.
#[derive(Debug, Clone, Copy)]
pub struct Side<'a> {
    /// The packages its objects' properties are declared in. `package` is
    /// read by them; it should be one of them, so that the classes it
    /// declares itself are found.
    pub packages: &'a Packages,
    pub package: &'a Rc<Package>,
    /// The exports to compare, by index; `None` for every one.
    pub exports: Option<&'a [usize]>,
}

/// The differences between two packages. Made by [`diff`].
#[derive(Debug, Default)]
pub struct PackageDiff {
    pub summary: SummaryDiff,
    /// The packages each depends on (its top-level imports), by name.
    pub dependencies: SetDiff,
    /// The objects each refers to in other packages, as
    /// `Class'Package.Group.Name'`.
    pub imports: SetDiff,
    /// The exports added, removed or changed, sorted by path (ignoring
    /// case), then class. Only those with a difference are listed, but it
    /// may be a volatile one (see [`ExportDiff::has_changes`]).
    pub exports: Vec<ExportDiff>,
}

impl PackageDiff {
    /// Whether anything differs; with `volatile`, counting what changes
    /// each time a package is saved.
    pub fn has_changes(&self, volatile: bool) -> bool {
        self.summary.has_changes(volatile)
            || !self.dependencies.is_empty()
            || !self.imports.is_empty()
            || self.exports.iter().any(|e| e.has_changes(volatile))
    }
}

/// The differences between two packages' summaries. A field is `None` if
/// it's the same in both.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SummaryDiff {
    pub tag: Option<Change<u32>>,
    pub version: Option<Change<u16>>,
    pub licensee_version: Option<Change<u16>>,
    pub package_flags: Option<Change<PackageFlags>>,
    /// The size of the package (once decompressed, for a `.uz2`).
    pub size: Option<Change<usize>>,
    /// The size of the `.uz2` file, or `None` if it isn't one.
    pub compressed_size: Option<Change<Option<usize>>>,
    /// Volatile.
    pub guid: Option<Change<Guid>>,
    /// Volatile.
    pub generations: Option<Change<Vec<Generation>>>,
    /// Volatile.
    pub heritage: Option<Change<Option<Heritage>>>,
    /// The name table's count and offset. Volatile.
    pub names: Option<Change<(u32, u32)>>,
    /// The import table's count and offset. Volatile.
    pub imports: Option<Change<(u32, u32)>>,
    /// The export table's count and offset. Volatile.
    pub exports: Option<Change<(u32, u32)>>,
}

impl SummaryDiff {
    /// Whether anything differs; with `volatile`, counting what changes
    /// each time a package is saved.
    pub fn has_changes(&self, volatile: bool) -> bool {
        let stable = self.tag.is_some()
            || self.version.is_some()
            || self.licensee_version.is_some()
            || self.package_flags.is_some()
            || self.size.is_some()
            || self.compressed_size.is_some();
        stable
            || volatile
                && (self.guid.is_some()
                    || self.generations.is_some()
                    || self.heritage.is_some()
                    || self.names.is_some()
                    || self.imports.is_some()
                    || self.exports.is_some())
    }
}

/// Items in only one of the packages, each in the order of its table.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SetDiff {
    pub added: Vec<String>,
    pub removed: Vec<String>,
}

impl SetDiff {
    pub fn is_empty(&self) -> bool {
        self.added.is_empty() && self.removed.is_empty()
    }

    /// The items of `new` not in `old`, and those of `old` not in `new`,
    /// ignoring case.
    fn of(old: Vec<String>, new: Vec<String>) -> SetDiff {
        let only = |a: &[String], b: &[String]| -> Vec<String> {
            let others: Vec<String> = b.iter().map(|s| s.to_lowercase()).collect();
            let mut seen = Vec::new();
            a.iter()
                .filter(|s| {
                    let key = s.to_lowercase();
                    let keep = !others.contains(&key) && !seen.contains(&key);
                    seen.push(key);
                    keep
                })
                .cloned()
                .collect()
        };
        SetDiff {
            added: only(&new, &old),
            removed: only(&old, &new),
        }
    }
}

/// An export added, removed or changed.
#[derive(Debug)]
pub struct ExportDiff {
    /// `Class'Package.Group.Name'`: in the new package, or in the old one if
    /// it was removed.
    pub reference: String,
    /// Its index in the old package's export table; `None` if it was added.
    pub old: Option<usize>,
    /// Its index in the new package's export table; `None` if it was
    /// removed.
    pub new: Option<usize>,
    /// The size of its data. Not compared for script objects (see
    /// [`is_script_object`]).
    pub size: Option<Change<u32>>,
    pub flags: Option<Change<ObjectFlags>>,
    /// The struct it extends (for a class, its superclass), as
    /// `Class'Package.Name'`, or `None`.
    pub super_struct: Option<Change<String>>,
    /// Where its data is in the file. Volatile.
    pub offset: Option<Change<Option<u32>>>,
    /// The size of its data, when nothing else about it changed: only how
    /// its object references and names are stored (as compact indices,
    /// whose size depends on their index). Not for classes. Volatile.
    pub encoded_size: Option<Change<u32>>,
    /// Its properties added, removed or changed: in the order of the new
    /// object's, then those removed, in the old one's order. Not compared
    /// for script objects. Those whose saved value the engine never uses are
    /// marked volatile.
    pub properties: Vec<PropertyDiff>,
    /// The sizes of its native data (the bytes after its properties), if
    /// they differ in content. Textures, sounds and sound groups are
    /// compared by what they hold (a texture's mipmaps; a sound's file type,
    /// likelihood and data; a sound group's sounds, by their references),
    /// not by where their lazy arrays say they end in the file or the
    /// indices of the names and objects in them, which change whenever the
    /// rest of the package does. Not compared for script
    /// objects, or if the properties of either couldn't all be read.
    pub native: Option<Change<usize>>,
    /// Why the old object's properties couldn't all be read.
    pub old_error: Option<Error>,
    /// Why the new object's properties couldn't all be read.
    pub new_error: Option<Error>,
}

impl ExportDiff {
    fn new(reference: String, old: Option<usize>, new: Option<usize>) -> ExportDiff {
        ExportDiff {
            reference,
            old,
            new,
            size: None,
            flags: None,
            super_struct: None,
            offset: None,
            encoded_size: None,
            properties: Vec::new(),
            native: None,
            old_error: None,
            new_error: None,
        }
    }

    pub fn is_added(&self) -> bool {
        self.old.is_none()
    }

    pub fn is_removed(&self) -> bool {
        self.new.is_none()
    }

    /// Whether it was added or removed, or anything about it differs; with
    /// `volatile`, counting its index and where its data is, which change
    /// whenever an object before it does.
    pub fn has_changes(&self, volatile: bool) -> bool {
        self.is_added()
            || self.is_removed()
            || self.size.is_some()
            || self.flags.is_some()
            || self.super_struct.is_some()
            || self.properties.iter().any(|p| !p.is_volatile())
            || self.native.is_some()
            || self.old_error.is_some()
            || self.new_error.is_some()
            || volatile
                && (self.old != self.new
                    || self.offset.is_some()
                    || self.encoded_size.is_some()
                    || !self.properties.is_empty())
    }
}

/// A property added, removed or changed, by its T3D name (`Foo`, or `Foo(2)`
/// for an element of a static array) and its values as T3D text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropertyDiff {
    pub name: String,
    /// `None` if it was added.
    pub old: Option<String>,
    /// `None` if it was removed.
    pub new: Option<String>,
    /// Why its change is volatile, if the engine never uses its saved value
    /// (see [`VOLATILE_PROPERTIES`]).
    pub volatile: Option<&'static str>,
}

impl PropertyDiff {
    pub fn is_volatile(&self) -> bool {
        self.volatile.is_some()
    }
}

/// Compares two packages.
pub fn diff(old: &Side, new: &Side) -> PackageDiff {
    PackageDiff {
        summary: summary(old.package, new.package),
        dependencies: SetDiff::of(dependencies(old.package), dependencies(new.package)),
        imports: SetDiff::of(imports(old.package), imports(new.package)),
        exports: exports(old, new),
    }
}

fn summary(old: &Package, new: &Package) -> SummaryDiff {
    let (o, n) = (&old.summary, &new.summary);
    SummaryDiff {
        tag: Change::of(o.tag, n.tag),
        version: Change::of(o.version(), n.version()),
        licensee_version: Change::of(o.licensee_version(), n.licensee_version()),
        package_flags: Change::of(o.package_flags, n.package_flags),
        size: Change::of(old.size, new.size),
        compressed_size: Change::of(old.compressed_size, new.compressed_size),
        guid: Change::of(o.guid, n.guid),
        generations: Change::of(o.generations.clone(), n.generations.clone()),
        heritage: Change::of(o.heritage, n.heritage),
        names: Change::of((o.name_count, o.name_offset), (n.name_count, n.name_offset)),
        imports: Change::of(
            (o.import_count, o.import_offset),
            (n.import_count, n.import_offset),
        ),
        exports: Change::of(
            (o.export_count, o.export_offset),
            (n.export_count, n.export_offset),
        ),
    }
}

/// The names of the packages a package depends on: its top-level imports.
fn dependencies(package: &Package) -> Vec<String> {
    package
        .imports()
        .iter()
        .filter(|import| import.outer == ObjectRef::Null)
        .map(|import| package.name(import.object_name).to_string())
        .collect()
}

fn imports(package: &Package) -> Vec<String> {
    (0..package.imports().len())
        .map(|i| package.reference(ObjectRef::Import(i)))
        .collect()
}

/// What identifies an export in either package: its class's path and its
/// path without the package's name (so packages of different names can be
/// compared), lowercase.
fn key(package: &Package, export: usize) -> (String, String) {
    let path = package
        .path(ObjectRef::Export(export))
        .expect("an export")
        .to_lowercase();
    (class_path(package, export).to_lowercase(), path)
}

/// The path of an export's class: `Core.Class` for a class.
fn class_path(package: &Package, export: usize) -> String {
    match package.exports()[export].class {
        ObjectRef::Null => "Core.Class".to_string(),
        class => package.full_path(class).expect("a class"),
    }
}

/// Whether an export is part of a class's script, rather than an object
/// with properties: a function, state, struct, property declaration, enum,
/// constant or text buffer (an object of one of those classes in `Core`).
/// Classes themselves aren't: their default properties are compared.
pub fn is_script_object(package: &Package, export: usize) -> bool {
    let class = class_path(package, export);
    let Some(name) = class
        .split_once('.')
        .filter(|(package, _)| package.eq_ignore_ascii_case("Core"))
        .map(|(_, name)| name.to_lowercase())
    else {
        return false;
    };
    name.ends_with("property")
        || ["function", "state", "struct", "enum", "const", "textbuffer"].contains(&name.as_str())
}

fn exports(old: &Side, new: &Side) -> Vec<ExportDiff> {
    let selected = |side: &Side| -> Vec<usize> {
        match side.exports {
            Some(exports) => exports.to_vec(),
            None => (0..side.package.exports().len()).collect(),
        }
    };
    // The old exports by key, in table order; matched ones are taken out.
    let mut unmatched: HashMap<(String, String), Vec<usize>> = HashMap::new();
    for i in selected(old) {
        unmatched.entry(key(old.package, i)).or_default().push(i);
    }
    let mut diffs = Vec::new();
    for j in selected(new) {
        let key = key(new.package, j);
        let reference = new.package.reference(ObjectRef::Export(j));
        let i = unmatched
            .get_mut(&key)
            .filter(|old| !old.is_empty())
            .map(|old| old.remove(0));
        let diff = match i {
            None => ExportDiff::new(reference, None, Some(j)),
            Some(i) => changed(old, i, new, j, reference),
        };
        diffs.push((key, diff));
    }
    for (key, exports) in unmatched {
        for i in exports {
            let reference = old.package.reference(ObjectRef::Export(i));
            diffs.push((key.clone(), ExportDiff::new(reference, Some(i), None)));
        }
    }
    diffs.retain(|(_, diff)| diff.has_changes(true));
    // Removed after the rest, for the same key.
    diffs.sort_by(|(a, x), (b, y)| {
        (&a.1, &a.0, x.is_removed(), x.old).cmp(&(&b.1, &b.0, y.is_removed(), y.old))
    });
    diffs.into_iter().map(|(_, diff)| diff).collect()
}

/// An export in both packages, compared.
fn changed(old: &Side, i: usize, new: &Side, j: usize, reference: String) -> ExportDiff {
    let (o, n) = (&old.package.exports()[i], &new.package.exports()[j]);
    let super_path = |package: &Package, object: ObjectRef| package.reference(object);
    let mut diff = ExportDiff {
        flags: Change::of(o.flags, n.flags),
        super_struct: Change::of(
            super_path(old.package, o.super_struct),
            super_path(new.package, n.super_struct),
        ),
        offset: Change::of(o.serial_offset, n.serial_offset),
        ..ExportDiff::new(reference, Some(i), Some(j))
    };
    if is_script_object(new.package, j) {
        return diff;
    }
    diff.size = Change::of(o.serial_size, n.serial_size);
    let old_object = Object::read(old, i);
    let new_object = Object::read(new, j);
    diff.properties = properties(&old_object.properties, &new_object.properties);
    for property in &mut diff.properties {
        property.volatile = volatile_reason(new, j, &property.name);
    }
    if let (Some(a), Some(b)) = (&old_object.native, &new_object.native)
        && !a.same_as(b)
    {
        diff.native = Some(Change {
            old: a.bytes.len(),
            new: b.bytes.len(),
        });
    }
    diff.old_error = old_object.error;
    diff.new_error = new_object.error;
    // The same properties and native data, of a different size: only how
    // references and names are stored changed (a compact index of another
    // size). Not for a class, whose own data isn't compared.
    let same = diff.properties.iter().all(PropertyDiff::is_volatile)
        && diff.native.is_none()
        && diff.old_error.is_none()
        && diff.new_error.is_none();
    if same && n.class != ObjectRef::Null {
        diff.encoded_size = diff.size.take();
    }
    diff
}

/// An export's properties as T3D, and its native data.
struct Object<'a> {
    /// `(name, value)`.
    properties: Vec<(String, String)>,
    /// `None` if the properties couldn't all be read.
    native: Option<Native<'a>>,
    error: Option<Error>,
}

/// An export's native data: the bytes after its properties, and for a
/// texture or a sound, what's read of them, and the bytes after that.
struct Native<'a> {
    bytes: &'a [u8],
    read: Option<(Media, &'a [u8])>,
}

/// Native data that's read rather than compared as bytes, without what
/// changes when it moves in the file or the tables change: its lazy arrays'
/// `SeekPos`, and names and objects by their index (their names and
/// references are kept).
#[derive(PartialEq)]
enum Media {
    Texture(Texture),
    Sound(Sound, String),
    /// A sound group's package, or its sounds' references.
    SoundGroup(Option<String>, Option<Vec<String>>),
}

impl Media {
    fn texture(mut texture: Texture) -> Media {
        for mip in &mut texture.mips {
            mip.data.seek_pos = None;
        }
        Media::Texture(texture)
    }

    fn sound(package: &Package, mut sound: Sound) -> Media {
        let file_type = package.name(sound.file_type.0 as usize).to_lowercase();
        sound.file_type = NameRef::default();
        sound.data.seek_pos = None;
        Media::Sound(sound, file_type)
    }

    fn sound_group(package: &Package, group: SoundGroup) -> Media {
        let sounds = group
            .sounds
            .map(|sounds| sounds.into_iter().map(|s| package.reference(s)).collect());
        Media::SoundGroup(group.package.map(|p| p.text()), sounds)
    }
}

impl Native<'_> {
    /// Whether two objects' native data is the same: what's read of it, and
    /// the bytes after that, or if it isn't read, its bytes.
    fn same_as(&self, other: &Native) -> bool {
        match (&self.read, &other.read) {
            (Some(a), Some(b)) => a == b,
            _ => self.bytes == other.bytes,
        }
    }
}

impl<'a> Object<'a> {
    fn read(side: &Side<'a>, export: usize) -> Object<'a> {
        let package = side.package;
        let mut object = Object {
            properties: Vec::new(),
            native: None,
            error: None,
        };
        let mut properties = match side.packages.properties(package, export) {
            Ok(properties) => properties,
            Err(e) => {
                object.error = Some(e);
                return object;
            }
        };
        for property in properties.by_ref() {
            match property {
                Ok(p) => object
                    .properties
                    .push((p.t3d_name(), package.t3d_value(&p.value))),
                Err(e) => object.error = Some(e),
            }
        }
        if object.error.is_some() {
            return object;
        }
        let end = properties.offset();
        let data = package.export_data(export);
        let mut native = Native {
            bytes: &data[end..],
            read: None,
        };
        // A sound's subclasses don't start with a sound's data.
        let is = |class: &str| class_path(package, export).eq_ignore_ascii_case(class);
        let read = if is_a(side, export, "Engine.Texture") {
            Some(
                package
                    .texture(export, end)
                    .map(|(texture, after)| (Media::texture(texture), after)),
            )
        } else if is("Engine.Sound") {
            Some(
                package
                    .read_native(export, end)
                    .map(|(sound, after)| (Media::sound(package, sound), after)),
            )
        } else if is("Engine.SoundGroup") {
            Some(
                package
                    .read_native(export, end)
                    .map(|(group, after)| (Media::sound_group(package, group), after)),
            )
        } else {
            None
        };
        match read {
            Some(Ok((media, after))) => native.read = Some((media, &data[after..])),
            Some(Err(e)) => {
                object.error = Some(e);
                return object;
            }
            None => {}
        }
        object.native = Some(native);
        object
    }
}

/// Whether an export's class is `class` (`Engine.Texture`) or a subclass of
/// it, as far as its classes can be found.
fn is_a(side: &Side, export: usize, class: &str) -> bool {
    let package = side.package;
    let mut scope = side
        .packages
        .resolve(package, package.exports()[export].class);
    if scope.is_none() {
        // Not found: by its class's path alone.
        return class_path(package, export).eq_ignore_ascii_case(class);
    }
    while let Some(s) = scope {
        let path = s.package.full_path(ObjectRef::Export(s.export));
        if path.is_some_and(|path| path.eq_ignore_ascii_case(class)) {
            return true;
        }
        // As `Packages` finds superclasses: by the class's own data.
        scope = s
            .package
            .structure(s.export)
            .ok()
            .flatten()
            .and_then(|structure| {
                side.packages
                    .resolve(&s.package, structure.field.super_field)
            });
    }
    false
}

/// Why a property of an export is volatile, if it is: its name (without a
/// static array's index) is in [`VOLATILE_PROPERTIES`], declared by the
/// export's class or a superclass.
fn volatile_reason(side: &Side, export: usize, t3d_name: &str) -> Option<&'static str> {
    let name = t3d_name.split('(').next().expect("a name");
    VOLATILE_PROPERTIES
        .iter()
        .find(|p| p.name.eq_ignore_ascii_case(name) && is_a(side, export, p.class))
        .map(|p| p.reason)
}

/// The properties added, removed or changed, by name in any case.
fn properties(old: &[(String, String)], new: &[(String, String)]) -> Vec<PropertyDiff> {
    let find = |list: &[(String, String)], name: &str| {
        list.iter()
            .find(|(n, _)| n.eq_ignore_ascii_case(name))
            .map(|(_, v)| v.clone())
    };
    let mut diffs = Vec::new();
    for (name, value) in new {
        let old_value = find(old, name);
        if old_value.as_ref() != Some(value) {
            diffs.push(PropertyDiff {
                name: name.clone(),
                old: old_value,
                new: Some(value.clone()),
                volatile: None,
            });
        }
    }
    for (name, value) in old {
        if find(new, name).is_none() {
            diffs.push(PropertyDiff {
                name: name.clone(),
                old: Some(value.clone()),
                new: None,
                volatile: None,
            });
        }
    }
    diffs
}
