//! `usagi pkg export`: write objects' native data and properties to files.

use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::io::Write;
use std::path::PathBuf;

use std::rc::Rc;

use usagi_pkg::{ObjectPattern, ObjectRef, Package, Packages, SearchPath};

use super::Style;
use super::obj::{Object, explain, is_class, not_found, write_t3d};
use crate::cli::{PkgExportArgs, PkgExportLayout};

/// A package file that has been read.
struct Input {
    file: String,
    package: Rc<Package>,
    /// The packages of the game it's in, which declare its objects' classes.
    packages: Rc<Packages>,
}

/// `usagi pkg export`: writes each object asked for to a `.bin` file (its
/// native data, after its properties) and a `.t3d` file (its properties).
/// Returns whether every object could be exported; problems are written to
/// `err`.
pub fn export(
    args: &PkgExportArgs,
    out: &mut impl Write,
    err: &mut impl Write,
) -> Result<bool, String> {
    // One set of packages per game, so each package is read once.
    let mut games: HashMap<Vec<SearchPath>, Rc<Packages>> = HashMap::new();
    let inputs = args
        .paths
        .iter()
        .map(|path| {
            let file = path.display().to_string();
            let package = Package::read(path).map_err(|e| format!("{}: {}", file, e))?;
            let packages = Rc::clone(
                games
                    .entry(super::game::search_paths(path))
                    .or_insert_with_key(|paths| Rc::new(Packages::new(paths.clone()))),
            );
            Ok(Input {
                file,
                package: packages.add(package),
                packages,
            })
        })
        .collect::<Result<Vec<_>, String>>()?;
    let selected = select(&inputs, args)?;

    let mut ok = true;
    let mut exports = Vec::new();
    for (input, export) in selected {
        let object = Object::read(&input.packages, &input.package, export, &[]);
        match (&object.error, object.end) {
            (None, Some(end)) => exports.push((input, object, end)),
            (error, _) => {
                let error = error.as_ref().expect("an error, since the end isn't known");
                writeln!(
                    err,
                    "usagi: {}: {}: {}",
                    input.file, object.reference, error
                )
                .map_err(|e| e.to_string())?;
                ok = false;
            }
        }
    }

    // Every destination, before anything is written. Different objects
    // only collide if the same package is given twice.
    let mut destinations: Vec<PathBuf> = Vec::new();
    let mut seen: HashMap<String, &str> = HashMap::new();
    let mut collisions = Vec::new();
    for (input, object, _) in &exports {
        let destination = destination(&input.package, object.export, args.layout);
        let key = destination.to_string_lossy().to_lowercase();
        match seen.get(&key) {
            Some(&other) => collisions.push(format!(
                "{} and {} would both be written to {}",
                other,
                object.reference,
                destination.display()
            )),
            None => {
                seen.insert(key, &object.reference);
            }
        }
        destinations.push(destination);
    }
    if !collisions.is_empty() {
        return Err(collisions.join("\n"));
    }

    for ((input, object, end), destination) in exports.iter().zip(&destinations) {
        let base = args.output.join(destination);
        if let Some(parent) = base.parent() {
            fs::create_dir_all(parent).map_err(|e| format!("{}: {}", parent.display(), e))?;
        }
        let write = |extension: &str, bytes: &[u8]| {
            // Appended: a flat name has dots in it already.
            let mut path = base.clone().into_os_string();
            path.push(".");
            path.push(extension);
            let path = PathBuf::from(path);
            fs::write(&path, bytes).map_err(|e| format!("{}: {}", path.display(), e))
        };
        write("bin", &input.package.export_data(object.export)[*end..])?;
        let mut t3d = Vec::new();
        write_t3d(&input.package, object, Style { color: false }, &mut t3d)
            .expect("writing to memory");
        write("t3d", &t3d)?;
    }
    writeln!(
        out,
        "Exported {} {} to {}",
        exports.len(),
        if exports.len() == 1 {
            "object"
        } else {
            "objects"
        },
        args.output.display()
    )
    .map_err(|e| e.to_string())?;
    Ok(ok)
}

/// The exports to write, in the order of the files and then of their export
/// tables: those matching the patterns (or every export), of the classes
/// asked for, except groups. A pattern only needs to match in one of the files.
fn select<'a>(
    inputs: &'a [Input],
    args: &PkgExportArgs,
) -> Result<Vec<(&'a Input, usize)>, String> {
    let patterns = args
        .objects
        .iter()
        .map(|text| text.parse::<ObjectPattern>())
        .collect::<Result<Vec<_>, _>>()?;
    let mut found: Vec<BTreeSet<usize>> = vec![BTreeSet::new(); inputs.len()];
    if patterns.is_empty() {
        for (input, found) in inputs.iter().zip(&mut found) {
            found.extend(0..input.package.exports().len());
        }
    }
    for pattern in &patterns {
        let mut any = false;
        for (input, found) in inputs.iter().zip(&mut found) {
            let matches = input.package.find_exports(pattern);
            any |= !matches.is_empty();
            found.extend(matches);
        }
        if !any {
            return Err(not_found_in(inputs, pattern));
        }
    }
    Ok(inputs
        .iter()
        .zip(found)
        .flat_map(|(input, found)| found.into_iter().map(move |export| (input, export)))
        // Groups are only containers: they're the folders or the path.
        .filter(|&(input, export)| !is_class(&input.package, export, "Core.Package"))
        .filter(|&(input, export)| {
            args.class.is_empty()
                || args
                    .class
                    .iter()
                    .any(|class| is_class(&input.package, export, class))
        })
        .collect())
}

/// Why `pattern` matches no export in any of the files.
fn not_found_in(inputs: &[Input], pattern: &ObjectPattern) -> String {
    if let [input] = inputs {
        return not_found(&input.package, pattern, &input.file);
    }
    if pattern.is_glob() {
        return format!(
            "{} matches no objects in the {} packages",
            pattern,
            inputs.len()
        );
    }
    inputs
        .iter()
        .find_map(|input| explain(&input.package, pattern, &input.file))
        .unwrap_or_else(|| format!("{} isn't in any of the {} packages", pattern, inputs.len()))
}

/// Where an export's files go in the output directory, without their
/// extension.
fn destination(package: &Package, export: usize, layout: PkgExportLayout) -> PathBuf {
    let object = ObjectRef::Export(export);
    match layout {
        PkgExportLayout::Flat => PathBuf::from(package.reference(object)),
        PkgExportLayout::Folder => {
            let mut path = PathBuf::from(&package.name);
            path.push(package.class_name(&package.exports()[export]));
            let groups = package.path(object).expect("an export");
            path.extend(groups.split('.'));
            path
        }
    }
}
