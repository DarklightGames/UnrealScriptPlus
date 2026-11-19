//! `usagi pkg`: inspect package files.

use std::io::{self, Write};
use std::path::Path;

use serde::Serialize;
use usagi_pkg::{ObjectFlags, ObjectRef, Package, Summary, TRIPWIRE_PACKAGE_FILE_TAG};

use crate::cli::{PkgFormat, PkgListArgs, PkgSort};

/// `usagi pkg list`: prints a package's summary and, if asked, its imports
/// and exports.
pub fn list(args: &PkgListArgs, color: bool, out: &mut impl Write) -> Result<(), String> {
    let package =
        Package::read(&args.path).map_err(|e| format!("{}: {}", args.path.display(), e))?;
    let view = View {
        path: &args.path,
        package: &package,
        package_name: package_name(&args.path),
        summary: !args.no_summary && args.format != PkgFormat::Concise,
        // The dependencies are listed when no table is asked for.
        dependencies: args.dependencies || args.all || !(args.imports || args.exports),
        imports: args.imports || args.all,
        exports: args.exports || args.all,
        sort: args.sort,
        reverse: args.reverse,
        humanize: args.humanize,
    };
    match args.format {
        PkgFormat::Text => view.text(out, Style { color }),
        PkgFormat::Concise => view.concise(out),
        PkgFormat::Json => view.json(out),
    }
    .map_err(|e| e.to_string())
}

/// The package's name, which the engine takes from the file name: the part
/// before the first `.` (so `Engine` for `Engine.u` and `Engine.u.uz2`).
fn package_name(path: &Path) -> String {
    let file_name = path.file_name().unwrap_or_default().to_string_lossy();
    file_name.split('.').next().unwrap_or_default().to_string()
}

/// What to print.
struct View<'a> {
    path: &'a Path,
    package: &'a Package,
    package_name: String,
    summary: bool,
    dependencies: bool,
    imports: bool,
    exports: bool,
    sort: PkgSort,
    reverse: bool,
    humanize: bool,
}

/// ANSI colours, or none.
#[derive(Clone, Copy)]
struct Style {
    color: bool,
}

const BOLD: &str = "1";
const DIM: &str = "2";
const RED: &str = "31";
const GREEN: &str = "32";
const CYAN: &str = "36";

impl Style {
    fn paint(self, code: &str, text: &str) -> String {
        if self.color && !text.is_empty() {
            format!("\x1b[{}m{}\x1b[0m", code, text)
        } else {
            text.to_string()
        }
    }
}

/// The widest of some strings, in characters.
fn width<S: AsRef<str>>(items: impl IntoIterator<Item = S>) -> usize {
    items
        .into_iter()
        .map(|s| s.as_ref().chars().count())
        .max()
        .unwrap_or(0)
}

/// Flag names joined with commas, followed by any unknown bits.
fn flags_text(flags: ObjectFlags) -> String {
    let mut names: Vec<String> = flags.names().iter().map(|n| n.to_string()).collect();
    if flags.unknown() != 0 {
        names.push(format!("{:#010X}", flags.unknown()));
    }
    names.join(",")
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Align {
    Left,
    Right,
}

/// A column of a table: its header, alignment and colour.
struct Column {
    header: &'static str,
    align: Align,
    color: Option<&'static str>,
}

const fn column(header: &'static str, align: Align, color: Option<&'static str>) -> Column {
    Column {
        header,
        align,
        color,
    }
}

/// Writes a table with a header row, or nothing if it has no rows. A column
/// whose cells are all empty is left out. Columns are as wide as their widest cell or header.
fn write_table(
    out: &mut impl Write,
    style: Style,
    columns: &[Column],
    rows: &[Vec<String>],
) -> io::Result<()> {
    if rows.is_empty() {
        return Ok(());
    }
    let shown: Vec<usize> = (0..columns.len())
        .filter(|&c| rows.iter().any(|row| !row[c].is_empty()))
        .collect();
    let widths: Vec<usize> = shown
        .iter()
        .map(|&c| width(rows.iter().map(|row| &row[c])).max(columns[c].header.len()))
        .collect();
    let line = |cells: Vec<(String, Align, Option<&str>)>| {
        let mut line = String::new();
        for ((text, align, color), &width) in cells.into_iter().zip(&widths) {
            let padding = " ".repeat(width - text.chars().count());
            let text = match color {
                Some(code) => style.paint(code, &text),
                None => text,
            };
            line.push_str("  ");
            match align {
                Align::Left => line.push_str(&format!("{}{}", text, padding)),
                Align::Right => line.push_str(&format!("{}{}", padding, text)),
            }
        }
        line.trim_end().to_string()
    };
    let header = shown
        .iter()
        .map(|&c| (columns[c].header.to_string(), columns[c].align, Some(BOLD)))
        .collect();
    writeln!(out, "{}", line(header))?;
    for row in rows {
        let cells = shown
            .iter()
            .map(|&c| (row[c].clone(), columns[c].align, columns[c].color))
            .collect();
        writeln!(out, "{}", line(cells))?;
    }
    Ok(())
}

/// A count with thousands separators: `15,530`.
fn separated(count: u64) -> String {
    let digits = count.to_string();
    let mut out = String::new();
    for (i, digit) in digits.chars().enumerate() {
        if i > 0 && (digits.len() - i) % 3 == 0 {
            out.push(',');
        }
        out.push(digit);
    }
    out
}

/// A size in binary units with one decimal: `40 B`, `1.3 MB`.
fn human_size(bytes: u64) -> String {
    const UNITS: [&str; 4] = ["KB", "MB", "GB", "TB"];
    if bytes < 1024 {
        return format!("{} B", bytes);
    }
    let mut value = bytes as f64 / 1024.0;
    let mut unit = 0;
    while value >= 1024.0 && unit + 1 < UNITS.len() {
        value /= 1024.0;
        unit += 1;
    }
    format!("{:.1} {}", value, UNITS[unit])
}

/// A file offset, in hexadecimal: `0x2A0`.
fn hex(offset: u32) -> String {
    format!("{:#X}", offset)
}

/// A package this one depends on: a top-level import.
struct Dependency<'a> {
    /// Its index in the import table.
    import: usize,
    name: &'a str,
    /// How many objects are imported from it, not counting groups.
    count: usize,
    /// The classes of those objects, with how many of each: the most common
    /// first, then by name.
    classes: Vec<(&'a str, usize)>,
}

/// Sorts `0..len` with `compare`, reversed if `reverse`. The sort is
/// stable, so ties keep table order.
fn sorted(
    len: usize,
    reverse: bool,
    compare: impl Fn(usize, usize) -> std::cmp::Ordering,
) -> Vec<usize> {
    let mut order: Vec<usize> = (0..len).collect();
    order.sort_by(|&a, &b| {
        let ordering = compare(a, b);
        if reverse {
            ordering.reverse()
        } else {
            ordering
        }
    });
    order
}

impl View<'_> {
    /// A count, with separators if humanizing.
    fn count(&self, count: impl Into<u64>) -> String {
        let count = count.into();
        if self.humanize {
            separated(count)
        } else {
            count.to_string()
        }
    }

    /// A size in bytes: in units if humanizing, otherwise the number.
    fn size(&self, bytes: impl Into<u64>) -> String {
        let bytes = bytes.into();
        if self.humanize {
            human_size(bytes)
        } else {
            bytes.to_string()
        }
    }

    /// A size in bytes, with `bytes` after the number if not humanizing.
    fn size_with_unit(&self, bytes: impl Into<u64>) -> String {
        let bytes = bytes.into();
        if self.humanize {
            human_size(bytes)
        } else {
            format!("{} bytes", bytes)
        }
    }

    /// An object's full path; `None` for no object. Exports start with this
    /// package's name; imports already start with theirs.
    fn full_path(&self, object: ObjectRef) -> Option<String> {
        let path = self.package.path(object)?;
        Some(match object {
            ObjectRef::Export(_) => format!("{}.{}", self.package_name, path),
            _ => path,
        })
    }

    fn text(&self, out: &mut impl Write, style: Style) -> io::Result<()> {
        writeln!(
            out,
            "{}",
            style.paint(BOLD, &self.path.display().to_string())
        )?;
        if self.summary {
            self.summary_text(out, style)?;
        }
        if self.dependencies {
            self.dependencies_text(out, style)?;
        }
        if self.imports {
            self.imports_text(out, style)?;
        }
        if self.exports {
            self.exports_text(out, style)?;
        }
        Ok(())
    }

    fn summary_text(&self, out: &mut impl Write, style: Style) -> io::Result<()> {
        let package = self.package;
        let summary = &package.summary;
        let label = |name: &str| style.paint(CYAN, &format!("  {:<13}", name));

        let tag = format!("{:#010X}", summary.tag);
        if summary.tag == TRIPWIRE_PACKAGE_FILE_TAG {
            let note = style.paint(DIM, "(Red Orchestra, Killing Floor)");
            writeln!(out, "{}{} {}", label("Tag"), tag, note)?;
        } else {
            writeln!(out, "{}{}", label("Tag"), tag)?;
        }
        writeln!(
            out,
            "{}{} (licensee {})",
            label("Version"),
            summary.version(),
            summary.licensee_version()
        )?;

        let flags = summary.package_flags;
        let mut names: Vec<String> = flags
            .names()
            .iter()
            .map(|n| style.paint(GREEN, n))
            .collect();
        if flags.unknown() != 0 {
            names.push(style.paint(RED, &format!("unknown {:#010X}", flags.unknown())));
        }
        if names.is_empty() {
            writeln!(out, "{}{:#010X}", label("Flags"), flags.0)?;
        } else {
            writeln!(
                out,
                "{}{:#010X} ({})",
                label("Flags"),
                flags.0,
                names.join(", ")
            )?;
        }

        writeln!(out, "{}{}", label("GUID"), summary.guid)?;
        let tables = [
            ("Names", summary.name_count, summary.name_offset),
            ("Imports", summary.import_count, summary.import_offset),
            ("Exports", summary.export_count, summary.export_offset),
        ];
        for (name, count, offset) in tables {
            writeln!(
                out,
                "{}{} at {}",
                label(name),
                self.count(count),
                hex(offset)
            )?;
        }
        if let Some(heritage) = summary.heritage {
            writeln!(
                out,
                "{}{} GUIDs at {}",
                label("Heritage"),
                self.count(heritage.count),
                hex(heritage.offset)
            )?;
        }
        let size = self.size_with_unit(package.size as u64);
        match package.compressed_size {
            Some(compressed) => writeln!(
                out,
                "{}{} ({} compressed)",
                label("Size"),
                size,
                self.size(compressed as u64)
            )?,
            None => writeln!(out, "{}{}", label("Size"), size)?,
        }

        let generations = &summary.generations;
        writeln!(
            out,
            "{}{}",
            label("Generations"),
            self.count(generations.len() as u64)
        )?;
        let number = generations.len().to_string().len();
        let exports: Vec<String> = generations
            .iter()
            .map(|g| self.count(g.export_count))
            .collect();
        let names: Vec<String> = generations
            .iter()
            .map(|g| self.count(g.name_count))
            .collect();
        let (exports_width, names_width) = (width(&exports), width(&names));
        for (i, (exports, names)) in exports.iter().zip(&names).enumerate() {
            writeln!(
                out,
                "    {}  {:>exports_width$} exports  {:>names_width$} names",
                style.paint(DIM, &format!("{:>number$}", i + 1)),
                exports,
                names
            )?;
        }
        Ok(())
    }

    /// The imports' indices, in the order to show them.
    fn import_order(&self) -> Vec<usize> {
        let package = self.package;
        let imports = package.imports();
        let lower = |text: &str| text.to_lowercase();
        let keys: Vec<String> = match self.sort {
            PkgSort::Path => (0..imports.len())
                .map(|i| lower(&self.full_path(ObjectRef::Import(i)).expect("a path")))
                .collect(),
            PkgSort::Class => imports
                .iter()
                .map(|import| lower(package.name(import.class_name)))
                .collect(),
            // Imports have no data, so these keep table order.
            PkgSort::Index | PkgSort::Size | PkgSort::Offset => Vec::new(),
        };
        sorted(imports.len(), self.reverse, |a, b| match self.sort {
            PkgSort::Index => a.cmp(&b),
            PkgSort::Path | PkgSort::Class => keys[a].cmp(&keys[b]),
            PkgSort::Size | PkgSort::Offset => std::cmp::Ordering::Equal,
        })
    }

    /// The exports' indices, in the order to show them.
    fn export_order(&self) -> Vec<usize> {
        let package = self.package;
        let exports = package.exports();
        let keys: Vec<String> = match self.sort {
            PkgSort::Path => (0..exports.len())
                .map(|i| {
                    let path = self.full_path(ObjectRef::Export(i)).expect("a path");
                    path.to_lowercase()
                })
                .collect(),
            PkgSort::Class => exports
                .iter()
                .map(|export| package.class_name(export).to_lowercase())
                .collect(),
            _ => Vec::new(),
        };
        sorted(exports.len(), self.reverse, |a, b| {
            let (x, y) = (&exports[a], &exports[b]);
            match self.sort {
                PkgSort::Index => a.cmp(&b),
                PkgSort::Path | PkgSort::Class => keys[a].cmp(&keys[b]),
                // Largest first.
                PkgSort::Size => y.serial_size.cmp(&x.serial_size),
                // Exports with no data last.
                PkgSort::Offset => (x.serial_offset.is_none(), x.serial_offset)
                    .cmp(&(y.serial_offset.is_none(), y.serial_offset)),
            }
        })
    }

    /// The packages this one depends on, in the order to show them.
    fn dependencies(&self) -> Vec<Dependency<'_>> {
        let package = self.package;
        let imports = package.imports();
        let mut dependencies: Vec<Dependency> = Vec::new();
        // Each top-level import's place in `dependencies`.
        let mut position = vec![None; imports.len()];
        for (i, import) in imports.iter().enumerate() {
            if import.outer == ObjectRef::Null {
                position[i] = Some(dependencies.len());
                dependencies.push(Dependency {
                    import: i,
                    name: package.name(import.object_name),
                    count: 0,
                    classes: Vec::new(),
                });
            }
        }
        for (i, import) in imports.iter().enumerate() {
            let is_group = package
                .name(import.class_package)
                .eq_ignore_ascii_case("Core")
                && package
                    .name(import.class_name)
                    .eq_ignore_ascii_case("Package");
            if import.outer == ObjectRef::Null || is_group {
                continue;
            }
            let ObjectRef::Import(top) = package.top_level(ObjectRef::Import(i)) else {
                unreachable!("an import's outers are imports");
            };
            let dependency = &mut dependencies[position[top].expect("a top-level import")];
            dependency.count += 1;
            let class = package.name(import.class_name);
            match dependency
                .classes
                .iter_mut()
                .find(|(name, _)| *name == class)
            {
                Some((_, count)) => *count += 1,
                None => dependency.classes.push((class, 1)),
            }
        }
        for dependency in &mut dependencies {
            dependency
                .classes
                .sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(b.0)));
        }
        // Stable, so ties keep table order.
        dependencies.sort_by(|x, y| {
            let ordering = match self.sort {
                PkgSort::Index => x.import.cmp(&y.import),
                PkgSort::Path => x.name.to_lowercase().cmp(&y.name.to_lowercase()),
                // Most objects first.
                PkgSort::Size => y.count.cmp(&x.count),
                PkgSort::Class | PkgSort::Offset => std::cmp::Ordering::Equal,
            };
            if self.reverse {
                ordering.reverse()
            } else {
                ordering
            }
        });
        dependencies
    }

    fn dependencies_text(&self, out: &mut impl Write, style: Style) -> io::Result<()> {
        let rows: Vec<Vec<String>> = self
            .dependencies()
            .into_iter()
            .map(|dependency| {
                let classes: Vec<String> = dependency
                    .classes
                    .iter()
                    .map(|(class, count)| format!("{} ({})", class, self.count(*count as u64)))
                    .collect();
                vec![
                    dependency.name.to_string(),
                    self.count(dependency.count as u64),
                    classes.join(", "),
                ]
            })
            .collect();
        writeln!(out)?;
        let heading = format!("Dependencies ({})", self.count(rows.len() as u64));
        writeln!(out, "{}", style.paint(BOLD, &heading))?;
        const COLUMNS: [Column; 3] = [
            column("Package", Align::Left, None),
            column("Imports", Align::Right, None),
            column("Classes", Align::Left, Some(CYAN)),
        ];
        write_table(out, style, &COLUMNS, &rows)
    }

    /// One name per line: the dependencies', then the imports' and exports'
    /// full paths.
    fn concise(&self, out: &mut impl Write) -> io::Result<()> {
        if self.dependencies {
            for dependency in self.dependencies() {
                writeln!(out, "{}", dependency.name)?;
            }
        }
        if self.imports {
            for i in self.import_order() {
                let path = self.full_path(ObjectRef::Import(i));
                writeln!(out, "{}", path.expect("an import has a path"))?;
            }
        }
        if self.exports {
            for i in self.export_order() {
                let path = self.full_path(ObjectRef::Export(i));
                writeln!(out, "{}", path.expect("an export has a path"))?;
            }
        }
        Ok(())
    }

    fn imports_text(&self, out: &mut impl Write, style: Style) -> io::Result<()> {
        let package = self.package;
        let imports = package.imports();
        let rows: Vec<Vec<String>> = self
            .import_order()
            .into_iter()
            .map(|i| {
                let import = &imports[i];
                let object = ObjectRef::Import(i);
                vec![
                    object.raw().to_string(),
                    format!(
                        "{}.{}",
                        package.name(import.class_package),
                        package.name(import.class_name)
                    ),
                    self.full_path(object).expect("an import has a path"),
                ]
            })
            .collect();
        writeln!(out)?;
        let heading = format!("Imports ({})", self.count(rows.len() as u64));
        writeln!(out, "{}", style.paint(BOLD, &heading))?;
        const COLUMNS: [Column; 3] = [
            column("#", Align::Right, Some(DIM)),
            column("Class", Align::Left, Some(CYAN)),
            column("Path", Align::Left, None),
        ];
        write_table(out, style, &COLUMNS, &rows)
    }

    fn exports_text(&self, out: &mut impl Write, style: Style) -> io::Result<()> {
        let package = self.package;
        let exports = package.exports();
        let rows: Vec<Vec<String>> = self
            .export_order()
            .into_iter()
            .map(|i| {
                let export = &exports[i];
                let object = ObjectRef::Export(i);
                vec![
                    object.raw().to_string(),
                    package.class_name(export).to_string(),
                    self.full_path(object).expect("an export has a path"),
                    self.size(export.serial_size),
                    export.serial_offset.map(hex).unwrap_or_default(),
                    self.full_path(export.super_struct).unwrap_or_default(),
                    flags_text(export.flags),
                ]
            })
            .collect();
        writeln!(out)?;
        let heading = format!("Exports ({})", self.count(rows.len() as u64));
        writeln!(out, "{}", style.paint(BOLD, &heading))?;
        const COLUMNS: [Column; 7] = [
            column("#", Align::Right, Some(DIM)),
            column("Class", Align::Left, Some(CYAN)),
            column("Path", Align::Left, None),
            column("Size", Align::Right, None),
            column("Offset", Align::Right, None),
            column("Super", Align::Left, None),
            column("Flags", Align::Left, Some(GREEN)),
        ];
        write_table(out, style, &COLUMNS, &rows)
    }

    fn json(&self, out: &mut impl Write) -> io::Result<()> {
        let package = self.package;
        let summary = self.summary.then(|| JsonSummary {
            size: package.size,
            compressed_size: package.compressed_size,
            version: package.summary.version(),
            licensee_version: package.summary.licensee_version(),
            summary: &package.summary,
        });
        let dependencies = self.dependencies.then(|| {
            self.dependencies()
                .into_iter()
                .map(|dependency| JsonDependency {
                    name: dependency.name,
                    index: ObjectRef::Import(dependency.import).raw(),
                    import_count: dependency.count,
                    classes: dependency
                        .classes
                        .into_iter()
                        .map(|(name, count)| JsonClassCount { name, count })
                        .collect(),
                })
                .collect()
        });
        let imports = self.imports.then(|| {
            self.import_order()
                .into_iter()
                .map(|i| {
                    let import = &package.imports()[i];
                    let object = ObjectRef::Import(i);
                    JsonImport {
                        index: object.raw(),
                        class_package: package.name(import.class_package),
                        class_name: package.name(import.class_name),
                        outer_index: import.outer.raw(),
                        object_name: package.name(import.object_name),
                        path: self.full_path(object).expect("an import has a path"),
                    }
                })
                .collect()
        });
        let exports = self.exports.then(|| {
            self.export_order()
                .into_iter()
                .map(|i| {
                    let export = &package.exports()[i];
                    let object = ObjectRef::Export(i);
                    JsonExport {
                        index: object.raw(),
                        class_index: export.class.raw(),
                        class_name: package.class_name(export),
                        super_index: export.super_struct.raw(),
                        super_path: self.full_path(export.super_struct),
                        outer_index: export.outer.raw(),
                        object_name: package.name(export.object_name),
                        path: self.full_path(object).expect("an export has a path"),
                        flags: export.flags,
                        serial_size: export.serial_size,
                        serial_offset: export.serial_offset,
                    }
                })
                .collect()
        });
        serde_json::to_writer_pretty(
            &mut *out,
            &JsonPackage {
                path: self.path.display().to_string(),
                summary,
                dependencies,
                imports,
                exports,
            },
        )?;
        writeln!(out)
    }
}

#[derive(Serialize)]
struct JsonPackage<'a> {
    path: String,
    #[serde(flatten)]
    summary: Option<JsonSummary<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    dependencies: Option<Vec<JsonDependency<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    imports: Option<Vec<JsonImport<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    exports: Option<Vec<JsonExport<'a>>>,
}

#[derive(Serialize)]
struct JsonSummary<'a> {
    size: usize,
    compressed_size: Option<usize>,
    version: u16,
    licensee_version: u16,
    summary: &'a Summary,
}

#[derive(Serialize)]
struct JsonImport<'a> {
    index: i32,
    class_package: &'a str,
    class_name: &'a str,
    outer_index: i32,
    object_name: &'a str,
    path: String,
}

#[derive(Serialize)]
struct JsonExport<'a> {
    index: i32,
    class_index: i32,
    class_name: &'a str,
    super_index: i32,
    super_path: Option<String>,
    outer_index: i32,
    object_name: &'a str,
    path: String,
    flags: ObjectFlags,
    serial_size: u32,
    serial_offset: Option<u32>,
}

#[derive(Serialize)]
struct JsonDependency<'a> {
    name: &'a str,
    index: i32,
    import_count: usize,
    classes: Vec<JsonClassCount<'a>>,
}

#[derive(Serialize)]
struct JsonClassCount<'a> {
    name: &'a str,
    count: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sizes_in_units() {
        assert_eq!(human_size(0), "0 B");
        assert_eq!(human_size(1023), "1023 B");
        assert_eq!(human_size(1024), "1.0 KB");
        assert_eq!(human_size(1536), "1.5 KB");
        assert_eq!(human_size(1024 * 1024 - 1), "1024.0 KB");
        assert_eq!(human_size(1024 * 1024), "1.0 MB");
        assert_eq!(human_size(2_722_417), "2.6 MB");
        assert_eq!(human_size(3 << 30), "3.0 GB");
        assert_eq!(human_size(5 << 40), "5.0 TB");
        assert_eq!(human_size(5000 << 40), "5000.0 TB");
    }

    #[test]
    fn counts_with_separators() {
        assert_eq!(separated(0), "0");
        assert_eq!(separated(999), "999");
        assert_eq!(separated(1000), "1,000");
        assert_eq!(separated(15530), "15,530");
        assert_eq!(separated(1_234_567), "1,234,567");
    }

    #[test]
    fn offsets_in_hex() {
        assert_eq!(hex(0), "0x0");
        assert_eq!(hex(672), "0x2A0");
    }
}
