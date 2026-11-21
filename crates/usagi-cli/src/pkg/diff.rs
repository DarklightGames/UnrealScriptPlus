//! `usagi pkg diff`: compare two versions of a package.

use std::collections::BTreeSet;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::rc::Rc;

use serde_json::{Value as Json, json};
use usagi_pkg::diff::{Change, ExportDiff, PackageDiff, Side};
use usagi_pkg::{Error, ObjectFlags, ObjectPattern, Package, Packages};

use super::obj::{is_class, not_found};
use super::{
    BOLD, CYAN, GREEN, RED, Style, YELLOW, flags_text, human_size, package_flags_text, width,
};
use crate::cli::{PkgDiffArgs, PkgDiffFormat};

/// `usagi pkg diff`: prints the differences between two packages. Returns
/// `false` if an object's properties couldn't all be read (each problem is
/// written to `err`), or with `--exit-code`, if the packages differ.
pub fn diff(
    args: &PkgDiffArgs,
    color: bool,
    out: &mut impl Write,
    err: &mut impl Write,
) -> Result<bool, String> {
    let [old, new] = resolve(args)?;
    // Offline unless --online is given (and comes after any --offline).
    let online = args.online;
    let (old, old_path, mut old_package) = read_source(old, online)?;
    let (new, new_path, new_package) = read_source(new, online)?;
    // A package's references to its own objects include its name: named
    // the same, the two compare equal.
    old_package.name = new_package.name.clone();
    let old = Input::new(old, &old_path, old_package);
    let new = Input::new(new, &new_path, new_package);
    let filtered = !args.objects.is_empty() || !args.class.is_empty();
    let (old_exports, new_exports) = if filtered {
        let (a, b) = select(args, &old.package, &new.package, &new.label)?;
        (Some(a), Some(b))
    } else {
        (None, None)
    };
    let diff = usagi_pkg::diff::diff(
        &Side {
            packages: &old.packages,
            package: &old.package,
            exports: old_exports.as_deref(),
        },
        &Side {
            packages: &new.packages,
            package: &new.package,
            exports: new_exports.as_deref(),
        },
    );
    let view = View {
        old: &old,
        new: &new,
        diff: &diff,
        all: args.all,
        filtered,
        humanize: args.humanize && args.format == PkgDiffFormat::Text,
    };
    match args.format {
        PkgDiffFormat::Text => view.text(out, Style { color }),
        PkgDiffFormat::Concise => view.concise(out),
        PkgDiffFormat::Json => view.json(out),
    }
    .map_err(|e| e.to_string())?;
    let mut ok = true;
    for export in &diff.exports {
        let errors = [(&old, &export.old_error), (&new, &export.new_error)];
        for (input, error) in errors {
            if let Some(error) = error {
                writeln!(
                    err,
                    "usagi: {}: {}: {}",
                    input.label, export.reference, error
                )
                .map_err(|e| e.to_string())?;
                ok = false;
            }
        }
    }
    let differs = if filtered {
        diff.exports.iter().any(|e| e.has_changes(args.all))
    } else {
        diff.has_changes(args.all)
    };
    Ok(ok && !(args.exit_code && differs))
}

/// Where a package is read from, and how it's named in the output.
struct Source {
    label: String,
    kind: SourceKind,
}

enum SourceKind {
    File(PathBuf),
    /// `REV:PATH` as git names it, and where its file is in the working
    /// tree, to find the game it's in.
    Git {
        spec: String,
        work_tree: PathBuf,
    },
}

impl Source {
    fn file(path: &str) -> Source {
        Source {
            label: path.to_string(),
            kind: SourceKind::File(PathBuf::from(path)),
        }
    }

    /// An argument naming a file: a file, or if there's no such file and it
    /// has a `:`, `REV:PATH` in git.
    fn explicit(arg: &str) -> Source {
        match arg.rsplit_once(':') {
            Some((_, path)) if !Path::new(arg).exists() => Source {
                label: arg.to_string(),
                kind: SourceKind::Git {
                    spec: arg.to_string(),
                    work_tree: work_tree_path(path),
                },
            },
            _ => Source::file(arg),
        }
    }

    /// `path` (relative to the current directory, `repo_path` in the
    /// repository) in revision `rev`.
    fn revision(rev: &str, path: &str, repo_path: &str) -> Source {
        let spec = format!("{}:{}", rev, repo_path);
        Source {
            label: spec.clone(),
            kind: SourceKind::Git {
                spec,
                work_tree: PathBuf::from(path),
            },
        }
    }
}

/// The two packages the arguments name: `[REV [REV]] [--] PATH`, or two
/// files (or `REV:PATH`s).
fn resolve(args: &PkgDiffArgs) -> Result<[Source; 2], String> {
    let (revs, path): (&[String], &str) = match &args.path {
        Some(path) => (&args.args, path),
        None => {
            let explicit = |arg: &String| Path::new(arg).exists() || arg.contains(':');
            if args.args.len() == 2 && args.args.iter().all(explicit) {
                return Ok([
                    Source::explicit(&args.args[0]),
                    Source::explicit(&args.args[1]),
                ]);
            }
            let (path, revs) = args
                .args
                .split_last()
                .ok_or("a package's path is needed: `pkg diff [REV [REV]] PATH`")?;
            (revs, path)
        }
    };
    if revs.len() > 2 {
        return Err(format!(
            "at most two revisions can be compared, not {}: `pkg diff [REV [REV]] PATH`",
            revs.len()
        ));
    }
    for rev in revs {
        if !is_revision(rev)? {
            return Err(format!("{} is neither a file nor a git revision", rev));
        }
    }
    let repo_path = repo_path(path)?;
    let in_revision = |rev: &str| -> Result<Source, String> {
        let source = Source::revision(rev, path, &repo_path);
        if !git(&["cat-file", "-e", &format!("{}:{}", rev, repo_path)])?
            .status
            .success()
        {
            return Err(format!("{} isn't in {}", repo_path, rev));
        }
        Ok(source)
    };
    let old = in_revision(revs.first().map_or("HEAD", String::as_str))?;
    let new = match revs.get(1) {
        Some(rev) => in_revision(rev)?,
        None => Source::file(path),
    };
    Ok([old, new])
}

/// Runs git with `args` in the current directory.
fn git(args: &[&str]) -> Result<std::process::Output, String> {
    Command::new("git")
        .args(args)
        .output()
        .map_err(|e| format!("can't run git: {}", e))
}

/// Whether `rev` names a commit.
fn is_revision(rev: &str) -> Result<bool, String> {
    let commit = format!("{}^{{commit}}", rev);
    Ok(git(&["rev-parse", "--verify", "--quiet", &commit])?
        .status
        .success())
}

/// Where `path` (relative to the current directory, or absolute) is in the
/// repository the current directory is in, as git names it.
fn repo_path(path: &str) -> Result<String, String> {
    let output = git(&["rev-parse", "--show-toplevel", "--show-prefix"])?;
    if !output.status.success() {
        let message = String::from_utf8_lossy(&output.stderr);
        return Err(format!("{}: {}", path, message.trim()));
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut lines = stdout.lines();
    let top = PathBuf::from(lines.next().unwrap_or_default());
    let prefix = lines.next().unwrap_or_default().to_string();
    let outside = || format!("{} is outside the repository at {}", path, top.display());
    let relative = if Path::new(path).is_absolute() {
        // Through symbolic links, as git gives the top.
        let canonical = |p: &Path| std::fs::canonicalize(p).unwrap_or_else(|_| p.to_path_buf());
        let file = Path::new(path);
        let resolved = match (file.parent(), file.file_name()) {
            (Some(dir), Some(name)) => canonical(dir).join(name),
            _ => file.to_path_buf(),
        };
        resolved
            .strip_prefix(canonical(&top))
            .map_err(|_| outside())?
            .to_path_buf()
    } else {
        Path::new(&prefix).join(path)
    };
    // `..` and `.`, without the file system, which may not have the file.
    let mut parts: Vec<String> = Vec::new();
    for part in relative.components() {
        match part {
            std::path::Component::Normal(name) => parts.push(name.to_string_lossy().into_owned()),
            std::path::Component::ParentDir => {
                parts.pop().ok_or_else(outside)?;
            }
            _ => {}
        }
    }
    Ok(parts.join("/"))
}

/// A package read from a file or a git revision, and the packages its
/// properties are declared in.
struct Input {
    /// As given on the command line.
    label: String,
    package: Rc<Package>,
    packages: Packages,
}

impl Input {
    /// `package`, read from `path` (or its place in the working tree), and
    /// the packages of the game that's in.
    fn new(label: String, path: &Path, package: Package) -> Input {
        // The package itself first, so that the classes it declares are
        // found in it rather than in the game's copy.
        let packages = Packages::new(super::game::search_paths(path));
        let package = packages.add(package);
        Input {
            label,
            package,
            packages,
        }
    }
}

/// Reads a package: its label, its path (or its place in the working tree),
/// and the package.
fn read_source(source: Source, online: bool) -> Result<(String, PathBuf, Package), String> {
    match source.kind {
        SourceKind::Git { spec, work_tree } => {
            let package = from_git(&spec, &work_tree, online)?;
            Ok((source.label, work_tree, package))
        }
        SourceKind::File(file) => {
            let package = Package::read(&file).map_err(|e| {
                // A file Git LFS hasn't downloaded is a pointer to it.
                let bytes = std::fs::read(&file).unwrap_or_default();
                match lfs_pointer(&bytes) {
                    Some(_) => format!(
                        "{}: it's a Git LFS pointer: its object isn't downloaded",
                        source.label
                    ),
                    None => format!("{}: {}", source.label, e),
                }
            })?;
            Ok((source.label, file, package))
        }
    }
}

/// The first line of a Git LFS pointer file.
const LFS_POINTER: &str = "version https://git-lfs.github.com/spec/v1\n";

/// The object a Git LFS pointer file points to: its SHA-256 (in hex) and
/// size. `None` if `bytes` isn't one.
fn lfs_pointer(bytes: &[u8]) -> Option<(String, u64)> {
    let text = std::str::from_utf8(bytes).ok()?.strip_prefix(LFS_POINTER)?;
    let mut oid = None;
    let mut size = None;
    for line in text.lines() {
        if let Some(hex) = line.strip_prefix("oid sha256:") {
            oid = Some(hex.to_string());
        } else if let Some(n) = line.strip_prefix("size ") {
            size = n.parse().ok();
        }
    }
    Some((oid?, size?))
}

/// Reads `REV:PATH` from git. Online, through git's filters, which may
/// download it (from Git LFS). Offline, as it's stored, or if that's a Git
/// LFS pointer, from the local LFS cache, without running Git LFS; it's an
/// error if the object isn't there.
fn from_git(arg: &str, path: &Path, online: bool) -> Result<Package, String> {
    let fail = |e: String| format!("{}: {}", arg, e);
    let command: &[&str] = if online {
        &["cat-file", "--filters", arg]
    } else {
        &["cat-file", "blob", arg]
    };
    let output = git(command).map_err(fail)?;
    if !output.status.success() {
        let message = String::from_utf8_lossy(&output.stderr);
        return Err(fail(message.trim().to_string()));
    }
    let mut bytes = output.stdout;
    if !online && let Some((oid, size)) = lfs_pointer(&bytes) {
        bytes = lfs_object(&oid, size).ok_or_else(|| {
            fail(
                "it's in Git LFS, and its object isn't downloaded; nothing was downloaded, \
                 since `pkg diff` works offline unless `--online` is given"
                    .to_string(),
            )
        })?;
    }
    let is_uz2 = path
        .extension()
        .is_some_and(|ext| ext.eq_ignore_ascii_case("uz2"));
    let mut package = if is_uz2 {
        Package::from_uz2_bytes(&bytes)
    } else {
        Package::from_bytes(&bytes)
    }
    .map_err(|e| fail(e.to_string()))?;
    package.name = usagi_pkg::package_name(path);
    Ok(package)
}

/// A Git LFS object, from the local cache: `lfs/objects/<ab>/<cd>/<oid>` in
/// the repository's git folder, or in `lfs.storage` if it's set. `None` if
/// it isn't there, or isn't all there.
fn lfs_object(oid: &str, size: u64) -> Option<Vec<u8>> {
    let output = |args: &[&str]| -> Option<String> {
        let output = git(args).ok().filter(|o| o.status.success())?;
        Some(String::from_utf8_lossy(&output.stdout).trim().to_string())
    };
    let git_dir = PathBuf::from(output(&["rev-parse", "--git-common-dir"])?);
    let storage = output(&["config", "--get", "lfs.storage"])
        .map_or_else(|| git_dir.join("lfs"), |dir| git_dir.join(dir));
    let (Some(a), Some(b)) = (oid.get(0..2), oid.get(2..4)) else {
        return None;
    };
    let bytes = std::fs::read(storage.join("objects").join(a).join(b).join(oid)).ok()?;
    (bytes.len() as u64 == size).then_some(bytes)
}

/// Where `REV:PATH`'s file is in the work tree, to find the game it's in:
/// as git reads `PATH`, relative to the current directory if it starts with
/// `./` or `../`, and otherwise to the top of the work tree.
fn work_tree_path(path: &str) -> PathBuf {
    if path.starts_with("./") || path.starts_with("../") {
        return PathBuf::from(path);
    }
    let top = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()
        .ok()
        .filter(|output| output.status.success())
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string());
    match top {
        Some(top) => Path::new(&top).join(path),
        None => PathBuf::from(path),
    }
}

/// The exports of each package that `--object` and `--class` choose. It's an
/// error if an object matches in neither.
fn select(
    args: &PkgDiffArgs,
    old: &Package,
    new: &Package,
    new_label: &str,
) -> Result<(Vec<usize>, Vec<usize>), String> {
    let (mut a, mut b) = (BTreeSet::new(), BTreeSet::new());
    if args.objects.is_empty() {
        a.extend(0..old.exports().len());
        b.extend(0..new.exports().len());
    }
    for text in &args.objects {
        let pattern: ObjectPattern = text.parse()?;
        let (found_old, found_new) = (old.find_exports(&pattern), new.find_exports(&pattern));
        if found_old.is_empty() && found_new.is_empty() {
            return Err(not_found(new, &pattern, new_label));
        }
        a.extend(found_old);
        b.extend(found_new);
    }
    let of_class = |package: &Package, exports: BTreeSet<usize>| -> Vec<usize> {
        exports
            .into_iter()
            .filter(|&i| {
                args.class.is_empty() || args.class.iter().any(|c| is_class(package, i, c))
            })
            .collect()
    };
    Ok((of_class(old, a), of_class(new, b)))
}

/// Added, removed or changed.
#[derive(Clone, Copy)]
enum Sign {
    Added,
    Removed,
    Changed,
}

impl Sign {
    fn text(self) -> &'static str {
        match self {
            Sign::Added => "+",
            Sign::Removed => "-",
            Sign::Changed => "~",
        }
    }

    fn color(self) -> &'static str {
        match self {
            Sign::Added => GREEN,
            Sign::Removed => RED,
            Sign::Changed => YELLOW,
        }
    }
}

/// A difference to print: a line of text, or of the concise format.
enum Item {
    /// A field of the summary, or of an export (`object`): its label and
    /// `old -> new`.
    Field {
        object: Option<String>,
        label: &'static str,
        change: String,
    },
    /// A dependency or import added or removed.
    Member {
        section: &'static str,
        sign: Sign,
        name: String,
    },
    /// An export: added or removed (with its size), or changed (followed by
    /// its fields and properties).
    Export {
        sign: Sign,
        reference: String,
        size: Option<String>,
    },
    /// A property: `Name=new`, `Name=old`, or `Name=old -> new`.
    Property {
        object: String,
        sign: Sign,
        name: String,
        value: String,
    },
    /// Why an export's properties couldn't all be read, in the old package
    /// or the new.
    Error {
        object: String,
        side: &'static str,
        message: String,
    },
}

struct View<'a> {
    old: &'a Input,
    new: &'a Input,
    diff: &'a PackageDiff,
    all: bool,
    filtered: bool,
    humanize: bool,
}

impl View<'_> {
    /// A size in bytes, as a value: in units if humanizing.
    fn size(&self, bytes: u64) -> String {
        if self.humanize {
            human_size(bytes)
        } else {
            format!("{} bytes", bytes)
        }
    }

    /// Two sizes and the difference between them: `100 -> 120 bytes (+20)`,
    /// or humanized, `1.0 KB -> 1.2 KB (+200 B)`.
    fn size_change(&self, change: &Change<u64>) -> String {
        let delta = change.new as i128 - change.old as i128;
        let sign = if delta < 0 { "-" } else { "+" };
        let magnitude = delta.unsigned_abs() as u64;
        if self.humanize {
            format!(
                "{} -> {} ({}{})",
                human_size(change.old),
                human_size(change.new),
                sign,
                human_size(magnitude)
            )
        } else {
            format!(
                "{} -> {} bytes ({}{})",
                change.old, change.new, sign, magnitude
            )
        }
    }

    /// The differences, in the order they're printed.
    fn items(&self) -> Vec<Item> {
        let mut items = Vec::new();
        if !self.filtered {
            self.summary_items(&mut items);
            let sets = [
                ("Dependencies", &self.diff.dependencies),
                ("Imports", &self.diff.imports),
            ];
            for (section, set) in sets {
                let added = set.added.iter().map(|n| (Sign::Added, n));
                let removed = set.removed.iter().map(|n| (Sign::Removed, n));
                for (sign, name) in added.chain(removed) {
                    items.push(Item::Member {
                        section,
                        sign,
                        name: name.clone(),
                    });
                }
            }
        }
        for export in &self.diff.exports {
            if export.has_changes(self.all) {
                self.export_items(export, &mut items);
            }
        }
        items
    }

    fn summary_items(&self, items: &mut Vec<Item>) {
        let (old, new) = (&self.old.package, &self.new.package);
        let summary = &self.diff.summary;
        let mut field = |label, change: String| {
            items.push(Item::Field {
                object: None,
                label,
                change,
            })
        };
        let arrow = |a: String, b: String| format!("{} -> {}", a, b);
        if summary.tag.is_some() {
            let tag = |p: &Package| format!("{:#010X}", p.summary.tag);
            field("Tag", arrow(tag(old), tag(new)));
        }
        if summary.version.is_some() || summary.licensee_version.is_some() {
            let version = |p: &Package| {
                format!(
                    "{} (licensee {})",
                    p.summary.version(),
                    p.summary.licensee_version()
                )
            };
            field("Version", arrow(version(old), version(new)));
        }
        if let Some(Change { old, new }) = summary.package_flags {
            let text = |f| package_flags_text(f, Style { color: false });
            field("Flags", arrow(text(old), text(new)));
        }
        if let Some(Change { old, new }) = summary.size {
            field(
                "Size",
                self.size_change(&Change {
                    old: old as u64,
                    new: new as u64,
                }),
            );
        }
        if let Some(Change { old, new }) = summary.compressed_size {
            let change = match (old, new) {
                (Some(old), Some(new)) => self.size_change(&Change {
                    old: old as u64,
                    new: new as u64,
                }),
                _ => {
                    let size = |s: Option<usize>| {
                        s.map_or("not compressed".to_string(), |s| self.size(s as u64))
                    };
                    arrow(size(old), size(new))
                }
            };
            field("Compressed", change);
        }
        if !self.all {
            return;
        }
        if let Some(Change { old, new }) = summary.guid {
            field("GUID", arrow(old.to_string(), new.to_string()));
        }
        if let Some(Change { old, new }) = &summary.generations {
            field(
                "Generations",
                arrow(old.len().to_string(), new.len().to_string()),
            );
        }
        if let Some(Change { old, new }) = summary.heritage {
            let text = |h: Option<(u32, u32)>| {
                h.map_or("none".to_string(), |(count, offset)| {
                    format!("{} GUIDs at {:#X}", count, offset)
                })
            };
            field(
                "Heritage",
                arrow(
                    text(old.map(|h| (h.count, h.offset))),
                    text(new.map(|h| (h.count, h.offset))),
                ),
            );
        }
        let tables = [
            ("Names", summary.names),
            ("Imports", summary.imports),
            ("Exports", summary.exports),
        ];
        for (label, change) in tables {
            if let Some(Change { old, new }) = change {
                let text = |(count, offset): (u32, u32)| format!("{} at {:#X}", count, offset);
                field(label, arrow(text(old), text(new)));
            }
        }
    }

    fn export_items(&self, export: &ExportDiff, items: &mut Vec<Item>) {
        let reference = export.reference.clone();
        let size_of =
            |input: &Input, i: usize| self.size(u64::from(input.package.exports()[i].serial_size));
        match (export.old, export.new) {
            (None, Some(j)) => {
                return items.push(Item::Export {
                    sign: Sign::Added,
                    reference,
                    size: Some(size_of(self.new, j)),
                });
            }
            (Some(i), None) => {
                return items.push(Item::Export {
                    sign: Sign::Removed,
                    reference,
                    size: Some(size_of(self.old, i)),
                });
            }
            _ => {}
        }
        items.push(Item::Export {
            sign: Sign::Changed,
            reference: reference.clone(),
            size: None,
        });
        let mut field = |label, change| {
            items.push(Item::Field {
                object: Some(reference.clone()),
                label,
                change,
            })
        };
        // With --all, a size that changed only in how references are stored.
        let size = export.size.or(export.encoded_size.filter(|_| self.all));
        if let Some(Change { old, new }) = size {
            field(
                "Size",
                self.size_change(&Change {
                    old: old.into(),
                    new: new.into(),
                }),
            );
        }
        if let Some(Change { old, new }) = export.flags {
            field("Flags", format!("{} -> {}", flags(old), flags(new)));
        }
        if let Some(Change { old, new }) = &export.super_struct {
            field("Super", format!("{} -> {}", old, new));
        }
        if self.all {
            let (i, j) = (export.old.expect("both"), export.new.expect("both"));
            if i != j {
                field("Index", format!("{} -> {}", i + 1, j + 1));
            }
            if let Some(Change { old, new }) = export.offset {
                let text = |o: Option<u32>| o.map_or("none".to_string(), |o| format!("{:#X}", o));
                field("Offset", format!("{} -> {}", text(old), text(new)));
            }
        }
        // Volatile properties only with --all.
        let shown = export
            .properties
            .iter()
            .filter(|p| self.all || !p.is_volatile());
        for property in shown {
            let (sign, value) = match (&property.old, &property.new) {
                (None, Some(new)) => (Sign::Added, new.clone()),
                (Some(old), None) => (Sign::Removed, old.clone()),
                (Some(old), Some(new)) => (Sign::Changed, format!("{} -> {}", old, new)),
                (None, None) => unreachable!("a property is in one of them"),
            };
            let value = if property.is_volatile() {
                format!("{} (volatile)", value)
            } else {
                value
            };
            items.push(Item::Property {
                object: reference.clone(),
                sign,
                name: property.name.clone(),
                value,
            });
        }
        if let Some(Change { old, new }) = export.native {
            let change = if old == new {
                format!("{} (changed)", self.size(old as u64))
            } else {
                self.size_change(&Change {
                    old: old as u64,
                    new: new as u64,
                })
            };
            items.push(Item::Field {
                object: Some(reference.clone()),
                label: "Native data",
                change,
            });
        }
        let errors = [("old", &export.old_error), ("new", &export.new_error)];
        for (side, error) in errors {
            if let Some(error) = error {
                items.push(Item::Error {
                    object: reference.clone(),
                    side,
                    message: error.to_string(),
                });
            }
        }
    }

    /// Text: the files' names, then a section for each kind of difference,
    /// like a unified diff. Nothing if there's no difference.
    fn text(&self, out: &mut impl Write, style: Style) -> io::Result<()> {
        let items = self.items();
        if items.is_empty() {
            return Ok(());
        }
        writeln!(
            out,
            "{}",
            style.paint(BOLD, &format!("--- {}", self.old.label))
        )?;
        writeln!(
            out,
            "{}",
            style.paint(BOLD, &format!("+++ {}", self.new.label))
        )?;
        let label_width = width(items.iter().filter_map(|item| match item {
            Item::Field { label, .. } => Some(*label),
            _ => None,
        }));
        let mut section = "";
        for item in &items {
            let heading = match item {
                Item::Field { object: None, .. } => "Summary",
                Item::Member { section, .. } => section,
                _ => "Exports",
            };
            if heading != section {
                writeln!(out, "{}", style.paint(BOLD, heading))?;
                section = heading;
            }
            let sign = |sign: Sign| style.paint(sign.color(), sign.text());
            match item {
                Item::Field {
                    object,
                    label,
                    change,
                } => {
                    let indent = if object.is_some() { "      " } else { "  " };
                    let label = format!("{:<1$}", label, label_width);
                    writeln!(out, "{}{}  {}", indent, style.paint(CYAN, &label), change)?;
                }
                Item::Member { sign: s, name, .. } => writeln!(out, "  {} {}", sign(*s), name)?,
                Item::Export {
                    sign: s,
                    reference,
                    size,
                } => match size {
                    Some(size) => writeln!(out, "  {} {} ({})", sign(*s), reference, size)?,
                    None => writeln!(out, "  {} {}", sign(*s), reference)?,
                },
                Item::Property {
                    sign: s,
                    name,
                    value,
                    ..
                } => writeln!(out, "      {} {}={}", sign(*s), name, value)?,
                Item::Error { side, message, .. } => writeln!(
                    out,
                    "      {} {}",
                    style.paint(RED, &format!("Error ({}):", side)),
                    message
                )?,
            }
        }
        Ok(())
    }

    /// One line per difference, without colour: its sign, what it's in, and
    /// what changed.
    fn concise(&self, out: &mut impl Write) -> io::Result<()> {
        for item in self.items() {
            match item {
                Item::Field {
                    object: None,
                    label,
                    change,
                } => writeln!(out, "~ {}: {}", label, change)?,
                Item::Field {
                    object: Some(object),
                    label,
                    change,
                } => writeln!(out, "~ {} {}: {}", object, label, change)?,
                Item::Member {
                    section,
                    sign,
                    name,
                } => writeln!(out, "{} {}: {}", sign.text(), section, name)?,
                Item::Export {
                    sign: Sign::Changed,
                    ..
                } => {}
                Item::Export {
                    sign, reference, ..
                } => writeln!(out, "{} {}", sign.text(), reference)?,
                Item::Property {
                    object,
                    sign,
                    name,
                    value,
                } => writeln!(out, "{} {} {}={}", sign.text(), object, name, value)?,
                Item::Error {
                    object,
                    side,
                    message,
                } => writeln!(out, "! {} Error ({}): {}", object, side, message)?,
            }
        }
        Ok(())
    }

    fn json(&self, out: &mut impl Write) -> io::Result<()> {
        let diff = self.diff;
        let mut object = serde_json::Map::new();
        object.insert("old".into(), json!(self.old.label));
        object.insert("new".into(), json!(self.new.label));
        if !self.filtered {
            object.insert("summary".into(), self.json_summary());
            for (key, set) in [
                ("dependencies", &diff.dependencies),
                ("imports", &diff.imports),
            ] {
                object.insert(
                    key.into(),
                    json!({ "added": set.added, "removed": set.removed }),
                );
            }
        }
        let exports: Vec<Json> = diff
            .exports
            .iter()
            .filter(|e| e.has_changes(self.all))
            .map(|e| self.json_export(e))
            .collect();
        object.insert("exports".into(), Json::Array(exports));
        serde_json::to_writer_pretty(&mut *out, &Json::Object(object))?;
        writeln!(out)
    }

    fn json_summary(&self) -> Json {
        let (old, new) = (&self.old.package, &self.new.package);
        let summary = &self.diff.summary;
        let mut fields = serde_json::Map::new();
        let mut field = |key: &str, value: Json| {
            fields.insert(key.to_string(), value);
        };
        let pair = |old: Json, new: Json| json!({ "old": old, "new": new });
        if summary.tag.is_some() {
            field("tag", pair(json!(old.summary.tag), json!(new.summary.tag)));
        }
        if summary.version.is_some() {
            field(
                "version",
                pair(json!(old.summary.version()), json!(new.summary.version())),
            );
        }
        if summary.licensee_version.is_some() {
            field(
                "licensee_version",
                pair(
                    json!(old.summary.licensee_version()),
                    json!(new.summary.licensee_version()),
                ),
            );
        }
        if let Some(Change { old, new }) = summary.package_flags {
            field("package_flags", pair(json!(old), json!(new)));
        }
        if let Some(Change { old, new }) = summary.size {
            field("size", size_json(Some(old as u64), Some(new as u64)));
        }
        if let Some(Change { old, new }) = summary.compressed_size {
            field(
                "compressed_size",
                size_json(old.map(|s| s as u64), new.map(|s| s as u64)),
            );
        }
        if self.all {
            if let Some(Change { old, new }) = summary.guid {
                field("guid", pair(json!(old), json!(new)));
            }
            if let Some(Change { old, new }) = &summary.generations {
                field("generations", pair(json!(old), json!(new)));
            }
            if let Some(Change { old, new }) = summary.heritage {
                field("heritage", pair(json!(old), json!(new)));
            }
            let tables = [
                ("names", summary.names),
                ("imports", summary.imports),
                ("exports", summary.exports),
            ];
            for (key, change) in tables {
                if let Some(Change { old, new }) = change {
                    let table =
                        |(count, offset): (u32, u32)| json!({ "count": count, "offset": offset });
                    field(key, pair(table(old), table(new)));
                }
            }
        }
        Json::Object(fields)
    }

    fn json_export(&self, export: &ExportDiff) -> Json {
        let status = match (export.old, export.new) {
            (None, _) => "added",
            (_, None) => "removed",
            _ => "changed",
        };
        let serial_size = |input: &Input, i: Option<usize>| {
            i.map(|i| u64::from(input.package.exports()[i].serial_size))
        };
        let size = match status {
            "changed" => export.size.as_ref().map_or(Json::Null, |c| {
                size_json(Some(c.old.into()), Some(c.new.into()))
            }),
            _ => size_json(
                serial_size(self.old, export.old),
                serial_size(self.new, export.new),
            ),
        };
        let change = |c: Option<Json>| c.unwrap_or(Json::Null);
        let properties: Vec<Json> = export
            .properties
            .iter()
            .filter(|p| self.all || !p.is_volatile())
            .map(|p| json!({ "name": p.name, "old": p.old, "new": p.new, "volatile": p.volatile }))
            .collect();
        let mut object = json!({
            "status": status,
            "reference": export.reference,
            "old_index": export.old.map(|i| i + 1),
            "new_index": export.new.map(|i| i + 1),
            "size": size,
            "flags": change(export.flags.map(|c| json!({ "old": c.old, "new": c.new }))),
            "super": change(export.super_struct.as_ref().map(|c| json!({ "old": c.old, "new": c.new }))),
            "properties": properties,
            "native": change(export.native.map(|c| {
                let mut native = size_json(Some(c.old as u64), Some(c.new as u64));
                native["changed"] = json!(true);
                native
            })),
            "old_error": error_json(&export.old_error),
            "new_error": error_json(&export.new_error),
        });
        if self.all {
            object["offset"] = change(export.offset.map(|c| json!({ "old": c.old, "new": c.new })));
            object["encoded_size"] = change(
                export
                    .encoded_size
                    .map(|c| size_json(Some(c.old.into()), Some(c.new.into()))),
            );
        }
        object
    }
}

/// `{old, new, delta}`, where a missing size counts as 0 in the delta.
fn size_json(old: Option<u64>, new: Option<u64>) -> Json {
    let delta = new.unwrap_or(0) as i128 - old.unwrap_or(0) as i128;
    json!({ "old": old, "new": new, "delta": delta as i64 })
}

fn error_json(error: &Option<Error>) -> Json {
    error.as_ref().map_or(Json::Null, |e| json!(e.to_string()))
}

/// Object flags as text: their names joined with commas, or `0` for none.
fn flags(flags: ObjectFlags) -> String {
    match flags_text(flags) {
        text if text.is_empty() => "0".to_string(),
        text => text,
    }
}
