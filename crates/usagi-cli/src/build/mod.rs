//! `usagi build`: compile out-of-date packages with UCC.
//!
//! The packages are the `EditPackages` of the INI that UCC reads, in order,
//! and their sources are in `<root>/<Package>/Classes`. UCC writes compiled
//! packages to `<root>/System`.
//!
//! - With a mod (`<root>/<Mod>`), the INI is the mod's `System/<Mod>.ini`
//!   (made by UCC from the game's and the mod's `Default.ini`), the game's own
//!   packages (compiled already in `<root>/System`) are skipped, and the
//!   compiled packages are moved to `<root>/<Mod>/System`.
//! - Without one, the INI is one of the game's (`build.ini`), and the
//!   compiled packages stay in `<root>/System`.
//!
//! Which packages are up to date is tracked in a `.usagi-build.toml` (in the
//! mod folder, or the game directory) with a CRC of each package's sources.
//! A package is rebuilt when its sources change, and so is every package after
//! it in `EditPackages` (which may depend on it), unless
//! `--ignore-dependencies` is given. Locked packages (`build.locked`) are
//! never deleted or rebuilt; a build that would have to is an error.

pub mod ini;
mod manifest;
mod ucc;
mod version;

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use globset::{GlobBuilder, GlobSet, GlobSetBuilder};
use walkdir::WalkDir;

use crate::check::{self, CheckOptions};
use crate::cli::OutputFormat;
use crate::output;
use ini::{Ini, Op};
use manifest::Manifest;

/// The file that records which packages are up to date, in the mod folder
/// or the game directory.
pub const MANIFEST_FILE_NAME: &str = ".usagi-build.toml";

const EDITOR_SECTION: &str = "Editor.EditorEngine";

pub struct Options {
    pub root: PathBuf,
    /// The mod to build, or `None` to build without one.
    pub mod_name: Option<String>,
    /// Without a mod: the INI in `<root>/System` that UCC reads.
    pub ini: Option<String>,
    /// Globs of the packages never to delete or rebuild.
    pub locked: Vec<String>,
    pub clean: bool,
    pub dumpint: bool,
    pub debug: bool,
    pub ignore_dependencies: bool,
    pub check: bool,
    pub strict: bool,
    /// How many files to parse, or UCC processes to run, at once.
    pub jobs: usize,
    pub verbose: bool,
    pub color: bool,
    pub wine: String,
    pub version: Option<String>,
    pub version_class: Option<PathBuf>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Status {
    UpToDate,
    /// Its sources changed.
    Changed,
    /// A package before it is being rebuilt.
    Dependency,
}

impl Status {
    fn describe(self) -> &'static str {
        match self {
            Status::UpToDate => "up to date",
            Status::Changed => "changed",
            Status::Dependency => "rebuilding because a dependency changed",
        }
    }
}

struct Package {
    name: String,
    status: Status,
    crc: u32,
    files: Vec<PathBuf>,
    locked: bool,
}

/// What a build compiles, and where: a mod, or the game itself.
struct Target {
    /// Shown when the build starts.
    label: String,
    /// The game's System folder, where UCC writes.
    sys_dir: PathBuf,
    /// Where compiled packages and localization go, if not `sys_dir`.
    out_dir: Option<PathBuf>,
    /// UCC's arguments that choose the mod or the INI.
    ucc_args: Vec<String>,
    /// `EditPackages`, in order.
    packages: Vec<String>,
    /// Skip packages compiled in `sys_dir`: with a mod, they're the game's.
    skip_compiled: bool,
    /// The mod's own packages, which locks don't apply to.
    unlockable: Vec<String>,
    manifest_path: PathBuf,
    manifest: Manifest,
    /// The CRC of the mod's `Default.ini`, to record.
    default_ini_crc: Option<u32>,
    clean: bool,
    dumpint: bool,
}

/// Package-name globs.
struct Locks(GlobSet);

impl Locks {
    fn new(patterns: &[String]) -> Result<Self, String> {
        let mut builder = GlobSetBuilder::new();
        for pattern in patterns {
            let glob = GlobBuilder::new(pattern)
                .case_insensitive(true)
                .build()
                .map_err(|e| format!("invalid build.locked glob {:?}: {}", pattern, e))?;
            builder.add(glob);
        }
        builder
            .build()
            .map(Locks)
            .map_err(|e| format!("invalid build.locked globs: {}", e))
    }

    fn is_locked(&self, package: &str) -> bool {
        self.0.is_match(package)
    }
}

/// Finds the entry called `name` in `dir`, ignoring case, the way Windows
/// (and so UCC) would.
pub(crate) fn find_ci(dir: &Path, name: &str) -> Option<PathBuf> {
    let exact = dir.join(name);
    if exact.exists() {
        return Some(exact);
    }
    fs::read_dir(dir).ok()?.flatten().find_map(|entry| {
        entry
            .file_name()
            .to_str()
            .is_some_and(|n| n.eq_ignore_ascii_case(name))
            .then(|| entry.path())
    })
}

/// Moves a file, copying it if it can't be renamed (e.g. across devices).
fn move_file(from: &Path, to: &Path) -> io::Result<()> {
    if fs::rename(from, to).is_ok() {
        return Ok(());
    }
    fs::copy(from, to)?;
    fs::remove_file(from)
}

fn remove_file(path: &Path) -> Result<(), String> {
    fs::remove_file(path).map_err(|e| {
        format!(
            "could not remove {}: {} (is the game, a server or the editor running?)",
            path.display(),
            e
        )
    })
}

fn require_dir(path: PathBuf, what: &str) -> Result<PathBuf, String> {
    if path.is_dir() {
        Ok(path)
    } else {
        Err(format!("{} not found: {}", what, path.display()))
    }
}

/// The CRC of a package's sources: every `.uc` file under `classes`, in
/// order of their (lower-case) paths, with the paths, so that renaming or
/// removing a file is a change too. Also returns the files.
fn package_crc(classes: Option<&Path>) -> Result<(u32, Vec<PathBuf>), String> {
    let Some(classes) = classes else {
        return Ok((0, Vec::new()));
    };
    let mut files: Vec<(String, PathBuf)> = Vec::new();
    for entry in WalkDir::new(classes) {
        let entry = entry.map_err(|e| e.to_string())?;
        let path = entry.path();
        let is_uc = path
            .extension()
            .and_then(|e| e.to_str())
            .is_some_and(|e| e.eq_ignore_ascii_case("uc"));
        if entry.file_type().is_file() && is_uc {
            let relative = path
                .strip_prefix(classes)
                .unwrap_or(path)
                .to_string_lossy()
                .replace('\\', "/")
                .to_lowercase();
            files.push((relative, path.to_path_buf()));
        }
    }
    files.sort();
    let mut hasher = crc32fast::Hasher::new();
    for (relative, path) in &files {
        hasher.update(relative.as_bytes());
        hasher.update(&[0]);
        hasher.update(&fs::read(path).map_err(|e| format!("{}: {}", path.display(), e))?);
    }
    Ok((
        hasher.finalize(),
        files.into_iter().map(|(_, p)| p).collect(),
    ))
}

/// Checks that no two of the files that the mod's `Paths` find have the same
/// name, which would make UCC load one of them unpredictably.
fn check_ambiguous_files(mod_dir: &Path, paths: &[String]) -> Result<(), String> {
    let mut found: HashMap<String, Vec<PathBuf>> = HashMap::new();
    for pattern in paths {
        let pattern = pattern.replace('\\', "/");
        let (dir, file_glob) = pattern.rsplit_once('/').unwrap_or((".", &pattern));
        let Some(dir) = std::path::absolute(mod_dir.join(dir))
            .ok()
            .and_then(|d| d.canonicalize().ok())
        else {
            continue;
        };
        let Ok(glob) = GlobBuilder::new(file_glob).case_insensitive(true).build() else {
            continue;
        };
        let matcher = glob.compile_matcher();
        let Ok(entries) = fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let name = entry.file_name().to_string_lossy().into_owned();
            if entry.path().is_file() && matcher.is_match(&name) {
                let paths = found.entry(name.to_lowercase()).or_default();
                if !paths.contains(&entry.path()) {
                    paths.push(entry.path());
                }
            }
        }
    }
    let mut ambiguous: Vec<_> = found.into_values().filter(|p| p.len() > 1).collect();
    if ambiguous.is_empty() {
        return Ok(());
    }
    ambiguous.sort();
    let mut message =
        String::from("files with the same name are in more than one of the mod's Paths:");
    for paths in ambiguous {
        for path in paths {
            message.push_str(&format!("\n    {}", path.display()));
        }
    }
    message.push_str("\nThis is usually a file saved to the wrong folder.");
    Err(message)
}

/// Prints `text` centred in a line of dashes.
fn print_header(text: &str, color: Option<&str>) {
    const WIDTH: usize = 70;
    let len = text.chars().count();
    let left = (WIDTH / 2).saturating_sub(len / 2);
    let right = WIDTH.saturating_sub(left + len);
    let text = match color {
        Some(code) => format!("\x1b[{}m{}\x1b[0m", code, text),
        None => text.to_string(),
    };
    println!("{}{}{}", "-".repeat(left), text, "-".repeat(right));
}

/// Builds the mod, or the game's packages without one. Returns whether every
/// package compiled; setup problems, including a locked package that would
/// have to be rebuilt, are errors.
pub fn run(options: &Options) -> Result<bool, String> {
    let root = require_dir(options.root.clone(), "game directory")?;
    let sys_dir = require_dir(root.join("System"), "System directory")?;
    let locks = Locks::new(&options.locked)?;

    if let Some(class_path) = &options.version_class {
        let version = options
            .version
            .as_deref()
            .ok_or("build.version-class is set, but build.version isn't")?;
        version::write_class(&root, &root.join(class_path), version)?;
    }

    let mut target = match &options.mod_name {
        Some(mod_name) => mod_target(&root, sys_dir, mod_name, options)?,
        None => game_target(&root, sys_dir, options)?,
    };

    println!("Checking for out-of-date packages...");
    let packages = find_out_of_date(&root, &target, &locks, options)?;
    check_locks(&packages, &target)?;
    let to_compile: Vec<&Package> = packages
        .iter()
        .filter(|p| p.status != Status::UpToDate)
        .collect();
    let up_to_date = packages.len() - to_compile.len();
    // Up-to-date packages keep (or, if locked, get) their CRC.
    for package in packages.iter().filter(|p| p.status == Status::UpToDate) {
        target
            .manifest
            .packages
            .insert(package.name.clone(), package.crc);
    }

    if to_compile.is_empty() {
        target.manifest.default_ini_crc = target.default_ini_crc;
        target.manifest.write(&target.manifest_path)?;
        print_summary(0, 0, up_to_date, options.color);
        return Ok(true);
    }

    if options.check && !check_sources(&to_compile, options)? {
        return Ok(false);
    }

    print_header(&format!(" Build started for {} ", target.label), None);
    for package in &to_compile {
        println!("{}: {}", package.name, package.status.describe());
    }

    // Remove the packages being rebuilt, so UCC compiles them.
    let sys_dir = target.sys_dir.clone();
    for package in &to_compile {
        let file_name = format!("{}.u", package.name);
        for dir in std::iter::once(&sys_dir).chain(&target.out_dir) {
            if let Some(path) = find_ci(dir, &file_name) {
                remove_file(&path)?;
            }
        }
    }

    let ucc = ucc::Ucc::find(&sys_dir, &options.wine, options.verbose, options.color)?;
    let mut make_args = vec!["Editor.MakeCommandlet".to_string()];
    make_args.extend(target.ucc_args.iter().cloned());
    if options.debug {
        make_args.push("-debug".to_string());
    }
    let status = ucc.run_streaming(&make_args)?;

    // Dumping localization overwrites UCC.log, which editors read for errors.
    let log_path = find_ci(&sys_dir, "UCC.log");
    let log = log_path.as_ref().and_then(|p| fs::read(p).ok());

    let mut compiled = Vec::new();
    for package in &to_compile {
        if let Some(path) = find_ci(&sys_dir, &format!("{}.u", package.name)) {
            move_to(&path, target.out_dir.as_deref())?;
            compiled.push(*package);
        }
    }

    if target.dumpint {
        let changed: Vec<&str> = compiled
            .iter()
            .filter(|p| p.status == Status::Changed)
            .map(|p| p.name.as_str())
            .collect();
        ucc.dumpint(&changed, &target.ucc_args, options.jobs)?;
        for package in &to_compile {
            if let Some(path) = find_ci(&sys_dir, &format!("{}.int", package.name)) {
                move_to(&path, target.out_dir.as_deref())?;
            }
        }
        if let (Some(path), Some(log)) = (&log_path, &log) {
            fs::write(path, log).map_err(|e| format!("{}: {}", path.display(), e))?;
        }
    }

    // A package that didn't compile has no CRC, so it's rebuilt next time,
    // whether its own sources changed or one of its dependencies'.
    for package in &to_compile {
        target.manifest.packages.remove(&package.name);
    }
    for package in &compiled {
        target
            .manifest
            .packages
            .insert(package.name.clone(), package.crc);
    }
    target.manifest.default_ini_crc = target.default_ini_crc;
    target.manifest.write(&target.manifest_path)?;

    let failed = to_compile.len() - compiled.len();
    if failed > 0 && !status.success() {
        match status.code() {
            Some(code) => println!("UCC exited with status {}", code),
            None => println!("UCC was terminated"),
        }
    }
    print_summary(compiled.len(), failed, up_to_date, options.color);
    Ok(failed == 0)
}

/// Moves a file UCC wrote into `out_dir`, if there is one.
fn move_to(path: &Path, out_dir: Option<&Path>) -> Result<(), String> {
    let Some(out_dir) = out_dir else {
        return Ok(());
    };
    let target = out_dir.join(path.file_name().expect("a file name"));
    move_file(path, &target).map_err(|e| format!("{}: {}", path.display(), e))
}

/// Prepares a mod's build: cleans up, updates the mod's INI if needed, and
/// reads its packages.
fn mod_target(
    root: &Path,
    sys_dir: PathBuf,
    mod_name: &str,
    options: &Options,
) -> Result<Target, String> {
    let mod_dir = require_dir(root.join(mod_name), "mod directory")?;
    let mod_sys_dir = require_dir(mod_dir.join("System"), "mod System directory")?;

    let default_ini_path = mod_sys_dir.join("Default.ini");
    let default_ini_bytes = fs::read(&default_ini_path)
        .map_err(|e| format!("{}: {}", default_ini_path.display(), e))?;
    let default_ini_crc = crc32fast::hash(&default_ini_bytes);
    let mod_default = Ini::from_bytes(&default_ini_bytes);
    let mod_packages = mod_default.values(EDITOR_SECTION, "EditPackages", Op::Add);
    let config_path = mod_sys_dir.join(format!("{}.ini", mod_name));
    let manifest_path = mod_dir.join(MANIFEST_FILE_NAME);

    // UCC must load the mod's packages from the mod's System folder, so
    // remove any left in the game's (by a build that was interrupted). This
    // happens even if they're locked: they're the mod's, not the game's.
    for package in &mod_packages {
        if let Some(path) = find_ci(&sys_dir, &format!("{}.u", package)) {
            println!(
                "Removing {} left in {}",
                path.file_name().unwrap_or_default().to_string_lossy(),
                sys_dir.display()
            );
            remove_file(&path)?;
        }
    }

    let mut manifest = if options.clean {
        Manifest::default()
    } else {
        Manifest::load(&manifest_path)
    };
    let mut clean = options.clean;
    let mut dumpint = options.dumpint || options.clean;
    if manifest.default_ini_crc != Some(default_ini_crc) {
        if !clean {
            println!("Default.ini changed: rebuilding everything and exporting localization");
        }
        clean = true;
        dumpint = true;
    }
    if clean {
        manifest.packages.clear();
        update_mod_config(&sys_dir, &mod_default, &config_path)?;
    }

    let config = config_path
        .is_file()
        .then(|| Ini::read(&config_path))
        .transpose()
        .map_err(|e| format!("{}: {}", config_path.display(), e))?;
    if let Some(config) = &config {
        check_ambiguous_files(&mod_dir, &config.values("Core.System", "Paths", Op::Set))?;
    }
    let packages = match &config {
        Some(config) => config.values(EDITOR_SECTION, "EditPackages", Op::Set),
        None => mod_packages.clone(),
    };
    Ok(Target {
        label: format!("mod: {}", mod_name),
        sys_dir,
        out_dir: Some(mod_sys_dir),
        ucc_args: vec![format!("-mod={}", mod_name)],
        packages,
        skip_compiled: true,
        unlockable: mod_packages.clone(),
        manifest_path,
        manifest,
        default_ini_crc: Some(default_ini_crc),
        clean,
        dumpint,
    })
}

/// Prepares a build without a mod, of the `EditPackages` of one of the
/// game's INIs.
fn game_target(root: &Path, sys_dir: PathBuf, options: &Options) -> Result<Target, String> {
    let ini_path = match &options.ini {
        Some(name) => find_ci(&sys_dir, name)
            .ok_or_else(|| format!("build.ini {:?} not found in {}", name, sys_dir.display()))?,
        None => find_game_ini(&sys_dir)?,
    };
    let ini = Ini::read(&ini_path).map_err(|e| format!("{}: {}", ini_path.display(), e))?;
    check_ambiguous_files(&sys_dir, &ini.values("Core.System", "Paths", Op::Set))?;
    let ini_name = ini_path
        .file_name()
        .expect("a file name")
        .to_string_lossy()
        .into_owned();
    let manifest_path = root.join(MANIFEST_FILE_NAME);
    let manifest = if options.clean {
        Manifest::default()
    } else {
        Manifest::load(&manifest_path)
    };
    Ok(Target {
        label: ini_name.clone(),
        sys_dir,
        out_dir: None,
        ucc_args: vec![format!("-ini={}", ini_name)],
        packages: ini.values(EDITOR_SECTION, "EditPackages", Op::Set),
        skip_compiled: false,
        unlockable: Vec::new(),
        manifest_path,
        manifest,
        default_ini_crc: None,
        clean: options.clean,
        dumpint: options.dumpint || options.clean,
    })
}

/// The INI files that a build without a mod could use: those in `sys_dir`
/// with `EditPackages`, except the templates and the user settings.
fn find_game_ini(sys_dir: &Path) -> Result<PathBuf, String> {
    const NOT_GAME_INIS: [&str; 3] = ["default.ini", "defuser.ini", "user.ini"];
    let mut candidates: Vec<PathBuf> = fs::read_dir(sys_dir)
        .map_err(|e| format!("{}: {}", sys_dir.display(), e))?
        .flatten()
        .map(|e| e.path())
        .filter(|path| {
            let name = path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .to_lowercase();
            name.ends_with(".ini")
                && !NOT_GAME_INIS.contains(&name.as_str())
                && Ini::read(path).is_ok_and(|ini| {
                    !ini.values(EDITOR_SECTION, "EditPackages", Op::Set)
                        .is_empty()
                })
        })
        .collect();
    candidates.sort();
    match candidates.len() {
        1 => Ok(candidates.remove(0)),
        0 => Err(format!(
            "no INI in {} has EditPackages; set build.ini, or build a mod with --mod",
            sys_dir.display()
        )),
        _ => {
            let names: Vec<String> = candidates
                .iter()
                .map(|p| {
                    p.file_name()
                        .unwrap_or_default()
                        .to_string_lossy()
                        .into_owned()
                })
                .collect();
            Err(format!(
                "more than one INI in {} has EditPackages ({}); choose one with build.ini",
                sys_dir.display(),
                names.join(", ")
            ))
        }
    }
}

/// Updates the arrays (`EditPackages`, `ServerPackages`, `Paths`, ...) that
/// the mod's `Default.ini` changes in the mod's generated configuration,
/// keeping every other setting. Without the game's `Default.ini` to start
/// from, the configuration is removed instead, and UCC makes a new one.
fn update_mod_config(sys_dir: &Path, mod_default: &Ini, config_path: &Path) -> Result<(), String> {
    if !config_path.is_file() {
        return Ok(());
    }
    let error = |path: &Path, e: io::Error| format!("{}: {}", path.display(), e);
    let Some(base_path) = find_ci(sys_dir, "Default.ini") else {
        println!(
            "Removing {} (the game's Default.ini is missing, so it can't be updated)",
            config_path.display()
        );
        return remove_file(config_path);
    };
    let base = Ini::read(&base_path).map_err(|e| error(&base_path, e))?;
    let mut config = Ini::read(config_path).map_err(|e| error(config_path, e))?;
    let updated = ini::sync_arrays(&base, mod_default, &mut config);
    config
        .write(config_path)
        .map_err(|e| error(config_path, e))?;
    if !updated.is_empty() {
        let keys: Vec<String> = updated
            .iter()
            .map(|(s, k)| format!("[{}] {}", s, k))
            .collect();
        println!(
            "Updated {} in {}",
            keys.join(", "),
            config_path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
        );
    }
    Ok(())
}

/// The packages and whether each is up to date, in build order. With a mod,
/// the game's own packages, which are compiled already in `<root>/System`,
/// are left out.
fn find_out_of_date(
    root: &Path,
    target: &Target,
    locks: &Locks,
    options: &Options,
) -> Result<Vec<Package>, String> {
    let file_names = |dir: &Path| -> Result<Vec<String>, String> {
        Ok(fs::read_dir(dir)
            .map_err(|e| format!("{}: {}", dir.display(), e))?
            .flatten()
            .filter(|e| e.path().is_file())
            .map(|e| e.file_name().to_string_lossy().to_lowercase())
            .collect())
    };
    let in_sys = file_names(&target.sys_dir)?;
    let in_out = match &target.out_dir {
        Some(dir) => file_names(dir)?,
        None => in_sys.clone(),
    };
    let mut result: Vec<Package> = Vec::new();
    for name in &target.packages {
        let file_name = format!("{}.u", name).to_lowercase();
        if (target.skip_compiled && in_sys.contains(&file_name))
            || result.iter().any(|p| p.name.eq_ignore_ascii_case(name))
        {
            continue;
        }
        let locked = locks.is_locked(name)
            && !target
                .unlockable
                .iter()
                .any(|p| p.eq_ignore_ascii_case(name));
        let classes = find_ci(root, name).and_then(|dir| find_ci(&dir, "Classes"));
        if classes.is_none() {
            // Without sources it can't be rebuilt, so it's never deleted.
            if !in_out.contains(&file_name) {
                return Err(format!(
                    "{} has no sources ({}) and isn't compiled",
                    name,
                    root.join(name).join("Classes").display()
                ));
            }
            result.push(Package {
                name: name.clone(),
                status: Status::UpToDate,
                crc: 0,
                files: Vec::new(),
                locked,
            });
            continue;
        }
        let (crc, files) = package_crc(classes.as_deref())?;
        let rebuilding = result.iter().any(|p| p.status != Status::UpToDate);
        let recorded = target.manifest.packages.get(name);
        // A locked package with no record is taken as it is, and its CRC
        // recorded, so that later changes to its sources can be noticed.
        let changed = if locked && recorded.is_none() && !target.clean {
            false
        } else {
            recorded != Some(&crc)
        };
        let status = if changed {
            Status::Changed
        } else if rebuilding && !options.ignore_dependencies {
            Status::Dependency
        } else {
            Status::UpToDate
        };
        result.push(Package {
            name: name.clone(),
            status,
            crc,
            files,
            locked,
        });
    }
    Ok(result)
}

/// Fails if a locked package would be deleted or rebuilt, or is missing
/// (UCC would compile it).
fn check_locks(packages: &[Package], target: &Target) -> Result<(), String> {
    let compiled_dir = target.out_dir.as_ref().unwrap_or(&target.sys_dir);
    let mut problems = Vec::new();
    for package in packages.iter().filter(|p| p.locked) {
        if find_ci(compiled_dir, &format!("{}.u", package.name)).is_none() {
            problems.push(format!(
                "{} isn't compiled, so UCC would compile it",
                package.name
            ));
            continue;
        }
        match package.status {
            Status::UpToDate => {}
            _ if target.clean => problems.push(format!(
                "{} would be rebuilt by a clean build",
                package.name
            )),
            Status::Changed => problems.push(format!(
                "{}'s sources changed since it was last built",
                package.name
            )),
            Status::Dependency => problems.push(format!(
                "{} comes after a package that's being rebuilt and may depend on it (pass --ignore-dependencies to leave it as it is)",
                package.name
            )),
        }
    }
    if problems.is_empty() {
        return Ok(());
    }
    Err(format!(
        "locked packages (build.locked) would have to be rebuilt:\n    {}\nUnlock them to rebuild them.",
        problems.join("\n    ")
    ))
}

/// Parses the sources of the packages to compile, and prints the files that
/// don't parse. Returns whether every file parsed.
fn check_sources(packages: &[&Package], options: &Options) -> Result<bool, String> {
    let files: Vec<PathBuf> = packages.iter().flat_map(|p| p.files.clone()).collect();
    let never = |_: &Path, _| false;
    let check_options = CheckOptions {
        strict: options.strict,
        lint: false,
        ast: false,
        fix: false,
        is_enabled: &never,
        jobs: options.jobs,
    };
    let reports: Vec<_> = check::check_all(&files, &check_options)?
        .into_iter()
        .filter(|r| r.outcome.is_failure())
        .collect();
    if reports.is_empty() {
        return Ok(true);
    }
    let cwd = std::env::current_dir().map_err(|e| e.to_string())?;
    output::problems(
        &mut io::stdout().lock(),
        &reports,
        OutputFormat::Full,
        &cwd,
        options.color,
    )
    .map_err(|e| e.to_string())?;
    println!(
        "Not compiling: {} {} didn't parse (use --no-check to compile anyway)",
        reports.len(),
        if reports.len() == 1 { "file" } else { "files" }
    );
    Ok(false)
}

fn print_summary(succeeded: usize, failed: usize, up_to_date: usize, color: bool) {
    let text = format!(
        " Build: {} succeeded, {} failed, 0 skipped, {} up-to-date ",
        succeeded, failed, up_to_date
    );
    let code = if failed == 0 { "92" } else { "91" };
    print_header(&text, color.then_some(code));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn crc_covers_names_and_contents() {
        let dir = tempfile::TempDir::new().unwrap();
        let classes = dir.path();
        fs::write(classes.join("A.uc"), "class A;").unwrap();
        let (first, files) = package_crc(Some(classes)).unwrap();
        assert_eq!(files.len(), 1);
        fs::rename(classes.join("A.uc"), classes.join("B.uc")).unwrap();
        let (renamed, _) = package_crc(Some(classes)).unwrap();
        assert_ne!(first, renamed);
        fs::write(classes.join("notes.txt"), "ignored").unwrap();
        assert_eq!(package_crc(Some(classes)).unwrap().0, renamed);
        assert_eq!(package_crc(None).unwrap(), (0, Vec::new()));
    }

    #[test]
    fn ambiguous_files_are_errors() {
        let dir = tempfile::TempDir::new().unwrap();
        let root = dir.path();
        for path in ["System/A.u", "Mod/System/a.U", "Mod/System/B.u"] {
            let path = root.join(path);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, "").unwrap();
        }
        let mod_dir = root.join("Mod");
        let one = vec![
            "../Mod/System/*.u".to_string(),
            "../Mod/System/*.u".to_string(),
        ];
        assert!(check_ambiguous_files(&mod_dir, &one).is_ok());
        let both = vec!["../System/*.u".to_string(), "../Mod/System/*.u".to_string()];
        let error = check_ambiguous_files(&mod_dir, &both).unwrap_err();
        assert!(error.contains("A.u") && !error.contains("B.u"), "{}", error);
    }
}
