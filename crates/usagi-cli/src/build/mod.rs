//! `usagi build`: compile a mod's out-of-date packages with UCC.
//!
//! A mod lives in `<root>/<Mod>`, next to the game's `<root>/System`. Its
//! packages are the `EditPackages` that its `System/Default.ini` adds, and
//! their sources are in `<root>/<Package>/Classes`. UCC writes compiled
//! packages to `<root>/System`; they are moved to `<root>/<Mod>/System`.
//!
//! Which packages are up to date is tracked in `<root>/<Mod>/.usagi-build.toml`
//! with a CRC of each package's sources. A package is rebuilt when its
//! sources change, and so is every package after it in `EditPackages`
//! (which may depend on it), unless `--ignore-dependencies` is given.

pub mod ini;
mod manifest;
mod ucc;
mod version;

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use globset::GlobBuilder;
use walkdir::WalkDir;

use crate::check::{self, CheckOptions};
use crate::cli::OutputFormat;
use crate::output;
use ini::{Ini, Op};
use manifest::Manifest;

/// The file that records which packages are up to date, in the mod folder.
pub const MANIFEST_FILE_NAME: &str = ".usagi-build.toml";

const EDITOR_SECTION: &str = "Editor.EditorEngine";

pub struct Options {
    pub root: PathBuf,
    pub mod_name: String,
    pub clean: bool,
    pub dumpint: bool,
    pub debug: bool,
    pub ignore_dependencies: bool,
    pub check: bool,
    pub strict: bool,
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
}

/// Finds the entry called `name` in `dir`, ignoring case, the way Windows
/// (and so UCC) would.
fn find_ci(dir: &Path, name: &str) -> Option<PathBuf> {
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

/// Builds the mod. Returns whether every package compiled; setup problems
/// are errors.
pub fn run(options: &Options) -> Result<bool, String> {
    let root = require_dir(options.root.clone(), "game directory")?;
    let sys_dir = require_dir(root.join("System"), "System directory")?;
    let mod_dir = require_dir(root.join(&options.mod_name), "mod directory")?;
    let mod_sys_dir = require_dir(mod_dir.join("System"), "mod System directory")?;

    let default_ini_path = mod_sys_dir.join("Default.ini");
    let default_ini_bytes = fs::read(&default_ini_path)
        .map_err(|e| format!("{}: {}", default_ini_path.display(), e))?;
    let default_ini_crc = crc32fast::hash(&default_ini_bytes);
    let mod_default = Ini::from_bytes(&default_ini_bytes);
    let mod_packages = mod_default.values(EDITOR_SECTION, "EditPackages", Op::Add);
    let config_path = mod_sys_dir.join(format!("{}.ini", options.mod_name));
    let manifest_path = mod_dir.join(MANIFEST_FILE_NAME);

    if let Some(class_path) = &options.version_class {
        let version = options
            .version
            .as_deref()
            .ok_or("build.version-class is set, but build.version isn't")?;
        version::write_class(&root, &root.join(class_path), version)?;
    }

    // UCC must load the mod's packages from the mod's System folder, so
    // remove any left in the game's (by a build that was interrupted).
    for package in &mod_packages {
        if let Some(path) = find_ci(&sys_dir, &format!("{}.u", package)) {
            remove_file(&path)?;
        }
    }

    let mut manifest = if options.clean {
        Manifest::default()
    } else {
        Manifest::load(&manifest_path)
    };
    let mut clean = options.clean;
    let mut dumpint = options.dumpint;
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

    println!("Checking for out-of-date packages...");
    let packages = find_out_of_date(&root, &sys_dir, &packages, &manifest, options)?;
    let to_compile: Vec<&Package> = packages
        .iter()
        .filter(|p| p.status != Status::UpToDate)
        .collect();
    let up_to_date = packages.len() - to_compile.len();

    if to_compile.is_empty() {
        manifest.default_ini_crc = Some(default_ini_crc);
        manifest.write(&manifest_path)?;
        print_summary(0, 0, up_to_date, options.color);
        return Ok(true);
    }

    if options.check && !check_sources(&to_compile, options)? {
        return Ok(false);
    }

    print_header(
        &format!(" Build started for mod: {} ", options.mod_name),
        None,
    );
    for package in &to_compile {
        println!("{}: {}", package.name, package.status.describe());
    }

    // Remove the packages being rebuilt, so UCC compiles them.
    for package in &to_compile {
        let file_name = format!("{}.u", package.name);
        for dir in [&sys_dir, &mod_sys_dir] {
            if let Some(path) = find_ci(dir, &file_name) {
                remove_file(&path)?;
            }
        }
    }

    let ucc = ucc::Ucc::find(&sys_dir, &options.wine, options.verbose, options.color)?;
    let mut make_args = vec![
        "Editor.MakeCommandlet".to_string(),
        format!("-mod={}", options.mod_name),
    ];
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
            let target = mod_sys_dir.join(path.file_name().expect("a file name"));
            move_file(&path, &target).map_err(|e| format!("{}: {}", path.display(), e))?;
            compiled.push(*package);
        }
    }

    if dumpint {
        let changed: Vec<&str> = compiled
            .iter()
            .filter(|p| p.status == Status::Changed)
            .map(|p| p.name.as_str())
            .collect();
        ucc.dumpint(&changed, &options.mod_name)?;
        for package in &to_compile {
            if let Some(path) = find_ci(&sys_dir, &format!("{}.int", package.name)) {
                let target = mod_sys_dir.join(path.file_name().expect("a file name"));
                move_file(&path, &target).map_err(|e| format!("{}: {}", path.display(), e))?;
            }
        }
        if let (Some(path), Some(log)) = (&log_path, &log) {
            fs::write(path, log).map_err(|e| format!("{}: {}", path.display(), e))?;
        }
    }

    // A package that didn't compile has no CRC, so it's rebuilt next time,
    // whether its own sources changed or one of its dependencies'.
    for package in &to_compile {
        manifest.packages.remove(&package.name);
    }
    for package in &compiled {
        manifest.packages.insert(package.name.clone(), package.crc);
    }
    manifest.default_ini_crc = Some(default_ini_crc);
    manifest.write(&manifest_path)?;

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

/// The mod's packages and whether each is up to date, in build order. The
/// game's own packages, which are compiled already in `<root>/System`, are
/// left out.
fn find_out_of_date(
    root: &Path,
    sys_dir: &Path,
    packages: &[String],
    manifest: &Manifest,
    options: &Options,
) -> Result<Vec<Package>, String> {
    let compiled: Vec<String> = fs::read_dir(sys_dir)
        .map_err(|e| format!("{}: {}", sys_dir.display(), e))?
        .flatten()
        .filter(|e| e.path().is_file())
        .map(|e| e.file_name().to_string_lossy().to_lowercase())
        .collect();
    let mut result: Vec<Package> = Vec::new();
    for name in packages {
        if compiled.contains(&format!("{}.u", name).to_lowercase())
            || result.iter().any(|p| p.name.eq_ignore_ascii_case(name))
        {
            continue;
        }
        let classes = find_ci(root, name).and_then(|dir| find_ci(&dir, "Classes"));
        let (crc, files) = package_crc(classes.as_deref())?;
        let rebuilding = result.iter().any(|p| p.status != Status::UpToDate);
        let status = if manifest.packages.get(name) != Some(&crc) {
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
        });
    }
    Ok(result)
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
