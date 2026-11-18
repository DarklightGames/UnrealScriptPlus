//! Configuration files, and merging them with command-line flags.

use std::fs;
use std::path::{Path, PathBuf};

use globset::{GlobBuilder, GlobSet, GlobSetBuilder};
use serde::Deserialize;
use usagi_core::LintSelection;

/// The names a configuration file may have, in order of preference.
pub const FILE_NAMES: [&str; 2] = ["usagi.toml", ".usagi.toml"];

/// The contents of a configuration file.
#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct ConfigFile {
    pub include: Option<Vec<String>>,
    pub exclude: Option<Vec<String>>,
    pub strict: Option<bool>,
    /// How many files to parse, or UCC processes to run, at once.
    pub jobs: Option<std::num::NonZeroU16>,
    #[serde(default)]
    pub lint: LintConfig,
    #[serde(default)]
    pub build: BuildConfig,
}

/// Settings for `usagi build`. `mod` and `root` are read from the
/// configuration found as usual; `version` and `version-class` only from the
/// mod's own configuration file, `<root>/<Mod>/usagi.toml`; `wine` from
/// either, the mod's winning.
#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct BuildConfig {
    /// The mod to build: the folder in the game directory that has a
    /// `System/Default.ini`.
    #[serde(rename = "mod")]
    pub mod_name: Option<String>,
    /// The game directory, relative to the configuration file. Default: the
    /// configuration file's directory.
    pub root: Option<PathBuf>,
    /// The program that runs `UCC.exe` on systems other than Windows.
    /// Default: `wine`.
    pub wine: Option<String>,
    /// The mod's version, `MAJOR.MINOR.PATCH` or `MAJOR.MINOR.PATCH-PRERELEASE`.
    pub version: Option<String>,
    /// Generate an UnrealScript class with the version and the Git commit and
    /// branch at this path, relative to the game directory.
    pub version_class: Option<PathBuf>,
}

/// Loads a mod's own configuration file, `usagi.toml` or `.usagi.toml` in
/// `mod_dir`, if it has one.
pub fn load_mod(mod_dir: &Path) -> Result<Option<LoadedConfig>, ConfigError> {
    let Some(path) = FILE_NAMES
        .iter()
        .map(|name| mod_dir.join(name))
        .find(|path| path.is_file())
    else {
        return Ok(None);
    };
    let config = load(Some(&path), false, mod_dir)?;
    let build = &config.file.build;
    for (key, set) in [
        ("mod", build.mod_name.is_some()),
        ("root", build.root.is_some()),
    ] {
        if set {
            return Err(ConfigError(format!(
                "{}: build.{} can't be set in a mod's configuration; set it in the game directory's, or pass --{}",
                path.display(),
                key,
                key
            )));
        }
    }
    Ok(Some(config))
}

/// The default number of jobs: half the logical CPUs, so that the rest of
/// the machine stays responsive.
pub fn default_jobs() -> usize {
    std::thread::available_parallelism().map_or(1, |n| (n.get() / 2).max(1))
}

/// The game directory: `build.root`, relative to the configuration file,
/// or the configuration file's directory.
pub fn game_root(config: &LoadedConfig) -> PathBuf {
    let root = match &config.file.build.root {
        Some(root) => config.dir.join(root),
        None => config.dir.clone(),
    };
    std::path::absolute(&root).unwrap_or(root)
}

/// The configuration for a mod: `config` with the settings of the mod's own
/// file, `<game_root>/<mod_name>/usagi.toml`, if it has one, on top.
/// `include`, `exclude`, `strict`, `jobs`, `lint.select` and `build.wine` in the
/// mod's file replace `config`'s; `lint.extend-select`, `lint.ignore` and
/// `lint.per-path` are added to them. Globs in both files are then relative
/// to the game directory.
pub fn merge_mod(
    config: LoadedConfig,
    game_root: &Path,
    mod_name: &str,
) -> Result<LoadedConfig, ConfigError> {
    let Some(mod_config) = load_mod(&game_root.join(mod_name))? else {
        return Ok(config);
    };
    let (root, modded) = (config.file, mod_config.file);
    let concat = |mut a: Vec<String>, b: Vec<String>| {
        a.extend(b);
        a
    };
    let mut per_path = root.lint.per_path;
    per_path.extend(modded.lint.per_path);
    let file = ConfigFile {
        include: modded.include.or(root.include),
        exclude: modded.exclude.or(root.exclude),
        strict: modded.strict.or(root.strict),
        jobs: modded.jobs.or(root.jobs),
        lint: LintConfig {
            select: modded.lint.select.or(root.lint.select),
            extend_select: concat(root.lint.extend_select, modded.lint.extend_select),
            ignore: concat(root.lint.ignore, modded.lint.ignore),
            per_path,
        },
        build: BuildConfig {
            mod_name: Some(mod_name.to_string()),
            root: root.build.root,
            wine: modded.build.wine.or(root.build.wine),
            version: modded.build.version,
            version_class: modded.build.version_class,
        },
    };
    Ok(LoadedConfig {
        path: mod_config.path,
        dir: game_root.to_path_buf(),
        file,
    })
}

/// `config` merged with the configuration of the mod it names in
/// `build.mod`, if any. Used by every command but `build`, which may be
/// given another mod.
pub fn with_default_mod(config: LoadedConfig) -> Result<LoadedConfig, ConfigError> {
    let Some(mod_name) = config.file.build.mod_name.clone() else {
        return Ok(config);
    };
    let root = game_root(&config);
    merge_mod(config, &root, &mod_name)
}

/// Checks that the configuration found as usual doesn't set what only a
/// mod's configuration may.
pub fn check_root_build(config: &LoadedConfig) -> Result<(), ConfigError> {
    let build = &config.file.build;
    for (key, set) in [
        ("version", build.version.is_some()),
        ("version-class", build.version_class.is_some()),
    ] {
        if set {
            let path = config
                .path
                .as_deref()
                .map_or_else(|| "usagi.toml".to_string(), |p| p.display().to_string());
            return Err(ConfigError(format!(
                "{}: build.{} can only be set in the mod's own configuration, <root>/<Mod>/usagi.toml (run `usagi build` from the game directory)",
                path, key
            )));
        }
    }
    Ok(())
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct LintConfig {
    pub select: Option<Vec<String>>,
    #[serde(default)]
    pub extend_select: Vec<String>,
    #[serde(default)]
    pub ignore: Vec<String>,
    #[serde(default)]
    pub per_path: Vec<PerPathConfig>,
}

/// Lint settings for the files matching `paths`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct PerPathConfig {
    pub paths: Vec<String>,
    #[serde(default)]
    pub extend_select: Vec<String>,
    #[serde(default)]
    pub ignore: Vec<String>,
}

/// A configuration problem. Reported with exit code 2.
#[derive(Debug)]
pub struct ConfigError(pub String);

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// A configuration file and the directory its globs are relative to.
pub struct LoadedConfig {
    pub path: Option<PathBuf>,
    pub dir: PathBuf,
    pub file: ConfigFile,
}

/// Finds the configuration file for `start`: the first `usagi.toml` or
/// `.usagi.toml` in `start` or one of its ancestors.
pub fn find(start: &Path) -> Option<PathBuf> {
    start.ancestors().find_map(|dir| {
        FILE_NAMES
            .iter()
            .map(|name| dir.join(name))
            .find(|path| path.is_file())
    })
}

/// Loads the configuration: `explicit` if given, else the file found from
/// `cwd`, unless `disabled`.
pub fn load(
    explicit: Option<&Path>,
    disabled: bool,
    cwd: &Path,
) -> Result<LoadedConfig, ConfigError> {
    let path = match (disabled, explicit) {
        (true, _) => None,
        (false, Some(path)) => Some(cwd.join(path)),
        (false, None) => find(cwd),
    };
    let Some(path) = path else {
        return Ok(LoadedConfig {
            path: None,
            dir: cwd.to_path_buf(),
            file: ConfigFile::default(),
        });
    };
    let text =
        fs::read_to_string(&path).map_err(|e| ConfigError(format!("{}: {}", path.display(), e)))?;
    let file: ConfigFile =
        toml::from_str(&text).map_err(|e| ConfigError(format!("{}: {}", path.display(), e)))?;
    let dir = path
        .parent()
        .map_or_else(|| cwd.to_path_buf(), Path::to_path_buf);
    Ok(LoadedConfig {
        path: Some(path),
        dir,
        file,
    })
}

/// Globs and the directory they are relative to.
#[derive(Debug, Clone)]
pub struct Globs {
    base: PathBuf,
    set: GlobSet,
    len: usize,
}

impl Globs {
    pub fn new(base: &Path, patterns: &[String], what: &str) -> Result<Self, ConfigError> {
        let mut builder = GlobSetBuilder::new();
        for pattern in patterns {
            let normalized = pattern.replace('\\', "/");
            let normalized = normalized.trim_start_matches("./").trim_end_matches('/');
            let glob = GlobBuilder::new(normalized)
                .case_insensitive(true)
                .literal_separator(true)
                .build()
                .map_err(|e| ConfigError(format!("invalid {} glob {:?}: {}", what, pattern, e)))?;
            builder.add(glob);
        }
        let set = builder
            .build()
            .map_err(|e| ConfigError(format!("invalid {} globs: {}", what, e)))?;
        Ok(Globs {
            base: base.to_path_buf(),
            set,
            len: patterns.len(),
        })
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Whether a glob matches `path` or one of the folders it's in. `path`
    /// must be absolute; it's matched relative to the globs' directory.
    pub fn matches(&self, path: &Path) -> bool {
        let Ok(relative) = path.strip_prefix(&self.base) else {
            return false;
        };
        relative
            .ancestors()
            .filter(|p| !p.as_os_str().is_empty())
            .any(|p| self.set.is_match(p))
    }
}

/// Settings for one run, after merging the configuration with flags.
pub struct Settings {
    pub include: Globs,
    pub exclude: Globs,
    pub strict: bool,
    pub selection: LintSelection,
    pub per_path: Vec<(Globs, LintSelection)>,
    /// How many files to check at once.
    pub jobs: usize,
}

/// Flags that override the configuration.
#[derive(Default)]
pub struct Overrides {
    pub include: Vec<String>,
    pub exclude: Vec<String>,
    pub strict: Option<bool>,
    pub jobs: Option<u16>,
    pub select: Vec<String>,
    pub extend_select: Vec<String>,
    pub ignore: Vec<String>,
}

impl Settings {
    /// Merges `config` with `overrides`. Flag globs are relative to `cwd`.
    pub fn new(
        config: &LoadedConfig,
        overrides: Overrides,
        cwd: &Path,
    ) -> Result<Self, ConfigError> {
        let in_config = |key: &str| match &config.path {
            Some(path) => format!("{} in {}", key, path.display()),
            None => key.to_string(),
        };
        let file = &config.file;

        let globs = |flag: Vec<String>, configured: &Option<Vec<String>>, what: &str| {
            if flag.is_empty() {
                Globs::new(
                    &config.dir,
                    configured.as_deref().unwrap_or_default(),
                    &in_config(what),
                )
            } else {
                Globs::new(cwd, &flag, &format!("--{}", what))
            }
        };
        let include = globs(overrides.include, &file.include, "include")?;
        let exclude = globs(overrides.exclude, &file.exclude, "exclude")?;

        let select_key = if overrides.select.is_empty() {
            in_config("lint.select")
        } else {
            "--select".to_string()
        };
        let mut select = if overrides.select.is_empty() {
            file.lint
                .select
                .clone()
                .unwrap_or_else(|| vec!["ALL".to_string()])
        } else {
            overrides.select
        };
        select.extend(file.lint.extend_select.iter().cloned());
        select.extend(overrides.extend_select);
        let mut ignore = file.lint.ignore.clone();
        ignore.extend(overrides.ignore);
        check_selectors(&select, &select_key)?;
        check_selectors(&ignore, &in_config("lint.ignore"))?;
        let selection = LintSelection::new(select, ignore);

        let mut per_path = Vec::new();
        for (i, entry) in file.lint.per_path.iter().enumerate() {
            let key = in_config(&format!("lint.per-path[{}]", i));
            check_selectors(&entry.extend_select, &key)?;
            check_selectors(&entry.ignore, &key)?;
            let globs = Globs::new(&config.dir, &entry.paths, &key)?;
            per_path.push((globs, entry));
        }
        // Each file's selection is the global one plus every matching
        // per-path entry, in order, so later entries win.
        let per_path = per_path
            .into_iter()
            .map(|(globs, entry)| {
                let selection =
                    LintSelection::new(entry.extend_select.clone(), entry.ignore.clone());
                (globs, selection)
            })
            .collect();

        Ok(Settings {
            include,
            exclude,
            strict: overrides.strict.or(file.strict).unwrap_or(false),
            jobs: overrides
                .jobs
                .or(file.jobs.map(|n| n.get()))
                .map_or_else(default_jobs, usize::from),
            selection,
            per_path,
        })
    }

    /// Whether `lint` is reported for the file at `path`.
    pub fn is_enabled(&self, path: &Path, lint: usagi_core::Lint) -> bool {
        let mut enabled = self.selection.is_enabled(lint);
        for (globs, entry) in &self.per_path {
            if globs.matches(path) {
                let selected = |list: &[String]| {
                    LintSelection::new(list.iter().map(String::as_str), std::iter::empty())
                        .is_enabled(lint)
                };
                if selected(&entry.select) {
                    enabled = true;
                }
                if selected(&entry.ignore) {
                    enabled = false;
                }
            }
        }
        enabled
    }
}

fn check_selectors(selectors: &[String], key: &str) -> Result<(), ConfigError> {
    match selectors
        .iter()
        .find(|s| !LintSelection::is_valid_selector(s))
    {
        Some(bad) => Err(ConfigError(format!("{}: unknown lint {:?}", key, bad))),
        None => Ok(()),
    }
}
