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
    #[serde(default)]
    pub lint: LintConfig,
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
}

/// Flags that override the configuration.
#[derive(Default)]
pub struct Overrides {
    pub include: Vec<String>,
    pub exclude: Vec<String>,
    pub strict: Option<bool>,
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
