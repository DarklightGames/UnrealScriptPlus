//! Finding the files to check.

use std::path::{Path, PathBuf};

use walkdir::WalkDir;

use crate::config::Settings;

/// Returns the `.uc` files under `paths`, sorted and without duplicates.
///
/// A file named directly is always included. Files found by searching a
/// directory are filtered by the include and exclude globs. Paths that don't
/// exist are returned as errors.
pub fn files(paths: &[PathBuf], settings: &Settings) -> (Vec<PathBuf>, Vec<String>) {
    let mut files = Vec::new();
    let mut errors = Vec::new();
    for path in paths {
        if path.is_file() {
            files.push(path.clone());
        } else if path.is_dir() {
            for entry in WalkDir::new(path).sort_by_file_name() {
                let entry = match entry {
                    Ok(entry) => entry,
                    Err(e) => {
                        errors.push(e.to_string());
                        continue;
                    }
                };
                if entry.file_type().is_file()
                    && is_source(entry.path())
                    && is_selected(entry.path(), settings)
                {
                    files.push(entry.into_path());
                }
            }
        } else {
            errors.push(format!("{}: no such file or directory", path.display()));
        }
    }
    files.sort();
    files.dedup();
    (files, errors)
}

fn is_source(path: &Path) -> bool {
    path.extension()
        .is_some_and(|e| e.eq_ignore_ascii_case("uc"))
}

fn is_selected(path: &Path, settings: &Settings) -> bool {
    let absolute = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
    (settings.include.is_empty() || settings.include.matches(&absolute))
        && !settings.exclude.matches(&absolute)
}

/// The path to show for `path`: relative to `cwd` if it's inside it.
pub fn display(path: &Path, cwd: &Path) -> String {
    let absolute = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
    let shown = absolute.strip_prefix(cwd).unwrap_or(path);
    shown.to_string_lossy().replace('\\', "/")
}
