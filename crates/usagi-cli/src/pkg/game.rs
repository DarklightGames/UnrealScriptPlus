//! Finding the game a package belongs to, and where the game looks for
//! packages: its `Paths` setting.

use std::path::{Path, PathBuf};

use usagi_pkg::SearchPath;

use crate::build::find_ci;
use crate::build::ini::{self, Ini, Op};

/// Where the game that `file` is in looks for packages: the `Paths` of its
/// `System/Default.ini`, relative to its `System` folder. For a package in
/// a mod (a folder with its own `System/Default.ini`, inside the game's),
/// the mod's `Default.ini` changes the game's with `+Paths` and `-Paths`, as
/// UCC does when it makes the mod's configuration. Empty if `file` isn't in
/// a game's folder.
pub fn search_paths(file: &Path) -> Vec<SearchPath> {
    let Ok(file) = std::path::absolute(file) else {
        return Vec::new();
    };
    let Some((root, own_ini)) = file.ancestors().skip(1).find_map(default_ini) else {
        return Vec::new();
    };
    let Ok(own) = Ini::read(&own_ini) else {
        return Vec::new();
    };
    // A mod: the game's `Default.ini` is the base.
    let (game_root, paths) = match root.parent().and_then(default_ini) {
        Some((game_root, game_ini)) => {
            let Ok(game) = Ini::read(&game_ini) else {
                return Vec::new();
            };
            let entries: Vec<_> = own
                .entries("Core.System")
                .into_iter()
                .filter(|e| e.key.eq_ignore_ascii_case("Paths"))
                .collect();
            let base = game.values("Core.System", "Paths", Op::Set);
            (game_root, ini::apply(&base, &entries))
        }
        None => (root, own.values("Core.System", "Paths", Op::Set)),
    };
    let Some(system) = find_ci(&game_root, "System") else {
        return Vec::new();
    };
    paths
        .iter()
        .map(|pattern| SearchPath::from_pattern(&system, pattern))
        .collect()
}

/// `dir` and its `System/Default.ini`, if it has one.
fn default_ini(dir: &Path) -> Option<(PathBuf, PathBuf)> {
    let ini = find_ci(&find_ci(dir, "System")?, "Default.ini")?;
    ini.is_file().then(|| (dir.to_path_buf(), ini))
}
