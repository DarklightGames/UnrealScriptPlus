//! The record of which packages are up to date, a TOML file.

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use serde::{Deserialize, Serialize};

#[derive(Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Manifest {
    /// The CRC of the mod's `Default.ini` at the last build. When it
    /// changes, everything is rebuilt.
    pub default_ini_crc: Option<u32>,
    /// The CRC of each package's sources when it last compiled.
    #[serde(default)]
    pub packages: BTreeMap<String, u32>,
}

impl Manifest {
    /// Loads the manifest. A missing or unreadable one is empty, which
    /// rebuilds everything.
    pub fn load(path: &Path) -> Self {
        fs::read_to_string(path)
            .ok()
            .and_then(|text| toml::from_str(&text).ok())
            .unwrap_or_default()
    }

    pub fn write(&self, path: &Path) -> Result<(), String> {
        let text = toml::to_string(self).expect("the manifest serializes");
        let text = format!(
            "# Written by `usagi build`: the packages that are up to date. Don't edit it.\n\n{}",
            text
        );
        fs::write(path, text).map_err(|e| format!("{}: {}", path.display(), e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_and_tolerates_bad_files() {
        let dir = tempfile::TempDir::new().unwrap();
        let path = dir.path().join(".usagi-build.toml");
        assert_eq!(Manifest::load(&path), Manifest::default());
        let mut manifest = Manifest {
            default_ini_crc: Some(7),
            ..Manifest::default()
        };
        manifest.packages.insert("Pkg".to_string(), 42);
        manifest.write(&path).unwrap();
        assert_eq!(Manifest::load(&path), manifest);
        let text = fs::read_to_string(&path).unwrap();
        assert!(text.contains("default_ini_crc = 7\n"), "{}", text);
        assert!(text.contains("[packages]\nPkg = 42\n"), "{}", text);
        fs::write(&path, "{\"not\": \"toml\"}").unwrap();
        assert_eq!(Manifest::load(&path), Manifest::default());
    }
}
