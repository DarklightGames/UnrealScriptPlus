//! Unreal Engine 2 configuration (`.ini`) files.
//!
//! These differ from ordinary INI files: a key may appear many times (an
//! array), and a mod's `Default.ini` changes the game's arrays with
//! `+Key=Value` (add if missing) and `-Key=Value` (remove) lines. Section and
//! key names are case-insensitive. Files are single-byte (Windows-1252); they
//! are read and written as Latin-1, which round-trips every byte.

use std::fs;
use std::io;
use std::path::Path;

/// What a line does to its key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    /// `Key=Value`
    Set,
    /// `+Key=Value`
    Add,
    /// `-Key=Value`
    Remove,
}

/// A `Key=Value` line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
    pub op: Op,
    pub key: String,
    pub value: String,
}

/// An INI file, kept line by line so it can be rewritten without disturbing
/// anything but the keys that change.
#[derive(Debug, Clone)]
pub struct Ini {
    lines: Vec<String>,
    newline: &'static str,
    trailing_newline: bool,
}

impl Ini {
    pub fn read(path: &Path) -> io::Result<Self> {
        Ok(Self::from_bytes(&fs::read(path)?))
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        let text: String = bytes.iter().map(|&b| b as char).collect();
        let newline = if text.contains("\r\n") { "\r\n" } else { "\n" };
        let trailing_newline = text.ends_with('\n');
        let mut lines: Vec<String> = text
            .split('\n')
            .map(|l| l.strip_suffix('\r').unwrap_or(l).to_string())
            .collect();
        if trailing_newline {
            lines.pop();
        }
        Ini {
            lines,
            newline,
            trailing_newline,
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut text = self.lines.join(self.newline);
        if self.trailing_newline {
            text.push_str(self.newline);
        }
        // Every char came from a single byte, or from `set_list`'s values,
        // which came from other INI files.
        text.chars().map(|c| c as u32 as u8).collect()
    }

    pub fn write(&self, path: &Path) -> io::Result<()> {
        fs::write(path, self.to_bytes())
    }

    /// The name of the section that `line` opens, if it opens one.
    fn section_name(line: &str) -> Option<&str> {
        let line = line.trim();
        line.strip_prefix('[')?.strip_suffix(']')
    }

    fn entry(line: &str) -> Option<Entry> {
        let line = line.trim_start();
        if line.starts_with(';') || line.starts_with('#') {
            return None;
        }
        let (key, value) = line.split_once('=')?;
        let (op, key) = if let Some(key) = key.strip_prefix('+') {
            (Op::Add, key)
        } else if let Some(key) = key.strip_prefix('-') {
            (Op::Remove, key)
        } else {
            (Op::Set, key)
        };
        let key = key.trim();
        if key.is_empty() {
            return None;
        }
        Some(Entry {
            op,
            key: key.to_string(),
            value: value.trim().to_string(),
        })
    }

    /// The line ranges of every section called `section` (a name may appear
    /// more than once), excluding the header lines.
    fn section_ranges(&self, section: &str) -> Vec<std::ops::Range<usize>> {
        let mut ranges = Vec::new();
        let mut start = None;
        for (i, line) in self.lines.iter().enumerate() {
            if let Some(name) = Self::section_name(line) {
                if let Some(s) = start.take() {
                    ranges.push(s..i);
                }
                if name.eq_ignore_ascii_case(section) {
                    start = Some(i + 1);
                }
            }
        }
        if let Some(s) = start {
            ranges.push(s..self.lines.len());
        }
        ranges
    }

    /// Every entry in `section`, in order.
    pub fn entries(&self, section: &str) -> Vec<Entry> {
        self.section_ranges(section)
            .into_iter()
            .flat_map(|r| self.lines[r].iter().filter_map(|l| Self::entry(l)))
            .collect()
    }

    /// The names of every section, in order, without repeats.
    pub fn sections(&self) -> Vec<String> {
        let mut names: Vec<String> = Vec::new();
        for line in &self.lines {
            if let Some(name) = Self::section_name(line)
                && !names.iter().any(|n| n.eq_ignore_ascii_case(name))
            {
                names.push(name.to_string());
            }
        }
        names
    }

    /// The values of `key` in `section` that have the operation `op`.
    pub fn values(&self, section: &str, key: &str, op: Op) -> Vec<String> {
        self.entries(section)
            .into_iter()
            .filter(|e| e.op == op && e.key.eq_ignore_ascii_case(key))
            .map(|e| e.value)
            .collect()
    }

    /// Replaces every `key` line in `section` with one line per value. The
    /// new lines go where the first old one was, or at the end of the
    /// section; the section is added if it's missing.
    pub fn set_list(&mut self, section: &str, key: &str, values: &[String]) {
        let ranges = self.section_ranges(section);
        let new_lines = values.iter().map(|v| format!("{}={}", key, v));
        let Some(first_range) = ranges.first().cloned() else {
            if self.lines.last().is_some_and(|l| !l.trim().is_empty()) {
                self.lines.push(String::new());
            }
            self.lines.push(format!("[{}]", section));
            self.lines.extend(new_lines);
            return;
        };
        // Remove the old lines, last first so indices stay valid.
        let mut insert_at = None;
        for range in ranges.iter().rev() {
            for i in range.clone().rev() {
                let is_key = Self::entry(&self.lines[i])
                    .is_some_and(|e| e.op == Op::Set && e.key.eq_ignore_ascii_case(key));
                if is_key {
                    self.lines.remove(i);
                    insert_at = Some(i);
                }
            }
        }
        let insert_at = insert_at.unwrap_or_else(|| {
            // After the section's last non-blank line.
            let mut end = first_range.end;
            while end > first_range.start && self.lines[end - 1].trim().is_empty() {
                end -= 1;
            }
            end
        });
        self.lines.splice(insert_at..insert_at, new_lines);
    }
}

/// Applies `+`/`-` entries to `base`, the way UCC does when it makes a mod's
/// configuration from the game's `Default.ini` and the mod's.
pub fn apply(base: &[String], entries: &[Entry]) -> Vec<String> {
    let mut list = base.to_vec();
    for entry in entries {
        match entry.op {
            Op::Add => {
                if !list.iter().any(|v| v.eq_ignore_ascii_case(&entry.value)) {
                    list.push(entry.value.clone());
                }
            }
            Op::Remove => list.retain(|v| !v.eq_ignore_ascii_case(&entry.value)),
            Op::Set => {}
        }
    }
    list
}

/// Updates `target` (a mod's generated configuration) with the arrays that
/// the mod's `Default.ini` (`mod_default`) changes, starting from the game's
/// `Default.ini` (`base`). Every other setting in `target`, and every array
/// that is already right, is left as it is. Returns the `(section, key)`s that
/// were updated.
pub fn sync_arrays(base: &Ini, mod_default: &Ini, target: &mut Ini) -> Vec<(String, String)> {
    let mut updated = Vec::new();
    for section in mod_default.sections() {
        let entries = mod_default.entries(&section);
        let mut keys: Vec<&str> = Vec::new();
        for entry in &entries {
            if entry.op != Op::Set && !keys.iter().any(|k| k.eq_ignore_ascii_case(&entry.key)) {
                keys.push(&entry.key);
            }
        }
        for key in keys {
            let ops: Vec<Entry> = entries
                .iter()
                .filter(|e| e.key.eq_ignore_ascii_case(key))
                .cloned()
                .collect();
            let list = apply(&base.values(&section, key, Op::Set), &ops);
            if target.values(&section, key, Op::Set) != list {
                target.set_list(&section, key, &list);
                updated.push((section.clone(), key.to_string()));
            }
        }
    }
    updated
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ini(text: &str) -> Ini {
        Ini::from_bytes(text.as_bytes())
    }

    fn strings(values: &[&str]) -> Vec<String> {
        values.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn reads_repeated_keys_and_ops() {
        let file = ini("[A]\r\n+Paths=x\r\n-Paths=y\r\nPaths=z\r\n; Paths=c\r\n[b]\r\nK = v \r\n");
        assert_eq!(file.values("a", "paths", Op::Add), strings(&["x"]));
        assert_eq!(file.values("A", "Paths", Op::Remove), strings(&["y"]));
        assert_eq!(file.values("A", "Paths", Op::Set), strings(&["z"]));
        assert_eq!(file.values("B", "k", Op::Set), strings(&["v"]));
        assert_eq!(file.sections(), strings(&["A", "b"]));
    }

    #[test]
    fn repeated_sections_are_merged() {
        let file = ini("[A]\nK=1\n[B]\nK=2\n[A]\nK=3\n");
        assert_eq!(file.values("A", "K", Op::Set), strings(&["1", "3"]));
    }

    #[test]
    fn round_trips_bytes() {
        let bytes = b"[A]\r\nName=Caf\xe9\r\n\r\nX=1".to_vec();
        assert_eq!(Ini::from_bytes(&bytes).to_bytes(), bytes);
    }

    #[test]
    fn apply_adds_and_removes_case_insensitively() {
        let entries = ini("[S]\n+K=b\n+K=A\n-K=c\n+K=d\n").entries("S");
        assert_eq!(
            apply(&strings(&["a", "C"]), &entries),
            strings(&["a", "b", "d"])
        );
    }

    #[test]
    fn set_list_replaces_in_place() {
        let mut file = ini("[A]\nX=1\nK=old\nY=2\nK=old2\n\n[B]\n");
        file.set_list("A", "K", &strings(&["n1", "n2"]));
        assert_eq!(
            String::from_utf8(file.to_bytes()).unwrap(),
            "[A]\nX=1\nK=n1\nK=n2\nY=2\n\n[B]\n"
        );
        file.set_list("B", "K", &strings(&["z"]));
        file.set_list("C", "K", &strings(&["c"]));
        assert_eq!(
            String::from_utf8(file.to_bytes()).unwrap(),
            "[A]\nX=1\nK=n1\nK=n2\nY=2\n\n[B]\nK=z\n\n[C]\nK=c\n"
        );
    }

    #[test]
    fn sync_keeps_other_settings() {
        let base = ini("[Editor.EditorEngine]\nEditPackages=Core\nEditPackages=Engine\n");
        let mod_default =
            ini("[Editor.EditorEngine]\n+EditPackages=Mine\n-EditPackages=Engine\nSetting=1\n");
        let mut target =
            ini("[Editor.EditorEngine]\nGridSize=4\nEditPackages=Core\nEditPackages=Old\n");
        let updated = sync_arrays(&base, &mod_default, &mut target);
        assert_eq!(
            updated,
            vec![(
                "Editor.EditorEngine".to_string(),
                "EditPackages".to_string()
            )]
        );
        assert_eq!(
            String::from_utf8(target.to_bytes()).unwrap(),
            "[Editor.EditorEngine]\nGridSize=4\nEditPackages=Core\nEditPackages=Mine\n"
        );
    }
}
