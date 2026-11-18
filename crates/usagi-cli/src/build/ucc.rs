//! Running UCC, the Unreal Engine 2 command-line tool.

use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::thread;

use super::find_ci;

/// Lines of UCC output that are only progress, hidden unless `--verbose`.
const NOISE_PREFIXES: &[&str] = &[
    "Analyzing...",
    "Parsing ",
    "Compiling ",
    "Importing Defaults for ",
    "Weapon exported successfully:",
    "Vehicle exported successfully:",
    "Mutator exported successfully:",
    "Game exported successfully:",
    "... deferring",
];

pub fn is_noise(line: &str) -> bool {
    let line = line.trim();
    line.is_empty() || NOISE_PREFIXES.iter().any(|p| line.starts_with(p))
}

const DUMPINT: &str = "Editor.DumpIntCommandlet";

/// DumpInt treats every word of its command line as a package to export,
/// including its own name, so it always reports this error, and counts it.
fn is_dumpint_self_match(line: &str) -> bool {
    line.trim() == format!("No packages found matching {}!", DUMPINT)
}

/// Whether DumpInt's `output` reports no errors besides its own name.
fn dumpint_succeeded(status_ok: bool, output: &str) -> bool {
    if status_ok {
        return true;
    }
    let errors = output.lines().find_map(|line| {
        let count = line
            .trim()
            .strip_prefix("Failure - ")?
            .split_once(" error(s)")?
            .0;
        count.trim().parse::<usize>().ok()
    });
    errors == Some(1) && output.lines().any(is_dumpint_self_match)
}

/// The ANSI colour for a line of UCC output, if it's an error or warning.
fn line_color(line: &str) -> Option<&'static str> {
    let line = line.trim_start();
    if line.contains(": Error") || line.starts_with("Error") || line.starts_with("Failure") {
        Some("91")
    } else if line.contains(": Warning") || line.starts_with("Warning") {
        Some("93")
    } else {
        None
    }
}

pub struct Ucc {
    /// `UCC.exe` itself on Windows; elsewhere, the program that runs it.
    program: String,
    /// `UCC.exe`'s file name, when it's run by another program.
    exe: Option<String>,
    sys_dir: PathBuf,
    verbose: bool,
    color: bool,
}

impl Ucc {
    pub fn find(sys_dir: &Path, wine: &str, verbose: bool, color: bool) -> Result<Self, String> {
        let exe = find_ci(sys_dir, "UCC.exe").ok_or_else(|| {
            format!(
                "UCC.exe not found in {} (is the SDK installed?)",
                sys_dir.display()
            )
        })?;
        let (program, exe) = if cfg!(windows) {
            (exe.to_string_lossy().into_owned(), None)
        } else {
            let name = exe.file_name().expect("a file name").to_string_lossy();
            (wine.to_string(), Some(name.into_owned()))
        };
        Ok(Ucc {
            program,
            exe,
            sys_dir: sys_dir.to_path_buf(),
            verbose,
            color,
        })
    }

    fn command(&self, args: &[String]) -> Command {
        let mut command = Command::new(&self.program);
        command
            .args(&self.exe)
            .args(args)
            .current_dir(&self.sys_dir)
            .env("WINEDEBUG", "-all")
            .stdin(Stdio::null());
        command
    }

    fn print_line(&self, line: &str) {
        match line_color(line).filter(|_| self.color) {
            Some(code) => println!("\x1b[{}m{}\x1b[0m", code, line),
            None => println!("{}", line),
        }
    }

    /// Runs UCC with `args`, printing its output as it comes.
    pub fn run_streaming(&self, args: &[String]) -> Result<ExitStatus, String> {
        let mut child = self
            .command(args)
            .stdout(Stdio::piped())
            .spawn()
            .map_err(|e| format!("could not run {}: {}", self.program, e))?;
        let mut stdout = BufReader::new(child.stdout.take().expect("piped stdout"));
        let mut buffer = Vec::new();
        loop {
            buffer.clear();
            match stdout.read_until(b'\n', &mut buffer) {
                Ok(0) | Err(_) => break,
                Ok(_) => {
                    // UCC's output is single-byte, like its sources.
                    let line: String = buffer.iter().map(|&b| b as char).collect();
                    let line = line.trim_end_matches(['\r', '\n']);
                    if self.verbose || !is_noise(line) {
                        self.print_line(line);
                    }
                }
            }
        }
        child.wait().map_err(|e| e.to_string())
    }

    /// Exports the localization of `packages` in parallel, and prints how
    /// many succeeded, with the output of any that failed. A package fails
    /// if UCC fails or doesn't write its `.int` file.
    pub fn dumpint(&self, packages: &[&str], mod_name: &str, jobs: usize) -> Result<(), String> {
        if packages.is_empty() {
            return Ok(());
        }
        // A `.int` left by an interrupted build would look like success.
        for package in packages {
            if let Some(path) = find_ci(&self.sys_dir, &format!("{}.int", package)) {
                std::fs::remove_file(&path).map_err(|e| format!("{}: {}", path.display(), e))?;
            }
        }
        // At most `jobs` at once, at a low priority: each is a whole UCC
        // (under Wine, elsewhere).
        let mut results: Vec<(String, Result<std::process::Output, String>)> = Vec::new();
        for chunk in packages.chunks(jobs.max(1)) {
            thread::scope(|s| {
                let handles: Vec<_> = chunk
                    .iter()
                    .map(|&package| {
                        let args = [
                            DUMPINT.to_string(),
                            format!("{}.u", package),
                            format!("-mod={}", mod_name),
                        ];
                        let mut command = self.command(&args);
                        s.spawn(move || {
                            crate::check::lower_priority();
                            let output = command.output().map_err(|e| e.to_string());
                            (package.to_string(), output)
                        })
                    })
                    .collect();
                results.extend(
                    handles
                        .into_iter()
                        .map(|h| h.join().expect("dumpint thread")),
                );
            });
        }
        let mut failed = Vec::new();
        for (package, output) in &results {
            let int_written = find_ci(&self.sys_dir, &format!("{}.int", package)).is_some();
            let text = output.as_ref().ok().map(|o| {
                [&o.stdout, &o.stderr]
                    .iter()
                    .flat_map(|bytes| bytes.iter().map(|&b| b as char))
                    .collect::<String>()
            });
            let ok = match (output, &text) {
                (Ok(o), Some(text)) => int_written && dumpint_succeeded(o.status.success(), text),
                _ => false,
            };
            if !ok {
                failed.push(package.as_str());
            }
            if !ok || self.verbose {
                match (output, &text) {
                    (Ok(_), Some(text)) => {
                        for line in text.lines() {
                            if !is_dumpint_self_match(line) {
                                self.print_line(line.trim_end_matches('\r'));
                            }
                        }
                        if !int_written {
                            println!("{}: no {}.int was written", package, package);
                        }
                    }
                    (Err(e), _) => println!("{}: could not run {}: {}", package, self.program, e),
                    _ => {}
                }
            }
        }
        let succeeded = results.len() - failed.len();
        println!(
            "Exported localization: {} succeeded, {} failed{}",
            succeeded,
            failed.len(),
            if failed.is_empty() {
                String::new()
            } else {
                format!(" ({})", failed.join(", "))
            }
        );
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classifies_lines() {
        assert!(is_noise("Parsing Foo"));
        assert!(is_noise("   "));
        assert!(!is_noise(r"Z:\Foo\Classes\A.uc(3) : Error, Bad expression"));
        assert_eq!(line_color(r"A.uc(3) : Error, Bad"), Some("91"));
        assert_eq!(line_color(r"A.uc(3) : Warning, Unused"), Some("93"));
        assert_eq!(line_color("Success - 0 error(s)"), None);
    }

    #[test]
    fn dumpint_tolerates_matching_its_own_name() {
        let quirk = "No packages found matching Editor.DumpIntCommandlet!\r\n\
                     Loading ../Mod/System/A.u...\r\nFailure - 1 error(s), 0 warning(s)\r\n";
        assert!(dumpint_succeeded(false, quirk));
        assert!(dumpint_succeeded(true, ""));
        let real = "No packages found matching Editor.DumpIntCommandlet!\n\
                    Unable to load package 'A.u'\nFailure - 2 error(s), 0 warning(s)\n";
        assert!(!dumpint_succeeded(false, real));
        assert!(!dumpint_succeeded(
            false,
            "Failure - 1 error(s), 0 warning(s)\n"
        ));
    }
}
