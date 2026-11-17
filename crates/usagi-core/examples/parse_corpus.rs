//! Parses every `*.uc` file under a directory and writes a TSV report.
//!
//! Usage: cargo run --release --example parse_corpus -- [--strict] [--lints <lints.tsv>]
//!            [--include <pattern>]... <root> [report.tsv]
//!
//! `--include` limits the run to files under `root` whose path matches one of
//! the patterns. It can be repeated, and takes comma-separated patterns. See
//! [`Includes`] for the pattern syntax. Examples:
//!
//!     --include DH_Engine,DH_Guns       # these packages
//!     --include 'DH_*' --include ROGame # every DH_ package, and ROGame
//!     --include '*/Classes/*Pawn*.uc'   # matching files in any package
//!
//! Columns: status, path, line, col, ms, message
//! Status is one of:
//! - ok: parsed with nothing to recover from;
//! - recovered: parsed, but permissive parsing recovered from problems (e.g.
//!   invalid defaultproperties lines); the message lists them. With
//!   `--strict` these are reported as `strict_error` instead;
//! - parse_error, panic;
//! - slow: still running after the watchdog limit (the run continues).
//!
//! With `--lints <lints.tsv>`, files are also parsed permissively and every
//! lint problem is written to `lints.tsv` (columns: code, name, path, line,
//! message), and a count of problems and files per lint is printed.

use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::panic;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use encoding::all::WINDOWS_1252;
use encoding::{DecoderTrap, Encoding};
use pest::error::LineColLocation;
use usagi_core::{
    Lint, ParseMode, ParseOptions, ParsingError, parse_program, parse_program_with,
};

/// A lint problem: (lint, line, message).
type LintHit = (Lint, usize, String);

const SLOW_LIMIT: Duration = Duration::from_secs(10);

const USAGE: &str = "usage: parse_corpus [--strict] [--lints <lints.tsv>] \
                     [--include <pattern>]... <root> [report.tsv]";

/// Path patterns that choose which files to parse.
///
/// A pattern is matched against a file's path relative to the root, with `/`
/// as the separator (`\` in a pattern is read as `/`), ignoring case:
/// - `*` matches any run of characters except `/`;
/// - `?` matches any one character except `/`;
/// - `**` matches any run of characters, including `/`.
///
/// A pattern that matches a folder includes everything inside it, so
/// `DH_Engine` and `DH_*` select whole folders. With no patterns, every file
/// is included.
struct Includes {
    /// Normalized (lowercase) patterns, for matching.
    patterns: Vec<String>,
    /// The patterns as given, for messages.
    given: Vec<String>,
}

impl Includes {
    fn new(patterns: Vec<String>) -> Self {
        let given: Vec<String> = patterns
            .into_iter()
            .map(|p| p.trim().to_string())
            .filter(|p| {
                !p.trim_start_matches("./")
                    .trim_matches(['/', '\\'])
                    .is_empty()
            })
            .collect();
        let patterns = given
            .iter()
            .map(|p| {
                let p = p.replace('\\', "/");
                let p = p.trim_start_matches("./").trim_matches('/');
                p.to_ascii_lowercase()
            })
            .collect();
        Includes { patterns, given }
    }

    /// The index of the first pattern that matches `relative_path`, or
    /// `Some(0)` if there are no patterns.
    fn matching_pattern(&self, relative_path: &Path) -> Option<usize> {
        if self.patterns.is_empty() {
            return Some(0);
        }
        let components: Vec<String> = relative_path
            .components()
            .map(|c| c.as_os_str().to_string_lossy().to_ascii_lowercase())
            .collect();
        // The file itself and each folder it's in.
        let candidates: Vec<String> = (1..=components.len())
            .map(|n| components[..n].join("/"))
            .collect();
        self.patterns.iter().position(|pattern| {
            candidates
                .iter()
                .any(|c| glob_match(pattern.as_bytes(), c.as_bytes()))
        })
    }
}

/// Matches `text` against a glob `pattern` (see [`Includes`]). Both must
/// already be lowercase.
fn glob_match(pattern: &[u8], text: &[u8]) -> bool {
    match pattern {
        [] => text.is_empty(),
        [b'*', b'*', rest @ ..] => {
            // `**/` also matches no folders at all.
            if let [b'/', after @ ..] = rest {
                if glob_match(after, text) {
                    return true;
                }
            }
            (0..=text.len()).any(|i| glob_match(rest, &text[i..]))
        }
        [b'*', rest @ ..] => {
            let end = text.iter().position(|&c| c == b'/').unwrap_or(text.len());
            (0..=end).any(|i| glob_match(rest, &text[i..]))
        }
        [b'?', rest @ ..] => matches!(text, [c, tail @ ..] if *c != b'/' && glob_match(rest, tail)),
        [p, rest @ ..] => matches!(text, [c, tail @ ..] if c == p && glob_match(rest, tail)),
    }
}

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let Ok(ft) = entry.file_type() else { continue };
        if ft.is_dir() {
            collect(&path, out);
        } else if ft.is_file()
            && path
                .extension()
                .is_some_and(|e| e.eq_ignore_ascii_case("uc"))
        {
            out.push(path);
        }
    }
}

fn decode(bytes: &[u8]) -> String {
    // Strip a UTF-8 BOM and accept valid UTF-8; otherwise the project mandates Windows-1252.
    let bytes = bytes.strip_prefix(b"\xEF\xBB\xBF").unwrap_or(bytes);
    match std::str::from_utf8(bytes) {
        Ok(s) => s.to_string(),
        Err(_) => WINDOWS_1252.decode(bytes, DecoderTrap::Replace).unwrap(),
    }
}

fn one_line(s: &str) -> String {
    s.replace(['\t', '\r', '\n'], " ")
}

/// Returns the lint problems found by permissive parsing.
fn lint_file(contents: &str) -> Vec<LintHit> {
    match panic::catch_unwind(|| parse_program(contents)) {
        Ok(Ok(result)) => result
            .errors
            .into_iter()
            .filter_map(|e| Some((e.lint?, e.span.line.unwrap_or(0), one_line(&e.message))))
            .collect(),
        _ => Vec::new(),
    }
}

/// Returns (status, line, col, message).
fn parse_file(
    path: &Path,
    strict: bool,
    lints: bool,
) -> ((&'static str, usize, usize, String), Vec<LintHit>) {
    let contents = match fs::read(path) {
        Ok(b) => decode(&b),
        Err(e) => return (("io_error", 0, 0, e.to_string()), Vec::new()),
    };
    let hits = if lints {
        lint_file(&contents)
    } else {
        Vec::new()
    };
    (parse_contents(&contents, strict), hits)
}

fn parse_contents(contents: &str, strict: bool) -> (&'static str, usize, usize, String) {
    // Always parse strictly: a `Strict` error lists exactly the problems that
    // permissive parsing recovers from.
    let options = ParseOptions {
        mode: ParseMode::Strict,
    };
    match panic::catch_unwind(|| parse_program_with(contents, &options).map(|r| r.errors.len())) {
        Ok(Ok(_warnings)) => ("ok", 0, 0, String::new()),
        Ok(Err(ParsingError::Strict(problems))) => {
            let line = problems.first().and_then(|p| p.span.line).unwrap_or(0);
            let messages: Vec<_> = problems
                .iter()
                .map(|p| format!("L{}: {}", p.span.line.unwrap_or(0), p.message))
                .collect();
            let status = if strict { "strict_error" } else { "recovered" };
            (status, line, 0, one_line(&messages.join(" | ")))
        }
        Ok(Err(ParsingError::PestError(e))) => {
            let (line, col) = match e.line_col {
                LineColLocation::Pos(p) => p,
                LineColLocation::Span(p, _) => p,
            };
            ("parse_error", line, col, one_line(&e.variant.message()))
        }
        Ok(Err(other)) => ("parse_error", 0, 0, one_line(&format!("{:?}", other))),
        Err(payload) => {
            let msg = payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(|s| s.to_string()))
                .unwrap_or_else(|| "<non-string panic>".into());
            ("panic", 0, 0, one_line(&msg))
        }
    }
}

fn main() {
    let mut strict = false;
    let mut lints_path = None;
    let mut patterns = Vec::new();
    let mut positional = Vec::new();
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        let mut value = |flag: &str| {
            args.next().unwrap_or_else(|| {
                eprintln!("{} needs a value\n{}", flag, USAGE);
                std::process::exit(2)
            })
        };
        match arg.as_str() {
            "--strict" => strict = true,
            "--lints" => lints_path = Some(value("--lints")),
            "--include" => patterns.extend(value("--include").split(',').map(String::from)),
            flag if flag.starts_with("--") => {
                eprintln!("unknown option {}\n{}", flag, USAGE);
                std::process::exit(2)
            }
            _ => positional.push(arg),
        }
    }
    let lints = lints_path.is_some();
    let root = PathBuf::from(positional.first().unwrap_or_else(|| {
        eprintln!("{}", USAGE);
        std::process::exit(2)
    }));
    let report_path = positional
        .get(1)
        .cloned()
        .unwrap_or_else(|| "corpus_report.tsv".into());

    let includes = Includes::new(patterns);
    let mut all_files = Vec::new();
    collect(&root, &mut all_files);
    let mut pattern_used = vec![false; includes.patterns.len()];
    let mut files: Vec<PathBuf> = all_files
        .into_iter()
        .filter(|f| {
            let relative = f.strip_prefix(&root).unwrap_or(f);
            match includes.matching_pattern(relative) {
                Some(i) => {
                    if let Some(used) = pattern_used.get_mut(i) {
                        *used = true;
                    }
                    true
                }
                None => false,
            }
        })
        .collect();
    for (pattern, used) in includes.given.iter().zip(&pattern_used) {
        if !used {
            eprintln!("warning: --include {:?} matched no files", pattern);
        }
    }
    if files.is_empty() {
        eprintln!("no .uc files to parse under {}", root.display());
        std::process::exit(1);
    }
    files.sort();
    let files = Arc::new(files);
    let total = files.len();

    panic::set_hook(Box::new(|_| {})); // panics are reported in the TSV instead

    let next = Arc::new(AtomicUsize::new(0));
    let results = Arc::new(Mutex::new(Vec::with_capacity(total)));
    let in_flight: Arc<Mutex<HashMap<usize, Instant>>> = Arc::default();
    let workers = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4);

    let handles: Vec<_> = (0..workers)
        .map(|_| {
            let (files, next, results, in_flight) = (
                files.clone(),
                next.clone(),
                results.clone(),
                in_flight.clone(),
            );
            thread::Builder::new()
                .stack_size(256 * 1024 * 1024)
                .spawn(move || {
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        if i >= files.len() {
                            break;
                        }
                        in_flight.lock().unwrap().insert(i, Instant::now());
                        let start = Instant::now();
                        let (r, hits) = parse_file(&files[i], strict, lints);
                        let ms = start.elapsed().as_millis();
                        in_flight.lock().unwrap().remove(&i);
                        results.lock().unwrap().push((i, r, ms, hits));
                    }
                })
                .unwrap()
        })
        .collect();

    // Watchdog: report files that exceed the limit, but stop waiting on them once
    // every other file is done so a pathological input can't stall the whole run.
    let mut reported_slow = std::collections::HashSet::new();
    loop {
        thread::sleep(Duration::from_millis(200));
        let done = results.lock().unwrap().len();
        let flight = in_flight.lock().unwrap().clone();
        for (&i, &t) in &flight {
            if t.elapsed() > SLOW_LIMIT && reported_slow.insert(i) {
                eprintln!("slow: {}", files[i].display());
            }
        }
        eprint!("\r{}/{}", done, total);
        if done == total
            || (done + flight.len() == total && flight.keys().all(|i| reported_slow.contains(i)))
        {
            break;
        }
    }
    eprintln!();

    let mut results = std::mem::take(&mut *results.lock().unwrap());
    for &i in &reported_slow {
        if !results.iter().any(|(j, ..)| *j == i) {
            results.push((
                i,
                ("slow", 0, 0, format!(">{}s", SLOW_LIMIT.as_secs())),
                0,
                Vec::new(),
            ));
        }
    }
    results.sort_by_key(|(i, ..)| *i);

    let mut out = fs::File::create(&report_path).unwrap();
    let mut counts: HashMap<&str, usize> = HashMap::new();
    for (i, (status, line, col, msg), ms, _) in &results {
        *counts.entry(status).or_default() += 1;
        let rel = files[*i].strip_prefix(&root).unwrap_or(&files[*i]);
        writeln!(
            out,
            "{}\t{}\t{}\t{}\t{}\t{}",
            status,
            rel.display(),
            line,
            col,
            ms,
            msg
        )
        .unwrap();
    }
    let mut counts: Vec<_> = counts.into_iter().collect();
    counts.sort();
    println!("{} files -> {}", total, report_path);
    for (status, n) in counts {
        println!("  {:12} {}", status, n);
    }

    if let Some(lints_path) = lints_path {
        let mut out = fs::File::create(&lints_path).unwrap();
        // lint -> (problems, files)
        let mut tally: HashMap<Lint, (usize, usize)> = HashMap::new();
        for (i, _, _, hits) in &results {
            let rel = files[*i].strip_prefix(&root).unwrap_or(&files[*i]);
            let mut in_file = std::collections::HashSet::new();
            for (lint, line, message) in hits {
                writeln!(
                    out,
                    "{}\t{}\t{}\t{}\t{}",
                    lint.code(),
                    lint.name(),
                    rel.display(),
                    line,
                    message
                )
                .unwrap();
                let entry = tally.entry(*lint).or_default();
                entry.0 += 1;
                if in_file.insert(*lint) {
                    entry.1 += 1;
                }
            }
        }
        println!("lints -> {}", lints_path);
        for &lint in Lint::ALL {
            let (problems, files) = tally.get(&lint).copied().unwrap_or_default();
            println!(
                "  {} {:40} {:>8} problems in {:>5} files",
                lint.code(),
                lint.name(),
                problems,
                files
            );
        }
    }

    if reported_slow.iter().any(|i| {
        !handles.is_empty()
            && results
                .iter()
                .any(|(j, (s, ..), ..)| j == i && *s == "slow")
    }) {
        // Hung workers would block a normal exit.
        std::process::exit(0);
    }
    for h in handles {
        h.join().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn includes(patterns: &[&str], path: &str) -> bool {
        Includes::new(patterns.iter().map(|p| p.to_string()).collect())
            .matching_pattern(Path::new(path))
            .is_some()
    }

    #[test]
    fn folders_include_their_contents() {
        assert!(includes(&["DH_Engine"], "DH_Engine/Classes/DHPawn.uc"));
        assert!(includes(&["dh_engine/"], "DH_Engine/Classes/DHPawn.uc"));
        assert!(includes(
            &["./DH_Engine/Classes"],
            "DH_Engine/Classes/DHPawn.uc"
        ));
        assert!(includes(
            &["DH_Engine\\Classes"],
            "DH_Engine/Classes/DHPawn.uc"
        ));
        assert!(!includes(&["DH_Engine"], "DH_EngineExtra/Classes/A.uc"));
        assert!(!includes(&["Classes"], "DH_Engine/Classes/DHPawn.uc"));
    }

    #[test]
    fn wildcards() {
        assert!(includes(&["DH_*"], "DH_Guns/Classes/A.uc"));
        assert!(!includes(&["DH_*"], "ROEngine/Classes/A.uc"));
        assert!(includes(
            &["*/Classes/*Pawn*.uc"],
            "ROEngine/Classes/ROPawn.uc"
        ));
        assert!(!includes(
            &["*/Classes/*Pawn*.uc"],
            "ROEngine/Classes/ROPlayer.uc"
        ));
        // `*` and `?` don't cross folders; `**` does.
        assert!(!includes(&["*Pawn.uc"], "ROEngine/Classes/ROPawn.uc"));
        assert!(includes(&["**Pawn.uc"], "ROEngine/Classes/ROPawn.uc"));
        assert!(includes(&["**/ROPawn.uc"], "ROEngine/Classes/ROPawn.uc"));
        assert!(includes(&["**/ROPawn.uc"], "ROPawn.uc"));
        assert!(includes(&["RO?ngine"], "ROEngine/Classes/A.uc"));
        assert!(!includes(&["R?"], "ROEngine/Classes/A.uc"));
    }

    #[test]
    fn any_pattern_and_no_patterns() {
        assert!(includes(&["ROGame", "DH_*"], "DH_Guns/Classes/A.uc"));
        assert!(includes(&[], "Anything/A.uc"));
        assert!(includes(&[" ", ""], "Anything/A.uc"));
    }
}
