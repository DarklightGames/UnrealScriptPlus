//! Parses every `*.uc` file under a directory and writes a TSV report.
//!
//! Usage: cargo run --release --example parse_corpus -- <root> [report.tsv]
//!
//! Columns: status, path, line, col, ms, message
//! Status is one of: ok, parse_error, panic, slow (slow = still running after
//! the watchdog limit; the file is reported and the run continues).

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
use unrealscript_parser::{ParsingError, parse_program};

const SLOW_LIMIT: Duration = Duration::from_secs(10);

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

/// Returns (status, line, col, message).
fn parse_file(path: &Path) -> (&'static str, usize, usize, String) {
    let contents = match fs::read(path) {
        Ok(b) => decode(&b),
        Err(e) => return ("io_error", 0, 0, e.to_string()),
    };
    match panic::catch_unwind(|| parse_program(&contents).map(|r| r.errors.len())) {
        Ok(Ok(_warnings)) => ("ok", 0, 0, String::new()),
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
    let args: Vec<String> = std::env::args().collect();
    let root = PathBuf::from(
        args.get(1)
            .expect("usage: parse_corpus <root> [report.tsv]"),
    );
    let report_path = args
        .get(2)
        .cloned()
        .unwrap_or_else(|| "corpus_report.tsv".into());

    let mut files = Vec::new();
    collect(&root, &mut files);
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
                        let r = parse_file(&files[i]);
                        let ms = start.elapsed().as_millis();
                        in_flight.lock().unwrap().remove(&i);
                        results.lock().unwrap().push((i, r, ms));
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
        if !results.iter().any(|(j, _, _)| *j == i) {
            results.push((i, ("slow", 0, 0, format!(">{}s", SLOW_LIMIT.as_secs())), 0));
        }
    }
    results.sort_by_key(|(i, _, _)| *i);

    let mut out = fs::File::create(&report_path).unwrap();
    let mut counts: HashMap<&str, usize> = HashMap::new();
    for (i, (status, line, col, msg), ms) in &results {
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

    if reported_slow.iter().any(|i| {
        !handles.is_empty() && results.iter().any(|(j, (s, ..), _)| j == i && *s == "slow")
    }) {
        // Hung workers would block a normal exit.
        std::process::exit(0);
    }
    for h in handles {
        h.join().unwrap();
    }
}
