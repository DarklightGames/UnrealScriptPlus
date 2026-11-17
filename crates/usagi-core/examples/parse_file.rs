//! Parses a single UnrealScript file and prints the result.
//!
//! Usage: cargo run --example parse_file -- [--strict] [--select=S,C001] [--ignore=N] <file.uc>
//!
//! `--select` and `--ignore` take comma-separated lint codes, code prefixes,
//! names or categories (see `LintSelection`). By default every lint is shown.

use encoding::all::WINDOWS_1252;
use encoding::{DecoderTrap, Encoding};
use usagi_core::{
    LintSelection, ParseMode, ParseOptions, ParsingError, parse_program_with,
};

fn main() {
    let strict = std::env::args().any(|a| a == "--strict");
    let list = |flag: &str| -> Option<Vec<String>> {
        std::env::args()
            .find_map(|a| a.strip_prefix(flag).map(|v| v.to_string()))
            .map(|v| v.split(',').map(|s| s.trim().to_string()).collect())
    };
    let mut selection = LintSelection::default();
    if let Some(select) = list("--select=") {
        selection.select = select;
    }
    if let Some(ignore) = list("--ignore=") {
        selection.ignore = ignore;
    }
    for selector in selection.select.iter().chain(&selection.ignore) {
        if !LintSelection::is_valid_selector(selector) {
            eprintln!("unknown lint selector: {}", selector);
            std::process::exit(2);
        }
    }
    let path = std::env::args()
        .skip(1)
        .find(|a| !a.starts_with("--"))
        .expect("usage: parse_file [--strict] [--select=...] [--ignore=...] <file.uc>");
    let mode = if strict {
        ParseMode::Strict
    } else {
        ParseMode::Permissive
    };
    let bytes = std::fs::read(&path).expect("failed to read file");
    let bytes = bytes.strip_prefix(b"\xEF\xBB\xBF").unwrap_or(&bytes);
    let contents = match std::str::from_utf8(bytes) {
        Ok(s) => s.to_string(),
        Err(_) => WINDOWS_1252.decode(bytes, DecoderTrap::Replace).unwrap(),
    };
    match parse_program_with(&contents, &ParseOptions { mode }) {
        Ok(mut result) => {
            selection.retain(&mut result.errors);
            for e in &result.errors {
                let code = e
                    .lint
                    .map(|l| format!(" [{}]", l.code()))
                    .unwrap_or_default();
                println!(
                    "{}:{}: {:?}{}: {}",
                    path,
                    e.span.line.unwrap_or(0),
                    e.severity,
                    code,
                    e.message
                );
            }
            println!("ok ({} problems)", result.errors.len());
        }
        Err(ParsingError::Strict(problems)) => {
            for p in &problems {
                eprintln!("{}:{}: {}", path, p.span.line.unwrap_or(0), p.message);
            }
            std::process::exit(1);
        }
        Err(ParsingError::PestError(e)) => {
            eprintln!("{}", e.with_path(&path));
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("{:?}", e);
            std::process::exit(1);
        }
    }
}
