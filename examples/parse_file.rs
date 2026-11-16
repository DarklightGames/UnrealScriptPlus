//! Parses a single UnrealScript file and prints the result.
//!
//! Usage: cargo run --example parse_file -- [--strict] <file.uc>

use encoding::all::WINDOWS_1252;
use encoding::{DecoderTrap, Encoding};
use unrealscript_parser::{ParseMode, ParseOptions, ParsingError, parse_program_with};

fn main() {
    let strict = std::env::args().any(|a| a == "--strict");
    let path = std::env::args()
        .skip(1)
        .find(|a| a != "--strict")
        .expect("usage: parse_file [--strict] <file.uc>");
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
        Ok(result) => {
            for e in &result.errors {
                println!(
                    "{}:{}: {:?}: {}",
                    path,
                    e.span.line.unwrap_or(0),
                    e.severity,
                    e.message
                );
            }
            println!("ok ({} warnings)", result.errors.len());
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
