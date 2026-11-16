//! Parses a single UnrealScript file and prints the result.
//!
//! Usage: cargo run --example parse_file -- <file.uc>

use encoding::all::WINDOWS_1252;
use encoding::{DecoderTrap, Encoding};
use unrealscript_parser::{ParsingError, parse_program};

fn main() {
    let path = std::env::args()
        .nth(1)
        .expect("usage: parse_file <file.uc>");
    let bytes = std::fs::read(&path).expect("failed to read file");
    let bytes = bytes.strip_prefix(b"\xEF\xBB\xBF").unwrap_or(&bytes);
    let contents = match std::str::from_utf8(bytes) {
        Ok(s) => s.to_string(),
        Err(_) => WINDOWS_1252.decode(bytes, DecoderTrap::Replace).unwrap(),
    };
    match parse_program(&contents) {
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
