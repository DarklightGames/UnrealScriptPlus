//! Reading UnrealScript source files.
//!
//! UCC loads `.uc` files with `appLoadFileToString` (`Core/Src/UnMisc.cpp`
//! in the UE2 source). A file that starts with a UTF-16 byte order mark and
//! has an even size is read as UTF-16 in that byte order. Any other file is
//! read one byte per character, with byte `n` becoming U+00nn: that is
//! Latin-1 (ISO-8859-1), not Windows-1252, and UTF-8 is never recognized.

use std::ops::Range;

/// How a source file was stored.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceEncoding {
    /// One byte per character, as UCC reads every file without a UTF-16
    /// byte order mark.
    Latin1,
    /// UTF-16, little-endian, with a byte order mark.
    Utf16Le,
    /// UTF-16, big-endian, with a byte order mark.
    Utf16Be,
    /// UTF-8 with non-ASCII characters, or with a byte order mark. UCC
    /// doesn't understand it: it reads each byte as a Latin-1 character.
    Utf8 { bom: bool },
}

/// A decoded source file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decoded {
    pub text: String,
    pub encoding: SourceEncoding,
}

const UTF8_BOM: &[u8] = b"\xEF\xBB\xBF";

/// Decodes the bytes of a source file, the way UCC does except for UTF-8:
/// a file that is valid UTF-8 and has non-ASCII characters (or a UTF-8 byte
/// order mark) is decoded as UTF-8, which is what its author meant. Use
/// [`encoding_problems`] to report that UCC will read it differently.
pub fn decode_source(bytes: &[u8]) -> Decoded {
    let utf16 = |little_endian: bool| {
        let units = bytes[2..].chunks_exact(2).map(|pair| {
            let pair = [pair[0], pair[1]];
            if little_endian {
                u16::from_le_bytes(pair)
            } else {
                u16::from_be_bytes(pair)
            }
        });
        // UCC stores each 16-bit unit as one character, so surrogates are
        // never paired up; an unpaired one becomes U+FFFD here.
        char::decode_utf16(units)
            .map(|c| c.unwrap_or(char::REPLACEMENT_CHARACTER))
            .collect()
    };
    let even = bytes.len() % 2 == 0;
    if even && bytes.starts_with(&[0xFF, 0xFE]) {
        return Decoded {
            text: utf16(true),
            encoding: SourceEncoding::Utf16Le,
        };
    }
    if even && bytes.starts_with(&[0xFE, 0xFF]) {
        return Decoded {
            text: utf16(false),
            encoding: SourceEncoding::Utf16Be,
        };
    }
    let (bom, rest) = match bytes.strip_prefix(UTF8_BOM) {
        Some(rest) => (true, rest),
        None => (false, bytes),
    };
    match std::str::from_utf8(rest) {
        Ok(text) if bom || !text.is_ascii() => Decoded {
            text: text.to_string(),
            encoding: SourceEncoding::Utf8 { bom },
        },
        _ => Decoded {
            text: latin1(bytes),
            encoding: SourceEncoding::Latin1,
        },
    }
}

/// Decodes the bytes of a source file. See [`decode_source`].
pub fn decode(bytes: &[u8]) -> String {
    decode_source(bytes).text
}

fn latin1(bytes: &[u8]) -> String {
    bytes.iter().map(|&b| char::from(b)).collect()
}

/// Problems with how a file is encoded: the parts of a UTF-8 file that UCC
/// will read differently. Each is a byte range in `decoded.text` and a
/// message.
pub fn encoding_problems(decoded: &Decoded) -> Vec<(Range<usize>, String)> {
    let SourceEncoding::Utf8 { bom } = decoded.encoding else {
        return Vec::new();
    };
    let mut problems = Vec::new();
    if bom {
        problems.push((
            0..0,
            "The file starts with a UTF-8 byte order mark, which UCC reads as the characters \"\u{ef}\u{bb}\u{bf}\""
                .to_string(),
        ));
    }
    let text = &decoded.text;
    let mut chars = text.char_indices().peekable();
    while let Some((start, c)) = chars.next() {
        if c.is_ascii() {
            continue;
        }
        let mut end = start + c.len_utf8();
        while let Some(&(i, next)) = chars.peek() {
            if next.is_ascii() {
                break;
            }
            end = i + next.len_utf8();
            chars.next();
        }
        let run = &text[start..end];
        let message = if run.contains(char::REPLACEMENT_CHARACTER) {
            format!(
                "{:?} contains U+FFFD, the Unicode replacement character: the original \
                 character was lost when the file was converted to UTF-8, and UCC reads it as {:?}",
                run,
                latin1(run.as_bytes())
            )
        } else {
            format!(
                "The file is UTF-8, so UCC reads {:?} as {:?}; save it as Windows-1252 or UTF-16",
                run,
                latin1(run.as_bytes())
            )
        };
        problems.push((start..end, message));
    }
    problems
}

/// Converts byte offsets in a source text to line and column numbers.
#[derive(Debug, Clone)]
pub struct LineIndex {
    /// The byte offset at which each line starts.
    starts: Vec<usize>,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let starts = std::iter::once(0)
            .chain(text.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        LineIndex { starts }
    }

    /// The 1-based line number of the byte offset `offset`.
    pub fn line(&self, offset: usize) -> usize {
        self.starts.partition_point(|&start| start <= offset)
    }

    /// The 1-based line and column of the byte offset `offset`. The column
    /// counts characters, not bytes.
    pub fn line_column(&self, text: &str, offset: usize) -> (usize, usize) {
        let line = self.line(offset);
        let start = self.starts[line - 1];
        let column = text
            .get(start..offset)
            .map_or(offset - start, |s| s.chars().count());
        (line, column + 1)
    }

    /// The byte range of the 1-based line `line`, without its line ending.
    pub fn line_range(&self, text: &str, line: usize) -> std::ops::Range<usize> {
        let start = self.starts[line - 1];
        let end = self.starts.get(line).map_or(text.len(), |&next| next - 1);
        let end = if text[start..end].ends_with('\r') {
            end - 1
        } else {
            end
        };
        start..end
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decodes_like_ucc() {
        let decoded = |bytes: &[u8]| decode_source(bytes);
        assert_eq!(
            decoded(b"class A;"),
            Decoded {
                text: "class A;".into(),
                encoding: SourceEncoding::Latin1
            }
        );
        // Every byte is a Latin-1 character, including 0x80-0x9F.
        assert_eq!(decode(b"caf\xE9 \x80\x81"), "caf\u{e9} \u{80}\u{81}");
        assert_eq!(
            decoded("caf\u{e9}".as_bytes()).encoding,
            SourceEncoding::Utf8 { bom: false }
        );
        assert_eq!(
            decoded(b"\xEF\xBB\xBFclass A;"),
            Decoded {
                text: "class A;".into(),
                encoding: SourceEncoding::Utf8 { bom: true }
            }
        );
        // A byte order mark followed by invalid UTF-8 is read byte by byte.
        assert_eq!(
            decoded(b"\xEF\xBB\xBFcaf\xE9"),
            Decoded {
                text: "\u{ef}\u{bb}\u{bf}caf\u{e9}".into(),
                encoding: SourceEncoding::Latin1
            }
        );
        assert_eq!(
            decoded(b"\xFF\xFEA\x00\xE9\x00\x00\xD8"),
            Decoded {
                text: "A\u{e9}\u{fffd}".into(),
                encoding: SourceEncoding::Utf16Le
            }
        );
        assert_eq!(
            decoded(b"\xFE\xFF\x00A\x20\xAC"),
            Decoded {
                text: "A\u{20ac}".into(),
                encoding: SourceEncoding::Utf16Be
            }
        );
        // UCC only reads UTF-16 when the size is even.
        assert_eq!(decoded(b"\xFF\xFEA").encoding, SourceEncoding::Latin1);
        assert_eq!(decoded(b"\xFE\xFFA").encoding, SourceEncoding::Latin1);
    }

    #[test]
    fn reports_utf8() {
        let problems = |bytes: &[u8]| encoding_problems(&decode_source(bytes));
        assert!(problems(b"caf\xE9").is_empty());
        assert!(problems(b"\xFF\xFEA\x00").is_empty());
        let found = problems("// G\u{fc}nther, \u{fffd}\u{e9}!".as_bytes());
        assert_eq!(found.len(), 2);
        assert_eq!(found[0].0, 4..6);
        assert!(found[0].1.contains("reads \"\u{fc}\" as \"\u{c3}\u{bc}\""));
        assert_eq!(found[1].0, 13..18);
        assert!(found[1].1.contains("U+FFFD"));
        let found = problems(b"\xEF\xBB\xBFclass A;");
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].0, 0..0);
    }

    #[test]
    fn line_numbers() {
        let text = "ab\r\nc\u{e9}d\n\nx";
        let index = LineIndex::new(text);
        assert_eq!(index.line(0), 1);
        assert_eq!(index.line(3), 1);
        assert_eq!(index.line(4), 2);
        assert_eq!(index.line(text.len()), 4);
        assert_eq!(index.line_column(text, 1), (1, 2));
        assert_eq!(index.line_column(text, 7), (2, 3));
        assert_eq!(index.line_column(text, 9), (3, 1));
        assert_eq!(&text[index.line_range(text, 1)], "ab");
        assert_eq!(&text[index.line_range(text, 2)], "c\u{e9}d");
        assert_eq!(&text[index.line_range(text, 3)], "");
        assert_eq!(&text[index.line_range(text, 4)], "x");
    }
}
