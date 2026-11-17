//! Reading UnrealScript source files.

use encoding::all::WINDOWS_1252;
use encoding::{DecoderTrap, Encoding};

/// Decodes the bytes of a source file.
///
/// A UTF-8 byte order mark is stripped. Valid UTF-8 is used as it is;
/// anything else is decoded as Windows-1252, the encoding most UE2 source
/// uses.
pub fn decode(bytes: &[u8]) -> String {
    let bytes = bytes.strip_prefix(b"\xEF\xBB\xBF").unwrap_or(bytes);
    match std::str::from_utf8(bytes) {
        Ok(s) => s.to_string(),
        Err(_) => WINDOWS_1252
            .decode(bytes, DecoderTrap::Replace)
            .expect("DecoderTrap::Replace never fails"),
    }
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
    fn decodes_utf8_and_windows_1252() {
        assert_eq!(decode(b"\xEF\xBB\xBFclass A;"), "class A;");
        assert_eq!(decode("caf\u{e9}".as_bytes()), "caf\u{e9}");
        assert_eq!(decode(b"caf\xE9 \x80"), "caf\u{e9} \u{20ac}");
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
