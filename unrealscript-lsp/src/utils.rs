use tower_lsp::lsp_types::{Position, Range};
use unrealscript_parser::ast::AstSpan;

/// Convert an AstSpan (byte offsets) to an LSP Range (line/character positions)
pub fn span_to_range(text: &str, span: &AstSpan) -> Range {
    let (start_line, start_char) = offset_to_position(text, span.start);
    let (end_line, end_char) = offset_to_position(text, span.end);
    
    Range {
        start: Position {
            line: start_line as u32,
            character: start_char as u32,
        },
        end: Position {
            line: end_line as u32,
            character: end_char as u32,
        },
    }
}

/// Convert a byte offset to a (line, character) position
fn offset_to_position(text: &str, offset: usize) -> (usize, usize) {
    let mut line = 0;
    let mut line_start = 0;
    
    for (i, ch) in text.char_indices() {
        if i >= offset {
            return (line, offset - line_start);
        }
        if ch == '\n' {
            line += 1;
            line_start = i + 1;
        }
    }
    
    // If we reach here, offset is at or past the end of the text
    (line, offset - line_start)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position() {
        let text = "hello\nworld\ntest";
        assert_eq!(offset_to_position(text, 0), (0, 0)); // 'h'
        assert_eq!(offset_to_position(text, 5), (0, 5)); // '\n'
        assert_eq!(offset_to_position(text, 6), (1, 0)); // 'w'
        assert_eq!(offset_to_position(text, 11), (1, 5)); // '\n'
        assert_eq!(offset_to_position(text, 12), (2, 0)); // 't'
    }

    #[test]
    fn test_span_to_range() {
        let text = "hello\nworld";
        let span = AstSpan {
            start: 0,
            end: 5,
            line: None,
        };
        let range = span_to_range(text, &span);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, 5);
    }
}
