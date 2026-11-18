//! Suggested fixes for problems, and applying them.
//!
//! A [`Fix`] is a set of [`Edit`]s, each replacing a byte range of the source
//! text. Fixes are what an editor offers as code actions and what
//! `usagi lint --fix` applies.

use std::ops::Range;

/// Replaces `range` (byte offsets into the source text) with `replacement`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Edit {
    pub range: Range<usize>,
    pub replacement: String,
}

impl Edit {
    pub fn replace(range: Range<usize>, replacement: impl Into<String>) -> Self {
        Edit {
            range,
            replacement: replacement.into(),
        }
    }

    pub fn delete(range: Range<usize>) -> Self {
        Edit::replace(range, "")
    }
}

/// How safe a fix is to apply without looking at it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Applicability {
    /// Never changes what the code does. Applied by `usagi lint --fix`.
    Safe,
    /// Might change what the code does. Only offered in an editor, for a
    /// person to accept or not.
    Unsafe,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fix {
    /// What the fix does, e.g. "Remove `Self.`".
    pub title: String,
    /// Edits that don't overlap.
    pub edits: Vec<Edit>,
    pub applicability: Applicability,
}

impl Fix {
    pub fn safe(title: impl Into<String>, edits: Vec<Edit>) -> Self {
        Fix {
            title: title.into(),
            edits,
            applicability: Applicability::Safe,
        }
    }

    /// The range from the start of the first edit to the end of the last.
    fn extent(&self) -> Range<usize> {
        let start = self.edits.iter().map(|e| e.range.start).min();
        let end = self.edits.iter().map(|e| e.range.end).max();
        start.unwrap_or(0)..end.unwrap_or(0)
    }
}

/// Applies `fixes` to `text`. A fix that overlaps one applied before it is
/// skipped; running the lints again and applying their fixes picks it up.
/// Returns the new text and the number of fixes applied.
pub fn apply<'a>(text: &str, fixes: impl IntoIterator<Item = &'a Fix>) -> (String, usize) {
    let mut fixes: Vec<&Fix> = fixes.into_iter().collect();
    fixes.sort_by_key(|f| (f.extent().start, f.extent().end));
    let mut edits: Vec<&Edit> = Vec::new();
    let mut applied = 0;
    let mut end = 0;
    for fix in fixes {
        let extent = fix.extent();
        // Touching ranges are fine unless both are insertions at one point.
        if !edits.is_empty() && (extent.start < end || (extent.start == end && extent.is_empty())) {
            continue;
        }
        let mut fix_edits: Vec<&Edit> = fix.edits.iter().collect();
        fix_edits.sort_by_key(|e| e.range.start);
        edits.extend(fix_edits);
        end = extent.end;
        applied += 1;
    }
    let mut result = String::with_capacity(text.len());
    let mut position = 0;
    for edit in edits {
        result.push_str(&text[position..edit.range.start]);
        result.push_str(&edit.replacement);
        position = edit.range.end;
    }
    result.push_str(&text[position..]);
    (result, applied)
}

/// The range to delete to remove `range` from `text`, and its line too if
/// nothing else is on it: the indentation before it, and the line ending
/// after it.
pub fn line_aware_deletion(text: &str, range: Range<usize>) -> Range<usize> {
    let line_start = text[..range.start].rfind('\n').map_or(0, |i| i + 1);
    let after = &text[range.end..];
    let rest_of_line = after.find('\n').map_or(after, |i| &after[..i]);
    let blank = |s: &str| s.chars().all(|c| c == ' ' || c == '\t' || c == '\r');
    if blank(&text[line_start..range.start]) && blank(rest_of_line) {
        let end = range.end + rest_of_line.len();
        let end = if text[end..].starts_with('\n') {
            end + 1
        } else {
            end
        };
        line_start..end
    } else {
        range
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fix(edits: &[(Range<usize>, &str)]) -> Fix {
        Fix::safe(
            "fix",
            edits
                .iter()
                .map(|(r, s)| Edit::replace(r.clone(), *s))
                .collect(),
        )
    }

    #[test]
    fn applies_fixes_in_order() {
        let text = "abcdef";
        let fixes = [fix(&[(4..5, "E")]), fix(&[(0..1, ""), (2..3, "C")])];
        assert_eq!(apply(text, &fixes), ("bCdEf".to_string(), 2));
        assert_eq!(apply(text, []), (text.to_string(), 0));
    }

    #[test]
    fn skips_overlapping_fixes() {
        let text = "abcdef";
        let fixes = [fix(&[(1..4, "")]), fix(&[(2..3, "X")]), fix(&[(4..5, "Y")])];
        assert_eq!(apply(text, &fixes), ("aYf".to_string(), 2));
        // Two insertions at one point would be ambiguous.
        let fixes = [fix(&[(1..1, "X")]), fix(&[(1..1, "Y")])];
        assert_eq!(apply(text, &fixes), ("aXbcdef".to_string(), 1));
    }

    #[test]
    fn deletes_whole_lines() {
        let text = "a;\n    return;\r\nb;\n";
        let start = text.find("return").unwrap();
        let range = start..start + "return;".len();
        let deletion = line_aware_deletion(text, range.clone());
        let mut fixed = text.to_string();
        fixed.replace_range(deletion, "");
        assert_eq!(fixed, "a;\nb;\n");
        // Something else on the line: only the range goes.
        let text = "a; return;\n";
        assert_eq!(line_aware_deletion(text, 3..10), 3..10);
        // The last line, without a line ending.
        let text = "a;\nreturn;";
        assert_eq!(line_aware_deletion(text, 3..10), 3..10);
    }
}
