use super::*;

#[test]
fn comment_single_line() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "// This is a single-line comment!",
        rule: Rule::comment_single_line,
        tokens: []
    )
}

#[test]
fn comment_multi_line() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "/* This is a multi-line comment! */",
        rule: Rule::comment_multi_line,
        tokens: []
    )
}

#[test]
fn comment_multi_line_nested() {
    parses_to!(
        parser: UnrealScriptParser,
        input: "/* /* This is a multi-line comment! */ Foo */",
        rule: Rule::comment_multi_line,
        tokens: []
    )
}
