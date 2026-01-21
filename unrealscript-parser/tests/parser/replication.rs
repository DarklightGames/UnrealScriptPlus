use super::*;

#[test]
fn replication_block_empty() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "replication { }",
        rule: Rule::replication_block,
        tokens: [
            replication_block(0, 15)
        ]
    }
}

#[test]
fn replication_reliability_reliable() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "reliable",
        rule: Rule::replication_reliability,
        tokens: [ replication_reliability(0, 8) ]
    }
}

#[test]
fn replication_reliability_unreliable() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "unreliable",
        rule: Rule::replication_reliability,
        tokens: [ replication_reliability(0, 10) ]
    }
}

#[test]
fn replication_statement_no_variables_fails() {
    fails_with! {
        parser: UnrealScriptParser,
        input: "reliable if (true);",
        rule: Rule::replication_statement,
        positives: [Rule::unqualified_identifier],
        negatives: [],
        pos: 18
    }
}

#[test]
fn replication_statement_single_variable() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "reliable if (Foo) Bar;",
        rule: Rule::replication_statement,
        tokens: [
            replication_statement(0, 22, [
                replication_reliability(0, 8),
                expression(13, 16, [ unqualified_identifier(13, 16) ]),
                unqualified_identifier(18, 21),
            ])
        ]
    }
}

#[test]
fn replication_statement_multiple_variables() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "reliable if (Foo) Bar, Baz;",
        rule: Rule::replication_statement,
        tokens: [
            replication_statement(0, 27, [
                replication_reliability(0, 8),
                expression(13, 16, [ unqualified_identifier(13, 16) ]),
                unqualified_identifier(18, 21),
                unqualified_identifier(23, 26),
            ])
        ]
    }
}
