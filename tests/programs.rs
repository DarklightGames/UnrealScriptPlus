// End-to-end tests that run the full `parse_program` pipeline (grammar + AST
// construction + visitor). Many of these are minimal reproductions of failures
// found by running `examples/parse_corpus.rs` against real UnrealScript code.
use unrealscript_parser::ast::{DefaultProperties, ProgramStatement};
use unrealscript_parser::{
    ParseMode, ParseOptions, ParsingError, ProgramErrorSeverity, ProgramResult, parse_program,
    parse_program_with,
};

const STRICT: ParseOptions = ParseOptions {
    mode: ParseMode::Strict,
};

/// Asserts that `source` parses in strict mode, i.e. without anything to
/// recover from.
#[track_caller]
fn assert_parses(source: &str) -> ProgramResult {
    match parse_program_with(source, &STRICT) {
        Ok(result) => result,
        Err(e) => panic!("failed to parse:\n{}\n\nerror: {:?}", source, e),
    }
}

/// Parses `source` permissively, asserting that strict mode rejects it, and
/// returns the result along with the messages of the recovered problems.
#[track_caller]
fn parse_recovering(source: &str) -> (ProgramResult, Vec<String>) {
    let problems = match parse_program_with(source, &STRICT) {
        Err(ParsingError::Strict(problems)) => problems,
        other => panic!(
            "strict mode should have failed for:\n{}\ngot {:?}",
            source,
            other.map(|r| r.errors)
        ),
    };
    let result = parse_program(source).expect("permissive parsing should succeed");
    for p in &problems {
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message == p.message && e.span.start == p.span.start),
            "permissive result should report {:?} as a warning",
            p.message
        );
    }
    (result, problems.into_iter().map(|p| p.message).collect())
}

fn default_properties(result: &ProgramResult) -> &DefaultProperties {
    result
        .program
        .statements
        .iter()
        .find_map(|s| match &s.data {
            ProgramStatement::DefaultProperties(d) => Some(&d.data),
            _ => None,
        })
        .expect("no defaultproperties block")
}

fn class(defaultproperties: &str) -> String {
    format!(
        "class Foo extends Object;\n\ndefaultproperties\n{{\n{}}}\n",
        defaultproperties
    )
}

#[test]
fn cpptext() {
    assert_parses(
        "class Foo extends Object;\n\
         cpptext\n{\n    void Bar() { if (x) { y(); } }\n}\n",
    );
}

#[test]
fn cppstruct() {
    assert_parses(
        "class Foo extends Object;\n\
         struct Bar\n{\n    var int A;\n    cppstruct\n    {\n        void Baz() {}\n    }\n};\n",
    );
}

#[test]
fn keywords_are_case_insensitive() {
    // Keywords are matched case-insensitively by the grammar, so converting them
    // to AST enums must be too (these used to panic or error).
    assert_parses(
        "Class Foo Extends Object Abstract;\n\
         Var() Localized Config String A;\n\
         Struct Transient Bar { Var Const Int B; };\n\
         Replication { Reliable If (Role == ROLE_Authority) A; }\n\
         Simulated Final Function Baz(Optional Out Int C) {}\n\
         Static Event Qux();\n\
         Delegate Quux();\n\
         Auto Simulated State Idle {}\n",
    );
}

#[test]
fn exec_directive_before_class() {
    assert_parses(
        "#exec Texture Import File=Textures\\S_Foo.pcx Name=S_Foo Mips=Off MASKED=1\n\n\
         class Foo extends Object;\n",
    );
}

#[test]
fn var_modifier_editinlineuse() {
    assert_parses(
        "class Foo extends Object;\nvar() editinlineuse Material M;\nvar() editinlinenotify Object N;\n",
    );
}

#[test]
fn do_until_without_semicolon() {
    assert_parses(
        "class Foo extends Object;\n\
         function F() { local int i; do { i++; } until (i > 10) i = 0; }\n",
    );
}

#[test]
fn do_until_with_semicolon() {
    assert_parses(
        "class Foo extends Object;\n\
         function F() { local int i; do { i++; } until (i > 10); }\n",
    );
}

#[test]
fn var_declaration_whitespace_before_semicolon() {
    assert_parses("class Foo extends Object;\nvar localized string A,\n    B\n    ;\n");
}

#[test]
fn local_declaration_double_semicolon() {
    assert_parses(
        "class Foo extends Object;\n\
         function F() { local int A;; local int B; B = A; }\n",
    );
}

#[test]
fn return_without_whitespace() {
    assert_parses("class Foo extends Object;\nfunction string F() { return\"\"; }\n");
}

#[test]
fn defaultproperties_leading_dot_float() {
    assert_parses(
        "class Foo extends Object;\n\
         defaultproperties\n{\n    A=.45\n    B=(X=-.25,Y=0.0,Z=+.75)\n    C=+750.0\n}\n",
    );
}

#[test]
fn leading_dot_float_in_code() {
    assert_parses("class Foo extends Object;\nfunction F() { local float A; A = .5 * -.25; }\n");
}

#[test]
fn defaultproperties_unquoted_subobject_reference() {
    assert_parses(
        "class Foo extends Object;\n\
         defaultproperties\n{\n    PageTabs=Pkg.Foo.PageTabs\n}\n",
    );
}

#[test]
fn struct_var_modifiers_sharing_a_prefix() {
    // `editinline` must not shadow `editinlineuse`/`editinlinenotify`, nor
    // `editconst` shadow `editconstarray`.
    assert_parses(
        "class Foo extends Object;\n\
         struct Bar\n{\n\
             var() editinlineuse Material A;\n\
             var() editinlinenotify Object B;\n\
             var() editconstarray array<int> C;\n\
             var() editinline Object D;\n\
             var() editconst int E;\n\
         };\n",
    );
}

#[test]
fn clockwisefrom_operator() {
    assert_parses(
        "class Foo extends Object;\n\
         function bool F(int A, int B) { if (A ClockwiseFrom B) { return true; } return false; }\n",
    );
}

// --- defaultproperties (parsed by the t3d crate) ---

#[test]
fn defaultproperties_values_and_objects() {
    let result = assert_parses(&class(
        "    // comment with a } brace\n\
         \x20   /* block\n       comment */\n\
         \x20   A=1\n\
         \x20   B=\"string with } brace\"\n\
         \x20   C(0)=Texture'Pkg.Tex'\n\
         \x20   C[1]=Texture 'Pkg.Tex2'\n\
         \x20   D=(X=1,Y=(2,3),Z=SomeEnum)\n\
         \x20   IdleAnim=idle-empty\n\
         \x20   Sound=20_seconds\n\
         \x20   Begin Object Class=GUIButton Name=OkButton\n\
         \x20       Caption=\"OK\"\n\
         \x20   End Object\n\
         \x20   OkButtonRef=GUIButton'OkButton'\n",
    ));
    let d = default_properties(&result);
    assert!(d.closed);
    assert!(d.diagnostics.is_empty());
    assert_eq!(d.statements.len(), 9);
}

#[test]
fn defaultproperties_spans_are_file_offsets() {
    let source = class("    A=Value\n");
    let result = assert_parses(&source);
    let span = default_properties(&result).statements[0].span;
    assert_eq!(&source[span.start..span.end], "A=Value");
}

#[test]
fn defaultproperties_crlf_and_comment_before_brace() {
    assert_parses(
        "class Foo extends Object;\r\n\r\ndefaultproperties // c\r\n{\r\n    A=1\r\n}\r\n",
    );
}

#[test]
fn defaultproperties_empty_block_warns() {
    let result = assert_parses(&class(""));
    assert!(
        result
            .errors
            .iter()
            .any(|e| e.message.contains("should not be empty"))
    );
}

#[test]
fn defaultproperties_invalid_lines_are_discarded() {
    // Each of these is from shipped UE2 code.
    for line in [
        "0.905471",
        "Texture'InterfaceArt_tex.Menu.changeme_texture'",
        "/ Moving fire anims",
        "WinHeight1.0",
        "RemoteRole==ROLE_None",
        "DefaultStyleNames(35)=\"GUI2K4.STY2CheckBox",
    ] {
        let source = class(&format!("    A=1\n    {}\n    B=2\n", line));
        let (result, problems) = parse_recovering(&source);
        assert_eq!(problems.len(), 1, "{:?}", problems);
        assert!(
            problems[0].contains("invalid line was ignored"),
            "{}",
            problems[0]
        );
        assert!(problems[0].contains(line), "{}", problems[0]);
        // The surrounding lines are kept.
        assert_eq!(default_properties(&result).statements.len(), 2);
    }
}

#[test]
fn defaultproperties_trailing_input_keeps_the_statement() {
    for (line, junk) in [
        ("PitchDownLimit=50000/", "`/`"),
        ("WinWidth= 0.43000                           .", "`.`"),
        (
            "Grenades(0)=(Item=class'Foo.Bar',Amount=2)        NebelGranate39Weapon",
            "`NebelGranate39Weapon`",
        ),
        ("Digits=(A=(X1=1),B=(X1=2)))", "`)`"),
        (
            "BindingLabel(35)=\"Toggle \"BehindView\"\"",
            "`BehindView\"\"`",
        ),
    ] {
        let (result, problems) = parse_recovering(&class(&format!("    {}\n", line)));
        assert_eq!(problems.len(), 1, "{:?}", problems);
        assert!(
            problems[0].contains(junk),
            "{} should mention {}",
            problems[0],
            junk
        );
        assert_eq!(default_properties(&result).statements.len(), 1);
    }
}

#[test]
fn defaultproperties_unclosed_block() {
    let source = "class Foo extends Object;\n\ndefaultproperties\n{\n    A=1\n    B=2\n";
    let (result, problems) = parse_recovering(source);
    assert_eq!(
        problems,
        vec!["defaultproperties block is not closed before the end of the file"]
    );
    let d = default_properties(&result);
    assert!(!d.closed);
    assert_eq!(d.statements.len(), 2);
}

#[test]
fn defaultproperties_unclosed_object() {
    let (_, problems) =
        parse_recovering(&class("    Begin Object Class=Foo Name=Bar\n        A=1\n"));
    assert_eq!(problems.len(), 1);
    assert!(
        problems[0].contains("has no matching `End Object`"),
        "{}",
        problems[0]
    );
}

#[test]
fn recovered_problems_have_correct_line_numbers() {
    // Non-ASCII text before the problem must not throw off the line number
    // (positions are byte offsets).
    let source = "class Foo extends Object;\n\n// Panzerführer – ü\ndefaultproperties\n{\n    A=\"Führer\"\n    ???\n}\n";
    let (result, _) = parse_recovering(source);
    let warning = result
        .errors
        .iter()
        .find(|e| e.message.contains("???"))
        .unwrap();
    assert_eq!(warning.span.line, Some(7));
    assert!(matches!(warning.severity, ProgramErrorSeverity::Warning));
}

#[test]
fn name_literal_in_defaultproperties_is_an_error() {
    let result = assert_parses(&class(
        "    PlayerModelAnim='stand_idlehip_satchel'\n    S=(N='Foo')\n",
    ));
    let errors: Vec<_> = result
        .errors
        .iter()
        .filter(|e| {
            e.message
                .contains("cannot be initialized with name literals")
        })
        .collect();
    assert_eq!(errors.len(), 2);
    assert!(
        errors
            .iter()
            .all(|e| matches!(e.severity, ProgramErrorSeverity::Error))
    );
}

#[test]
fn defaultproperties_redundant_default_lint() {
    let result = assert_parses(
        "class Foo extends Object;\nvar int I;\nvar byte B;\nvar bool bX;\n\
         defaultproperties\n{\n    I=0\n    B=300\n    bX=False\n    Begin Object Class=Bar Name=Baz\n        I=0\n    End Object\n}\n",
    );
    let messages: Vec<_> = result.errors.iter().map(|e| e.message.as_str()).collect();
    assert!(
        messages
            .iter()
            .any(|m| m.contains("Redundant specification of default value (0) of variable (I)"))
    );
    assert!(
        messages
            .iter()
            .any(|m| m.contains("Value 300 is outside the range of [0..255]; effective value: 44"))
    );
    assert!(
        messages.iter().any(
            |m| m.contains("Redundant specification of default value (false) of variable (bX)")
        )
    );
    // Assignments inside `Begin Object` belong to the subobject, not this class.
    assert_eq!(
        messages
            .iter()
            .filter(|m| m.contains("variable (I)"))
            .count(),
        1
    );
}

#[test]
fn malformed_float_literal() {
    // From XVoting/Classes/MapVoteGameConfigPage.uc.
    let source =
        "class Foo extends Object;\nfunction F() { local float A; A = 0.0.5; A = -1.25.; }\n";
    let (_, problems) = parse_recovering(source);
    assert_eq!(
        problems,
        vec![
            "Malformed numeric literal \"0.0.5\" was read as \"0.0\"",
            "Malformed numeric literal \"-1.25.\" was read as \"-1.25\"",
        ]
    );
}

#[test]
fn argument_list_slots() {
    use unrealscript_parser::ast::Expression;
    use unrealscript_parser::parse_expression;
    // `Some` for an argument, `None` for an empty slot.
    for (source, expected) in [
        ("F()", vec![]),
        ("F( )", vec![]),
        ("F(A)", vec![true]),
        ("F(A, B)", vec![true, true]),
        ("F(A,,B)", vec![true, false, true]),
        ("F(,B)", vec![false, true]),
        ("F(A,)", vec![true, false]),
        ("F(A, , )", vec![true, false, false]),
        ("F(,)", vec![false, false]),
    ] {
        let e = parse_expression(source).unwrap();
        let Expression::Call { arguments, .. } = &e.data else {
            panic!("{} is not a call", source)
        };
        let slots: Vec<bool> = arguments.expressions.iter().map(|a| a.is_some()).collect();
        assert_eq!(slots, expected, "{}", source);
    }
}

// --- Expressions ---

/// Parses `source` as an expression.
#[track_caller]
fn expression(
    source: &str,
) -> Box<unrealscript_parser::ast::AstNode<unrealscript_parser::ast::Expression>> {
    unrealscript_parser::parse_expression(source)
        .unwrap_or_else(|e| panic!("{} failed to parse: {:?}", source, e))
}

#[test]
fn operator_precedence() {
    use unrealscript_parser::ast::{DyadicVerb, Expression};
    // An expression is split at its loosest operator.
    for (source, verb) in [
        ("A ** B", DyadicVerb::Exponentiate),
        ("A ** B * C", DyadicVerb::Multiply),
        ("A << B", DyadicVerb::ShiftLeft),
        ("A >> B", DyadicVerb::ShiftRight),
        ("A >>> B", DyadicVerb::DoubleShiftRight),
        ("A << B < C", DyadicVerb::CompareLessThan),
        ("A && B", DyadicVerb::LogicalAnd),
        ("A ^^ B", DyadicVerb::LogicalExclusiveOr),
        ("A ^^ B || C", DyadicVerb::LogicalOr),
        ("A != B && C", DyadicVerb::LogicalAnd),
        ("A -= B + C", DyadicVerb::SubtractAssign),
        ("A = B -= C", DyadicVerb::Assign),
        ("A dot B + C", DyadicVerb::Add),
        ("A dot B * C", DyadicVerb::Dot), // a tie goes to the first
        ("A ClockwiseFrom B", DyadicVerb::ClockwiseFrom),
    ] {
        match &expression(source).data {
            Expression::DyadicExpression { verb: v, .. } => assert_eq!(*v, verb, "{}", source),
            other => panic!("{}: {:?}", source, other),
        }
    }
}

#[test]
fn unknown_operator_is_an_error() {
    // The grammar accepts any identifier as an operator.
    match unrealscript_parser::parse_expression("A Foo B") {
        Err(ParsingError::PestError(e)) => {
            assert!(e.to_string().contains("unregistered dyadic verb"), "{}", e)
        }
        other => panic!("{:?}", other.map(|e| e.data)),
    }
    let source = "class Foo extends Object;\nfunction F() { local int A; A = A Foo 1; }\n";
    assert!(matches!(
        parse_program(source),
        Err(ParsingError::PestError(_))
    ));
}

#[test]
fn new_with_arguments() {
    use unrealscript_parser::ast::Expression;
    match &expression("new (Outer) class'Foo'").data {
        Expression::New {
            arguments: Some(arguments),
            type_,
        } => {
            assert_eq!(arguments.expressions.len(), 1);
            assert!(matches!(type_.data, Expression::Literal(_)));
        }
        other => panic!("{:?}", other),
    }
    assert!(matches!(
        expression("new class'Foo'").data,
        Expression::New {
            arguments: None,
            ..
        }
    ));
}

#[test]
fn default_and_static_access() {
    use unrealscript_parser::ast::Expression;
    match &expression("Other.default.Health").data {
        Expression::DefaultAccess {
            operand: Some(operand),
            target,
        } => {
            assert!(matches!(operand.data, Expression::Identifier(_)));
            assert_eq!(target.string, "Health");
        }
        other => panic!("{:?}", other),
    }
    let Expression::Call { operand, .. } = &expression("static.F()").data else {
        panic!()
    };
    assert!(matches!(
        &operand.data,
        Expression::StaticAccess { operand: None, target } if target.string == "F"
    ));
    let Expression::Call { operand, .. } = &expression("class'Foo'.static.F()").data else {
        panic!()
    };
    assert!(matches!(
        &operand.data,
        Expression::StaticAccess {
            operand: Some(_),
            ..
        }
    ));
}

#[test]
fn hexadecimal_integers() {
    use unrealscript_parser::ast::{Expression, Literal, NumericLiteral};
    for (source, value) in [("0xFF", 255), ("0x7FFFFFFF", i32::MAX), ("0xFFFFFFFF", -1)] {
        let e = expression(source);
        let Expression::Literal(literal) = &e.data else {
            panic!("{}", source)
        };
        let Literal::Numeric(n) = &literal.data else {
            panic!("{}", source)
        };
        assert_eq!(n.data, NumericLiteral::Integer(value), "{}", source);
    }
}

#[test]
fn integer_overflow_is_an_error_wherever_it_appears() {
    for source in [
        "9999999999",
        "0x1FFFFFFFF",
        "-9999999999",
        "vect(9999999999, 0, 0)",
        "(9999999999)",
        "F(9999999999)",
        "new 9999999999",
        "new (9999999999) Foo",
        "new (None) 9999999999",
        "F(9999999999).default.X",
        "F(9999999999).static.G()",
        "A + 9999999999",
    ] {
        match unrealscript_parser::parse_expression(source) {
            Err(ParsingError::PestError(_)) => {}
            other => panic!("{} should fail: {:?}", source, other.map(|e| e.data)),
        }
    }
}

#[test]
fn io_errors_convert_to_parsing_errors() {
    let error = ParsingError::from(std::io::Error::other("disk on fire"));
    assert!(matches!(error, ParsingError::IoError(_)));
}

#[test]
fn switch_default_cases() {
    use unrealscript_parser::ast::{
        CodeStatement, FunctionBodyStatement, ProgramStatement, SwitchCaseType,
    };
    // `true` for a `case`, `false` for a `default`, with each one's
    // statement count.
    for (cases, expected) in [
        (
            "case 1: break; default: break;",
            vec![(true, 1), (false, 1)],
        ),
        // `default:` after a case's statements used to be read as a label.
        (
            "case 1: A = 1; default: A = 2;",
            vec![(true, 1), (false, 1)],
        ),
        (
            "default: A = 1; case 1: break;",
            vec![(false, 1), (true, 1)],
        ),
        (
            "default: break; default: break;",
            vec![(false, 1), (false, 1)],
        ),
        // Labels named like `default` are still labels.
        ("case 1: defaultX: A = 1;", vec![(true, 2)]),
    ] {
        let source = format!(
            "class Foo extends Object;\nfunction F(int A) {{ switch (A) {{ {} }} }}\n",
            cases
        );
        let result = assert_parses(&source);
        let switch = result
            .program
            .statements
            .iter()
            .find_map(|s| match &s.data {
                ProgramStatement::FunctionDeclaration(f) => f.body.as_ref(),
                _ => None,
            })
            .and_then(|body| {
                body.statements.iter().find_map(|s| match s {
                    FunctionBodyStatement::CodeStatement(s) => match &s.data {
                        CodeStatement::Switch(switch) => Some(switch),
                        _ => None,
                    },
                    _ => None,
                })
            })
            .unwrap();
        let actual: Vec<_> = switch
            .cases
            .iter()
            .map(|c| {
                (
                    matches!(c.type_, SwitchCaseType::Expression(_)),
                    c.statements.len(),
                )
            })
            .collect();
        assert_eq!(actual, expected, "{}", cases);
    }
}

#[test]
fn syntax_errors() {
    assert!(matches!(
        unrealscript_parser::parse_expression(")"),
        Err(ParsingError::PestError(_))
    ));
    assert!(matches!(
        parse_program("class Foo extends Object;\nvar int;\n"),
        Err(ParsingError::PestError(_))
    ));
}

#[test]
fn errors_propagate_out_of_every_kind_of_expression() {
    for source in [
        "class<Foo>(9999999999)",
        "global.F(9999999999)",
        "F(9999999999)[0]",
        "A[9999999999]",
        "F(9999999999).X",
        "9999999999 + A",
        "F(9999999999)++",
        "!F(9999999999)",
    ] {
        match unrealscript_parser::parse_expression(source) {
            Err(ParsingError::PestError(_)) => {}
            other => panic!("{} should fail: {:?}", source, other.map(|e| e.data)),
        }
    }
}
