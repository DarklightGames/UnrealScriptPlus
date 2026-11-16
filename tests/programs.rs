// End-to-end tests that run the full `parse_program` pipeline (grammar + AST
// construction + visitor). Many of these are minimal reproductions of failures
// found by running `examples/parse_corpus.rs` against real UnrealScript code.
use unrealscript_parser::parse_program;

#[track_caller]
fn assert_parses(source: &str) {
    if let Err(e) = parse_program(source) {
        panic!("failed to parse:\n{}\n\nerror: {:?}", source, e);
    }
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
