// Tests for the lints reported by the visitor. Each lint has a case that
// triggers it and, where there is a known false positive to avoid, a case
// that must not.
use unrealscript_parser::{Lint, LintSelection, ProgramErrorSeverity, parse_program};

/// Parses `source` and returns the codes of the lints it reports, in order.
#[track_caller]
fn lints(source: &str) -> Vec<&'static str> {
    let result = parse_program(source).unwrap_or_else(|e| panic!("{}\n{:?}", source, e));
    result
        .errors
        .iter()
        .filter_map(|e| e.lint.map(|l| l.code()))
        .collect()
}

/// Wraps `body` in a class with a function `F`.
fn function(locals: &str, body: &str) -> String {
    format!(
        "class Foo extends Actor;\nvar int I;\nvar byte B;\nvar int Arr[4];\nvar bool bX;\n\
         function F(int A, int Z)\n{{\n{}\n{}\n}}\n",
        locals, body
    )
}

#[track_caller]
fn assert_lint(code: &str, body: &str) {
    let source = function("", body);
    let found = lints(&source);
    assert!(
        found.contains(&code),
        "expected {} for:\n{}\ngot {:?}",
        code,
        body,
        found
    );
}

#[track_caller]
fn assert_no_lint(code: &str, body: &str) {
    let source = function("", body);
    let found = lints(&source);
    assert!(
        !found.contains(&code),
        "did not expect {} for:\n{}\ngot {:?}",
        code,
        body,
        found
    );
}

#[test]
fn every_problem_from_the_visitor_has_a_lint() {
    let result = parse_program(&function("", "if (A == A) {}")).unwrap();
    assert!(result.errors.iter().all(|e| e.lint.is_some()));
}

#[test]
fn severity_comes_from_the_lint() {
    let result = parse_program(&function("", "break;")).unwrap();
    let e = result
        .errors
        .iter()
        .find(|e| e.lint == Some(Lint::BreakOutsideLoop))
        .unwrap();
    assert_eq!(e.severity, ProgramErrorSeverity::Error);
}

#[test]
fn selection_filters_problems() {
    let mut result = parse_program(&function("", "A = A;;")).unwrap();
    LintSelection::new(["S"], ["S002"]).retain(&mut result.errors);
    let codes: Vec<_> = result
        .errors
        .iter()
        .map(|e| e.lint.unwrap().code())
        .collect();
    assert_eq!(codes, vec!["S003"]);
}

// Correctness

#[test]
fn break_and_continue_outside_loop() {
    assert_lint("C004", "break;");
    assert_lint("C005", "continue;");
    assert_lint("C005", "switch (A) { case 1: continue; }");
    assert_no_lint("C004", "while (A > 0) { break; }");
    assert_no_lint("C004", "switch (A) { case 1: break; }");
    assert_no_lint("C004", "for (A = 0; A < 3; A++) { if (A == 1) break; }");
    assert_no_lint("C005", "do { continue; } until (A > 0);");
    assert_no_lint(
        "C005",
        "foreach AllActors(class'Actor', Other) { continue; }",
    );
    assert_no_lint("C005", "while (A > 0) { switch (A) { case 1: continue; } }");
}

#[test]
fn division_by_zero() {
    assert_lint("C006", "A = A / 0;");
    assert_lint("C006", "A = A % 0.0;");
    assert_lint("C006", "A /= 0;");
    assert_no_lint("C006", "A = A / 2;");
    assert_no_lint("C006", "A = A * 0;");
}

#[test]
fn array_index_out_of_bounds() {
    assert_lint("C007", "Arr[4] = 1;");
    assert_lint("C007", "A = Arr[-1];");
    assert_no_lint("C007", "Arr[3] = 1;");
    assert_no_lint("C007", "Arr[A] = 1;");

    // Sizes given by constants, and locals.
    let source = "class Foo extends Object;\nconst N = 2;\nvar int C[N];\n\
                  function F() { local int L[3]; C[2] = 1; L[3] = 1; L[2] = 1; }\n";
    assert_eq!(lints(source), vec!["C007", "C007"]);

    // In defaultproperties.
    let source = "class Foo extends Object;\nvar int C[2];\n\
                  defaultproperties\n{\n    C(1)=1\n    C(2)=1\n}\n";
    assert_eq!(lints(source), vec!["C007"]);
}

#[test]
fn latent_call_outside_state_code() {
    assert_lint("C008", "Sleep(1.0);");
    assert_lint("C008", "FinishAnim();");
    assert_no_lint("C008", "Other.Sleep(1.0);");
    let source =
        "class Foo extends Actor;\nstate S\n{\nBegin:\n    Sleep(1.0);\n    FinishAnim();\n}\n";
    assert_eq!(lints(source), Vec::<&str>::new());
}

#[test]
fn conflicting_access_modifiers() {
    let source = "class Foo extends Object;\nprivate protected function F();\n";
    assert!(lints(source).contains(&"C009"));
    let source = "class Foo extends Object;\nvar private public int X;\n";
    assert!(lints(source).contains(&"C009"));
    let source = "class Foo extends Object;\nprivate function F();\n";
    assert!(!lints(source).contains(&"C009"));
}

#[test]
fn byte_value_out_of_range() {
    assert_lint("C010", "B = 256;");
    assert_lint("C010", "B = -1;");
    assert_no_lint("C010", "B = 255;");
    assert_no_lint("C010", "I = 256;");
    let source = function("local byte L;", "L = 300;");
    assert!(lints(&source).contains(&"C010"));
    // A local hides a class variable of the same name.
    let source = function("local int B;", "B = 300;");
    assert!(!lints(&source).contains(&"C010"));
}

// Suspicious

#[test]
fn empty_body() {
    assert_lint("S001", "if (A > 0);");
    assert_lint("S001", "if (A > 0) {} else if (A < 0);");
    assert_lint("S001", "if (A > 0) {} else;");
    assert_lint("S001", "while (A > 0);");
    assert_lint("S001", "for (A = 0; A < 3;);");
    assert_no_lint("S001", "if (A > 0) {}");
    // The loop header does the work.
    assert_no_lint("S001", "for (A = 0; A < 3; A++);");
    assert_no_lint("S001", "while (--A > 0);");
    // ...and the `;` is then not reported as an empty statement either.
    assert_no_lint("R001", "for (A = 0; A < 3; A++);");
}

#[test]
fn useless_expression() {
    assert_lint("S002", "A == Z;");
    assert_lint("S002", "A + Z;");
    assert_lint("S002", "A;");
    assert_lint("S002", "F(A) + F(Z);");
    assert_no_lint("S002", "A = Z;");
    assert_no_lint("S002", "A += Z;");
    assert_no_lint("S002", "A++;");
    assert_no_lint("S002", "--A;");
    assert_no_lint("S002", "F(A, Z);");
    assert_no_lint("S002", "Other.F(A, Z);");
    assert_no_lint(
        "S002",
        "class<Actor>(DynamicLoadObject(\"X\", class'Class'));",
    );
    assert_no_lint("S002", "new class'Object';");
    let source = "class Foo extends Actor;\nstate S\n{\nBegin:\n    Stop;\n}\n";
    assert!(!lints(source).contains(&"S002"));
}

#[test]
fn self_assignment() {
    assert_lint("S003", "A = A;");
    assert_lint("S003", "A = a;");
    assert_lint("S003", "Other.X = Other.X;");
    assert_no_lint("S003", "A = Z;");
    assert_no_lint("S003", "Other.X = X;");
    assert_no_lint("S003", "Arr[Rand(4)] = Arr[Rand(4)];");
    // Continues a loop from the current value.
    assert_no_lint("S003", "for (A = A; A < 3; A++) {}");
}

#[test]
fn identifiers_starting_with_new_are_not_new_expressions() {
    // `NewA = A` once parsed as `new (A = A)`.
    let source = function("local int NewA;", "NewA = A;");
    assert!(!lints(&source).contains(&"S003"));
}

#[test]
fn identical_operands() {
    assert_lint("S004", "if (A == A) {}");
    assert_lint("S004", "if (A > 0 || A > 0) {}");
    assert_lint("S004", "for (A = 0; A < A; A++) {}");
    assert_lint("S004", "Z = A - A;");
    assert_no_lint("S004", "if (A == Z) {}");
    assert_no_lint("S004", "if (Rand(2) == Rand(2)) {}");
    // The idiom for a NaN check.
    assert_no_lint("S004", "if (A != A) {}");
    // `$` can join a string to itself.
    assert_no_lint("S004", "Other.Tag = Other.Tag $ Other.Tag;");
}

#[test]
fn constant_condition() {
    assert_lint("S005", "if (true) {}");
    assert_lint("S005", "if (False) {}");
    assert_lint("S005", "if (None) {}");
    assert_lint("S005", "if (A > 0) {} else if ((false)) {}");
    assert_lint("S005", "while (false) {}");
    assert_no_lint("S005", "while (true) { break; }");
    assert_no_lint("S005", "if (A > 0) {}");
}

#[test]
fn unreachable_code() {
    assert_lint("S006", "return; A = 1;");
    assert_lint("S006", "while (A > 0) { break; A = 1; }");
    assert_lint("S006", "if (A > 0) { return; A = 1; }");
    assert_lint("S006", "switch (A) { case 1: return; A = 1; }");
    assert_no_lint("S006", "if (A > 0) return; A = 1;");
    // `return X; break;` in a switch is a harmless habit.
    assert_no_lint(
        "S006",
        "switch (A) { case 1: return; break; case 2: break; }",
    );

    // Only the first unreachable statement is reported.
    let source = function("", "return; A = 1; A = 2;");
    assert_eq!(lints(&source).iter().filter(|c| **c == "S006").count(), 1);

    // A label makes the code after it reachable again.
    let source = "class Foo extends Actor;\nstate S\n{\nBegin:\n    Goto('Next');\n    Stop;\nNext:\n    Stop;\n}\n";
    assert!(!lints(source).contains(&"S006"));
    let source = "class Foo extends Actor;\nstate S\n{\nBegin:\n    Stop;\n    Sleep(1);\n}\n";
    assert!(lints(source).contains(&"S006"));
}

#[test]
fn duplicate_switch_case() {
    assert_lint("S007", "switch (A) { case 1: break; case 1: break; }");
    assert_lint("S007", "switch (A) { case 'X': break; case 'X': break; }");
    assert_no_lint("S007", "switch (A) { case 1: break; case 2: break; }");
}

#[test]
fn switch_case_fallthrough() {
    assert_lint("S008", "switch (A) { case 1: A = 2; case 2: break; }");
    assert_lint(
        "S008",
        "switch (A) { case 1: if (Z > 0) return; case 2: break; }",
    );
    assert_no_lint("S008", "switch (A) { case 1: case 2: A = 3; break; }");
    assert_no_lint(
        "S008",
        "switch (A) { case 1: A = 2; break; case 2: return; case 3: A = 4; }",
    );
    assert_no_lint(
        "S008",
        "switch (A) { case 1: if (Z > 0) return; else { A = 1; break; } case 2: break; }",
    );
    assert_no_lint(
        "S008",
        "while (A > 0) { switch (A) { case 1: continue; case 2: break; } }",
    );
}

#[test]
fn negated_comparison_operand() {
    assert_lint("S009", "if (!bX == true) {}");
    assert_no_lint("S009", "if (!(A == Z)) {}");
    assert_no_lint("S009", "if (!bX) {}");
}

#[test]
fn duplicate_default_property() {
    let source = "class Foo extends Object;\n\
                  defaultproperties\n{\n    A=1\n    a=2\n    B(0)=1\n    B(1)=1\n    B(0)=2\n\
                  \x20   Begin Object Class=Bar Name=Baz\n        A=1\n    End Object\n}\n";
    assert_eq!(lints(source), vec!["S010", "S010"]);
}

#[test]
fn rotator_literals() {
    assert_lint("S011", "Other.Rotation = rot(0, 32768, 0);");
    assert_lint("S012", "Other.Rotation = rot(0, 1.5, 0);");
    assert_no_lint("S011", "Other.Rotation = rot(0, 32767, -32768);");
}

#[test]
fn duplicate_replicated_member() {
    let source = "class Foo extends Actor;\nvar int X, Y;\n\
                  replication\n{\n    reliable if (Role == ROLE_Authority)\n        X, Y;\n\
                  \x20   reliable if (bNetInitial)\n        X;\n}\n";
    assert_eq!(lints(source), vec!["S014"]);
}

// Performance

#[test]
fn actor_iterator_in_per_frame_function() {
    let source = "class Foo extends Actor;\nfunction Tick(float DT)\n{\n    local Pawn P;\n\
                  \x20   foreach DynamicActors(class'Pawn', P) {}\n\
                  \x20   foreach RadiusActors(class'Pawn', P, 100) {}\n}\n\
                  function Timer()\n{\n    local Pawn P;\n    foreach AllActors(class'Pawn', P) {}\n}\n";
    assert_eq!(lints(source), vec!["P001"]);
}

// Redundancy

#[test]
fn duplicate_modifier() {
    let source = "class Foo extends Object;\nvar config config int X;\n";
    assert_eq!(lints(source), vec!["R003"]);
    let source = "class Foo extends Object;\nfinal final function F();\n";
    assert_eq!(lints(source), vec!["R003"]);
    let source = "class Foo extends Object\n    abstract\n    abstract\n    dependson(A)\n    dependson(B);\n";
    assert_eq!(lints(source), vec!["R003"]);
}

#[test]
fn redundant_parentheses_are_reported_once() {
    assert_eq!(
        lints(&function("", "if (((A > 0))) {}"))
            .iter()
            .filter(|c| **c == "R002")
            .count(),
        1
    );
    assert_lint("R002", "A = (Z + 1);");
    assert_no_lint("R002", "A = (Z + 1) * 2;");
}

// Naming

#[test]
fn operator_names_are_not_function_names() {
    let source = "class Foo extends Object;\n\
                  native(130) static final operator(30) bool && (bool A, skip bool B);\n";
    assert!(!lints(source).contains(&"N002"));
    let source = "class Foo extends Object;\nfunction f();\n";
    assert_eq!(lints(source), vec!["N002"]);
}

// Unused locals

/// Returns the lints reported for the locals declared by `locals` and used by
/// `body`, among R009 and R010.
#[track_caller]
fn local_lints(locals: &str, body: &str) -> Vec<&'static str> {
    lints(&function(locals, body))
        .into_iter()
        .filter(|c| *c == "R009" || *c == "R010")
        .collect()
}

#[test]
fn unused_local() {
    assert_eq!(local_lints("local int L;", ""), vec!["R009"]);
    assert_eq!(local_lints("local int L, M;", "A = M;"), vec!["R009"]);
    // Arguments aren't locals.
    assert_eq!(local_lints("", ""), Vec::<&str>::new());
}

#[test]
fn local_set_but_never_used() {
    for body in [
        "L = 1;",
        "L = 1; L = 2;",
        "L += 1;",
        "L $= \"x\";",
        "L++;",
        "--L;",
        "for (L = 0; A < 3; L++) {}",
    ] {
        assert_eq!(local_lints("local int L;", body), vec!["R010"], "{}", body);
    }
}

#[test]
fn local_used() {
    for body in [
        "L = 1; A = L;",
        "L = 1; return L;",
        "L = 1; if (L > 0) {}",
        "L = L + 1;",
        "L = 1; F(L, A);",
        "if (++L > 3) {}",
        "A = L++;",
        "for (L = 0; L < 3; L++) {}",
        "L = 1; switch (A) { case L: break; }",
        "L = 1; while (L > 0) { L--; }",
        "L = 1; Arr[L] = 0;",
        // Passed by reference: may be an `out` argument.
        "F(L, A);",
        "foreach AllActors(class'Actor', L) {}",
        // `X.Y = ` and `X[0] = ` use `X` (it may be an object reference).
        "L.X = 1;",
        "L[0] = 1;",
        "L.Length = 0;",
    ] {
        assert_eq!(
            local_lints("local int L;", body),
            Vec::<&str>::new(),
            "{}",
            body
        );
    }
}

#[test]
fn locals_are_case_insensitive_and_shadow_class_variables() {
    assert_eq!(
        local_lints("local int L;", "l = 1; A = L;"),
        Vec::<&str>::new()
    );
    // `I` is a class variable too; the local hides it.
    assert_eq!(local_lints("local int I;", "I = 1;"), vec!["R010"]);
    // Class variables aren't tracked.
    assert_eq!(local_lints("", "I = 1;"), Vec::<&str>::new());
}

#[test]
fn locals_are_tracked_per_function() {
    let source = "class Foo extends Object;\n\
                  function F() { local int L; L = 1; }\n\
                  function G() { local int L; L = 1; G2(L); }\n";
    let result = parse_program(source).unwrap();
    let hits: Vec<_> = result
        .errors
        .iter()
        .filter(|e| e.lint == Some(Lint::LocalSetButNeverUsed))
        .collect();
    assert_eq!(hits.len(), 1);
    // Reported at the declaration in `F`.
    assert_eq!(&source[hits[0].span.start..hits[0].span.end], "L");
    assert_eq!(hits[0].span.line, Some(2));
}

// Symbol table lints

/// The lints among `codes` reported for `source`.
#[track_caller]
fn only(codes: &[&str], source: &str) -> Vec<&'static str> {
    lints(source)
        .into_iter()
        .filter(|c| codes.contains(c))
        .collect()
}

#[test]
fn duplicate_declaration() {
    for source in [
        "class Foo extends Object;\nvar int A;\nvar int a;\n",
        "class Foo extends Object;\nconst A = 1;\nvar int A;\n",
        "class Foo extends Object;\nfunction F();\nfunction F();\n",
        "class Foo extends Object;\nfunction F(int A) { local int A; }\n",
        "class Foo extends Object;\nfunction F() { local int L, L; }\n",
        "class Foo extends Object;\nenum EA\n{\n    X,\n    X\n};\n",
        "class Foo extends Object;\nstruct S\n{\n    var int M, M;\n};\n",
        "class Foo extends Actor;\nstate S {}\nstate S {}\n",
        "class Foo extends Actor;\nstate S\n{\nBegin:\n    Stop;\nBegin:\n    Stop;\n}\n",
    ] {
        assert_eq!(only(&["C011"], source), vec!["C011"], "{}", source);
    }
    for source in [
        // A state's function overrides the class's.
        "class Foo extends Actor;\nfunction F();\nstate S\n{\n    function F();\n}\n",
        // Operators are overloaded by type.
        "class Foo extends Object;\n\
         native(1) static final operator(24) bool == (int A, int B);\n\
         native(2) static final operator(24) bool == (float A, float B);\n",
        // UCC accepts the same value in different enums.
        "class Foo extends Object;\nenum EA\n{\n    X\n};\nenum EB\n{\n    X\n};\n",
        // Different namespaces.
        "class Foo extends Object;\nvar int A;\nfunction A();\n",
        // Different functions.
        "class Foo extends Object;\nfunction F() { local int L; }\nfunction G() { local int L; }\n",
    ] {
        assert_eq!(only(&["C011"], source), Vec::<&str>::new(), "{}", source);
    }
    let result = parse_program("class Foo extends Object;\nvar int A;\nvar int A;\n").unwrap();
    let e = result
        .errors
        .iter()
        .find(|e| e.lint == Some(Lint::DuplicateDeclaration))
        .unwrap();
    assert_eq!(e.message, "\"A\" is already declared on line 2");
    assert_eq!(e.span.line, Some(3));
    assert_eq!(e.severity, ProgramErrorSeverity::Error);
}

#[test]
fn shadowed_variable() {
    let source = function("local int I;", "A = I;");
    assert_eq!(only(&["S015"], &source), vec!["S015"]);
    let source = "class Foo extends Object;\nvar int V;\nfunction F(int V) {}\n";
    assert_eq!(only(&["S015"], source), vec!["S015"]);
    let source =
        "class Foo extends Object;\nconst K = 1;\nfunction F() { local int K; K = 2; G(K); }\n";
    assert_eq!(only(&["S015"], source), vec!["S015"]);
    // Within a state.
    let source =
        "class Foo extends Actor;\nstate S\n{\n    const K = 1;\n    function F(int K) {}\n}\n";
    assert_eq!(only(&["S015"], source), vec!["S015"]);

    assert_eq!(
        only(&["S015"], &function("local int L;", "A = L;")),
        Vec::<&str>::new()
    );
    // Enum values and functions aren't variables.
    let source = "class Foo extends Object;\nenum EA\n{\n    X\n};\nfunction G();\n\
                  function F(int X, int G) {}\n";
    assert_eq!(only(&["S015"], source), Vec::<&str>::new());
}

#[test]
fn identifier_case_mismatch() {
    assert_eq!(
        only(&["N006"], &function("local int L;", "l = 1; A = L;")),
        vec!["N006"]
    );
    assert_eq!(only(&["N006"], &function("", "i = A;")), vec!["N006"]);
    let source =
        "class Foo extends Object;\nvar int Speed;\ndefaultproperties\n{\n    speed=1\n}\n";
    assert_eq!(only(&["N006"], source), vec!["N006"]);
    let source = "class Foo extends Object;\nfunction Fire();\nfunction F() { fire(); }\n";
    assert_eq!(only(&["N006"], source), vec!["N006"]);

    assert_eq!(
        only(&["N006"], &function("local int L;", "L = 1; A = L;")),
        Vec::<&str>::new()
    );
    // Names the table can't resolve aren't checked.
    assert_eq!(
        only(&["N006"], &function("", "health = 1;")),
        Vec::<&str>::new()
    );
}

#[test]
fn unused_private_member() {
    for source in [
        "class Foo extends Object;\nvar private int V;\n",
        "class Foo extends Object;\nprivate function F();\n",
        // A local of the same name hides it.
        "class Foo extends Object;\nvar private int V;\nfunction F() { local int V; V = 1; G(V); }\n",
    ] {
        assert_eq!(only(&["R011"], source), vec!["R011"], "{}", source);
    }
    for source in [
        "class Foo extends Object;\nvar private int V;\nfunction F() { G(V); }\n",
        "class Foo extends Object;\nvar private int V;\nfunction F() { V = 1; }\n",
        "class Foo extends Object;\nprivate function F();\nfunction G() { F(); }\n",
        "class Foo extends Object;\nvar private int V;\ndefaultproperties\n{\n    V=1\n}\n",
        "class Foo extends Actor;\nvar private int V;\n\
         replication\n{\n    reliable if (bNetInitial)\n        V;\n}\n",
        // Not private.
        "class Foo extends Object;\nvar int V;\nvar protected int W;\nfunction F();\n",
        // Native code may use it.
        "class Foo extends Object\n    native;\nvar private int V;\n",
        "class Foo extends Object;\nvar private native int V;\n",
        "class Foo extends Object;\nprivate native function F();\n",
        // Called by the engine or the console.
        "class Foo extends Object;\nprivate event F();\n",
        "class Foo extends Object;\nprivate exec function F();\n",
        // Called through a state's override.
        "class Foo extends Actor;\nprivate function F();\nstate S\n{\n    function F();\nBegin:\n    F();\n}\n",
    ] {
        assert_eq!(only(&["R011"], source), Vec::<&str>::new(), "{}", source);
    }
}

#[test]
fn redundant_self() {
    for body in [
        "Self.I = 1;",
        "A = Self.I;",
        "self.SetTimer(1.0, false);",
        "Self.Arr[0] = 1;",
        // Inherited members too: nothing in the function hides them.
        "Self.Health = 1;",
        "if (Self.bHidden) {}",
        "Self.Other.Health = 1;",
    ] {
        assert_eq!(
            only(&["R012"], &function("", body)),
            vec!["R012"],
            "{}",
            body
        );
    }
    for body in [
        "I = 1;",
        // The argument `A` hides the member.
        "Self.A = A;",
        // As does a local.
        "local int Health; Self.Health = Health;",
        "Other.Self = 1;",
        // `Name` and `Class` alone would be read as types.
        "Other.Tag = Self.Name;",
        "if (Self.Class == class'Foo') {}",
        "Other.Owner = Self;",
    ] {
        let (locals, body) = match body.strip_prefix("local int Health; ") {
            Some(rest) => ("local int Health;", rest),
            None => ("", body),
        };
        assert_eq!(
            only(&["R012"], &function(locals, body)),
            Vec::<&str>::new(),
            "{}",
            body
        );
    }

    // In state code and state functions.
    let source = "class Foo extends Actor;\nstate S\n{\n    function F(int X) { Self.X = X; Self.Y = 1; }\n\
                  Begin:\n    Self.Z = 1;\n}\n";
    assert_eq!(only(&["R012"], source), vec!["R012", "R012"]);

    // The span covers just the qualifier.
    let source = function("", "Self.I = 1;");
    let result = parse_program(&source).unwrap();
    let e = result
        .errors
        .iter()
        .find(|e| e.lint == Some(Lint::RedundantSelf))
        .unwrap();
    assert_eq!(&source[e.span.start..e.span.end], "Self.");
}

#[test]
fn every_lint_is_documented() {
    let doc = include_str!("../STATIC_ANALYSIS.md");
    for &lint in Lint::ALL {
        let row = format!(
            "| {} | `{}` | {:?} |",
            lint.code(),
            lint.name(),
            lint.severity()
        );
        assert!(
            doc.contains(&row),
            "STATIC_ANALYSIS.md has no row starting {:?}",
            row
        );
    }
    // ...and lists nothing that no longer exists.
    for line in doc.lines() {
        let Some(code) = line.strip_prefix("| ").and_then(|l| l.split(' ').next()) else {
            continue;
        };
        let is_code = code.len() == 4
            && code.starts_with(['C', 'S', 'P', 'R', 'N', 'F'])
            && code[1..].chars().all(|c| c.is_ascii_digit());
        if is_code {
            assert!(
                Lint::ALL.iter().any(|l| l.code() == code),
                "STATIC_ANALYSIS.md lists unknown lint {}",
                code
            );
        }
    }
}
