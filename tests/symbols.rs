// Tests for the symbol table: declarations, scopes and resolved references.
use unrealscript_parser::ast::{Identifier, PodType};
use unrealscript_parser::symbols::{
    Access, ConstValue, Namespace, ScopeId, ScopeKind, SymbolId, TypeRef,
};
use unrealscript_parser::{ReferenceKind, SymbolKind, SymbolTable, parse_program};

fn table(source: &str) -> SymbolTable {
    parse_program(source)
        .unwrap_or_else(|e| panic!("{}\n{:?}", source, e))
        .symbols
}

fn id(name: &str) -> Identifier {
    Identifier::new(name)
}

#[track_caller]
fn find(table: &SymbolTable, scope: ScopeId, ns: Namespace, name: &str) -> SymbolId {
    table
        .lookup(scope, ns, &id(name))
        .unwrap_or_else(|| panic!("{} not found", name))
}

/// The scope of the function or state named `name` at its first occurrence
/// after `after` in `source`.
#[track_caller]
fn scope_named(table: &SymbolTable, source: &str, after: &str, name: &str) -> ScopeId {
    let start = source.find(after).unwrap() + after.len();
    let offset = start + source[start..].find(name).unwrap();
    table.scope_of(offset).expect("no scope")
}

/// The kinds of the references to `symbol`, with the text at each.
fn references(table: &SymbolTable, source: &str, symbol: SymbolId) -> Vec<(ReferenceKind, String)> {
    table
        .references_to(symbol)
        .map(|r| (r.kind, source[r.span.start..r.span.end].to_string()))
        .collect()
}

const DECLARATIONS: &str = "class Foo extends Bar;\n\
    const N = 2;\n\
    var int A[N];\n\
    var private byte B;\n\
    enum EColor\n{\n    C_Red,\n    C_Green\n};\n\
    struct SPoint\n{\n    var int X, Y;\n};\n\
    function F(int Arg)\n{\n    local int L;\n    L = Arg;\n}\n\
    state S\n{\n    const K = 1;\n    function F(int Arg2) {}\nBegin:\n    Stop;\n}\n";

#[test]
fn class_info() {
    let t = table(DECLARATIONS);
    let class = t.class().unwrap();
    assert_eq!(class.name, id("Foo"));
    assert_eq!(class.parent, Some(id("Bar")));
    assert!(!class.is_native);
    assert_eq!(class.span.line, Some(1));
}

#[test]
fn class_members() {
    let t = table(DECLARATIONS);
    let n = t.symbol(find(&t, ScopeId::CLASS, Namespace::Value, "N"));
    assert_eq!(
        n.kind,
        SymbolKind::Const {
            value: ConstValue::Integer(2)
        }
    );

    // Lookup is case-insensitive; the symbol keeps the declared case.
    let a = t.symbol(find(&t, ScopeId::CLASS, Namespace::Value, "a"));
    assert_eq!(a.name.string, "A");
    assert_eq!(a.kind.array_size(), Some(2));
    assert_eq!(a.kind.type_(), Some(&TypeRef::Pod(PodType::Int)));
    assert_eq!(a.span.line, Some(3));

    let b = t.symbol(find(&t, ScopeId::CLASS, Namespace::Value, "B"));
    assert_eq!(b.access, Access::Private);
    assert_eq!(b.kind.type_(), Some(&TypeRef::Pod(PodType::Byte)));

    let green = t.symbol(find(&t, ScopeId::CLASS, Namespace::Value, "C_Green"));
    let color = find(&t, ScopeId::CLASS, Namespace::Type, "EColor");
    assert_eq!(
        green.kind,
        SymbolKind::EnumValue {
            enum_: color,
            index: 1
        }
    );

    // Namespaces are separate.
    assert!(
        t.lookup(ScopeId::CLASS, Namespace::Value, &id("F"))
            .is_none()
    );
    assert!(
        t.lookup(ScopeId::CLASS, Namespace::Function, &id("F"))
            .is_some()
    );
}

#[test]
fn struct_scope() {
    let t = table(DECLARATIONS);
    let point = find(&t, ScopeId::CLASS, Namespace::Type, "SPoint");
    let (scope, _) = t
        .scopes()
        .find(|(_, s)| s.kind == ScopeKind::Struct(point))
        .unwrap();
    let names: Vec<_> = t
        .scope(scope)
        .symbols()
        .iter()
        .map(|&s| t.symbol(s).name.string.clone())
        .collect();
    assert_eq!(names, vec!["X", "Y"]);
    // Members aren't class members.
    assert!(
        t.lookup(ScopeId::CLASS, Namespace::Value, &id("X"))
            .is_none()
    );
}

#[test]
fn function_and_state_scopes() {
    let t = table(DECLARATIONS);
    let f = scope_named(&t, DECLARATIONS, "function ", "F");
    assert!(matches!(
        t.symbol(find(&t, f, Namespace::Value, "Arg")).kind,
        SymbolKind::Argument { .. }
    ));
    assert!(matches!(
        t.symbol(find(&t, f, Namespace::Value, "L")).kind,
        SymbolKind::Local { .. }
    ));
    // Class members are visible from the function.
    assert_eq!(
        find(&t, f, Namespace::Value, "N"),
        find(&t, ScopeId::CLASS, Namespace::Value, "N")
    );

    let s = scope_named(&t, DECLARATIONS, "state ", "S");
    assert!(matches!(t.scope(s).kind, ScopeKind::State(_)));
    assert!(matches!(
        t.symbol(find(&t, s, Namespace::Label, "Begin")).kind,
        SymbolKind::Label
    ));

    // The state's `F` overrides the class's.
    let state_f = scope_named(&t, DECLARATIONS, "state S", "F");
    assert_eq!(t.scope(state_f).parent, Some(s));
    assert_ne!(
        find(&t, s, Namespace::Function, "F"),
        find(&t, ScopeId::CLASS, Namespace::Function, "F")
    );
    // A state function sees the state's consts and the class's members.
    assert!(matches!(
        t.symbol(find(&t, state_f, Namespace::Value, "K")).kind,
        SymbolKind::Const { .. }
    ));
    assert!(t.lookup(state_f, Namespace::Value, &id("A")).is_some());
    assert!(t.lookup(state_f, Namespace::Value, &id("Arg")).is_none());
}

const RESOLUTION: &str = "class Foo extends Object;\n\
    var int V;\n\
    function F(int V2)\n{\n    local int V;\n    V = 1;\n    Self.V = 2;\n    default.V = 3;\n    G(V2);\n    Inherited = 1;\n    InheritedFunction();\n}\n\
    function G(int A) {}\n";

#[test]
fn locals_hide_class_variables() {
    let t = table(RESOLUTION);
    let f = scope_named(&t, RESOLUTION, "function ", "F");
    let local = find(&t, f, Namespace::Value, "V");
    let member = find(&t, ScopeId::CLASS, Namespace::Value, "V");
    assert_ne!(local, member);
    assert_eq!(
        references(&t, RESOLUTION, local),
        vec![(ReferenceKind::Write, "V".into())]
    );
    // `Self.V` and `default.V` are the class's.
    assert_eq!(
        references(&t, RESOLUTION, member),
        vec![
            (ReferenceKind::Write, "V".into()),
            (ReferenceKind::Write, "V".into())
        ]
    );
    let g = find(&t, ScopeId::CLASS, Namespace::Function, "G");
    assert_eq!(
        references(&t, RESOLUTION, g),
        vec![(ReferenceKind::Call, "G".into())]
    );
    let v2 = find(&t, f, Namespace::Value, "V2");
    assert_eq!(
        references(&t, RESOLUTION, v2),
        vec![(ReferenceKind::Read, "V2".into())]
    );
}

#[test]
fn inherited_names_are_unresolved() {
    let t = table(RESOLUTION);
    for name in ["Inherited", "InheritedFunction"] {
        let offset = RESOLUTION.find(name).unwrap();
        assert!(t.reference_at(offset).is_none(), "{}", name);
    }
    // Every reference points at something declared in the file.
    assert_eq!(t.references().len(), 5);
}

#[test]
fn reference_kinds() {
    let cases = [
        ("L = 1;", ReferenceKind::Write),
        ("L += 1;", ReferenceKind::Write),
        ("L $= \"x\";", ReferenceKind::Write),
        ("L++;", ReferenceKind::Write),
        ("--L;", ReferenceKind::Write),
        ("for (L = 0; true; L++) {}", ReferenceKind::Write),
        ("if (++L > 0) {}", ReferenceKind::Read),
        ("A = L;", ReferenceKind::Read),
        ("F(L);", ReferenceKind::Read),
        ("L.X = 1;", ReferenceKind::Read),
        ("L[0] = 1;", ReferenceKind::Read),
        ("switch (A) { case L: break; }", ReferenceKind::Read),
    ];
    for (body, kind) in cases {
        let source = format!(
            "class Foo extends Object;\nfunction F(int A)\n{{\n    local int L;\n    {}\n}}\n",
            body
        );
        let t = table(&source);
        let f = scope_named(&t, &source, "function ", "F");
        let l = find(&t, f, Namespace::Value, "L");
        let kinds: Vec<_> = t.references_to(l).map(|r| r.kind).collect();
        assert!(
            !kinds.is_empty() && kinds.iter().all(|&k| k == kind),
            "{}: {:?}",
            body,
            kinds
        );
    }
}

#[test]
fn state_resolution() {
    let source = "class Foo extends Object;\n\
        function F() {}\n\
        state S\n{\n    function F() {}\n    function G() { F(); global.F(); Self.F(); }\nBegin:\n    F();\n    goto 'Begin';\n}\n\
        state T extends S\n{\n    ignores F;\n}\n";
    let t = table(source);
    let class_f = find(&t, ScopeId::CLASS, Namespace::Function, "F");
    let s = scope_named(&t, source, "state ", "S");
    let state_f = find(&t, s, Namespace::Function, "F");
    // `global.F()` and `ignores F` name the class's version.
    assert_eq!(
        references(&t, source, class_f),
        vec![
            (ReferenceKind::Call, "F".into()),
            (ReferenceKind::Name, "F".into())
        ]
    );
    // The rest resolve to the state's.
    assert_eq!(t.references_to(state_f).count(), 3);
    let begin = find(&t, s, Namespace::Label, "Begin");
    assert_eq!(
        references(&t, source, begin),
        vec![(ReferenceKind::Name, "Begin".into())]
    );
    let state_s = find(&t, ScopeId::CLASS, Namespace::State, "S");
    assert_eq!(
        references(&t, source, state_s),
        vec![(ReferenceKind::Name, "S".into())]
    );
}

#[test]
fn replication_and_defaultproperties() {
    let source = "class Foo extends Actor;\nvar int V;\nfunction ServerF();\n\
        replication\n{\n    reliable if (Role < ROLE_Authority)\n        ServerF;\n    reliable if (bNetInitial)\n        V;\n}\n\
        defaultproperties\n{\n    v=1\n    Inherited=2\n}\n";
    let t = table(source);
    let v = find(&t, ScopeId::CLASS, Namespace::Value, "V");
    assert_eq!(
        references(&t, source, v),
        vec![
            (ReferenceKind::Replication, "V".into()),
            (ReferenceKind::DefaultProperty, "v".into())
        ]
    );
    let server_f = find(&t, ScopeId::CLASS, Namespace::Function, "ServerF");
    assert_eq!(
        references(&t, source, server_f),
        vec![(ReferenceKind::Replication, "ServerF".into())]
    );
    // The reference keeps the case it was written in.
    let r = t.references_to(v).last().unwrap();
    assert_eq!(r.text, "v");
    assert_eq!(r.span.line, Some(13));
}

#[test]
fn symbol_and_reference_at_offsets() {
    let t = table(RESOLUTION);
    let declaration = RESOLUTION.find("var int V").unwrap() + "var int ".len();
    let member = find(&t, ScopeId::CLASS, Namespace::Value, "V");
    assert_eq!(t.symbol_at(declaration), Some(member));
    assert_eq!(t.symbol_at(declaration + 1), None);

    let call = RESOLUTION.find("G(V2)").unwrap();
    let r = t.reference_at(call).unwrap();
    assert_eq!(r.symbol, find(&t, ScopeId::CLASS, Namespace::Function, "G"));
    assert!(t.reference_at(call + 1).is_none());
    let argument = t.reference_at(call + 3).unwrap();
    assert_eq!(argument.text, "V2");
}

#[test]
fn native_classes() {
    let t = table("class Foo extends Object\n    native;\n");
    assert!(t.class().unwrap().is_native);
}
