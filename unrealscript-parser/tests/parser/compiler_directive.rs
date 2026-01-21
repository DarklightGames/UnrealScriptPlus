use super::*;

#[test]
fn compiler_directive_start_of_line() {
    parses_to! {
        parser: UnrealScriptParser,
        input: "#exec OBJ LOAD FILE=..\\Foo\\Bar.utx",
        rule: Rule::compiler_directive,
        tokens: [ compiler_directive(0, 34, [ compiler_directive_inner(1, 34), EOI(34, 34) ]) ]
    }
}
