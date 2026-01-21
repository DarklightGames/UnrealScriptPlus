// Integration tests for public API functions
use unrealscript_parser::{parse_with_rule, parse_expression, Rule};

#[test]
fn test_parse_with_rule_expression() {
    let result = parse_with_rule(Rule::expression, "1 + 2");
    assert!(result.is_ok(), "Failed to parse expression: {:?}", result.err());
    
    let nodes = result.unwrap();
    let collected: Vec<_> = nodes.collect();
    assert!(!collected.is_empty(), "Expected at least one node from expression parse");
}

#[test]
fn test_parse_with_rule_var_declaration() {
    let result = parse_with_rule(Rule::var_declaration, "var int Foo;");
    assert!(result.is_ok(), "Failed to parse var declaration: {:?}", result.err());
    
    let nodes = result.unwrap();
    let collected: Vec<_> = nodes.collect();
    assert!(!collected.is_empty(), "Expected at least one node from var declaration parse");
}

#[test]
fn test_parse_with_rule_function_declaration() {
    let result = parse_with_rule(Rule::function_declaration, "function Foo();");
    assert!(result.is_ok(), "Failed to parse function declaration: {:?}", result.err());
    
    let nodes = result.unwrap();
    let collected: Vec<_> = nodes.collect();
    assert!(!collected.is_empty(), "Expected at least one node from function declaration parse");
}

#[test]
fn test_parse_with_rule_struct_declaration() {
    let result = parse_with_rule(Rule::struct_declaration, "struct Foo { var int Bar; }");
    assert!(result.is_ok(), "Failed to parse struct declaration: {:?}", result.err());
    
    let nodes = result.unwrap();
    let collected: Vec<_> = nodes.collect();
    assert!(!collected.is_empty(), "Expected at least one node from struct declaration parse");
}

#[test]
fn test_parse_with_rule_invalid_input() {
    // Use input that cannot match the start of any expression
    let result = parse_with_rule(Rule::expression, "@#$%");
    assert!(result.is_err(), "Expected parsing to fail for invalid input");
}

#[test]
fn test_parse_expression_still_works() {
    // Ensure existing parse_expression function still works
    let result = parse_expression("42");
    assert!(result.is_ok(), "Failed to parse simple expression: {:?}", result.err());
}

#[test]
fn test_parse_with_rule_class_declaration() {
    let result = parse_with_rule(Rule::class_declaration, "class Foo extends Bar;");
    assert!(result.is_ok(), "Failed to parse class declaration: {:?}", result.err());
    
    let nodes = result.unwrap();
    let collected: Vec<_> = nodes.collect();
    assert!(!collected.is_empty(), "Expected at least one node from class declaration parse");
}
