use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};
use unrealscript_parser::ast::*;

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::NUMBER,
    SemanticTokenType::STRING,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::TYPE,
    SemanticTokenType::new("boolean"),
    SemanticTokenType::new("vector"),
    SemanticTokenType::new("rotator"),
];

const MODIFIER_DECLARATION: u32 = 0b00000001;

pub struct TokenCollector<'a> {
    tokens: Vec<(usize, usize, usize, u32, u32)>, // (line, start_char, length, token_type, modifiers)
    source: &'a str,
}

impl<'a> TokenCollector<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            tokens: Vec::new(),
            source,
        }
    }

    fn add_token(&mut self, span: AstSpan, token_type: u32, modifiers: u32) {
        let length = span.end - span.start;
        
        // Calculate line number and character position within the line
        let before_span = &self.source[..span.start];
        let line = before_span.chars().filter(|&c| c == '\n').count();
        let line_start = before_span
            .rfind('\n')
            .map(|pos| pos + 1)
            .unwrap_or(0);
        let char_pos = span.start - line_start;
        
        self.tokens.push((line, char_pos, length, token_type, modifiers));
    }

    fn get_token_type_index(token_type: &SemanticTokenType) -> u32 {
        LEGEND_TYPE
            .iter()
            .position(|t| t == token_type)
            .unwrap_or(0) as u32
    }

    pub fn collect(mut self, program: &Program) -> Vec<SemanticToken> {
        for statement in &program.statements {
            self.visit_program_statement(statement);
        }

        // Sort tokens by position (line, then character).
        self.tokens.sort_by_key(|(line, start, _, _, _)| (*line, *start));

        // Delta-encode tokens per LSP specification.
        let mut result = Vec::new();
        let mut prev_line = 0;
        let mut prev_start = 0;

        for (line, start, length, token_type, modifiers) in self.tokens {
            let delta_line = (line as u32).saturating_sub(prev_line);
            let delta_start = if delta_line == 0 {
                (start as u32).saturating_sub(prev_start)
            } else {
                start as u32
            };

            result.push(SemanticToken {
                delta_line,
                delta_start,
                length: length as u32,
                token_type,
                token_modifiers_bitset: modifiers,
            });

            prev_line = line as u32;
            prev_start = start as u32;
        }

        result
    }

    fn visit_struct_var_declaration(&mut self, var_decl: &AstNode<StructVarDeclaration>) {
        // Visit the type to handle inline enum declarations.
        self.visit_type(&var_decl.type_);
        
        // Add tokens for variable names.
        let var_token_type = Self::get_token_type_index(&SemanticTokenType::VARIABLE);
        for name in &var_decl.names {
            self.add_token(name.identifier.span, var_token_type, MODIFIER_DECLARATION);
        }
    }

    fn visit_struct_declaration(&mut self, struct_decl: &AstNode<StructDeclaration>) {
        let token_type = Self::get_token_type_index(&SemanticTokenType::TYPE);
        self.add_token(struct_decl.name.span, token_type, MODIFIER_DECLARATION);
        if let Some(parent) = &struct_decl.parent {
            self.add_token(parent.span, token_type, 0);
        }
        for member in &struct_decl.members {
            self.visit_struct_var_declaration(member);
        }
    }

    fn visit_class_modifier(&mut self, modifier: &AstNode<ClassModifier>) {
        let token_type = Self::get_token_type_index(&SemanticTokenType::KEYWORD);
        self.add_token(modifier.type_.span, token_type, 0);
    }

    fn visit_class_declaration(&mut self, class_decl: &AstNode<ClassDeclaration>) {
        let token_type = Self::get_token_type_index(&SemanticTokenType::CLASS);
        self.add_token(class_decl.name.span, token_type, MODIFIER_DECLARATION);
        class_decl.modifiers.iter().for_each(|modifier| {
            self.visit_class_modifier(modifier);
        });
        if let Some(parent) = &class_decl.parent_class {
            self.add_token(parent.span, token_type, 0);
        }
    }

    fn visit_replication_block(&mut self, replication_block: &AstNode<ReplicationBlock>) {
        for statement in &replication_block.statements {
            self.visit_expression(&statement.condition);
            statement.variables.iter().for_each(|variable| {
                let token_type = Self::get_token_type_index(&SemanticTokenType::VARIABLE);
                self.add_token(variable.span, token_type, 0);
            });
        }
    }

    fn visit_program_statement(&mut self, statement: &AstNode<ProgramStatement>) {
        match &statement.data {
            ProgramStatement::FunctionDeclaration(func_decl) => {
                self.visit_function_declaration(func_decl);
            }
            ProgramStatement::StateDeclaration(state_decl) => {
                self.visit_state_declaration(state_decl);
            }
            ProgramStatement::DefaultProperties(default_props) => {
                self.visit_default_properties(default_props);
            }
            ProgramStatement::ConstDeclaration(const_decl) => {
                self.visit_const_declaration(const_decl);
            }
            ProgramStatement::EnumDeclaration(enum_decl) => {
                self.visit_enum_declaration(enum_decl);
            }
            ProgramStatement::VarDeclaration(var_decl) => {
                self.visit_var_declaration(var_decl);
            }
            ProgramStatement::ClassDeclaration(class_decl) => {
                self.visit_class_declaration(class_decl);
            }
            ProgramStatement::ReplicationBlock(replication_block) => {
                self.visit_replication_block(replication_block);
            }
            ProgramStatement::StructDeclaration(struct_decl) => {
                self.visit_struct_declaration(struct_decl);
            }
            ProgramStatement::CompilerDirective(_) => {
                let token_type = Self::get_token_type_index(&SemanticTokenType::MACRO);
                self.add_token(statement.span, token_type, 0);
            }
            _ => {}
        }
    }

    fn visit_var_name(&mut self, var_name: &AstNode<VarName>) {
        let token_type = Self::get_token_type_index(&SemanticTokenType::VARIABLE);
        self.add_token(var_name.identifier.span, token_type, 0);
    }

    fn visit_function_argument(&mut self, arg: &AstNode<FunctionArgument>) {
        self.visit_type(&arg.type_);
        self.visit_var_name(&arg.name);
    }

    fn visit_function_declaration(&mut self, func_decl: &AstNode<FunctionDeclaration>) {
        // Add function name as declaration.
        let token_type = Self::get_token_type_index(&SemanticTokenType::FUNCTION);
        self.add_token(func_decl.name.span, token_type, MODIFIER_DECLARATION);

        // Return type, if present.
        if let Some(return_type) = &func_decl.return_type {
            self.visit_type(return_type);
        }

        func_decl.arguments.iter().for_each(|arg| {
            self.visit_function_argument(arg);
        });

        // Visit function body if present.
        if let Some(body) = &func_decl.body {
            self.visit_function_body(body);
        }
    }

    fn visit_function_body(&mut self, body: &AstNode<FunctionBody>) {
        for statement in &body.statements {
            match statement {
                FunctionBodyStatement::CodeStatement(code_stmt) => {
                    self.visit_code_statement(code_stmt);
                }
                FunctionBodyStatement::ConstDeclaration(const_decl) => {
                    self.visit_const_declaration(const_decl);
                }
                FunctionBodyStatement::LocalDeclaration(local_decl) => {
                    // Visit the type to handle inline enums.
                    self.visit_type(&local_decl.type_);
                    
                    // Add tokens for local variable names.
                    let var_token_type = Self::get_token_type_index(&SemanticTokenType::VARIABLE);
                    for name in &local_decl.names {
                        self.add_token(name.identifier.span, var_token_type, MODIFIER_DECLARATION);
                    }
                }
            }
        }
    }

    fn visit_code_statement(&mut self, statement: &AstNode<CodeStatement>) {
        match &statement.data {
            CodeStatement::Expression(expr) => {
                self.visit_expression(expr);
            }
            CodeStatement::Return(Some(expr)) => {
                self.visit_expression(expr);
            }
            CodeStatement::ForEach(for_each) => {
                self.visit_expression(&for_each.predicate);
                self.visit_code_statement_or_block(&for_each.body);
            }
            CodeStatement::For(for_stmt) => {
                if let Some(init) = &for_stmt.init {
                    self.visit_expression(init);
                }
                if let Some(predicate) = &for_stmt.predicate {
                    self.visit_expression(predicate);
                }
                if let Some(post) = &for_stmt.post {
                    self.visit_expression(post);
                }
                self.visit_code_statement_or_block(&for_stmt.body);
            }
            CodeStatement::While(while_stmt) => {
                self.visit_expression(&while_stmt.predicate);
                self.visit_code_statement_or_block(&while_stmt.body);
            }
            CodeStatement::DoUntil(do_until) => {
                if let Some(predicate) = &do_until.predicate {
                    self.visit_expression(predicate);
                }
                self.visit_code_statement_or_block(&do_until.body);
            }
            CodeStatement::Switch(switch_stmt) => {
                self.visit_expression(&switch_stmt.predicate);
                for case in &switch_stmt.cases {
                    if let SwitchCaseType::Expression(expr) = &case.type_ {
                        self.visit_expression(expr);
                    }
                    for stmt in &case.statements {
                        self.visit_code_statement(stmt);
                    }
                }
            }
            CodeStatement::Conditional(conditional) => {
                self.visit_expression(&conditional.if_statement.predicate);
                if let Some(body) = &conditional.if_statement.body {
                    self.visit_code_statement_or_block(body);
                }
                for elif in &conditional.elif_statements {
                    self.visit_expression(&elif.predicate);
                    self.visit_code_statement_or_block(&elif.body);
                }
                if let Some(else_stmt) = &conditional.else_statement {
                    self.visit_code_statement_or_block(&else_stmt.body);
                }
            }
            CodeStatement::ConstDeclaration(const_decl) => {
                self.visit_const_declaration(const_decl);
            }
            _ => {}
        }
    }

    fn visit_code_statement_or_block(&mut self, stmt_or_block: &AstNode<CodeStatementOrBlock>) {
        match &stmt_or_block.data {
            CodeStatementOrBlock::CodeStatement(stmt) => {
                self.visit_code_statement(stmt);
            }
            CodeStatementOrBlock::CodeBlock(block) => {
                for stmt in &block.statements {
                    self.visit_code_statement(stmt);
                }
            }
        }
    }

    fn visit_expression(&mut self, expr: &AstNode<Expression>) {
        match &expr.data {
            Expression::Literal(literal) => {
                self.visit_literal(literal);
            }
            Expression::Call { operand, arguments } => {
                self.visit_expression(operand);
                self.visit_expression_list(arguments);
            }
            Expression::GlobalCall { name, arguments } => {
                let token_type = Self::get_token_type_index(&SemanticTokenType::FUNCTION);
                self.add_token(name.span, token_type, 0);
                self.visit_expression_list(arguments);
            }
            Expression::MonadicPreExpression { operand, .. } => {
                self.visit_expression(operand);
            }
            Expression::MonadicPostExpression { operand, .. } => {
                self.visit_expression(operand);
            }
            Expression::DyadicExpression { lhs, rhs, .. } => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
            }
            Expression::ArrayAccess { operand, argument } => {
                self.visit_expression(operand);
                self.visit_expression(argument);
            }
            Expression::MemberAccess { operand, target } => {
                self.visit_expression(operand);
                let token_type = Self::get_token_type_index(&SemanticTokenType::PROPERTY);
                self.add_token(target.span, token_type, 0);
            }
            Expression::DefaultAccess { operand, target } => {
                if let Some(operand) = operand {
                    self.visit_expression(operand);
                }
                let token_type = Self::get_token_type_index(&SemanticTokenType::PROPERTY);
                self.add_token(target.span, token_type, 0);
            }
            Expression::StaticAccess { operand, .. } => {
                if let Some(operand) = operand {
                    self.visit_expression(operand);
                }
            }
            Expression::Cast { operand, .. } => {
                self.visit_expression(operand);
            }
            Expression::ParentheticalExpression(inner) => {
                self.visit_expression(inner);
            }
            Expression::New { arguments, type_ } => {
                if let Some(args) = arguments {
                    self.visit_expression_list(args);
                }
                self.visit_expression(type_);
            }
            Expression::Identifier (identifier) => {
                let token_type = Self::get_token_type_index(&SemanticTokenType::VARIABLE);
                self.add_token(identifier.span, token_type, 0);
            }
            _ => {}
        }
    }

    fn visit_expression_list(&mut self, list: &AstNode<ExpressionList>) {
        for expr in &list.expressions {
            if let Some(expr) = expr {
                self.visit_expression(expr);
            }
        }
    }

    fn visit_literal(&mut self, literal: &AstNode<Literal>) {
        match &literal.data {
            Literal::Numeric(num) => {
                let token_type = Self::get_token_type_index(&SemanticTokenType::NUMBER);
                self.add_token(num.span, token_type, 0);
            }
            Literal::String(_) => {
                let token_type = Self::get_token_type_index(&SemanticTokenType::STRING);
                self.add_token(literal.span, token_type, 0);
            }
            Literal::Name(_) => {
                let token_type = Self::get_token_type_index(&SemanticTokenType::PROPERTY);
                self.add_token(literal.span, token_type, 0);
            }
            Literal::Boolean(_) => {
                let token_type = Self::get_token_type_index(&SemanticTokenType::new("boolean"));
                self.add_token(literal.span, token_type, 0);
            }
            Literal::Vector(values) => {
                let token_type = Self::get_token_type_index(&SemanticTokenType::new("vector"));
                self.add_token(literal.span, token_type, 0);
                // Also add individual numeric literals.
                let num_token_type = Self::get_token_type_index(&SemanticTokenType::NUMBER);
                for value in values {
                    self.add_token(value.span, num_token_type, 0);
                }
            }
            Literal::Rotator(values) => {
                let token_type = Self::get_token_type_index(&SemanticTokenType::new("rotator"));
                self.add_token(literal.span, token_type, 0);
                // Also add individual numeric literals.
                let num_token_type = Self::get_token_type_index(&SemanticTokenType::NUMBER);
                for value in values {
                    self.add_token(value.span, num_token_type, 0);
                }
            }
            Literal::Object { type_, .. } => {
                let token_type = Self::get_token_type_index(&SemanticTokenType::TYPE);
                self.add_token(type_.span, token_type, 0);
            }
            _ => {}
        }
    }

    fn visit_const_declaration(&mut self, const_decl: &AstNode<ConstDeclaration>) {
        // Add const name as declaration.
        let const_token_type = Self::get_token_type_index(&SemanticTokenType::PROPERTY);
        self.add_token(const_decl.name.span, const_token_type, MODIFIER_DECLARATION);
        self.visit_literal(&const_decl.value);
    }

    fn visit_enum_declaration(&mut self, enum_decl: &AstNode<EnumDeclaration>) {
        // Add enum name as declaration.
        let enum_token_type = Self::get_token_type_index(&SemanticTokenType::ENUM);
        self.add_token(enum_decl.name.span, enum_token_type, MODIFIER_DECLARATION);

        // Add each enum member.
        let enum_member_token_type = Self::get_token_type_index(&SemanticTokenType::ENUM_MEMBER);
        for value in &enum_decl.values {
            self.add_token(value.span, enum_member_token_type, 0);
        }
    }

    fn visit_var_declaration(&mut self, var_decl: &AstNode<VarDeclaration>) {
        // Visit the type to handle inline enum declarations.
        self.visit_type(&var_decl.type_);
        
        // Add tokens for variable names.
        let var_token_type = Self::get_token_type_index(&SemanticTokenType::VARIABLE);
        for name in &var_decl.names {
            self.add_token(name.identifier.span, var_token_type, MODIFIER_DECLARATION);
        }
    }

    fn visit_type(&mut self, type_: &Type) {
        match type_ {
            Type::Class(class_identifier) => {
                // Handle Class<SomeType> - emit token for the type parameter.
                let type_token_type = Self::get_token_type_index(&SemanticTokenType::TYPE);
                self.add_token(class_identifier.span, type_token_type, 0);
            }
            Type::Identifier(type_identifier) => {
                // Handle named type references (e.g., Sound, Texture, etc.)
                // Don't emit tokens for POD types as they're handled by the From impl.
                let type_token_type = Self::get_token_type_index(&SemanticTokenType::TYPE);
                self.add_token(type_identifier.span, type_token_type, 0);
            }
            Type::Enum(enum_decl) => {
                // Handle inline enum declaration.
                self.visit_enum_declaration(enum_decl);
            }
            Type::Array(inner_type) => {
                self.visit_type(inner_type);
            }
            Type::Struct(struct_decl) => {
                // Handle inline struct declaration if needed in the future.
                // For now, we can visit struct members to find nested enums.
                for member in &struct_decl.members {
                    self.visit_type(&member.type_);
                }
            }
            _ => {}
        }
    }

    fn visit_state_declaration(&mut self, state_decl: &StateDeclaration) {
        let token_type = Self::get_token_type_index(&SemanticTokenType::NAMESPACE);
        self.add_token(state_decl.name.span, token_type, MODIFIER_DECLARATION);

        if let Some(parent) = &state_decl.parent {
            self.add_token(parent.span, token_type, 0);
        }

        state_decl.ignores.iter().for_each(|ignores| {
            let token_type = Self::get_token_type_index(&SemanticTokenType::FUNCTION);
            self.add_token(ignores.span, token_type, 0);
        });

        for statement in &state_decl.statements {
            match statement {
                StateStatement::FunctionDeclaration(func_decl) => {
                    self.visit_function_declaration(func_decl);
                }
                StateStatement::ConstDeclaration(const_decl) => {
                    self.visit_const_declaration(const_decl);
                }
            }
        }

        for label in &state_decl.labels {
            for stmt in &label.statements {
                self.visit_code_statement(stmt);
            }
        }
    }

    fn visit_default_properties(&mut self, default_props: &AstNode<DefaultProperties>) {
        for statement in &default_props.statements {
            self.visit_default_properties_statement(statement);
        }
    }

    fn visit_default_properties_target(&mut self, target: &AstNode<DefaultPropertiesTarget>) {
        let token_type = Self::get_token_type_index(&SemanticTokenType::VARIABLE);
        self.add_token(target.data.target.span, token_type, 0);
    }

    fn visit_default_properties_statement(&mut self, statement: &AstNode<DefaultPropertiesStatement>) {
        match &statement.data {
            DefaultPropertiesStatement::Assignment(assignment) => {
                self.visit_default_properties_target(&assignment.target);
                if let Some(value) = &assignment.value {
                    self.visit_default_properties_value(value);
                }
            }
            DefaultPropertiesStatement::Object(obj) => {
                for stmt in &obj.statements {
                    self.visit_default_properties_statement(stmt);
                }
            }
        }
    }

    fn visit_default_properties_value(&mut self, value: &AstNode<DefaultPropertiesValue>) {
        match &value.data {
            DefaultPropertiesValue::Literal(literal) => {
                self.visit_literal(literal);
            }
            DefaultPropertiesValue::Struct(struct_val) => {
                for assignment in &struct_val.assignments {
                    self.visit_default_properties_target(&assignment.target);
                    if let Some(val) = &assignment.value {
                        self.visit_default_properties_value(val);
                    }
                }
            }
            DefaultPropertiesValue::Array(array) => {
                for element in &array.elements {
                    if let Some(elem) = element {
                        self.visit_default_properties_value(elem);
                    }
                }
            }
            _ => {}
        }
    }
}
