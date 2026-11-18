//! The lints reported by the [`Visitor`](crate::visitor::Visitor).
//!
//! Each lint has a stable code (e.g. `S003`) and a kebab-case name (e.g.
//! `self-assignment`), like Ruff's rules, and belongs to a [`Category`], like
//! Clippy's lint groups. A code's letter is its category's prefix.
//!
//! Every lint is reported by default; use [`LintSelection`] to choose which
//! ones to keep.

use crate::parser::{ProgramError, ProgramErrorSeverity};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Category {
    /// Code that is wrong: it won't compile, or won't do what it says.
    Correctness,
    /// Code that is very likely a mistake.
    Suspicious,
    /// Code that is needlessly slow.
    Performance,
    /// Code that does nothing and can be removed.
    Redundancy,
    /// Naming conventions.
    Naming,
    /// Formatting and ordering conventions.
    Formatting,
}

impl Category {
    pub const ALL: [Category; 6] = [
        Category::Correctness,
        Category::Suspicious,
        Category::Performance,
        Category::Redundancy,
        Category::Naming,
        Category::Formatting,
    ];

    /// The letter that starts the code of every lint in this category.
    pub fn prefix(self) -> &'static str {
        match self {
            Category::Correctness => "C",
            Category::Suspicious => "S",
            Category::Performance => "P",
            Category::Redundancy => "R",
            Category::Naming => "N",
            Category::Formatting => "F",
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Category::Correctness => "correctness",
            Category::Suspicious => "suspicious",
            Category::Performance => "performance",
            Category::Redundancy => "redundancy",
            Category::Naming => "naming",
            Category::Formatting => "formatting",
        }
    }
}

impl fmt::Display for Category {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// Whether a lint's problems come with a [`crate::fix::Fix`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FixAvailability {
    /// Every problem has a fix.
    Always,
    /// Some problems have a fix.
    Sometimes,
    /// No problem has a fix.
    Never,
}

macro_rules! lints {
    ($($variant:ident = ($code:literal, $name:literal, $category:ident, $severity:ident, $fix:ident, $summary:literal),)+) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub enum Lint {
            $(#[doc = $summary] $variant,)+
        }

        impl Lint {
            pub const ALL: &'static [Lint] = &[$(Lint::$variant,)+];

            /// The stable code, e.g. `S003`.
            pub fn code(self) -> &'static str {
                match self { $(Lint::$variant => $code,)+ }
            }

            /// The kebab-case name, e.g. `self-assignment`.
            pub fn name(self) -> &'static str {
                match self { $(Lint::$variant => $name,)+ }
            }

            pub fn category(self) -> Category {
                match self { $(Lint::$variant => Category::$category,)+ }
            }

            /// The severity this lint is reported with.
            pub fn severity(self) -> ProgramErrorSeverity {
                match self { $(Lint::$variant => ProgramErrorSeverity::$severity,)+ }
            }

            /// Whether problems found by this lint come with a fix.
            pub fn fix_availability(self) -> FixAvailability {
                match self { $(Lint::$variant => FixAvailability::$fix,)+ }
            }

            /// A one-line description of what the lint checks.
            pub fn summary(self) -> &'static str {
                match self { $(Lint::$variant => $summary,)+ }
            }

            /// Looks a lint up by its code or name (case-insensitive).
            pub fn find(s: &str) -> Option<Lint> {
                Lint::ALL.iter().copied().find(|l| {
                    l.code().eq_ignore_ascii_case(s) || l.name().eq_ignore_ascii_case(s)
                })
            }
        }
    };
}

lints! {
    // Correctness
    MultipleFunctionTypes = ("C001", "multiple-function-types", Correctness, Error, Never,
        "A function declares more than one of `function`, `event`, `delegate`, ..."),
    ExecConsecutiveFloatArguments = ("C002", "exec-consecutive-float-arguments", Correctness, Error, Never,
        "An `exec` function has consecutive `float` arguments, which the console can't parse."),
    DefaultPropertiesNameLiteral = ("C003", "defaultproperties-name-literal", Correctness, Error, Never,
        "A `defaultproperties` value is a name literal, which doesn't work; use a string."),
    BreakOutsideLoop = ("C004", "break-outside-loop", Correctness, Error, Never,
        "`break` outside a loop or `switch`."),
    ContinueOutsideLoop = ("C005", "continue-outside-loop", Correctness, Error, Never,
        "`continue` outside a loop."),
    DivisionByZero = ("C006", "division-by-zero", Correctness, Warning, Never,
        "Division or modulo by a literal zero."),
    ArrayIndexOutOfBounds = ("C007", "array-index-out-of-bounds", Correctness, Warning, Never,
        "A literal index outside the bounds of a static array."),
    LatentCallOutsideStateCode = ("C008", "latent-call-outside-state-code", Correctness, Warning, Never,
        "A latent function (`Sleep`, `FinishAnim`, ...) called from a function rather than state code."),
    ConflictingAccessModifiers = ("C009", "conflicting-access-modifiers", Correctness, Warning, Never,
        "More than one of `private`, `protected` and `public`."),
    ByteValueOutOfRange = ("C010", "byte-value-out-of-range", Correctness, Warning, Never,
        "A literal outside [0..255] assigned to a `byte`."),
    DuplicateDeclaration = ("C011", "duplicate-declaration", Correctness, Error, Never,
        "A name declared more than once in the same scope."),
    AssignmentToConstant = ("C012", "assignment-to-constant", Correctness, Error, Never,
        "An assignment to a const, an enum value or a function."),
    UndeclaredReplicatedMember = ("C014", "undeclared-replicated-member", Correctness, Error, Never,
        "A replicated member that isn't declared in the class itself."),
    ReturnFormMismatch = ("C015", "return-form-mismatch", Correctness, Error, Never,
        "`return X;` in a function with no return type, or `return;` in one with a return type."),

    // Suspicious
    EmptyBody = ("S001", "empty-body", Suspicious, Warning, Never,
        "An `if`, `else`, loop or `foreach` whose body is a lone `;`."),
    UselessExpression = ("S002", "useless-expression", Suspicious, Warning, Never,
        "An expression statement that has no effect, e.g. `A == B;`."),
    SelfAssignment = ("S003", "self-assignment", Suspicious, Warning, Never,
        "A variable assigned to itself."),
    IdenticalOperands = ("S004", "identical-operands", Suspicious, Warning, Never,
        "Both operands of a comparison, logical or arithmetic operator are the same."),
    ConstantCondition = ("S005", "constant-condition", Suspicious, Warning, Never,
        "An `if` or `while` condition that is a literal."),
    UnreachableCode = ("S006", "unreachable-code", Suspicious, Warning, Never,
        "A statement after `return`, `break`, `continue`, `goto` or `stop`."),
    DuplicateSwitchCase = ("S007", "duplicate-switch-case", Suspicious, Warning, Never,
        "The same `case` value appears more than once in a `switch`."),
    SwitchCaseFallthrough = ("S008", "switch-case-fallthrough", Suspicious, Warning, Never,
        "A non-empty `case` that falls through to the next one."),
    NegatedComparisonOperand = ("S009", "negated-comparison-operand", Suspicious, Warning, Never,
        "`!A == B`, which means `(!A) == B`."),
    DuplicateDefaultProperty = ("S010", "duplicate-default-property", Suspicious, Warning, Never,
        "The same property is assigned more than once in a `defaultproperties` block."),
    RotatorComponentOutOfRange = ("S011", "rotator-component-out-of-range", Suspicious, Warning, Never,
        "A rotator literal component outside the 16-bit range."),
    RotatorComponentNotInteger = ("S012", "rotator-component-not-integer", Suspicious, Warning, Never,
        "A rotator literal component that isn't an integer."),
    DefaultPropertyTypeMismatch = ("S013", "default-property-type-mismatch", Suspicious, Warning, Never,
        "A `defaultproperties` value whose literal type doesn't match the variable's type."),
    DuplicateReplicatedMember = ("S014", "duplicate-replicated-member", Suspicious, Warning, Never,
        "A member listed more than once in the replication block."),
    ShadowedVariable = ("S015", "shadowed-variable", Suspicious, Warning, Never,
        "A local or argument with the same name as a variable or const of the class."),
    MissingReturn = ("S016", "missing-return", Suspicious, Warning, Never,
        "A function with a return type where a path doesn't end in `return`."),
    // Performance
    ActorIteratorInPerFrameFunction = ("P001", "actor-iterator-in-per-frame-function", Performance, Warning, Never,
        "`foreach AllActors`/`DynamicActors` in a function called every frame, such as `Tick`."),

    // Redundancy
    EmptyStatement = ("R001", "empty-statement", Redundancy, Warning, Never,
        "A lone `;`."),
    RedundantParentheses = ("R002", "redundant-parentheses", Redundancy, Warning, Never,
        "Parentheses that don't change the meaning of an expression."),
    DuplicateModifier = ("R003", "duplicate-modifier", Redundancy, Warning, Never,
        "The same modifier appears more than once."),
    RedundantDefaultValue = ("R004", "redundant-default-value", Redundancy, Warning, Never,
        "A `defaultproperties` value equal to the type's zero value."),
    EmptyDefaultProperties = ("R005", "empty-defaultproperties", Redundancy, Warning, Never,
        "An empty `defaultproperties` block."),
    EmptyReplicationBlock = ("R006", "empty-replication-block", Redundancy, Warning, Never,
        "An empty `replication` block."),
    DuplicateReplicationCondition = ("R007", "duplicate-replication-condition", Redundancy, Warning, Never,
        "Two replication statements with the same reliability and condition."),
    TrailingEmptyArguments = ("R008", "trailing-empty-arguments", Redundancy, Warning, Sometimes,
        "Empty arguments at the end of a call, e.g. `Foo(A,,)`."),
    UnusedLocal = ("R009", "unused-local", Redundancy, Warning, Never,
        "A local variable that is never used."),
    LocalSetButNeverUsed = ("R010", "local-set-but-never-used", Redundancy, Warning, Never,
        "A local variable that is assigned to, but whose value is never used."),
    UnusedPrivateMember = ("R011", "unused-private-member", Redundancy, Warning, Never,
        "A `private` variable or function that is never used in its class."),
    RedundantSelf = ("R012", "redundant-self", Redundancy, Warning, Always,
        "A `Self.` qualifier that doesn't change what a name refers to."),
    RedundantReturn = ("R013", "redundant-return", Redundancy, Warning, Always,
        "A bare `return;` as the last statement of a function."),

    // Naming
    ConstNameCase = ("N001", "const-name-case", Naming, Warning, Never,
        "A `const` name that isn't SCREAMING_SNAKE_CASE."),
    FunctionNameCase = ("N002", "function-name-case", Naming, Warning, Never,
        "A function name that doesn't begin with a capital letter."),
    EnumNamePrefix = ("N003", "enum-name-prefix", Naming, Warning, Never,
        "An enum name that doesn't begin with `E`."),
    BoolNamePrefix = ("N004", "bool-name-prefix", Naming, Warning, Never,
        "A `bool` variable name that doesn't begin with `b`."),
    ObjectLiteralTypeCase = ("N005", "object-literal-type-case", Naming, Warning, Never,
        "An object literal type that doesn't begin with a capital letter, e.g. `texture'Foo'`."),
    IdentifierCaseMismatch = ("N006", "identifier-case-mismatch", Naming, Warning, Never,
        "A name written with different capitalization than its declaration."),

    // Formatting
    BooleanLiteralCase = ("F001", "boolean-literal-case", Formatting, Warning, Always,
        "A boolean literal that isn't lowercase."),
    ObjectLiteralKeywordCase = ("F002", "object-literal-keyword-case", Formatting, Warning, Always,
        "A `class'...'` or `enum'...'` keyword that isn't written `Class` or `Enum`."),
    UnsortedModifiers = ("F003", "unsorted-modifiers", Formatting, Warning, Never,
        "Modifiers that aren't in alphabetical order."),
}

impl fmt::Display for Lint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.code(), self.name())
    }
}

/// Chooses which lints to report, like Ruff's `select` and `ignore`.
///
/// Each selector is `ALL`, a code or code prefix (`S`, `S00`, `S003`), a lint
/// name (`self-assignment`) or a category name (`suspicious`), all
/// case-insensitive. A lint is enabled if a `select` selector matches it and
/// no `ignore` selector does.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LintSelection {
    pub select: Vec<String>,
    pub ignore: Vec<String>,
}

impl Default for LintSelection {
    fn default() -> Self {
        LintSelection {
            select: vec!["ALL".to_string()],
            ignore: vec![],
        }
    }
}

impl LintSelection {
    pub fn new<S: Into<String>>(
        select: impl IntoIterator<Item = S>,
        ignore: impl IntoIterator<Item = S>,
    ) -> Self {
        LintSelection {
            select: select.into_iter().map(Into::into).collect(),
            ignore: ignore.into_iter().map(Into::into).collect(),
        }
    }

    /// Whether `selector` is something [`LintSelection`] understands.
    pub fn is_valid_selector(selector: &str) -> bool {
        Lint::ALL.iter().any(|&l| selector_matches(selector, l))
    }

    pub fn is_enabled(&self, lint: Lint) -> bool {
        self.select.iter().any(|s| selector_matches(s, lint))
            && !self.ignore.iter().any(|s| selector_matches(s, lint))
    }

    /// Removes the problems of disabled lints. Problems that aren't lints
    /// (e.g. recovered parse problems) are always kept.
    pub fn retain(&self, errors: &mut Vec<ProgramError>) {
        errors.retain(|e| e.lint.is_none_or(|l| self.is_enabled(l)))
    }
}

fn selector_matches(selector: &str, lint: Lint) -> bool {
    let selector = selector.trim();
    selector.eq_ignore_ascii_case("ALL")
        || (!selector.is_empty()
            && lint
                .code()
                .get(..selector.len())
                .is_some_and(|p| p.eq_ignore_ascii_case(selector)))
        || lint.name().eq_ignore_ascii_case(selector)
        || lint.category().name().eq_ignore_ascii_case(selector)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn codes_and_names_are_unique_and_well_formed() {
        let mut codes = HashSet::new();
        let mut names = HashSet::new();
        for &lint in Lint::ALL {
            assert!(codes.insert(lint.code()), "duplicate code {}", lint.code());
            assert!(names.insert(lint.name()), "duplicate name {}", lint.name());
            assert!(lint.code().starts_with(lint.category().prefix()));
            assert_eq!(lint.code().len(), 4);
            assert!(lint.code()[1..].chars().all(|c| c.is_ascii_digit()));
            assert!(
                lint.name()
                    .chars()
                    .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-')
            );
        }
    }

    #[test]
    fn selection() {
        let s = LintSelection::new(["S", "C001"], ["S008"]);
        assert!(s.is_enabled(Lint::SelfAssignment));
        assert!(s.is_enabled(Lint::MultipleFunctionTypes));
        assert!(!s.is_enabled(Lint::SwitchCaseFallthrough));
        assert!(!s.is_enabled(Lint::EmptyStatement));

        let s = LintSelection::new(["ALL"], ["naming", "unsorted-modifiers"]);
        assert!(!s.is_enabled(Lint::BoolNamePrefix));
        assert!(!s.is_enabled(Lint::UnsortedModifiers));
        assert!(s.is_enabled(Lint::BooleanLiteralCase));

        assert!(LintSelection::default().is_enabled(Lint::UnsortedModifiers));
        assert!(LintSelection::is_valid_selector("s00"));
        assert!(!LintSelection::is_valid_selector("Z"));
    }
}
