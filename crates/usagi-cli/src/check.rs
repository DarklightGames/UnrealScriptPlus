//! Parsing and linting one file.

use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};

use pest::error::InputLocation;
use usagi_core::source::LineIndex;
use usagi_core::{
    Lint, ParseMode, ParseOptions, ParsingError, ProgramError, ProgramErrorSeverity,
    parse_program_with,
};

/// What parsing a file produced.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Outcome {
    /// Parsed with nothing to recover from.
    Ok,
    /// Parsed, but permissive parsing recovered from problems.
    Recovered,
    /// Recovered problems, rejected by `--strict`.
    StrictError,
    /// A syntax error.
    SyntaxError,
    /// The parser panicked.
    Panic,
    /// The file couldn't be read.
    IoError,
}

impl Outcome {
    pub fn name(self) -> &'static str {
        match self {
            Outcome::Ok => "ok",
            Outcome::Recovered => "recovered",
            Outcome::StrictError => "strict_error",
            Outcome::SyntaxError => "syntax_error",
            Outcome::Panic => "panic",
            Outcome::IoError => "io_error",
        }
    }

    /// Whether the outcome is a failure of the file as a whole.
    pub fn is_failure(self) -> bool {
        !matches!(self, Outcome::Ok | Outcome::Recovered)
    }
}

/// What a problem is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Lint(Lint),
    /// A problem that permissive parsing recovered from.
    Recovered,
    SyntaxError,
    Panic,
    IoError,
}

impl Kind {
    /// The code shown for the problem.
    pub fn code(self) -> &'static str {
        match self {
            Kind::Lint(lint) => lint.code(),
            Kind::Recovered => "parse",
            Kind::SyntaxError => "syntax",
            Kind::Panic => "panic",
            Kind::IoError => "io",
        }
    }

    /// The name shown for the problem.
    pub fn name(self) -> &'static str {
        match self {
            Kind::Lint(lint) => lint.name(),
            Kind::Recovered => "recovered-problem",
            Kind::SyntaxError => "syntax-error",
            Kind::Panic => "internal-error",
            Kind::IoError => "io-error",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Problem {
    pub kind: Kind,
    pub message: String,
    pub is_error: bool,
    /// Byte range in the source.
    pub start: usize,
    pub end: usize,
    /// 1-based line and column of `start` and `end`.
    pub line: usize,
    pub column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

/// The result of checking one file.
pub struct FileReport {
    pub path: PathBuf,
    pub source: String,
    pub outcome: Outcome,
    pub problems: Vec<Problem>,
    /// The syntax tree, when asked for.
    pub ast: Option<String>,
}

pub struct CheckOptions<'a> {
    pub strict: bool,
    pub lint: bool,
    pub ast: bool,
    /// Whether a lint is reported for a file.
    pub is_enabled: &'a (dyn Fn(&Path, Lint) -> bool + Sync),
}

pub fn check_file(path: &Path, options: &CheckOptions) -> FileReport {
    let source = match std::fs::read(path) {
        Ok(bytes) => usagi_core::decode(&bytes),
        Err(e) => {
            return FileReport {
                path: path.to_path_buf(),
                source: String::new(),
                outcome: Outcome::IoError,
                problems: vec![Problem::at(Kind::IoError, e.to_string(), "", 0, 0)],
                ast: None,
            };
        }
    };
    let (outcome, problems, ast) =
        match panic::catch_unwind(AssertUnwindSafe(|| check_source(path, &source, options))) {
            Ok(result) => result,
            Err(payload) => {
                let message = payload
                    .downcast_ref::<String>()
                    .cloned()
                    .or_else(|| payload.downcast_ref::<&str>().map(|s| s.to_string()))
                    .unwrap_or_else(|| "the parser panicked".to_string());
                let problem = Problem::at(Kind::Panic, message, &source, 0, 0);
                (Outcome::Panic, vec![problem], None)
            }
        };
    FileReport {
        path: path.to_path_buf(),
        source,
        outcome,
        problems,
        ast,
    }
}

fn check_source(
    path: &Path,
    source: &str,
    options: &CheckOptions,
) -> (Outcome, Vec<Problem>, Option<String>) {
    let mode = if options.strict {
        ParseMode::Strict
    } else {
        ParseMode::Permissive
    };
    let index = LineIndex::new(source);
    let problem = |kind: Kind, error: &ProgramError| {
        let mut problem = Problem::new(
            kind,
            error.message.clone(),
            source,
            &index,
            error.span.start,
            error.span.end,
        );
        problem.is_error = error.severity == ProgramErrorSeverity::Error;
        problem
    };
    match parse_program_with(source, &ParseOptions { mode }) {
        Ok(result) => {
            let mut problems = Vec::new();
            let mut outcome = Outcome::Ok;
            for error in &result.errors {
                match error.lint {
                    None => {
                        outcome = Outcome::Recovered;
                        problems.push(problem(Kind::Recovered, error));
                    }
                    Some(lint) if options.lint && (options.is_enabled)(path, lint) => {
                        problems.push(problem(Kind::Lint(lint), error));
                    }
                    Some(_) => {}
                }
            }
            let ast = options.ast.then(|| format!("{:#?}", result.program));
            (outcome, problems, ast)
        }
        Err(ParsingError::Strict(errors)) => {
            let problems = errors
                .iter()
                .map(|e| {
                    let mut p = problem(Kind::Recovered, e);
                    p.is_error = true;
                    p
                })
                .collect();
            (Outcome::StrictError, problems, None)
        }
        Err(ParsingError::PestError(e)) => {
            let (start, end) = match e.location {
                InputLocation::Pos(p) => (p, p),
                InputLocation::Span(span) => span,
            };
            let message = e.variant.message().to_string();
            let problem = Problem::new(Kind::SyntaxError, message, source, &index, start, end);
            (Outcome::SyntaxError, vec![problem], None)
        }
        Err(other) => {
            let problem = Problem::at(Kind::SyntaxError, format!("{:?}", other), source, 0, 0);
            (Outcome::SyntaxError, vec![problem], None)
        }
    }
}

impl Problem {
    fn new(
        kind: Kind,
        message: String,
        source: &str,
        index: &LineIndex,
        start: usize,
        end: usize,
    ) -> Self {
        let (line, column) = index.line_column(source, start);
        let (end_line, end_column) = index.line_column(source, end.max(start));
        Problem {
            kind,
            message,
            is_error: true,
            start,
            end: end.max(start),
            line,
            column,
            end_line,
            end_column,
        }
    }

    fn at(kind: Kind, message: String, source: &str, start: usize, end: usize) -> Self {
        Problem::new(kind, message, source, &LineIndex::new(source), start, end)
    }
}
