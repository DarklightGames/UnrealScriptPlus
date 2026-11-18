//! Parsing and linting one file.

use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};

use pest::error::InputLocation;
use usagi_core::source::LineIndex;
use usagi_core::{
    Applicability, Lint, ParseMode, ParseOptions, ParsingError, ProgramError, ProgramErrorSeverity,
    parse_bytes,
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
    /// The title of the problem's fix, and whether it's safe to apply.
    pub fix: Option<(String, Applicability)>,
}

/// The result of checking one file.
pub struct FileReport {
    pub path: PathBuf,
    pub source: String,
    pub outcome: Outcome,
    pub problems: Vec<Problem>,
    /// The syntax tree, when asked for.
    pub ast: Option<String>,
    /// With fixing: the source before fixes, if any were applied.
    pub original: Option<String>,
    /// With fixing: the fixed file's bytes, in its original encoding.
    pub fixed_bytes: Option<Vec<u8>>,
    /// With fixing: the number of fixes applied.
    pub fixed: usize,
}

pub struct CheckOptions<'a> {
    pub strict: bool,
    pub lint: bool,
    pub ast: bool,
    /// Apply safe fixes (in memory; the caller writes or diffs the result).
    pub fix: bool,
    /// Whether a lint is reported for a file.
    pub is_enabled: &'a (dyn Fn(&Path, Lint) -> bool + Sync),
}

pub fn check_file(path: &Path, options: &CheckOptions) -> FileReport {
    let bytes = match std::fs::read(path) {
        Ok(bytes) => bytes,
        Err(e) => {
            return FileReport {
                path: path.to_path_buf(),
                source: String::new(),
                outcome: Outcome::IoError,
                problems: vec![Problem::at(Kind::IoError, e.to_string(), "", 0, 0)],
                ast: None,
                original: None,
                fixed_bytes: None,
                fixed: 0,
            };
        }
    };
    let original = usagi_core::decode(&bytes);
    let mut fixed_bytes = None;
    let mut fixed = 0;
    let mut source = original.clone();
    let result = panic::catch_unwind(AssertUnwindSafe(|| {
        if options.fix {
            let (bytes, count) = fix_bytes(path, &bytes, options);
            if count > 0 {
                source = usagi_core::decode(&bytes);
                fixed_bytes = Some(bytes);
                fixed = count;
            }
        }
        // Decoded again by `parse_bytes`, which is cheap next to parsing.
        check_source(
            path,
            fixed_bytes.as_deref().unwrap_or(&bytes),
            &source,
            options,
        )
    }));
    let (outcome, problems, ast) = match result {
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
        original: fixed_bytes.is_some().then_some(original),
        source,
        outcome,
        problems,
        ast,
        fixed_bytes,
        fixed,
    }
}

/// The most rounds of fixing: each round re-lints the fixed text, which
/// picks up fixes that overlapped one applied in the round before.
const MAX_FIX_ROUNDS: usize = 10;

/// Applies the safe fixes of the enabled lints until there are none left.
/// Returns the fixed bytes, in the file's own encoding, and the number of
/// fixes applied.
fn fix_bytes(path: &Path, bytes: &[u8], options: &CheckOptions) -> (Vec<u8>, usize) {
    let mode = if options.strict {
        ParseMode::Strict
    } else {
        ParseMode::Permissive
    };
    let mut bytes = bytes.to_vec();
    let mut total = 0;
    for _ in 0..MAX_FIX_ROUNDS {
        let Ok(parsed) = parse_bytes(&bytes, &ParseOptions { mode }) else {
            break;
        };
        let fixes = parsed.result.errors.iter().filter_map(|e| {
            let lint = e.lint?;
            let fix = e.fix.as_ref()?;
            (fix.applicability == Applicability::Safe && (options.is_enabled)(path, lint))
                .then_some(fix)
        });
        let (text, count) = usagi_core::fix::apply(&parsed.source.text, fixes);
        // A fix whose text the file's encoding can't store isn't applied.
        let Some(encoded) = usagi_core::encode(&text, parsed.source.encoding) else {
            break;
        };
        if count == 0 {
            break;
        }
        bytes = encoded;
        total += count;
    }
    (bytes, total)
}

fn check_source(
    path: &Path,
    bytes: &[u8],
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
        problem.fix = error
            .fix
            .as_ref()
            .map(|f| (f.title.clone(), f.applicability));
        problem
    };
    match parse_bytes(bytes, &ParseOptions { mode }) {
        Ok(parsed) => {
            let result = parsed.result;
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
            fix: None,
        }
    }

    fn at(kind: Kind, message: String, source: &str, start: usize, end: usize) -> Self {
        Problem::new(kind, message, source, &LineIndex::new(source), start, end)
    }
}
