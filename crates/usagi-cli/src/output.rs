//! Printing problems and statistics.

use std::collections::{BTreeMap, HashSet};
use std::io::{self, Write};
use std::path::Path;

use annotate_snippets::{Level, Renderer, Snippet};
use serde::Serialize;
use usagi_core::source::LineIndex;

use crate::check::{FileReport, Kind, Outcome, Problem};
use crate::cli::OutputFormat;
use crate::discover;

#[derive(Serialize)]
struct JsonProblem<'a> {
    path: String,
    line: usize,
    column: usize,
    end_line: usize,
    end_column: usize,
    code: &'a str,
    name: &'a str,
    severity: &'a str,
    message: &'a str,
}

/// Prints every problem in `reports`, which must already be sorted.
pub fn problems(
    out: &mut dyn Write,
    reports: &[FileReport],
    format: OutputFormat,
    cwd: &Path,
    color: bool,
) -> io::Result<()> {
    let all = || {
        reports.iter().flat_map(|r| {
            let path = discover::display(&r.path, cwd);
            r.problems.iter().map(move |p| (r, path.clone(), p))
        })
    };
    match format {
        OutputFormat::Quiet => {}
        OutputFormat::Full => {
            let renderer = if color {
                Renderer::styled()
            } else {
                Renderer::plain()
            };
            for (report, path, problem) in all() {
                writeln!(out, "{}\n", render(&renderer, report, &path, problem))?;
            }
        }
        OutputFormat::Concise => {
            for (_, path, p) in all() {
                writeln!(
                    out,
                    "{}:{}:{}: {} {}",
                    path,
                    p.line,
                    p.column,
                    p.kind.code(),
                    one_line(&p.message)
                )?;
            }
        }
        OutputFormat::Tsv => {
            for (_, path, p) in all() {
                writeln!(
                    out,
                    "{}\t{}\t{}\t{}\t{}",
                    p.kind.code(),
                    p.kind.name(),
                    path,
                    p.line,
                    one_line(&p.message)
                )?;
            }
        }
        OutputFormat::Json => {
            let problems: Vec<_> = all()
                .map(|(_, path, p)| JsonProblem {
                    path,
                    line: p.line,
                    column: p.column,
                    end_line: p.end_line,
                    end_column: p.end_column,
                    code: p.kind.code(),
                    name: p.kind.name(),
                    severity: if p.is_error { "error" } else { "warning" },
                    message: &p.message,
                })
                .collect();
            serde_json::to_writer_pretty(&mut *out, &problems)?;
            writeln!(out)?;
        }
    }
    Ok(())
}

fn render(renderer: &Renderer, report: &FileReport, path: &str, problem: &Problem) -> String {
    let level = if problem.is_error {
        Level::Error
    } else {
        Level::Warning
    };
    let title = match problem.kind {
        Kind::Lint(lint) => format!("{} [{}]", problem.message, lint.name()),
        _ => problem.message.clone(),
    };
    let source = &report.source;
    let message = level.title(&title).id(problem.kind.code());
    if source.is_empty() {
        return renderer.render(message).to_string();
    }
    // Show only the lines the problem covers.
    let index = LineIndex::new(source);
    let first = index.line_range(source, problem.line);
    let last = index.line_range(source, problem.end_line.max(problem.line));
    let text = &source[first.start..last.end];
    let start = problem.start.clamp(first.start, last.end) - first.start;
    let end = problem.end.clamp(first.start, last.end) - first.start;
    let end = if end > start {
        end
    } else {
        // Point at one character, or past the end of an empty line.
        text[start..]
            .chars()
            .next()
            .map_or(start, |c| start + c.len_utf8())
    };
    let snippet = Snippet::source(text)
        .line_start(problem.line)
        .origin(path)
        .annotation(level.span(start..end));
    renderer.render(message.snippet(snippet)).to_string()
}

fn one_line(s: &str) -> String {
    s.replace(['\t', '\r', '\n'], " ")
}

/// Prints how many files had each outcome.
pub fn outcome_statistics(out: &mut dyn Write, reports: &[FileReport]) -> io::Result<()> {
    let mut counts: BTreeMap<Outcome, usize> = BTreeMap::new();
    for report in reports {
        *counts.entry(report.outcome).or_default() += 1;
    }
    writeln!(out, "{} files", reports.len())?;
    for (outcome, count) in counts {
        writeln!(out, "  {:14} {:>6}", outcome.name(), count)?;
    }
    Ok(())
}

/// Prints how many problems, in how many files, each lint found.
pub fn lint_statistics(out: &mut dyn Write, reports: &[FileReport]) -> io::Result<()> {
    // (code, name) -> (problems, files)
    let mut counts: BTreeMap<(&str, &str), (usize, usize)> = BTreeMap::new();
    for report in reports {
        let mut seen = HashSet::new();
        for problem in &report.problems {
            let key = (problem.kind.code(), problem.kind.name());
            let entry = counts.entry(key).or_default();
            entry.0 += 1;
            if seen.insert(key) {
                entry.1 += 1;
            }
        }
    }
    for ((code, name), (problems, files)) in counts {
        writeln!(
            out,
            "{:6} {:40} {:>8} problems in {:>6} files",
            code, name, problems, files
        )?;
    }
    Ok(())
}
