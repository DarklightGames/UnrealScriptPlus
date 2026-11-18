//! `usagi`: parse and lint Unreal Engine 2 UnrealScript.

mod check;
mod cli;
mod config;
mod discover;
mod output;

use std::io::{self, IsTerminal, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use usagi_core::{Applicability, Lint, ProgramErrorSeverity};

use check::{CheckOptions, FileReport};
use cli::{Cli, ColorChoice, Command, FileArgs, OutputFormat};
use config::{ConfigError, Overrides, Settings};

fn main() -> ExitCode {
    let cli = Cli::parse();
    match run(cli) {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::from(1),
        Err(message) => {
            eprintln!("usagi: {}", message);
            ExitCode::from(2)
        }
    }
}

/// Runs a command. Returns whether it found nothing to report.
fn run(cli: Cli) -> Result<bool, String> {
    let cwd = std::env::current_dir().map_err(|e| e.to_string())?;
    let color = match cli.color {
        ColorChoice::Always => true,
        ColorChoice::Never => false,
        ColorChoice::Auto => io::stdout().is_terminal() && std::env::var_os("NO_COLOR").is_none(),
    };
    let load =
        || config::load(cli.config.as_deref(), cli.no_config, &cwd).map_err(|ConfigError(e)| e);
    match &cli.command {
        Command::Parse(args) => {
            let config = load()?;
            let settings = settings(&config, &args.files, Overrides::default(), &cwd)?;
            let reports = check_files(&args.files.paths, &settings, false, args.ast, false)?;
            let mut out = io::stdout().lock();
            if args.ast {
                for report in &reports {
                    if let Some(ast) = &report.ast {
                        writeln!(out, "{}\n{}", discover::display(&report.path, &cwd), ast)
                            .map_err(|e| e.to_string())?;
                    }
                }
            }
            output::problems(&mut out, &reports, args.format, &cwd, color)
                .map_err(|e| e.to_string())?;
            if args.statistics {
                output::outcome_statistics(&mut out, &reports).map_err(|e| e.to_string())?;
            }
            Ok(!reports.iter().any(|r| r.outcome.is_failure()))
        }
        Command::Lint(args) => {
            let config = load()?;
            let overrides = Overrides {
                select: args.select.clone(),
                extend_select: args.extend_select.clone(),
                ignore: args.ignore.clone(),
                ..Overrides::default()
            };
            let settings = settings(&config, &args.files, overrides, &cwd)?;
            let fixing = args.fix || args.diff;
            let reports = check_files(&args.files.paths, &settings, true, false, fixing)?;
            let mut out = io::stdout().lock();
            let fixed: usize = reports.iter().map(|r| r.fixed).sum();
            let fixed_files = reports.iter().filter(|r| r.fixed > 0).count();
            if args.diff {
                output::diffs(&mut out, &reports, &cwd).map_err(|e| e.to_string())?;
                eprintln!(
                    "Would fix {} {} in {} {}",
                    fixed,
                    plural(fixed, "problem"),
                    fixed_files,
                    plural(fixed_files, "file")
                );
                return Ok(fixed == 0);
            }
            if args.fix {
                for report in &reports {
                    if let Some(bytes) = &report.fixed_bytes {
                        std::fs::write(&report.path, bytes).map_err(|e| {
                            format!("{}: {}", discover::display(&report.path, &cwd), e)
                        })?;
                    }
                }
            }
            output::problems(&mut out, &reports, args.format, &cwd, color)
                .map_err(|e| e.to_string())?;
            if args.fix {
                eprintln!(
                    "Fixed {} {} in {} {}",
                    fixed,
                    plural(fixed, "problem"),
                    fixed_files,
                    plural(fixed_files, "file")
                );
            } else {
                let fixable = reports
                    .iter()
                    .flat_map(|r| &r.problems)
                    .filter(|p| matches!(&p.fix, Some((_, Applicability::Safe))))
                    .count();
                if fixable > 0 && !matches!(args.format, OutputFormat::Json | OutputFormat::Tsv) {
                    eprintln!(
                        "{} {} can be fixed with `usagi lint --fix` (preview with `--diff`)",
                        fixable,
                        plural(fixable, "problem")
                    );
                }
            }
            if args.statistics {
                output::lint_statistics(&mut out, &reports).map_err(|e| e.to_string())?;
            }
            Ok(!reports.iter().any(|r| {
                r.outcome.is_failure()
                    || r.problems
                        .iter()
                        .any(|p| matches!(p.kind, check::Kind::Lint(_)))
            }))
        }
        Command::Rule(args) => {
            let mut out = io::stdout().lock();
            if args.all {
                for &lint in Lint::ALL {
                    writeln!(
                        out,
                        "{}  {:40} {}",
                        lint.code(),
                        lint.name(),
                        lint.summary()
                    )
                    .map_err(|e| e.to_string())?;
                }
                return Ok(true);
            }
            let name = args.lint.as_deref().unwrap_or_default();
            let lint = Lint::find(name).ok_or_else(|| format!("unknown lint {:?}", name))?;
            let severity = match lint.severity() {
                ProgramErrorSeverity::Error => "error",
                ProgramErrorSeverity::Warning => "warning",
            };
            writeln!(
                out,
                "{} {}\ncategory: {}\nseverity: {}\n\n{}",
                lint.code(),
                lint.name(),
                lint.category(),
                severity,
                lint.summary()
            )
            .map_err(|e| e.to_string())?;
            Ok(true)
        }
        Command::Format(_) => Err("format is not implemented yet".to_string()),
    }
}

fn plural(count: usize, word: &str) -> String {
    if count == 1 {
        word.to_string()
    } else {
        format!("{}s", word)
    }
}

fn settings(
    config: &config::LoadedConfig,
    files: &FileArgs,
    overrides: Overrides,
    cwd: &Path,
) -> Result<Settings, String> {
    let overrides = Overrides {
        include: files.include.clone(),
        exclude: files.exclude.clone(),
        strict: files.strict(),
        ..overrides
    };
    Settings::new(config, overrides, cwd).map_err(|ConfigError(e)| e)
}

/// Checks every file under `paths`, in parallel. Reports come back sorted
/// by path, with each file's problems sorted by position.
fn check_files(
    paths: &[PathBuf],
    settings: &Settings,
    lint: bool,
    ast: bool,
    fix: bool,
) -> Result<Vec<FileReport>, String> {
    let default = [PathBuf::from(".")];
    let paths = if paths.is_empty() {
        &default[..]
    } else {
        paths
    };
    let (files, errors) = discover::files(paths, settings);
    if let Some(error) = errors.first() {
        return Err(error.clone());
    }
    let is_enabled = |path: &Path, lint: Lint| {
        let absolute = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
        settings.is_enabled(&absolute, lint)
    };
    let options = CheckOptions {
        strict: settings.strict,
        lint,
        ast,
        fix,
        is_enabled: &is_enabled,
    };
    check::check_all(&files, &options)
}
