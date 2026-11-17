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
use rayon::prelude::*;
use usagi_core::{Lint, ProgramErrorSeverity};

use check::{CheckOptions, FileReport};
use cli::{Cli, ColorChoice, Command, FileArgs};
use config::{ConfigError, Overrides, Settings};

/// The parser recurses deeply on some inputs.
const STACK_SIZE: usize = 256 * 1024 * 1024;

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
            let reports = check_files(&args.files.paths, &settings, false, args.ast)?;
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
            let reports = check_files(&args.files.paths, &settings, true, false)?;
            let mut out = io::stdout().lock();
            output::problems(&mut out, &reports, args.format, &cwd, color)
                .map_err(|e| e.to_string())?;
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
        is_enabled: &is_enabled,
    };
    std::panic::set_hook(Box::new(|_| {})); // panics are reported as problems
    let pool = rayon::ThreadPoolBuilder::new()
        .stack_size(STACK_SIZE)
        .build()
        .map_err(|e| e.to_string())?;
    let mut reports: Vec<FileReport> = pool.install(|| {
        files
            .par_iter()
            .map(|path| check::check_file(path, &options))
            .collect()
    });
    for report in &mut reports {
        report
            .problems
            .sort_by_key(|p| (p.start, p.end, p.kind.code()));
    }
    Ok(reports)
}
