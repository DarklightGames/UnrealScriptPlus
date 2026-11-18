//! Command-line arguments.

use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Debug, Parser)]
#[command(
    name = "usagi",
    version,
    about = "Parse and lint Unreal Engine 2 UnrealScript"
)]
pub struct Cli {
    /// Use this configuration file instead of searching for one.
    #[arg(long, global = true, value_name = "FILE", conflicts_with = "no_config")]
    pub config: Option<PathBuf>,

    /// Don't read any configuration file.
    #[arg(long, global = true)]
    pub no_config: bool,

    /// When to use colour.
    #[arg(long, global = true, value_enum, default_value_t = ColorChoice::Auto)]
    pub color: ColorChoice,

    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Parse files and report syntax errors and recovered problems.
    Parse(ParseArgs),
    /// Parse files and report lint problems.
    Lint(LintArgs),
    /// Explain a lint, or list every lint.
    Rule(RuleArgs),
    /// Format files (not implemented yet).
    Format(FormatArgs),
    /// Compile a mod's out-of-date packages with UCC.
    Build(BuildArgs),
}

#[derive(Debug, Args)]
pub struct BuildArgs {
    /// The mod to build (overrides `build.mod`).
    #[arg(long = "mod", value_name = "NAME")]
    pub mod_name: Option<String>,

    /// The game directory (overrides `build.root`).
    #[arg(long, value_name = "DIR")]
    pub root: Option<PathBuf>,

    /// Rebuild every package and regenerate localization.
    #[arg(long)]
    pub clean: bool,

    /// Export localization (`.int`) files for the packages that changed.
    #[arg(long)]
    pub dumpint: bool,

    /// Compile debug packages, for UDebugger.
    #[arg(long)]
    pub debug: bool,

    /// Don't rebuild the packages after one that changed.
    #[arg(long)]
    pub ignore_dependencies: bool,

    /// Don't parse the packages before compiling them.
    #[arg(long)]
    pub no_check: bool,

    /// Print all of UCC's output.
    #[arg(long, short)]
    pub verbose: bool,
}

#[derive(Debug, Args)]
pub struct FileArgs {
    /// Files or directories to check. Directories are searched for `.uc`
    /// files. Defaults to the current directory.
    pub paths: Vec<PathBuf>,

    /// Only check files matching these globs (replaces the configured list).
    #[arg(long, value_name = "GLOB", value_delimiter = ',')]
    pub include: Vec<String>,

    /// Skip files matching these globs (replaces the configured list).
    #[arg(long, value_name = "GLOB", value_delimiter = ',')]
    pub exclude: Vec<String>,

    /// Treat problems that permissive parsing recovers from as errors.
    #[arg(long, overrides_with = "no_strict")]
    pub strict: bool,

    /// Recover from malformed input the way UE2 does (the default).
    #[arg(long, hide = true)]
    pub no_strict: bool,
}

impl FileArgs {
    /// The `--strict`/`--no-strict` choice, if one was given.
    pub fn strict(&self) -> Option<bool> {
        match (self.strict, self.no_strict) {
            (true, _) => Some(true),
            (_, true) => Some(false),
            _ => None,
        }
    }
}

#[derive(Debug, Args)]
pub struct ParseArgs {
    #[command(flatten)]
    pub files: FileArgs,

    /// Print the syntax tree of each file.
    #[arg(long)]
    pub ast: bool,

    /// Print how many files had each outcome.
    #[arg(long)]
    pub statistics: bool,

    /// How to print problems.
    #[arg(long, value_enum, default_value_t = OutputFormat::Full)]
    pub format: OutputFormat,
}

#[derive(Debug, Args)]
pub struct LintArgs {
    #[command(flatten)]
    pub files: FileArgs,

    /// Lints to report (replaces the configured list). Each is `ALL`, a code
    /// or code prefix, a lint name or a category name.
    #[arg(long, value_name = "LINT", value_delimiter = ',')]
    pub select: Vec<String>,

    /// Lints to report in addition to the selected ones.
    #[arg(long, value_name = "LINT", value_delimiter = ',')]
    pub extend_select: Vec<String>,

    /// Lints not to report (added to the configured list).
    #[arg(long, value_name = "LINT", value_delimiter = ',')]
    pub ignore: Vec<String>,

    /// How to print problems.
    #[arg(long, value_enum, default_value_t = OutputFormat::Full)]
    pub format: OutputFormat,

    /// Print how many problems each lint found.
    #[arg(long)]
    pub statistics: bool,

    /// Apply safe fixes to the files.
    #[arg(long)]
    pub fix: bool,

    /// Print the changes that safe fixes would make, as a diff, without
    /// changing any file.
    #[arg(long, conflicts_with = "fix")]
    pub diff: bool,
}

#[derive(Debug, Args)]
pub struct RuleArgs {
    /// A lint code or name, e.g. `S003` or `self-assignment`.
    #[arg(required_unless_present = "all")]
    pub lint: Option<String>,

    /// List every lint.
    #[arg(long, conflicts_with = "lint")]
    pub all: bool,
}

#[derive(Debug, Args)]
pub struct FormatArgs {
    pub paths: Vec<PathBuf>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum OutputFormat {
    /// Each problem with its source line.
    Full,
    /// One line per problem: `path:line:column: CODE message`.
    Concise,
    /// A JSON array of problems.
    Json,
    /// Tab-separated: code, name, path, line, message.
    Tsv,
    /// Nothing; only statistics and the exit code.
    Quiet,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum ColorChoice {
    Auto,
    Always,
    Never,
}
