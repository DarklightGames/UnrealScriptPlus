//! Command-line arguments.

use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Debug, Parser)]
#[command(
    name = "usagi",
    version,
    about = "Parse and lint Unreal Engine 2 UnrealScript, and build mods"
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

    /// How many files to parse, or UCC processes to run, at once. Default:
    /// half the logical CPUs.
    #[arg(long, short, global = true, value_name = "N", value_parser = clap::value_parser!(u16).range(1..))]
    pub jobs: Option<u16>,

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
    /// Inspect package files (`.u`, `.utx`, `.ut2`, ...).
    Pkg(PkgArgs),
}

#[derive(Debug, Args)]
pub struct PkgArgs {
    #[command(subcommand)]
    pub command: PkgCommand,
}

#[derive(Debug, Subcommand)]
pub enum PkgCommand {
    /// Print a package's summary (its version, flags, GUID, generations and
    /// where its tables are) and the packages it depends on; with
    /// `--imports`/`--exports`, its imports and exports.
    List(PkgListArgs),
}

#[derive(Debug, Args)]
pub struct PkgListArgs {
    /// The package file. A `.uz2` file is decompressed first.
    pub path: PathBuf,

    /// List the packages this one depends on: the top-level packages of its
    /// imports. `--dependencies=objects` also lists the objects referenced in
    /// each, by group. This is the default when no table is asked for.
    #[arg(
        long,
        value_enum,
        value_name = "LEVEL",
        num_args = 0..=1,
        require_equals = true,
        default_missing_value = "packages"
    )]
    pub dependencies: Option<PkgDependencies>,

    /// List this package's groups (subpackages), with the objects in each.
    #[arg(long)]
    pub groups: bool,

    /// List the imports: the objects in other packages that this one refers
    /// to.
    #[arg(long)]
    pub imports: bool,

    /// List the exports: the objects in this package.
    #[arg(long)]
    pub exports: bool,

    /// List the dependencies, the groups, the imports and the exports.
    #[arg(long)]
    pub all: bool,

    /// Don't print the summary.
    #[arg(long)]
    pub no_summary: bool,

    /// How to sort the tables. `size` puts the largest first; the others
    /// sort in ascending order. Ties keep table order.
    #[arg(long, value_enum, default_value_t = PkgSort::Index)]
    pub sort: PkgSort,

    /// Reverse the sort order.
    #[arg(long)]
    pub reverse: bool,

    /// Show sizes in units (`1.3 MB`) and counts with thousands separators
    /// (`15,530`), in the text format.
    #[arg(long)]
    pub humanize: bool,

    /// How to print the package.
    #[arg(long, value_enum, default_value_t = PkgFormat::Text)]
    pub format: PkgFormat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum PkgDependencies {
    /// The packages only.
    Packages,
    /// The packages, and the objects referenced in each.
    Objects,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum PkgSort {
    /// Table order.
    Index,
    /// Full path (or package name), ignoring case.
    Path,
    /// Class name, ignoring case. Dependencies keep table order.
    Class,
    /// Size of the export's data, or the number of objects imported from a
    /// dependency; largest first. Imports keep table order.
    Size,
    /// File offset of the export's data; exports with no data last. Imports
    /// and dependencies keep table order.
    Offset,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum PkgFormat {
    /// Readable text: the summary and tables.
    Text,
    /// Names only, one per line: the dependencies' names (and with
    /// `--dependencies=objects`, their objects' paths), and the groups',
    /// imports' and exports' paths, without the summary.
    Concise,
    /// A JSON object.
    Json,
}

#[derive(Debug, Args)]
pub struct BuildArgs {
    /// The mod to build (overrides `build.mod`).
    #[arg(long = "mod", value_name = "NAME")]
    pub mod_name: Option<String>,

    /// Build without a mod, even if `build.mod` is set.
    #[arg(long, conflicts_with = "mod_name")]
    pub no_mod: bool,

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
