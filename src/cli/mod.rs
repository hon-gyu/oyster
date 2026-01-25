//! CLI argument parsing and command dispatch.

pub mod args;
pub mod commands;

use args::{BuildArgs, QueryOutputFormat};
use clap::{ArgMatches, CommandFactory, FromArgMatches, Parser, Subcommand};
use oyster::query::Expr;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "oyster")]
#[command(about = "Tools for working with Markdown(s)", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Query {
        /// The path of the file to query
        file: PathBuf,
        /// Output file to write the query result to
        #[arg(short, long)]
        output: Option<PathBuf>,
        // Output format
        #[arg(short, long, default_value = "json")]
        format: QueryOutputFormat,

        // -- Expression arguments (piped in order of appearance) --
        /// Select a section by title
        #[arg(long)]
        field: Vec<String>,
        /// Select a child section by index
        #[arg(long)]
        index: Vec<i64>,
        /// Select a slice of child sections (e.g., "0:2")
        #[arg(long)]
        slice: Vec<String>,
        /// Get the title of a child section by index
        #[arg(long)]
        title: Vec<i64>,
        /// Output a summary tree
        #[arg(long, action = clap::ArgAction::Count)]
        summary: u8,
        /// Count the number of child sections
        #[arg(long, action = clap::ArgAction::Count)]
        nchildren: u8,
        /// Extract the frontmatter
        #[arg(long, action = clap::ArgAction::Count)]
        frontmatter: u8,
        /// Strip the frontmatter, output only sections
        #[arg(long, action = clap::ArgAction::Count)]
        body: u8,
        /// Extract the preface (content before first section)
        #[arg(long, action = clap::ArgAction::Count)]
        preface: u8,
        /// Check if a section with the given title exists
        #[arg(long)]
        has: Vec<String>,
        /// Delete a section by title
        #[arg(long)]
        delete: Vec<String>,
        /// Increment heading levels by delta
        #[arg(long)]
        inc: Vec<i64>,
        /// Decrement heading levels by delta
        #[arg(long)]
        dec: Vec<i64>,
    },

    /// Generate a static site from an Obsidian vault
    Build {
        #[command(flatten)]
        args: BuildArgs,

        /// Output directory for the generated site
        #[arg(short, long)]
        output: PathBuf,
    },

    /// Serve the generated site with optional live reload
    #[cfg(feature = "serve")]
    Serve {
        #[command(flatten)]
        args: BuildArgs,

        /// Port to serve on
        #[arg(long, default_value = "3000")]
        port: u16,

        /// Watch source files and regenerate on changes
        #[arg(short, long)]
        watch: bool,
    },
}

pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    let matches = Cli::command().get_matches();
    let cli = Cli::from_arg_matches(&matches)?;

    match cli.command {
        Commands::Query {
            file,
            output,
            format,
            ..
        } => {
            let sub_matches = matches.subcommand_matches("query").unwrap();
            let exprs = extract_ordered_exprs(sub_matches);
            commands::query::run(file, output, format, exprs)
        }

        Commands::Build { args, output } => commands::build::run(args, output),

        #[cfg(feature = "serve")]
        Commands::Serve { args, port, watch } => {
            commands::serve::run(args, port, watch)
        }
    }
}

/// Extract expression arguments from ArgMatches, sorted by their
/// position in the original argument list so that piping order is
/// preserved.
fn extract_ordered_exprs(matches: &ArgMatches) -> Vec<Expr> {
    let mut indexed: Vec<(usize, Expr)> = Vec::new();

    // Value-taking repeated args
    collect_string_arg(matches, "field", &mut indexed, Expr::Field);
    collect_i64_arg(matches, "index", &mut indexed, |n| {
        Expr::Index(n as isize)
    });
    collect_slice_arg(matches, &mut indexed);
    collect_i64_arg(matches, "title", &mut indexed, |n| {
        Expr::Title(n as isize)
    });
    collect_string_arg(matches, "has", &mut indexed, Expr::Has);
    collect_string_arg(matches, "delete", &mut indexed, Expr::Del);
    collect_i64_arg(matches, "inc", &mut indexed, |n| Expr::Inc(n as isize));
    collect_i64_arg(matches, "dec", &mut indexed, |n| Expr::Dec(n as isize));

    // Boolean flags
    collect_flag(matches, "summary", &mut indexed, Expr::Summary);
    collect_flag(matches, "nchildren", &mut indexed, Expr::NChildren);
    collect_flag(matches, "frontmatter", &mut indexed, Expr::Frontmatter);
    collect_flag(matches, "body", &mut indexed, Expr::Body);
    collect_flag(matches, "preface", &mut indexed, Expr::Preface);

    indexed.sort_by_key(|(idx, _)| *idx);
    indexed.into_iter().map(|(_, expr)| expr).collect()
}

fn collect_string_arg(
    matches: &ArgMatches,
    name: &str,
    out: &mut Vec<(usize, Expr)>,
    f: fn(String) -> Expr,
) {
    if let (Some(indices), Some(values)) =
        (matches.indices_of(name), matches.get_many::<String>(name))
    {
        for (idx, val) in indices.zip(values) {
            out.push((idx, f(val.clone())));
        }
    }
}

fn collect_i64_arg(
    matches: &ArgMatches,
    name: &str,
    out: &mut Vec<(usize, Expr)>,
    f: fn(i64) -> Expr,
) {
    if let (Some(indices), Some(values)) =
        (matches.indices_of(name), matches.get_many::<i64>(name))
    {
        for (idx, val) in indices.zip(values) {
            out.push((idx, f(*val)));
        }
    }
}

fn collect_flag(
    matches: &ArgMatches,
    name: &str,
    out: &mut Vec<(usize, Expr)>,
    expr: Expr,
) {
    if matches.get_count(name) > 0 {
        if let Some(indices) = matches.indices_of(name) {
            for idx in indices {
                out.push((idx, expr.clone()));
            }
        }
    }
}

fn collect_slice_arg(matches: &ArgMatches, out: &mut Vec<(usize, Expr)>) {
    if let (Some(indices), Some(values)) = (
        matches.indices_of("slice"),
        matches.get_many::<String>("slice"),
    ) {
        for (idx, val) in indices.zip(values) {
            out.push((idx, parse_slice(val)));
        }
    }
}

fn parse_slice(s: &str) -> Expr {
    let parts: Vec<&str> = s.splitn(2, ':').collect();
    let start = parts.first().and_then(|p| {
        let trimmed = p.trim();
        if trimmed.is_empty() {
            None
        } else {
            trimmed.parse::<isize>().ok()
        }
    });
    let end = parts.get(1).and_then(|p| {
        let trimmed = p.trim();
        if trimmed.is_empty() {
            None
        } else {
            trimmed.parse::<isize>().ok()
        }
    });
    Expr::Slice(start, end)
}
