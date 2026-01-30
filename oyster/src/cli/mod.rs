//! CLI argument parsing and command dispatch.

pub mod args;
pub mod commands;

use args::{BuildArgs, QueryOutputFormat};
use clap::{CommandFactory, FromArgMatches, Parser, Subcommand};
use oyster_lib::cli::extract_ordered_exprs;
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
        /// Extract the Nth code block's content (0-indexed)
        #[arg(long)]
        code: Vec<i64>,
        /// Extract the Nth code block as JSON metadata (0-indexed)
        #[arg(long)]
        codemeta: Vec<i64>,
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
