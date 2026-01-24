//! CLI argument parsing and command dispatch.

pub mod args;
pub mod commands;

use args::{GenerateArgs, QueryOutputFormat};
use clap::{Parser, Subcommand};
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
    },

    /// Generate a static site from an Obsidian vault
    Generate {
        #[command(flatten)]
        args: GenerateArgs,

        /// Output directory for the generated site
        #[arg(short, long)]
        output: PathBuf,
    },

    /// Serve the generated site with optional live reload
    #[cfg(feature = "serve")]
    Serve {
        #[command(flatten)]
        args: GenerateArgs,

        /// Port to serve on
        #[arg(long, default_value = "3000")]
        port: u16,

        /// Watch source files and regenerate on changes
        #[arg(short, long)]
        watch: bool,
    },
}

pub fn run() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Query {
            file,
            output,
            format,
        } => commands::query::run(file, output, format),

        Commands::Generate { args, output } => commands::generate::run(args, output),

        #[cfg(feature = "serve")]
        Commands::Serve { args, port, watch } => commands::serve::run(args, port, watch),
    }
}
