use clap::{CommandFactory, FromArgMatches, Parser};
use oyster_lib::cli::{extract_ordered_exprs, pipe_exprs};
use oyster_lib::query::{Markdown, eval};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "mdq")]
#[command(about = "Query and extract data from Markdown files", long_about = None)]
struct Cli {
    /// The path of the file to query
    file: PathBuf,

    /// Output file to write the query result to
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Output format
    #[arg(short, long, default_value = "json")]
    format: OutputFormat,

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
}

#[derive(Clone, Copy, clap::ValueEnum)]
enum OutputFormat {
    Json,
    Markdown,
    Summary,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let matches = Cli::command().get_matches();
    let cli = Cli::from_arg_matches(&matches)?;

    let md = Markdown::from_path(&cli.file)?;

    let exprs = extract_ordered_exprs(&matches);
    let results = if exprs.is_empty() {
        vec![md]
    } else {
        let expr = pipe_exprs(exprs);
        eval(expr, &md).map_err(|e| format!("{:?}", e))?
    };

    if results.len() != 1 {
        return Err(
            "Never: Expected 1 result. Comma is not supported yet.".into()
        );
    }

    let result = &results[0];
    let out = match cli.format {
        OutputFormat::Json => serde_json::to_string(&result)?,
        OutputFormat::Summary => result.to_string(),
        OutputFormat::Markdown => result.to_src(),
    };

    if let Some(ref output_path) = cli.output {
        std::fs::write(output_path, &out)?;
        eprintln!("Output written to: {}", output_path.display());
    } else {
        println!("{}", out);
    }

    Ok(())
}
