use clap::{ArgMatches, CommandFactory, FromArgMatches, Parser};
use oyster_lib::query::{Expr, Markdown, eval};
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
    collect_i64_arg(matches, "code", &mut indexed, |n| Expr::Code(n as isize));
    collect_i64_arg(matches, "codemeta", &mut indexed, |n| {
        Expr::CodeMeta(n as isize)
    });

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

fn pipe_exprs(exprs: Vec<Expr>) -> Expr {
    let mut iter = exprs.into_iter();
    let first = iter.next().unwrap();
    iter.fold(first, |acc, expr| Expr::Pipe(Box::new(acc), Box::new(expr)))
}
