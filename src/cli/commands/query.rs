//! Query command implementataion.
use oyster::query::{Expr, Markdown, eval};

use crate::cli::args::QueryOutputFormat;
use std::path::PathBuf;

pub fn run(
    file: PathBuf,
    output: Option<PathBuf>,
    format: QueryOutputFormat,
    exprs: Vec<Expr>,
) -> Result<(), Box<dyn std::error::Error>> {
    let md = Markdown::from_path(&file)?;

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
    let out = match format {
        QueryOutputFormat::Json => serde_json::to_string(&result)?,
        QueryOutputFormat::Summary => result.to_string(),
        QueryOutputFormat::Markdown => result.to_src(),
    };

    if let Some(ref output_path) = output {
        std::fs::write(output_path, &out)?;
        eprintln!("Output written to: {}", output_path.display());
    } else {
        println!("{}", out);
    }

    Ok(())
}

fn pipe_exprs(exprs: Vec<Expr>) -> Expr {
    let mut iter = exprs.into_iter();
    let first = iter.next().unwrap();
    iter.fold(first, |acc, expr| Expr::Pipe(Box::new(acc), Box::new(expr)))
}
