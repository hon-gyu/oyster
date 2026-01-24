//! Query command implementataion.
use crate::cli::args::QueryOutputFormat;
use oyster::query::query_file;
use std::path::PathBuf;

pub fn run(
    file: PathBuf,
    output: Option<PathBuf>,
    format: QueryOutputFormat,
) -> Result<(), Box<dyn std::error::Error>> {
    let result = query_file(&file)?;
    let out = match format {
        QueryOutputFormat::Json => serde_json::to_string(&result)?,
        QueryOutputFormat::Markdown => result.to_src(),
        QueryOutputFormat::Summary => result.to_string(),
    };

    if let Some(output_path) = output {
        std::fs::write(&output_path, &out)?;
        eprintln!("Output written to: {}", output_path.display());
    } else {
        println!("{}", out);
    }

    Ok(())
}
