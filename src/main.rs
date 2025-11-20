use clap::{Parser, Subcommand};
use markdown_tools::export::{SiteConfig, generate_site};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "markdown-tools")]
#[command(about = "Tools for working with Markdown and Obsidian vaults", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate a static site from an Obsidian vault
    Generate {
        /// Path to the vault directory
        vault_path: PathBuf,

        /// Output directory for the generated site
        #[arg(short, long, default_value = "dist")]
        output: PathBuf,

        /// Site title
        #[arg(short, long, default_value = "My Knowledge Base")]
        title: String,

        /// Base URL for the site
        #[arg(short, long, default_value = None)]
        base_url: Option<String>,

        /// Disable backlinks generation
        #[arg(long)]
        no_backlinks: bool,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Generate {
            vault_path,
            output,
            title,
            base_url,
            no_backlinks,
        } => {
            println!("Generating site from vault: {}", vault_path.display());

            // Default to empty string for relative paths (best for local dev)
            let url = base_url.unwrap_or_else(|| String::new());

            let config = SiteConfig {
                title,
                base_url: url,
                output_dir: output,
                generate_backlinks: !no_backlinks,
            };

            generate_site(&vault_path, &config)?;
        }
    }

    Ok(())
}
