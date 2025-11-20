use clap::{Parser, Subcommand};
use markdown_tools::export::{generate_site, SiteConfig};
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
        #[arg(short, long, default_value = "/")]
        base_url: String,

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

            let config = SiteConfig {
                title,
                base_url,
                output_dir: output,
                generate_backlinks: !no_backlinks,
            };

            generate_site(&vault_path, &config)?;
        }
    }

    Ok(())
}
