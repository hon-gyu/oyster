use clap::{Parser, Subcommand};
use markdown_tools::export::{
    MermaidRenderMode, NodeRenderConfig, render_vault,
};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "markdown-tools")]
#[command(about = "Tools for working with Markdown(s)", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate a static site from an Obsidian vault
    Generate {
        /// Path to the vault directory
        vault_root_dir: PathBuf,

        /// Output directory for the generated site
        #[arg(short, long)]
        output: PathBuf,

        #[arg(short, long, default_value = "default")]
        theme: String,

        #[arg(short, long, default_value = "false")]
        no_filter_publish: bool,

        #[arg(short, long, default_value = "build-time")]
        mermaid_render_mode: String,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Generate {
            vault_root_dir,
            output: output_dir,
            theme,
            no_filter_publish,
            mermaid_render_mode,
        } => {
            println!(
                "Generating site from vault: {}",
                vault_root_dir.display()
            );

            let mermaid_render_mode =
                MermaidRenderMode::from_str(&mermaid_render_mode)
                    .unwrap_or(MermaidRenderMode::BuildTime);
            let node_render_config = NodeRenderConfig {
                mermaid_render_mode,
            };

            render_vault(
                &vault_root_dir,
                &output_dir,
                &theme,
                !no_filter_publish,
                &node_render_config,
            )?;

            println!("Site generated to: {}", output_dir.display());
        }
    }

    Ok(())
}
