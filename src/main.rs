use clap::{Parser, Subcommand};
use markdown_tools::export::{
    MermaidRenderMode, NodeRenderConfig, QuiverRenderMode, TikzRenderMode,
    render_vault,
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

        /// CSS theme to use. Available themes: dracula, tokyonight, and gruvbox
        #[arg(short, long, default_value = "default")]
        theme: String,

        /// Whether to only export notes with the publish flag set in the frontmatter
        #[arg(short, long, default_value = "true")]
        filter_publish: bool,

        /// Whether to render softbreaks as line breaks
        #[arg(short, long, default_value = "true")]
        preserve_softbreak: bool,

        /// Render mermaid diagrams using `mmdc` (build-time) or using mermaid.js (client-side)
        #[arg(short, long, default_value = "client-side")]
        mermaid_render_mode: String,

        /// Render tikz diagrams using `latex2pdf` and `pdf2svg` (build-time) or TikZTeX (client-side)
        #[arg(long, default_value = "client-side")]
        tikz_render_mode: String,

        /// Render tikz diagrams using `latex2pdf` and `pdf2svg` (build-time) or keeping raw LaTeX
        #[arg(long, default_value = "raw")]
        quiver_render_mode: String,

        /// Path to custom CSS file for callout customization
        #[arg(long)]
        custom_callout_css: Option<PathBuf>,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Generate {
            vault_root_dir,
            output: output_dir,
            theme,
            filter_publish,
            preserve_softbreak,
            mermaid_render_mode,
            tikz_render_mode,
            quiver_render_mode,
            custom_callout_css,
        } => {
            println!(
                "Generating site from vault: {}",
                vault_root_dir.display()
            );

            let mermaid_render_mode =
                MermaidRenderMode::from_str(&mermaid_render_mode)
                    .unwrap_or(MermaidRenderMode::BuildTime);
            let tikz_render_mode = TikzRenderMode::from_str(&tikz_render_mode)
                .unwrap_or(TikzRenderMode::ClientSide);
            let quiver_render_mode =
                QuiverRenderMode::from_str(&quiver_render_mode)
                    .unwrap_or(QuiverRenderMode::Raw);
            let node_render_config = NodeRenderConfig {
                preserve_softbreak,
                mermaid_render_mode,
                tikz_render_mode,
                quiver_render_mode,
            };

            render_vault(
                &vault_root_dir,
                &output_dir,
                &theme,
                filter_publish,
                &node_render_config,
                custom_callout_css.as_deref(),
            )?;

            println!("Site generated to: {}", output_dir.display());
        }
    }

    Ok(())
}
