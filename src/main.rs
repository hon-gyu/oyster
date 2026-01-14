use clap::{Args, Parser, Subcommand};
use oyster::export::{
    MermaidRenderMode, NodeRenderConfig, QuiverRenderMode, TikzRenderMode,
    render_vault,
};
use oyster::query::query_file;
#[cfg(feature = "serve")]
use oyster::serve::{ServeConfig, serve_site};
use std::path::PathBuf;
#[cfg(feature = "serve")]
use tempfile;

#[derive(Parser)]
#[command(name = "oyster")]
#[command(about = "Tools for working with Markdown(s)", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

/// Common arguments for site generation
#[derive(Args, Clone)]
struct GenerateArgs {
    /// Path to the vault directory
    vault_root_dir: PathBuf,

    /// CSS theme to use. Available themes: dracula, tokyonight, and gruvbox
    #[arg(short, long, default_value = "default")]
    theme: String,

    /// Whether to only export notes with the publish flag set in the frontmatter
    #[arg(short, long, default_value = "true")]
    filter_publish: bool,

    /// Path of note to use as the home page
    #[arg(long)]
    home_note_path: Option<PathBuf>,

    /// Home page name
    #[arg(long)]
    home_name: Option<String>,

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
}

impl GenerateArgs {
    fn node_render_config(&self) -> NodeRenderConfig {
        let mermaid_render_mode =
            MermaidRenderMode::from_str(&self.mermaid_render_mode)
                .unwrap_or(MermaidRenderMode::BuildTime);
        let tikz_render_mode = TikzRenderMode::from_str(&self.tikz_render_mode)
            .unwrap_or(TikzRenderMode::ClientSide);
        let quiver_render_mode =
            QuiverRenderMode::from_str(&self.quiver_render_mode)
                .unwrap_or(QuiverRenderMode::Raw);

        NodeRenderConfig {
            preserve_softbreak: self.preserve_softbreak,
            mermaid_render_mode,
            tikz_render_mode,
            quiver_render_mode,
        }
    }

    fn generate(
        &self,
        output: &PathBuf,
    ) -> Result<String, Box<dyn std::error::Error>> {
        println!(
            "Generating site from vault: {}",
            self.vault_root_dir.display()
        );

        let home_slug = render_vault(
            &self.vault_root_dir,
            output,
            &self.theme,
            self.filter_publish,
            self.home_note_path.as_deref(),
            self.home_name.as_deref(),
            &self.node_render_config(),
            self.custom_callout_css.as_deref(),
        )?;

        println!("Site generated to: {}", output.display());
        Ok(home_slug)
    }
}

#[derive(Subcommand)]
enum Commands {
    Query {
        /// The path of the file to query
        file: PathBuf,
        /// Output file to write the query result to
        #[arg(short, long)]
        output: Option<PathBuf>,
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Query { file, output } => {
            let result = query_file(&file).map_err(|e| e)?;
            let json = serde_json::to_string_pretty(&result)?;

            if let Some(output_path) = output {
                std::fs::write(&output_path, &json)?;
                eprintln!("Output written to: {}", output_path.display());
            } else {
                println!("{}", json);
            }
        }

        Commands::Generate { args, output } => {
            args.generate(&output)?;
        }

        #[cfg(feature = "serve")]
        Commands::Serve { args, port, watch } => {
            // Use temp directory for output
            let temp_dir = tempfile::tempdir()?;
            let output_dir = temp_dir.path().to_path_buf();

            // Initial build - returns the home slug
            let home_slug = args.generate(&output_dir)?;

            // Build config before moving args
            let node_render_config = args.node_render_config();

            // Start server (temp_dir is kept alive until server stops)
            let rt = tokio::runtime::Runtime::new()?;
            rt.block_on(async {
                let config = ServeConfig {
                    vault_root_dir: args.vault_root_dir,
                    output_dir,
                    port,
                    watch,
                    theme: args.theme,
                    filter_publish: args.filter_publish,
                    home_note_path: args.home_note_path,
                    home_name: args.home_name,
                    home_slug,
                    node_render_config,
                    custom_callout_css: args.custom_callout_css,
                };
                serve_site(config).await
            })?;

            // temp_dir is dropped here, cleaning up the temp directory
        }
    }

    Ok(())
}
