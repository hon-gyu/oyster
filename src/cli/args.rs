//! Shared argument types for CLI commands.

use oyster::export::{
    MermaidRenderMode, NodeRenderConfig, QuiverRenderMode, TikzRenderMode,
};
use clap::Args;
use std::path::PathBuf;

/// Common arguments for site generation
#[derive(Args, Clone)]
pub struct GenerateArgs {
    /// Path to the vault directory
    pub vault_root_dir: PathBuf,

    /// CSS theme to use. Available themes: dracula, tokyonight, gruvbox, github-light, github-dark, material-light, material-dark
    #[arg(short, long, default_value = "default")]
    pub theme: String,

    /// Whether to only export notes with the publish flag set in the frontmatter
    #[arg(short, long, default_value = "true")]
    pub filter_publish: bool,

    /// Path of note to use as the home page
    #[arg(long)]
    pub home_note_path: Option<PathBuf>,

    /// Home page name
    #[arg(long)]
    pub home_name: Option<String>,

    /// Whether to render softbreaks as line breaks
    #[arg(short, long, default_value = "true")]
    pub preserve_softbreak: bool,

    /// Render mermaid diagrams using `mmdc` (build-time) or using mermaid.js (client-side)
    #[arg(short, long, default_value = "client-side")]
    pub mermaid_render_mode: String,

    /// Render tikz diagrams using `latex2pdf` and `pdf2svg` (build-time) or TikZTeX (client-side)
    #[arg(long, default_value = "client-side")]
    pub tikz_render_mode: String,

    /// Render tikz diagrams using `latex2pdf` and `pdf2svg` (build-time) or keeping raw LaTeX
    #[arg(long, default_value = "raw")]
    pub quiver_render_mode: String,

    /// Path to custom CSS file for callout customization
    #[arg(long)]
    pub custom_callout_css: Option<PathBuf>,
}

impl GenerateArgs {
    pub fn node_render_config(&self) -> NodeRenderConfig {
        let mermaid_render_mode = MermaidRenderMode::from_str(&self.mermaid_render_mode)
            .unwrap_or(MermaidRenderMode::BuildTime);
        let tikz_render_mode =
            TikzRenderMode::from_str(&self.tikz_render_mode).unwrap_or(TikzRenderMode::ClientSide);
        let quiver_render_mode =
            QuiverRenderMode::from_str(&self.quiver_render_mode).unwrap_or(QuiverRenderMode::Raw);

        NodeRenderConfig {
            preserve_softbreak: self.preserve_softbreak,
            mermaid_render_mode,
            tikz_render_mode,
            quiver_render_mode,
        }
    }
}

#[derive(Clone, Copy, clap::ValueEnum)]
pub enum QueryOutputFormat {
    Json,
    Markdown,
    Summary,
}
