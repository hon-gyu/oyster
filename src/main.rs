use axum::Router;
use clap::{Parser, Subcommand};
use notify_debouncer_mini::{DebouncedEventKind, new_debouncer};
use oyster::export::{
    MermaidRenderMode, NodeRenderConfig, QuiverRenderMode, TikzRenderMode, render_vault,
};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;
use tower_http::services::ServeDir;
use tower_livereload::LiveReloadLayer;

#[derive(Parser)]
#[command(name = "oyster")]
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
    },

    /// Serve the generated site with optional live reload
    Serve {
        /// Path to the vault directory
        vault_root_dir: PathBuf,

        /// Output directory for the generated site
        #[arg(short, long)]
        output: PathBuf,

        /// Port to serve on
        #[arg(long, default_value = "3000")]
        port: u16,

        /// Watch source files and regenerate on changes
        #[arg(short, long)]
        watch: bool,

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
            home_note_path,
            home_name,
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
                home_note_path.as_deref(),
                home_name.as_deref(),
                &node_render_config,
                custom_callout_css.as_deref(),
            )?;

            println!("Site generated to: {}", output_dir.display());
        }

        Commands::Serve {
            vault_root_dir,
            output: output_dir,
            port,
            watch,
            theme,
            filter_publish,
            home_note_path,
            home_name,
            preserve_softbreak,
            mermaid_render_mode,
            tikz_render_mode,
            quiver_render_mode,
            custom_callout_css,
        } => {
            let mermaid_render_mode = MermaidRenderMode::from_str(&mermaid_render_mode)
                .unwrap_or(MermaidRenderMode::BuildTime);
            let tikz_render_mode =
                TikzRenderMode::from_str(&tikz_render_mode).unwrap_or(TikzRenderMode::ClientSide);
            let quiver_render_mode =
                QuiverRenderMode::from_str(&quiver_render_mode).unwrap_or(QuiverRenderMode::Raw);
            let node_render_config = NodeRenderConfig {
                preserve_softbreak,
                mermaid_render_mode,
                tikz_render_mode,
                quiver_render_mode,
            };

            // Initial build
            println!(
                "Generating site from vault: {}",
                vault_root_dir.display()
            );
            render_vault(
                &vault_root_dir,
                &output_dir,
                &theme,
                filter_publish,
                home_note_path.as_deref(),
                home_name.as_deref(),
                &node_render_config,
                custom_callout_css.as_deref(),
            )?;
            println!("Site generated to: {}", output_dir.display());

            // Start async runtime for server
            let rt = tokio::runtime::Runtime::new()?;
            rt.block_on(async {
                serve_site(
                    vault_root_dir,
                    output_dir,
                    port,
                    watch,
                    theme,
                    filter_publish,
                    home_note_path,
                    home_name,
                    node_render_config,
                    custom_callout_css,
                )
                .await
            })?;
        }
    }

    Ok(())
}

async fn serve_site(
    vault_root_dir: PathBuf,
    output_dir: PathBuf,
    port: u16,
    watch: bool,
    theme: String,
    filter_publish: bool,
    home_note_path: Option<PathBuf>,
    home_name: Option<String>,
    node_render_config: NodeRenderConfig,
    custom_callout_css: Option<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    let addr = format!("0.0.0.0:{}", port);

    if watch {
        // Create livereload layer
        let livereload = LiveReloadLayer::new();
        let reloader = livereload.reloader();

        // Create router with livereload
        let app = Router::new()
            .fallback_service(ServeDir::new(&output_dir))
            .layer(livereload);

        // Create channel for file change events
        let (tx, mut rx) = mpsc::channel::<()>(1);

        // Set up file watcher
        let vault_path = vault_root_dir.clone();
        let tx_clone = tx.clone();
        std::thread::spawn(move || {
            let (notify_tx, notify_rx) = std::sync::mpsc::channel();
            let mut debouncer =
                new_debouncer(Duration::from_millis(500), notify_tx).expect("Failed to create debouncer");

            debouncer
                .watcher()
                .watch(&vault_path, notify_debouncer_mini::notify::RecursiveMode::Recursive)
                .expect("Failed to watch vault directory");

            println!("Watching for changes in: {}", vault_path.display());

            loop {
                match notify_rx.recv() {
                    Ok(Ok(events)) => {
                        // Filter for actual file changes (not just access)
                        let has_changes = events.iter().any(|e| {
                            matches!(e.kind, DebouncedEventKind::Any)
                                && e.path.extension().is_some_and(|ext| ext == "md")
                        });
                        if has_changes {
                            let _ = tx_clone.blocking_send(());
                        }
                    }
                    Ok(Err(e)) => eprintln!("Watch error: {:?}", e),
                    Err(_) => break,
                }
            }
        });

        // Spawn rebuild task
        let vault_for_rebuild = Arc::new(vault_root_dir);
        let output_for_rebuild = Arc::new(output_dir);
        let theme = Arc::new(theme);
        let home_note_path = Arc::new(home_note_path);
        let home_name = Arc::new(home_name);
        let custom_callout_css = Arc::new(custom_callout_css);

        tokio::spawn(async move {
            while rx.recv().await.is_some() {
                println!("\nFile change detected, regenerating...");
                let result = render_vault(
                    &vault_for_rebuild,
                    &output_for_rebuild,
                    &theme,
                    filter_publish,
                    home_note_path.as_ref().as_deref(),
                    home_name.as_ref().as_deref(),
                    &node_render_config,
                    custom_callout_css.as_ref().as_deref(),
                );
                match result {
                    Ok(()) => {
                        println!("Regenerated. Reloading browser...");
                        reloader.reload();
                    }
                    Err(e) => eprintln!("Regeneration failed: {}", e),
                }
            }
        });

        println!("Serving site at http://localhost:{} (with live reload)", port);
        let listener = tokio::net::TcpListener::bind(&addr).await?;
        axum::serve(listener, app).await?;
    } else {
        // Simple static file server without livereload
        let app = Router::new().fallback_service(ServeDir::new(&output_dir));

        println!("Serving site at http://localhost:{}", port);
        let listener = tokio::net::TcpListener::bind(&addr).await?;
        axum::serve(listener, app).await?;
    }

    Ok(())
}
