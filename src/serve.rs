//! HTTP server with optional live reload for development.
//!
//! This module provides functionality to serve a generated static site over HTTP,
//! with optional file watching and live reload capabilities for a smooth development
//! experience.
//!
//! # Example
//!
//! ```ignore
//! use oyster::serve::{ServeConfig, serve_site};
//!
//! let config = ServeConfig {
//!     vault_root_dir: PathBuf::from("./my-vault"),
//!     output_dir: PathBuf::from("./output"),
//!     port: 3000,
//!     watch: true,
//!     // ... other fields
//! };
//!
//! serve_site(config).await?;
//! ```

use crate::export::{NodeRenderConfig, render_vault};
use axum::Router;
use axum::response::Redirect;
use axum::routing::get;
use notify_debouncer_mini::{DebouncedEventKind, new_debouncer};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;
use tower_http::services::ServeDir;
use tower_livereload::LiveReloadLayer;

/// Configuration for serving a generated site.
pub struct ServeConfig {
    pub vault_root_dir: PathBuf,
    pub output_dir: PathBuf,
    pub port: u16,
    pub watch: bool,
    pub theme: String,
    pub filter_publish: bool,
    pub home_note_path: Option<PathBuf>,
    pub home_name: Option<String>,
    pub home_slug: String,
    pub node_render_config: NodeRenderConfig,
    pub custom_callout_css: Option<PathBuf>,
}

/// Starts the HTTP server to serve the generated site.
///
/// This is the main entry point for the serve functionality. Based on the
/// configuration, it either starts a simple static file server or a server
/// with live reload capabilities.
///
/// # Returns
///
/// Returns `Ok(())` when the server shuts down gracefully, or an error if
/// the server fails to start or encounters a fatal error.
///
/// # Behavior
///
/// - If `config.watch` is `false`: Starts a simple static file server
/// - If `config.watch` is `true`: Starts a server with live reload that:
///   - Injects a live reload script into HTML responses
///   - Watches the vault directory for `.md` file changes
///   - Regenerates the site and triggers browser refresh on changes
pub async fn serve_site(
    config: ServeConfig,
) -> Result<(), Box<dyn std::error::Error>> {
    let addr = format!("0.0.0.0:{}", config.port);

    if config.watch {
        serve_with_watch(config, &addr).await
    } else {
        serve_static(config.output_dir, config.port, &addr, &config.home_slug)
            .await
    }
}

/// Starts a simple static file server without live reload.
///
/// Serves files from the output directory using axum's ServeDir service.
/// This is used when the `--watch` flag is not provided.
async fn serve_static(
    output_dir: PathBuf,
    port: u16,
    addr: &str,
    home_slug: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let home_path: &'static str =
        Box::leak(format!("/{}", home_slug).into_boxed_str());
    let app = Router::new()
        .route("/", get(|| async { Redirect::to(home_path) }))
        .fallback_service(ServeDir::new(&output_dir));

    println!("Serving site at http://localhost:{}", port);
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

/// Starts the server with file watching and live reload enabled.
///
/// This function sets up:
/// 1. An axum router with the `tower-livereload` middleware
/// 2. A file watcher (using `notify`) that monitors the vault directory
/// 3. A rebuild task that regenerates the site when changes are detected
///
/// The file watcher uses a 500ms debounce to avoid rapid rebuilds when
/// multiple files change in quick succession. Only `.md` file changes
/// trigger a rebuild.
async fn serve_with_watch(
    config: ServeConfig,
    addr: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let ServeConfig {
        vault_root_dir,
        output_dir,
        port,
        watch: _,
        theme,
        filter_publish,
        home_note_path,
        home_name,
        home_slug,
        node_render_config,
        custom_callout_css,
    } = config;

    // Create livereload layer
    let livereload = LiveReloadLayer::new();
    let reloader = livereload.reloader();

    // Create router with livereload
    let home_path: &'static str =
        Box::leak(format!("/{}", home_slug).into_boxed_str());
    let app = Router::new()
        .route("/", get(|| async { Redirect::to(home_path) }))
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
            new_debouncer(Duration::from_millis(500), notify_tx)
                .expect("Failed to create debouncer");

        debouncer
            .watcher()
            .watch(
                &vault_path,
                notify_debouncer_mini::notify::RecursiveMode::Recursive,
            )
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
                Ok(_) => {
                    println!("Regenerated. Reloading browser...");
                    reloader.reload();
                }
                Err(e) => eprintln!("Regeneration failed: {}", e),
            }
        }
    });

    println!(
        "Serving site at http://localhost:{} (with live reload)",
        port
    );
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
