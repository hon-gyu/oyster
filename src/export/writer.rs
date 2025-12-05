//! Render a vault to HTML; the SSG
//!
//! File-level information:
//!   - file referenceable: note and asset
//!   - in-note referenceable: heading, block
//!   - reference: outgoing edges
//!   - frontmatter: note referenceable path |-> frontmatter
//!
//! Vault-level information:
//!   - links: matched edges
//!   - unresolved references
//!   - map: file vault path |-> slug path
//!   - map: file vault path |-> title
//!   - map: file valut path |-> in-note referenceable range |-> anchod id
//!   - map: reference path |-> reference range |-> anchor id
//!     - where the anchor id = the byte range of the reference
//!
//! Render a note (file referenceable) to HTML
//! - Args
//!   - its vault path
//!   - its content (Tree)
//!   - vault-level info
use super::codeblock::mermaid::MermaidRenderMode;
use super::content::{NodeRenderConfig, render_content};
use super::home;
use super::sidebar;
use super::style;
use super::toc;
use super::utils::{get_relative_dest, range_to_anchor_id};
use super::vault_db::{
    FileLevelInfo, StaticVaultStore, VaultDB, VaultLevelInfo,
};
use crate::ast::Tree;
use crate::link::{Referenceable, scan_vault};
use maud::{DOCTYPE, Markup, PreEscaped, html};
use std::fs;
use std::path::{Path, PathBuf};

const HOME_NAME: &str = "home";
const KATEX_ASSETS_DIR_IN_OUTPUT: &str = "katex";

pub fn render_vault(
    vault_root_dir: &Path,
    output_dir: &Path,
    theme: &str,
    filter_publish: bool,
    node_render_config: &NodeRenderConfig,
) -> Result<(), Box<dyn std::error::Error>> {
    let home_name = HOME_NAME.to_string();

    // Create vault DB
    let vault_db =
        StaticVaultStore::new_from_dir(vault_root_dir, filter_publish);

    let note_vault_paths = vault_db.get_note_vault_paths();

    fs::create_dir_all(output_dir)?;

    // Setup CSS files
    style::setup_styles(output_dir, theme)?;

    // Copy matched static assets to output dir
    let matched_asset_vault_paths: Vec<PathBuf> = vault_db
        .get_resolved_links()
        .iter()
        .map(|link| &link.to)
        .filter(|referenceable| {
            matches!(referenceable, Referenceable::Asset { .. })
        })
        .map(|referenceable| referenceable.path().to_path_buf())
        .collect();
    matched_asset_vault_paths.iter().for_each(|vault_path| {
        let cp_from = vault_root_dir.join(vault_path);
        if let Some(slug_path) =
            vault_db.get_slug_from_file_vault_path(vault_path)
        {
            let cp_to = output_dir.join(slug_path);
            if let Some(cp_to_parant) = cp_to.parent() {
                fs::create_dir_all(cp_to_parant).ok();
            };
            fs::copy(&cp_from, &cp_to).ok();
        }
    });

    // Render each page
    for note_vault_path in &note_vault_paths {
        let md_src = fs::read_to_string(vault_root_dir.join(note_vault_path))?;
        let tree = Tree::new(&md_src);
        let note_slug = vault_db
            .get_slug_from_file_vault_path(note_vault_path)
            .expect("Note path should be slugified and stored in vault_db");
        let note_slug_path = Path::new(&note_slug);

        let title = vault_db
            .get_title_from_note_vault_path(note_vault_path)
            .expect("Note should have a title");

        let home_nav = home::render_simple_home_back_nav(
            &note_slug_path,
            &Path::new(&home_name),
        );

        let frontmatter_info = vault_db
            .get_frontmatter(note_vault_path)
            .and_then(|fm| super::frontmatter::render_frontmatter(fm));

        let toc = toc::render_toc(
            note_vault_path,
            vault_db.get_referenceables(),
            |path, range| {
                vault_db
                    .get_innote_refable_anchor_id(&path.to_path_buf(), &range)
                    .map(|s| s.to_string())
            },
        );

        let content = render_content(
            &tree,
            note_vault_path,
            &vault_db,
            &node_render_config,
            0,
            5,
        );

        let backlink = render_backlinks(note_vault_path, &vault_db);

        let katex_rel_dir = get_relative_dest(
            &Path::new(note_slug_path),
            Path::new(&KATEX_ASSETS_DIR_IN_OUTPUT),
        );
        let katex_css_path = format!("{}/{}", katex_rel_dir, "katex.min.css");

        let sidebar = Some(sidebar::render_explorer(
            note_slug_path,
            vault_db.get_referenceables(),
            |path| {
                vault_db
                    .get_slug_from_file_vault_path(path)
                    .map(|s| s.to_string())
            },
        ));

        let output_path = output_dir.join(note_slug_path);
        let css_paths = style::get_style_paths(&output_path, output_dir, theme);

        let html = render_page(
            &title,
            &frontmatter_info,
            &toc,
            &Some(content),
            &backlink,
            &Some(home_nav),
            &sidebar,
            &css_paths,
            &katex_css_path,
            node_render_config.mermaid_render_mode,
        );

        if let Some(parent) = output_path.parent() {
            let _ = fs::create_dir_all(parent);
        }

        fs::write(&output_path, html)?;
    }

    let home_slug_path = format!("{}.html", home_name);
    let home_content = home::render_home_page(
        vault_db.get_referenceables(),
        |path| {
            vault_db
                .get_slug_from_file_vault_path(path)
                .map(|s| s.to_string())
        },
        Path::new(&home_slug_path),
    );
    let home_path = output_dir.join(&home_slug_path);
    let katex_rel_dir = get_relative_dest(
        Path::new(&home_slug_path),
        Path::new(&KATEX_ASSETS_DIR_IN_OUTPUT),
    );
    let katex_css_path = format!("{}/{}", katex_rel_dir, "katex.min.css");
    let home_css_paths = style::get_style_paths(&home_path, output_dir, theme);
    let home_main_content = html! {
        article {
            h1 { "Home" }
            (home_content)
        }
    };
    let home_html = render_page(
        &home_name,
        &None,
        &None,
        &Some(home_main_content),
        &None,
        &None,
        &None,
        &home_css_paths,
        &katex_css_path,
        node_render_config.mermaid_render_mode,
    );
    fs::write(&home_path, home_html)?;

    // Copy katex assets
    cp_katex_assets(output_dir)?;

    Ok(())
}

fn render_page(
    title: &str,
    frontmatter: &Option<Markup>,
    toc: &Option<Markup>,
    content: &Option<Markup>,
    backlink: &Option<Markup>,
    home_nav: &Option<Markup>,
    left_sidebar: &Option<Markup>,
    css_paths: &[String],
    katex_css_path: &str,
    mermaid_mode: MermaidRenderMode,
) -> String {
    html! {
        (DOCTYPE)
        html {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width, initial-scale=1";
                link rel="stylesheet" href=(katex_css_path);
                @for css_path in css_paths {
                    link rel="stylesheet" href=(css_path);
                }
                @if matches!(mermaid_mode, MermaidRenderMode::ClientSide) {
                    script type="module" {
                        (PreEscaped(r#"
import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs';
mermaid.initialize({ startOnLoad: true });
"#))
                    }
                }
                title { (title) }
            }
            body {
                .left-sidebar {
                    @if let Some(left_sidebar) = left_sidebar {
                        (left_sidebar)
                    }
                }
                .main-content {
                    nav class="top-nav" {
                        @if let Some(home_nav) = home_nav {
                            (home_nav)
                        }
                    }
                    header {
                        h1 { (title) }
                    }
                    @if let Some(frontmatter) = frontmatter {
                        (frontmatter)
                    }
                    @if let Some(toc) = toc {
                        (toc)
                    }
                    @if let Some(content) = content {
                        (content)
                    }
                    @if let Some(backlink) = backlink {
                        hr;
                        section class="backlinks" {
                            h5 { "Backlinks" }
                            ul {
                                (backlink)
                            }
                        }
                    }
                }
                .right-sidebar {
                }
            }
        }
    }
    .into_string()
}

/// Incoming links for a note
fn render_backlinks(
    vault_path: &Path,
    vault_db: &dyn VaultDB,
) -> Option<Markup> {
    // For each incoming links
    // we need:
    //   - src's title
    //   - src's slug (for link href)
    //   - src's anchor id
    let backlink_infos: Vec<_> = vault_db
        .get_resolved_links()
        .iter()
        .filter(|link| link.tgt_path_eq(vault_path))
        .filter_map(|link| {
            let src = &link.from;
            let src_title =
                vault_db.get_title_from_note_vault_path(&src.path)?;
            let src_slug = vault_db.get_slug_from_file_vault_path(&src.path)?;
            let base_slug = vault_db
                .get_slug_from_file_vault_path(&vault_path.to_path_buf())?;
            let rel_src_slug =
                get_relative_dest(Path::new(&base_slug), Path::new(&src_slug));
            let src_anchor = range_to_anchor_id(&src.range);
            let src_href = format!("{}#{}", rel_src_slug, src_anchor);
            let tgt = &link.to;
            let tgt_anchor: Option<String> = match tgt {
                Referenceable::Heading { path, range, .. } => vault_db
                    .get_innote_refable_anchor_id(path, range)
                    .map(|s| s.to_string()),
                _ => None,
            };
            Some((src_href, src_title, tgt_anchor))
        })
        .collect();

    if backlink_infos.is_empty() {
        return None;
    }

    let markup = html! {
        @for backlink_info in backlink_infos {
            li {
                a href=(backlink_info.0) { (backlink_info.1) }
                // If there backlink is in-note, we also show the tgt anchor ID
                @if let Some(tgt_anchor) = backlink_info.2 {
                    span { " " }
                    a href=(format!("#{}", tgt_anchor)) {"⤴"}
                }
            }
        }
    };
    Some(markup)
}

fn cp_katex_assets(
    output_dir: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    let katex_src_dir = Path::new("static/katex");
    let katex_dest_dir = output_dir.join("katex");
    fs::create_dir_all(&katex_dest_dir)?;
    if katex_src_dir.exists() {
        let css_from = katex_src_dir.join("katex.min.css");
        let css_to = katex_dest_dir.join("katex.min.css");
        fs::copy(&css_from, &css_to)?;

        let fonts_from = katex_src_dir.join("fonts");
        let fonts_to = katex_dest_dir.join("fonts");
        fs::create_dir_all(&fonts_to)?;
        for entry in fs::read_dir(&fonts_from)?.flatten() {
            let file_name = entry.file_name();
            fs::copy(entry.path(), fonts_to.join(file_name))?;
        }
    }

    Ok(())
}
