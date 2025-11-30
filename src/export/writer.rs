//! Render a vault to HTML; the SSG
//!
//! File-level information:
//!   - file referenceable: note and asset
//!   - in-note referenceable: heading, block
//!   - reference: outgoing edges
//! Vault-level information:
//!   - links: matched edges
//!   - vault paths to slug map
//!   - in-note referenceable anchor id map
use super::content::render_content;
use super::frontmatter;
use super::home;
use super::sidebar;
use super::style;
use super::toc;
use super::utils::{
    build_in_note_anchor_id_map, build_vault_paths_to_slug_map,
    get_relative_dest, range_to_anchor_id,
};
use crate::ast::Tree;
use crate::link::{
    Link as ResolvedLink, Referenceable, build_links, scan_vault,
};
use maud::{DOCTYPE, Markup, PreEscaped, html};
use std::collections::HashMap;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};

const HOME_NAME: &str = "home";
const KATEX_ASSETS_DIR_IN_OUTPUT: &str = "katex";

pub fn render_vault(
    vault_root_dir: &Path,
    output_dir: &Path,
    theme: &str,
    filter_publish: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    let home_name = HOME_NAME.to_string();

    // Scan the vault and build links
    let (fms, referenceables, references) =
        scan_vault(vault_root_dir, vault_root_dir, filter_publish);
    let (links, _unresolved) = build_links(references, referenceables.clone());

    // Build map: vault file path |-> slug
    let vault_file_paths = referenceables
        .iter()
        .filter(|referenceable| !referenceable.is_innote())
        .map(|referenceable| referenceable.path().as_path())
        .collect::<Vec<_>>();
    let vault_path_to_slug_map =
        build_vault_paths_to_slug_map(&vault_file_paths);

    // Build map: vault file path |-> in-note refable range |-> anchor id map
    let referenceable_refs = referenceables.iter().collect::<Vec<_>>();
    let innote_refable_anchor_id_map =
        build_in_note_anchor_id_map(&referenceable_refs);

    // There's an implicit map: reference path |-> reference range |-> anchor id
    // where the anchor id IS the byte range of the reference

    // Build map: vault file path |-> frontmatter
    let vault_path_to_frontmatter_map = referenceables
        .iter()
        .zip(fms)
        .map(|(referenceable, fm)| (referenceable.path().as_path(), fm))
        .collect::<HashMap<_, _>>();
    let note_vault_paths = referenceables
        .iter()
        .filter(|referenceable| {
            matches!(referenceable, Referenceable::Note { .. })
        })
        .map(|referenceable| referenceable.path().as_path())
        .collect::<Vec<_>>();
    // Build map for notes: vault file path |-> title
    let vault_path_to_title_map = note_vault_paths
        .iter()
        .map(|path| {
            let title = vault_path_to_frontmatter_map
                .get(path)
                .expect("Did not find maybe frontmatter for note")
                .as_ref()
                .and_then(|fm| frontmatter::get_title(&fm))
                .unwrap_or_else(|| title_from_path(path));
            (path.to_path_buf(), title)
        })
        .collect::<HashMap<_, _>>();

    fs::create_dir_all(output_dir)?;

    // Setup CSS files
    style::setup_styles(output_dir, theme)?;

    // Copy matched static assets to output dir
    let matched_asset_vault_paths = links
        .iter()
        .map(|link| &link.to)
        .filter(|referenceable| {
            matches!(referenceable, Referenceable::Asset { .. })
        })
        .map(|referenceable| referenceable.path().as_path())
        .collect::<Vec<_>>();
    matched_asset_vault_paths.iter().for_each(|&vault_path| {
        let cp_from = vault_root_dir.join(vault_path);
        let slug_path = vault_path_to_slug_map.get(vault_path).unwrap();
        let cp_to = output_dir.join(slug_path);
        if let Some(cp_to_parant) = cp_to.parent() {
            fs::create_dir_all(cp_to_parant).ok();
        };
        fs::copy(&cp_from, &cp_to).ok();
    });

    // Render each page
    for note_vault_path in note_vault_paths {
        let md_src = fs::read_to_string(vault_root_dir.join(note_vault_path))?;
        let tree = Tree::new(&md_src);
        let note_slug_path =
            Path::new(vault_path_to_slug_map.get(note_vault_path).expect(
                "Note path should be slugified and stored in vault_path_to_slug_map",
            ));

        let title = vault_path_to_title_map.get(note_vault_path).unwrap();

        let home_nav = home::render_simple_home_back_nav(
            &note_slug_path,
            &Path::new(&home_name),
        );

        let frontmatter_info = vault_path_to_frontmatter_map
            .get(note_vault_path)
            .expect("Did not find frontmatter for note")
            .as_ref()
            .and_then(|fm| frontmatter::render_frontmatter(&fm));

        let toc = toc::render_toc(
            note_vault_path,
            &referenceables,
            &innote_refable_anchor_id_map,
        );

        let content = render_content(
            &tree,
            note_vault_path,
            &links,
            &vault_path_to_slug_map,
            &innote_refable_anchor_id_map,
        );

        let backlink = render_backlinks(
            note_vault_path,
            &links,
            &vault_path_to_slug_map,
            &vault_path_to_title_map,
            &innote_refable_anchor_id_map,
        );

        let katex_rel_dir = get_relative_dest(
            &Path::new(note_slug_path),
            Path::new(&KATEX_ASSETS_DIR_IN_OUTPUT),
        );
        let katex_css_path = format!("{}/{}", katex_rel_dir, "katex.min.css");

        let sidebar = sidebar::render_sidebar_explorer(
            note_slug_path,
            &referenceables,
            &vault_path_to_slug_map,
        );

        let output_path = output_dir.join(note_slug_path);
        let css_paths = style::get_style_paths(&output_path, output_dir, theme);

        let html = render_page(
            title,
            &frontmatter_info,
            &toc,
            &content,
            &backlink,
            &home_nav,
            &sidebar,
            &css_paths,
            &katex_css_path,
        );

        if let Some(parent) = output_path.parent() {
            let _ = fs::create_dir_all(parent);
        }

        fs::write(&output_path, html)?;
    }

    // Generate home page
    let home_content = home::render_home_page(
        &referenceables,
        &vault_path_to_slug_map,
        Path::new(&home_name),
    );
    let home_path = output_dir.join(format!("{}.html", home_name));
    let katex_rel_dir =
        get_relative_dest(&home_path, Path::new(&KATEX_ASSETS_DIR_IN_OUTPUT));
    let katex_css_path = format!("{}/{}", katex_rel_dir, "katex.min.css");
    let home_css_paths = style::get_style_paths(&home_path, output_dir, theme);
    let home_html = html! {
        (DOCTYPE)
        html {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width, initial-scale=1";
                link rel="stylesheet" href=(katex_css_path);
                @for css_path in &home_css_paths {
                    link rel="stylesheet" href=(css_path);
                }
                title { "Home" }
            }
            body {
                div class="main-content" {
                    article {
                        h1 { "Home" }
                        (home_content)
                    }
                }
            }
        }
    }
    .into_string();
    fs::write(&home_path, home_html)?;

    // Copy katex assets
    cp_katex_assets(output_dir)?;

    Ok(())
}

fn render_page(
    title: &str,
    frontmatter: &Option<Markup>,
    toc: &Option<Markup>,
    content: &str,
    backlink: &Option<Markup>,
    home_nav: &Markup,
    sidebar: &Markup,
    css_paths: &[String],
    katex_css_path: &str,
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
                title { (title) }
            }
            body {
                (sidebar)
                div class="main-content" {
                    nav class="top-nav" {
                        (home_nav)
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
                    article {
                        (PreEscaped(content))
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
            }
        }
    }
    .into_string()
}

/// Extract title from path
fn title_from_path(path: &Path) -> String {
    path.file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap()
        .to_string()
}

/// Incoming links for a note
fn render_backlinks(
    vault_path: &Path,
    links: &[ResolvedLink],
    vault_path_to_slug_map: &HashMap<PathBuf, String>,
    vault_path_to_title_map: &HashMap<PathBuf, String>,
    innote_refable_anchor_id_map: &HashMap<
        PathBuf,
        HashMap<Range<usize>, String>,
    >,
) -> Option<Markup> {
    let refable_anchor_id_map =
        innote_refable_anchor_id_map.get(vault_path).unwrap();

    // For each incoming linkes
    // we need:
    //   - src's title
    //   - src's slug (for link href)
    //   - src's anchor id
    let backlink_infos = links
        .iter()
        .filter(|link| link.tgt_path_eq(vault_path))
        .map(|link| {
            let src = &link.from;
            let src_title = vault_path_to_title_map.get(&src.path).unwrap();
            let src_slug = vault_path_to_slug_map.get(&src.path).unwrap();
            let base_slug = vault_path_to_slug_map.get(vault_path).unwrap();
            let rel_src_slug =
                get_relative_dest(Path::new(base_slug), Path::new(src_slug));
            let src_anchor = range_to_anchor_id(&src.range);
            let src_href = format!("{}#{}", rel_src_slug, src_anchor);
            let tgt = &link.to;
            let tgt_anchor: Option<String> = match tgt {
                Referenceable::Heading { range, .. } => {
                    let anchor_id = refable_anchor_id_map.get(range).unwrap();
                    Some(anchor_id.clone())
                }
                _ => None,
            };
            (src_href, src_title, tgt_anchor)
        })
        .collect::<Vec<_>>();

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
