//!
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
use super::style::get_style;
use super::toc::render_toc;
use super::utils::{
    build_in_note_anchor_id_map, build_vault_paths_to_slug_map,
    get_relative_dest,
};
use crate::ast::Tree;
use crate::export::utils::range_to_anchor_id;
use crate::link::{
    Link as ResolvedLink, Referenceable, build_links, scan_vault,
};
use maud::{DOCTYPE, Markup, PreEscaped, html};
use std::collections::HashMap;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub fn render_vault(
    vault_root_dir: &Path,
    output_dir: &Path,
    theme: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    // Scan the vault and build links
    let (referenceables, references) =
        scan_vault(vault_root_dir, vault_root_dir);
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

    // Build map: vault file path |-> title
    let note_vault_paths = referenceables
        .iter()
        .filter(|referenceable| {
            matches!(referenceable, Referenceable::Note { .. })
        })
        .map(|referenceable| referenceable.path().as_path())
        .collect::<Vec<_>>();
    let vault_path_to_title_map = note_vault_paths
        .iter()
        .map(|path| {
            let title = title_from_path(path);
            (path.to_path_buf(), title)
        })
        .collect::<HashMap<_, _>>();

    fs::create_dir_all(output_dir)?;
    // Render each page
    for note_vault_path in note_vault_paths {
        let md_src = fs::read_to_string(vault_root_dir.join(note_vault_path))?;
        let tree = Tree::new(&md_src);
        let note_slug_path =
            vault_path_to_slug_map.get(note_vault_path).unwrap();

        let title = vault_path_to_title_map.get(note_vault_path).unwrap();

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

        let toc = render_toc(
            note_vault_path,
            &referenceables,
            &innote_refable_anchor_id_map,
        );

        let home =
            render_home_ref(note_vault_path, &vault_path_to_slug_map, None);

        let html = render_page(
            &title,
            &content,
            &toc,
            &backlink,
            &home,
            get_style(theme),
        );
        let output_path = output_dir.join(format!("{}.html", note_slug_path));

        if let Some(parent) = output_path.parent() {
            let _ = fs::create_dir_all(parent);
        }

        fs::write(&output_path, html)?;
    }
    Ok(())
}

fn render_page(
    title: &str,
    content: &str,
    toc: &Option<Markup>,
    backlink: &Option<Markup>,
    home: &Markup,
    style: &str,
) -> String {
    html! {
        (DOCTYPE)
        html {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width, initial-scale=1";
                title { (title) }
                style {
                    (PreEscaped(style))
                }
            }
            body {
                nav class="top-nav" {
                    (home)
                }
                @if let Some(toc) = toc {
                    (toc)
                }
                article {
                    h1 { (title) }
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
            let src_href = format!("{}.html#{}", rel_src_slug, src_anchor);
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

fn render_home_ref(
    note_vault_path: &Path,
    vault_path_to_slug_map: &HashMap<PathBuf, String>,
    home_slug_path: Option<&Path>,
) -> Markup {
    let note_slug_path = vault_path_to_slug_map.get(note_vault_path).unwrap();
    let home_slug_path_val = home_slug_path.unwrap_or(Path::new("index"));
    // Calculate relative path to home (index.html)
    let home_href =
        get_relative_dest(Path::new(note_slug_path), home_slug_path_val);
    let home_href = format!("{}.html", home_href);

    html! {a href=(home_href) class="home-link" { "-" }}
}

fn render_home() -> Markup {
    todo!()
}
