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
use super::home::{render_home_page, render_home_ref};
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
use serde_yaml::Value as Y;
use std::collections::HashMap;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub fn render_vault(
    vault_root_dir: &Path,
    output_dir: &Path,
    theme: &str,
    filter_publish: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    // Scan the vault and build links
    let (fms, referenceables, references) =
        scan_vault(vault_root_dir, vault_root_dir);

    // Filter out unpublished notes
    let (referenceables, references) = if filter_publish {
        let publish_flags = fms
            .iter()
            .map(|fm| {
                if let Some(Y::Mapping(fm_val)) = fm {
                    if let Some(publish) = fm_val.get("publish") {
                        publish.as_bool().unwrap_or(false)
                    } else {
                        false
                    }
                } else {
                    false
                }
            })
            .collect::<Vec<_>>();
        debug_assert_eq!(
            publish_flags.len(),
            referenceables
                .iter()
                .filter(|r| matches!(r, Referenceable::Note { .. }))
                .collect::<Vec<_>>()
                .len(),
            r#"publish flags and note referenceables should exactly match,
            hense the length of publish flags should be equal to the number of notes"#
        );

        // Build a set of published note paths for quick lookup
        let published_note_paths = referenceables
            .iter()
            .zip(publish_flags.iter())
            .filter(|(r, publish)| {
                **publish && matches!(r, Referenceable::Note { .. })
            })
            .map(|(r, _)| r.path().clone())
            .collect::<std::collections::HashSet<_>>();

        // Filter referenceables
        let filtered_referenceables = referenceables
            .into_iter()
            .zip(publish_flags)
            .filter(|(_, publish)| *publish)
            .map(|(r, _)| r)
            .collect::<Vec<_>>();

        // Filter references to only include those from published notes
        let filtered_references = references
            .into_iter()
            .filter(|reference| published_note_paths.contains(&reference.path))
            .collect::<Vec<_>>();

        (filtered_referenceables, filtered_references)
    } else {
        (referenceables, references)
    };

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
        fs::copy(cp_from, cp_to).unwrap();
    });

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
            title,
            &content,
            &toc,
            &backlink,
            &home,
            get_style(theme),
        );
        let output_path = output_dir.join(note_slug_path);

        if let Some(parent) = output_path.parent() {
            let _ = fs::create_dir_all(parent);
        }

        fs::write(&output_path, html)?;
    }

    // Generate home page
    let home_content = render_home_page(
        &referenceables,
        &vault_path_to_slug_map,
        Path::new("index"),
    );
    let home_html = html! {
        (DOCTYPE)
        html {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width, initial-scale=1";
                link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/katex.min.css" integrity="sha384-wcIxkf4k558AjM3Yz3BBFQUbk/zgIYC2R0QpeeYb+TwlBVMrlgLqwRjRtGZiK7ww" crossorigin="anonymous";
                title { "Home" }
                style {
                    (PreEscaped(get_style(theme)))
                }
            }
            body {
                article {
                    h1 { "Home" }
                    (home_content)
                }
            }
        }
    }
    .into_string();

    let home_path = output_dir.join("index.html");
    fs::write(&home_path, home_html)?;

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
                link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/katex.min.css" integrity="sha384-wcIxkf4k558AjM3Yz3BBFQUbk/zgIYC2R0QpeeYb+TwlBVMrlgLqwRjRtGZiK7ww" crossorigin="anonymous";
                title { (title) }
                style {
                    (PreEscaped(style))
                }
            }
            body {
                nav class="top-nav" {
                    (home)
                }
                header {
                    h1 { (title) }
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
