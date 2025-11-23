use itertools::Itertools;

/// Main SSG generator that processes a vault
use super::html::export_to_html_body;
use super::template::render_page;
use super::types::{LinkInfo, PageContext, PageData, SiteConfig, SiteContext};
use super::utils::{
    build_in_note_anchor_id_map, build_vault_paths_to_slug_map,
};
use crate::link::{
    Link, Referenceable, build_links, mut_transform_referenceable_path,
    scan_vault,
};
use std::collections::HashMap;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};

/// Generates a static site from an Obsidian vault
pub fn generate_site(
    vault_path: &Path,
    config: &SiteConfig,
) -> Result<(), Box<dyn std::error::Error>> {
    // Create output directory
    fs::create_dir_all(&config.output_dir)?;

    // Scan the vault and build links
    let (referenceables, references) = scan_vault(vault_path, vault_path);
    let (links, _unresolved) = build_links(references, referenceables.clone());

    let pre_slug_paths = referenceables
        .iter()
        .map(|r| r.path().as_path())
        .unique()
        .collect::<Vec<_>>();
    let path_to_slug_map = build_vault_paths_to_slug_map(&pre_slug_paths);
    let in_note_anchor_id_map = build_in_note_anchor_id_map(&referenceables);

    // TODO: generate page for assets

    // Process each note
    for referenceable in &referenceables {
        if let Referenceable::Note {
            path: note_path, ..
        } = referenceable
        {
            let absolute_note_path = vault_path.join(note_path);
            let md_src =
                fs::read_to_string(&absolute_note_path).map_err(|e| {
                    format!("Failed to read {:?}: {}", absolute_note_path, e)
                })?;

            // Main content
            let html_content = export_to_html_body(
                &md_src,
                note_path,
                &links,
                &path_to_slug_map,
                &in_note_anchor_id_map,
            );

            // Backlinks
            let backlinks = links
                .iter()
                .filter(|link| {
                    if link.to.path() == note_path {
                        true
                    } else {
                        false
                    }
                })
                .map(|link| {
                    let src_vault_path = &link.from.path;
                    let title = get_title_from_vault_path(&src_vault_path);
                    let src_slug_path = path_to_slug_map
                        .get(src_vault_path)
                        .unwrap()
                        .to_string();

                    LinkInfo {
                        title,
                        path: format!("{}.html", src_slug_path),
                    }
                })
                .collect::<Vec<_>>();
            let backlinks_opt = if backlinks.len() == 0 {
                None
            } else {
                Some(backlinks)
            };

            // Create page context
            let title = get_title_from_vault_path(note_path);
            let note_slug = path_to_slug_map.get(note_path).unwrap();
            let context = PageContext {
                site: SiteContext {
                    title: config.title.clone(),
                    base_url: config.base_url.clone(),
                },
                page: PageData {
                    title: title,
                    content: html_content,
                    path: note_slug.clone(),
                },
                backlinks: backlinks_opt,
            };

            // Render the page and write to output
            let html = render_page(&context)?;
            let output_path =
                config.output_dir.join(note_slug).with_extension("html");
            fs::write(&output_path, html).map_err(|e| {
                format!("Failed to write {:?}: {}", output_path, e)
            })?;

            println!("  Generated: {}", output_path.display());
        }
    }

    println!("✓ Site generated in {:?}", config.output_dir);
    Ok(())
}

fn get_title_from_vault_path(path: &Path) -> String {
    let path = path.as_os_str().to_string_lossy();
    let path = path.strip_suffix(".md").unwrap_or(&path);
    let path = path.strip_suffix(".markdown").unwrap_or(&path);
    path.to_string()
}
