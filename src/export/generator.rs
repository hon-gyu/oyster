use itertools::Itertools;

/// Main SSG generator that processes a vault
use super::html::export_to_html_body;
use super::template::render_page;
use super::types::{PageContext, PageData, SiteConfig, SiteContext};
use crate::link::{
    Link, Referenceable, build_in_note_anchor_id_map, build_links,
    build_vault_paths_to_slug_map, mut_transform_referenceable_path,
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
            let note_slug = path_to_slug_map.get(note_path).unwrap();
            let md_src = fs::read_to_string(note_path)?;
            let html_content = export_to_html_body(
                &md_src,
                note_path,
                &links,
                &path_to_slug_map,
                &in_note_anchor_id_map,
            );

            let title = note_path.as_os_str().to_string_lossy().to_string();
            // Create page context
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
            };

            // Render the page
            let html = render_page(&context)?;

            // Write to output
            let output_path = config.output_dir.join(note_slug);
            fs::write(&output_path, html)?;

            println!("  Generated: {}", output_path.display());
        }
    }

    println!("✓ Site generated in {:?}", config.output_dir);
    Ok(())
}
