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

    // // Build backlink map: path -> list of pages that link to it
    // let backlink_map = build_backlink_map(&links);

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

// /// Builds a map of backlinks: target_path -> vec of source paths
// fn build_backlink_map(links: &[Link]) -> HashMap<PathBuf, Vec<PathBuf>> {
//     let mut backlinks: HashMap<PathBuf, Vec<PathBuf>> = HashMap::new();

//     for link in links {
//         let target = link.to.path().clone();
//         let source = link.from.path.clone();

//         backlinks
//             .entry(target)
//             .or_insert_with(Vec::new)
//             .push(source);
//     }

//     backlinks
// }

// /// Generates a single HTML page from a note
// ///
// /// writes to config.output_dir
// fn generate_page(
//     vault_path: &Path,
//     note_path: &Path,
//     links: &[Link],
//     path_to_slug_map: &HashMap<PathBuf, String>,
//     in_note_anchor_id_map: &HashMap<Range<usize>, String>,
//     config: &SiteConfig,
// ) -> Result<(), Box<dyn std::error::Error>> {
//     // Read the markdown file
//     let full_path = vault_path.join(note_path);
//     let md_src = fs::read_to_string(&full_path)?;

//     // Convert to HTML with link rewriting
//     let html_content = export_to_html_body(
//         &md_src,
//         note_path,
//         links,
//         path_to_slug_map,
//         in_note_anchor_id_map,
//     );

//     let title = note_path.as_os_str().to_string_lossy().to_string();

//     // // Extract title (from first heading or filename)
//     // let title = extract_title(&md_src, note_path);

//     // // Build backlinks for this page
//     // let backlinks = if config.generate_backlinks {
//     //     backlink_map.get(note_path).map(|sources| {
//     //         sources
//     //             .iter()
//     //             .map(|src| {
//     //                 let slug = path_to_slug(src);
//     //                 let path = if config.base_url.is_empty() {
//     //                     // Relative path
//     //                     slug
//     //                 } else if config.base_url == "/" {
//     //                     // Absolute from root
//     //                     format!("/{}", slug)
//     //                 } else {
//     //                     // With base URL
//     //                     format!(
//     //                         "{}/{}",
//     //                         config.base_url.trim_end_matches('/'),
//     //                         slug
//     //                     )
//     //                 };
//     //                 BacklinkInfo {
//     //                     title: extract_title_from_path(src),
//     //                     path,
//     //                 }
//     //             })
//     //             .collect()
//     //     })
//     // } else {
//     //     None
//     // };

//     // Create page context
//     let context = PageContext {
//         site: SiteContext {
//             title: config.title.clone(),
//             base_url: config.base_url.clone(),
//         },
//         page: PageData {
//             title: title,
//             content: html_content,
//             path: {
//                 let slug = path_to_slug(note_path);
//                 slug
//             },
//         },
//         // links: backlinks.map(|backlinks| LinkContext {
//         //     backlinks,
//         //     outgoing_links: vec![], // Can be added later
//         // }),
//         // toc: None, // Can be added later
//     };

//     // Render the page
//     let html = render_page(&context)?;

//     // Write to output
//     let output_path = config.output_dir.join(path_to_slug(note_path));
//     fs::write(&output_path, html)?;

//     println!("  Generated: {}", output_path.display());

//     Ok(())
// }
