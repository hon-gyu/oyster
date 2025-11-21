/// Main SSG generator that processes a vault
use super::html::{markdown_to_html, path_to_slug};
use super::template::render_page;
use super::types::{
    BacklinkInfo, LinkContext, PageContext, PageData, SiteConfig, SiteContext,
};
use crate::link::{Link, Referenceable, build_links, scan_vault};
use std::collections::HashMap;
use std::fs;
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

    // Build backlink map: path -> list of pages that link to it
    let backlink_map = build_backlink_map(&links);

    // Process each note
    for referenceable in &referenceables {
        if let Referenceable::Note { path, .. } = referenceable {
            generate_page(vault_path, path, &links, &backlink_map, config)?;
        }
    }

    println!("✓ Site generated in {:?}", config.output_dir);
    Ok(())
}

/// Builds a map of backlinks: target_path -> vec of source paths
fn build_backlink_map(links: &[Link]) -> HashMap<PathBuf, Vec<PathBuf>> {
    let mut backlinks: HashMap<PathBuf, Vec<PathBuf>> = HashMap::new();

    for link in links {
        let target = link.to.path().clone();
        let source = link.from.path.clone();

        backlinks
            .entry(target)
            .or_insert_with(Vec::new)
            .push(source);
    }

    backlinks
}

/// Generates a single HTML page from a note
fn generate_page(
    vault_path: &Path,
    note_path: &Path,
    links: &[Link],
    backlink_map: &HashMap<PathBuf, Vec<PathBuf>>,
    config: &SiteConfig,
) -> Result<(), Box<dyn std::error::Error>> {
    // Read the markdown file
    let full_path = vault_path.join(note_path);
    let markdown_content = fs::read_to_string(&full_path)?;

    // Convert to HTML with link rewriting
    let html_content = markdown_to_html(&markdown_content, note_path, links);

    // Extract title (from first heading or filename)
    let title = extract_title(&markdown_content, note_path);

    // Build backlinks for this page
    let backlinks = if config.generate_backlinks {
        backlink_map.get(note_path).map(|sources| {
            sources
                .iter()
                .map(|src| {
                    let slug = path_to_slug(src);
                    let path = if config.base_url.is_empty() {
                        // Relative path
                        slug
                    } else if config.base_url == "/" {
                        // Absolute from root
                        format!("/{}", slug)
                    } else {
                        // With base URL
                        format!(
                            "{}/{}",
                            config.base_url.trim_end_matches('/'),
                            slug
                        )
                    };
                    BacklinkInfo {
                        title: extract_title_from_path(src),
                        path,
                    }
                })
                .collect()
        })
    } else {
        None
    };

    // Create page context
    let context = PageContext {
        site: SiteContext {
            title: config.title.clone(),
            base_url: config.base_url.clone(),
        },
        page: PageData {
            title: title.clone(),
            content: html_content,
            path: {
                let slug = path_to_slug(note_path);
                slug
            },
        },
        links: backlinks.map(|backlinks| LinkContext {
            backlinks,
            outgoing_links: vec![], // Can be added later
        }),
        toc: None, // Can be added later
    };

    // Render the page
    let html = render_page(&context)?;

    // Write to output
    let output_path = config.output_dir.join(path_to_slug(note_path));
    fs::write(&output_path, html)?;

    println!("  Generated: {}", output_path.display());

    Ok(())
}

/// Extracts title from markdown content (first heading) or falls back to filename
fn extract_title(markdown: &str, path: &Path) -> String {
    // Try to find first heading
    for line in markdown.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('#') {
            // Extract heading text
            let text = trimmed.trim_start_matches('#').trim();
            if !text.is_empty() {
                return text.to_string();
            }
        }
    }

    // Fall back to filename
    extract_title_from_path(path)
}

/// Extracts a title from a file path
fn extract_title_from_path(path: &Path) -> String {
    path.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Untitled")
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_title() {
        let md = "# Hello World\n\nSome content";
        assert_eq!(extract_title(md, Path::new("test.md")), "Hello World");

        let md_no_heading = "Just some text";
        assert_eq!(
            extract_title(md_no_heading, Path::new("My Note.md")),
            "My Note"
        );

        let md_h2 = "## Second Level";
        assert_eq!(extract_title(md_h2, Path::new("test.md")), "Second Level");
    }
}
