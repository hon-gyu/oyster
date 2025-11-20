/// HTML conversion from markdown with link rewriting
use crate::link::Link;
use crate::parse::default_opts;
use pulldown_cmark::{CowStr, Event, LinkType, Parser, Tag};
use std::collections::HashMap;
use std::path::Path;

/// Converts a path to a URL-friendly slug
/// e.g., "Note 1.md" -> "note-1.html"
pub fn path_to_slug(path: &Path) -> String {
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("index");

    // Convert to lowercase and replace spaces with hyphens
    let slug = stem
        .to_lowercase()
        .replace(' ', "-")
        .replace(['(', ')', '[', ']', '{', '}'], "");

    format!("{}.html", slug)
}

/// Converts a markdown file to HTML with proper link rewriting
///
/// Strategy:
/// 1. Parse markdown with default_opts
/// 2. Transform Link events using resolved links
/// 3. Render to HTML
///
/// Arguments:
/// - `markdown`: The raw markdown content
/// - `current_path`: Path of the current file being converted
/// - `links`: Resolved links for this file
///
/// Returns: HTML string
pub fn markdown_to_html(
    markdown: &str,
    current_path: &Path,
    links: &[Link],
) -> String {
    // Build a lookup map: dest string -> resolved link
    let link_map: HashMap<&str, &Link> = links
        .iter()
        .filter(|link| link.from.path == current_path)
        .map(|link| (link.from.dest.as_str(), link))
        .collect();

    // Parse with the same options as in link resolution
    let opts = default_opts();
    let parser = Parser::new_ext(markdown, opts);

    // Transform wikilinks
    let transformed = parser.map(|event| match event {
        Event::Start(Tag::Link {
            dest_url, title, ..
        }) => {
            // Look up the resolved link
            if let Some(resolved_link) = link_map.get(dest_url.as_ref()) {
                // Rewrite to point to generated HTML
                let target_slug = path_to_slug(resolved_link.to.path());
                let new_dest = format!("/{}", target_slug);

                Event::Start(Tag::Link {
                    link_type: LinkType::Inline,
                    dest_url: CowStr::from(new_dest),
                    title: title.clone(),
                    id: CowStr::from(""),
                })
            } else {
                // Keep original wikilink (unresolved)
                Event::Start(Tag::Link {
                    link_type: LinkType::Inline,
                    dest_url,
                    title,
                    id: CowStr::from(""),
                })
            }
        }
        other => other,
    });

    // Render to HTML
    let mut html_output = String::new();
    pulldown_cmark::html::push_html(&mut html_output, transformed);

    html_output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_to_slug() {
        assert_eq!(path_to_slug(Path::new("Note 1.md")), "note-1.html");
        assert_eq!(
            path_to_slug(Path::new("Three laws of motion.md")),
            "three-laws-of-motion.html"
        );
        assert_eq!(path_to_slug(Path::new("dir/Note.md")), "note.html");
        assert_eq!(path_to_slug(Path::new("(Test).md")), "test.html");
    }
}
