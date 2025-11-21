/// HTML conversion from markdown with link rewriting
use crate::link::Link;
use crate::parse::default_opts;
use percent_encoding::percent_decode_str;
use pulldown_cmark::{CowStr, Event, LinkType, Parser, Tag};
use std::collections::HashMap;
use std::path::Path;

/// Converts a path to a URL-friendly slug
/// e.g., "Note 1.md" -> "note-1.html"
pub fn path_to_slug(path: &Path) -> String {
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("index");

    let slug = stem
        .to_lowercase()
        .replace(' ', "-")
        .replace(['(', ')', '[', ']', '{', '}'], "")
        // Remove or replace non-ASCII characters
        .chars()
        .filter(|c| c.is_ascii_alphanumeric() || *c == '-' || *c == '_')
        .collect::<String>();

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
/// - `base_url`: Base URL to prepend to all links
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

    // Transform wikilinks and markdown links to .md files
    let transformed = parser.map(|event| match event {
        Event::Start(Tag::Link {
            dest_url, title, ..
        }) => {
            // Decode percent-encoded URLs (e.g., "Three%20laws" -> "Three laws")
            let dest_str = dest_url.as_ref();
            let decoded = percent_decode_str(dest_str)
                .decode_utf8()
                .unwrap_or(std::borrow::Cow::Borrowed(dest_str));

            // Try to look up the resolved link (try both original and decoded)
            let resolved_opt = link_map
                .get(dest_str)
                .or_else(|| link_map.get(decoded.as_ref()));

            // If not found, check if it's a markdown link to a .md file
            let resolved_opt = resolved_opt.or_else(|| {
                // Handle markdown links like [text](Note.md) or [text](Note.md#heading)
                if decoded.contains(".md") {
                    // Try without .md extension
                    let without_md = decoded
                        .split('#')
                        .next()
                        .unwrap_or(&decoded)
                        .trim_end_matches(".md");
                    link_map.get(without_md)
                } else {
                    None
                }
            });

            if let Some(resolved_link) = resolved_opt {
                // Rewrite to point to generated HTML
                let target_slug = path_to_slug(resolved_link.to.path());
                let new_dest = target_slug;
                Event::Start(Tag::Link {
                    link_type: LinkType::Inline,
                    dest_url: CowStr::from(new_dest),
                    title: title.clone(),
                    id: CowStr::from(""),
                })
            } else {
                // Keep original link (unresolved or external)
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
