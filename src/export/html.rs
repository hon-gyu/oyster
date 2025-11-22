/// HTML conversion from markdown with link rewriting
use crate::link::Link;
use crate::link::percent_decode;
use crate::parse::default_opts;
use pulldown_cmark::{CowStr, Event, LinkType, Parser, Tag};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Converts a path to a URL-friendly slug
/// e.g., "Note 1.md" -> "note-1.html"
///
/// - lower-cases
/// - replaces spaces with hyphens
/// - special characters
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

/// Exports markdown to HTML
///
/// Arguments:
/// - `md_src`: The raw markdown content
/// - `path`: Path of the current file being converted
/// - `links`: Resolved links for this file
///
/// Returns: HTML string
///
/// We are usign pulldown-cmark's HTML writer to convert the markdown to HTML.
/// But with some modifications:
/// -
pub fn export_to_html_body(
    md_src: &str,
    path: &Path,
    resolved_links: &[Link],
    file_path_to_slug: &HashMap<PathBuf, String>,
) -> String {
    // Build a lookup map: dest string -> resolved link
    let link_map: HashMap<&str, &Link> = resolved_links
        .iter()
        .filter(|link| link.from.path == path)
        .map(|link| (link.from.dest.as_str(), link))
        .collect();

    // Parse with the same options as in link resolution
    let opts = default_opts();
    let parser = Parser::new_ext(md_src, opts);

    // Transform wikilinks and markdown links to .md files
    let transformed = parser.map(|event| match event {
        Event::Start(Tag::Link {
            link_type,
            dest_url,
            title,
            ..
        }) => {
            // Decode percent-encoded URLs for inline links
            let decoded = percent_decode(dest_url.as_ref());
            let dest_str = if link_type == LinkType::Inline {
                decoded.as_str()
            } else {
                dest_url.as_ref()
            };

            let resolved_link_opt = link_map.get(dest_str);
            // TODO: rewrite links to resolved ones

            if let Some(resolved_link) = resolved_link_opt {
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
