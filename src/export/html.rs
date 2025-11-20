/// HTML conversion from markdown with link rewriting
use crate::link::Link;
use pulldown_cmark::{html, Parser};
use std::path::Path;

/// Converts a path to a URL-friendly slug
/// e.g., "Note 1.md" -> "note-1.html"
pub fn path_to_slug(path: &Path) -> String {
    let stem = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("index");

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
/// 1. Replace wikilinks [[...]] with markdown links [text](url) using the resolved links
/// 2. Render the result to HTML using pulldown-cmark
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
    // Replace wikilinks with markdown links
    let rewritten = rewrite_wikilinks(markdown, current_path, links);

    // Convert to HTML
    let parser = Parser::new(&rewritten);
    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);

    html_output
}

/// Rewrites wikilinks in markdown to standard markdown links
fn rewrite_wikilinks(
    markdown: &str,
    current_path: &Path,
    links: &[Link],
) -> String {
    let mut result = markdown.to_string();

    // Sort links by range in reverse order so replacements don't affect positions
    let mut sorted_links: Vec<_> = links
        .iter()
        .filter(|link| link.from.path == current_path)
        .collect();
    sorted_links.sort_by_key(|link| std::cmp::Reverse(link.from.range.start));

    for link in sorted_links {
        let range = &link.from.range;
        let original_text = &markdown[range.clone()];

        // Generate the target URL
        let target_slug = path_to_slug(link.to.path());
        let href = format!("/{}", target_slug);

        // Get display text (use display_text from reference, or default to dest)
        let display = if !link.from.display_text.is_empty() {
            &link.from.display_text
        } else {
            // Extract note name from path
            link.to.path()
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or(&link.from.dest)
        };

        // Create markdown link
        let markdown_link = format!("[{}]({})", display, href);

        // Replace in string
        result.replace_range(range.clone(), &markdown_link);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_to_slug() {
        assert_eq!(path_to_slug(Path::new("Note 1.md")), "note-1.html");
        assert_eq!(path_to_slug(Path::new("Three laws of motion.md")), "three-laws-of-motion.html");
        assert_eq!(path_to_slug(Path::new("dir/Note.md")), "note.html");
        assert_eq!(path_to_slug(Path::new("(Test).md")), "test.html");
    }
}
