use super::types::Referenceable;
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub fn percent_decode(url: &str) -> String {
    percent_encoding::percent_decode_str(url)
        .decode_utf8_lossy()
        .to_string()
}

#[allow(dead_code)]
pub fn percent_encode(url: &str) -> String {
    percent_encoding::utf8_percent_encode(
        url,
        percent_encoding::NON_ALPHANUMERIC,
    )
    .to_string()
    .replace("%23", "#") // Preserve # for heading anchors
    .replace("%2F", "/") // Preserve / for file paths
}

/// Check if a string is a valid block identifier
///
/// A block identifier allow only alphanumeric characters and `-`
pub fn is_block_identifier(text: &str) -> bool {
    text.chars().all(|c| c.is_ascii_alphanumeric() || c == '-')
}

// ====================

/// Slugify a string
///
/// - lower-cases
/// - replaces spaces with hyphens
/// - special characters
fn slugify(s: &str) -> String {
    s.to_lowercase()
        .chars()
        .map(|c| match c {
            'a'..='z' | '0'..='9' => c,
            _ => '-',
        })
        .collect::<String>()
        .split('-')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("-")
}

/// Convert a text to an anchor ID
///
/// -
fn text_to_anchor_id(text: &str) -> String {
    slugify(text)
}

/// Converts a path to a URL-friendly slug
///
/// Examples:
/// - "Note 1.md" -> "note-1"
/// - "dir/Note.md" -> "dir/note"
fn file_path_to_slug(path: &Path) -> String {
    let path_str = path.to_str().unwrap();
    let without_ext = path_str.strip_suffix(".md").unwrap_or(path_str);

    without_ext
        .split('/')
        .map(|s| slugify(s))
        .collect::<Vec<_>>()
        .join("/")
}

/// Build a map of paths to slugs
pub fn build_vault_paths_to_slug_map(
    paths: &[&Path],
) -> HashMap<PathBuf, String> {
    let mut map: HashMap<PathBuf, String> = HashMap::new();
    let mut slug_count: HashMap<String, usize> = HashMap::new();

    for path in paths {
        let slug = file_path_to_slug(path);
        let final_slug = if let Some(count) = slug_count.get_mut(&slug) {
            *count += 1;
            let new_slug = format!("{}-{}", slug, count);
            slug_count.insert(new_slug.clone(), 1);
            new_slug
        } else {
            slug_count.insert(slug.clone(), 1);
            slug
        };
        map.insert(path.to_path_buf(), final_slug);
    }

    map
}

/// For a list of in-note referenceables, we build a map of their byte ranges to anchor IDs
///
/// Note: we don't de-duplicate anchor IDs here.
pub fn build_in_note_anchor_id_map(
    referenceables: &[Referenceable],
) -> HashMap<Range<usize>, String> {
    let mut map: HashMap<Range<usize>, String> = HashMap::new();
    for refable in referenceables {
        match refable {
            Referenceable::Heading {
                path: _path,
                level: _level,
                text,
                range,
            } => {
                let id = text_to_anchor_id(text);
                map.insert(range.clone(), id);
            }
            Referenceable::Block {
                path: _path,
                identifier,
                kind: _kind,
                range,
            } => {
                map.insert(range.clone(), identifier.clone());
            }
            _ => {}
        }
    }
    map
}

// ====================

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn test_path_to_slug() {
        assert_snapshot!(
            file_path_to_slug(Path::new("Note 1.md")),
            @"note-1"
        );
        assert_snapshot!(
            file_path_to_slug(Path::new("Three laws of motion.md")),
            @"three-laws-of-motion"
        );
        assert_snapshot!(
            file_path_to_slug(Path::new("dir/Note.md")),
            @"dir/note"
        );
        assert_snapshot!(
            file_path_to_slug(Path::new("(Test).md")),
            @"test"
        );
    }
}
