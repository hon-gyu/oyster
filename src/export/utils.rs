use crate::link::Referenceable;
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

/// ====================
/// Slugify a path in vault
/// ====================

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
/// Mainly used for headings
fn text_to_anchor_id(text: &str) -> String {
    text.to_lowercase()
        .chars()
        .map(|c| match c {
            'a'..='z' | '0'..='9' | '-' | '_' => c,
            _ => '-',
        })
        .collect::<String>()
        .split('-')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("-")
}

pub fn range_to_anchor_id(range: &Range<usize>) -> String {
    format!("{}-{}", range.start, range.end)
}

/// Converts a path to a URL-friendly slug
///
/// Examples:
/// - "Note 1.md" -> "note-1"
/// - "dir/Note.md" -> "dir/note"
fn file_name_to_slug(path: &Path) -> String {
    let path_str = path.to_str().unwrap();
    let without_ext = path_str.strip_suffix(".md").unwrap_or(path_str);

    without_ext
        .split('/')
        .map(slugify)
        .collect::<Vec<_>>()
        .join("/")
}

/// Converts a path to a URL-friendly slug with extension
/// markdown -> html
/// other -> other
fn file_path_to_slug(path: &Path) -> String {
    let slug = file_name_to_slug(&path.with_extension(""));
    let ext = path.extension().and_then(|s| s.to_str()).unwrap_or("");
    let slug_ext = if ext == "md" || ext == "markdown" {
        "html"
    } else {
        ext
    };
    let full_slug = format!("{}.{}", slug, slug_ext);
    full_slug
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

/// For a list of referenceables, we build a map of their byte ranges to anchor IDs
///
/// Returns: HashMap<PathBuf, HashMap<Range<usize>, String>>
/// - vault path |-> byte range |-> anchor ID
///
/// For heading, the anchor ID is kebab-cased text of the heading
/// For block, the anchor ID is the block identifier
///
/// Note: we don't de-duplicate anchor IDs here.
pub fn build_in_note_anchor_id_map(
    referenceables: &[&Referenceable],
) -> HashMap<PathBuf, HashMap<Range<usize>, String>> {
    let mut map: HashMap<PathBuf, HashMap<Range<usize>, String>> =
        HashMap::new();
    for refable in referenceables {
        match refable {
            Referenceable::Heading {
                path,
                level: _level,
                text,
                range,
            } => {
                let id = text_to_anchor_id(text);
                let curr_map = map.entry(path.clone()).or_default();
                curr_map.insert(range.clone(), id);
            }
            Referenceable::Block {
                path,
                identifier,
                kind: _kind,
                range,
            } => {
                let curr_map = map.entry(path.clone()).or_default();
                curr_map.insert(range.clone(), identifier.clone());
            }
            Referenceable::Note { path, children } => {
                // Ensure the note path has an entry even if it has no children
                map.entry(path.clone()).or_default();
                let refs: Vec<&Referenceable> = children.iter().collect();
                let child_map = build_in_note_anchor_id_map(&refs);
                map.extend(child_map);
            }
            _ => {}
        }
    }
    map
}

pub fn get_relative_dest(base_file: &Path, dest: &Path) -> String {
    let abs_dest = Path::new("root").join(dest);
    let abs_base_file = Path::new("root").join(base_file);
    // Get the directory containing the base file
    let abs_base_dir = abs_base_file.parent().unwrap();
    let rel_dest = pathdiff::diff_paths(abs_dest, abs_base_dir).unwrap();
    rel_dest.as_os_str().to_str().unwrap().to_string()
}

pub fn parse_resize_spec(resize_spec: &str) -> (Option<u32>, Option<u32>) {
    let resize_spec = resize_spec.trim();
    if let Some((width, height)) = resize_spec.split_once('x') {
        let width = width.parse::<u32>().ok();
        let height = height.parse::<u32>().ok();
        if let (Some(width), Some(height)) = (width, height) {
            (Some(width), Some(height))
        } else {
            (None, None)
        }
    } else if let Some(width) = resize_spec.parse::<u32>().ok() {
        (Some(width), None)
    } else {
        (None, None)
    }
}

// ====================

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn test_file_name_to_slug() {
        assert_snapshot!(
            file_name_to_slug(Path::new("Note 1")),
            @"note-1"
        );
        assert_snapshot!(
            file_name_to_slug(Path::new("Three laws of motion")),
            @"three-laws-of-motion"
        );
        assert_snapshot!(
            file_name_to_slug(Path::new("dir/Note")),
            @"dir/note"
        );
        assert_snapshot!(
            file_name_to_slug(Path::new("(Test)")),
            @"test"
        );
    }

    #[test]
    fn test_file_path_to_slug() {
        assert_snapshot!(
            file_path_to_slug(Path::new("Note 1.md")),
            @"note-1.html"
        );
        assert_snapshot!(
            file_path_to_slug(Path::new("Three laws of motion.md")),
            @"three-laws-of-motion.html"
        );
        assert_snapshot!(
            file_path_to_slug(Path::new("dir/Note.md")),
            @"dir/note.html"
        );
        assert_snapshot!(
            file_path_to_slug(Path::new("(Test).md")),
            @"test.html"
        );
        assert_snapshot!(
            file_path_to_slug(Path::new("img.jpg")),
            @"img.jpg"
        )
    }
}
