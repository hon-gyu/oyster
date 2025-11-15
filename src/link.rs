#![allow(warnings)] // reason: WIP
/// Extracts references and referenceables from a Markdown AST.
/// Referenceable can be
///     - items in a note: headings, block
///     - notes: markdown files
///     - assets other than notes: images, videos, audios, PDFs, etc.
use crate::ast::{Node, NodeKind, Tree};
use crate::heading::HasLevel;
use ego_tree::iter::Edge;
use pulldown_cmark::{HeadingLevel, LinkType};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::{ops::Range, path::Path};

#[derive(Clone, Debug, PartialEq)]
pub enum Referenceable {
    Asset {
        path: PathBuf,
    },
    Note {
        path: PathBuf,
        children: Vec<Referenceable>,
    },
    Heading {
        path: PathBuf,
        level: HeadingLevel,
        text: String,
        range: Range<usize>,
    },
    Block {
        path: PathBuf,
    },
}

impl Referenceable {
    pub fn path(&self) -> &PathBuf {
        match self {
            Referenceable::Asset { path, .. } => path,
            Referenceable::Note { path, .. } => path,
            Referenceable::Heading { path, .. } => path,
            Referenceable::Block { path, .. } => path,
        }
    }

    pub fn add_in_note_referenceables(
        &mut self,
        referenceables: Vec<Referenceable>,
    ) -> () {
        match self {
            Referenceable::Note { children, .. } => {
                children.extend(referenceables);
            }
            _ => {}
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ReferenceKind {
    WikiLink,
    MarkdownLink,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Reference {
    range: Range<usize>,
    dest: String,
    kind: ReferenceKind,
    display_text: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Link {
    from: Reference,
    to: Referenceable,
}

fn percent_decode(url: &str) -> String {
    percent_encoding::percent_decode_str(url)
        .decode_utf8_lossy()
        .to_string()
}

fn percent_encode(url: &str) -> String {
    percent_encoding::utf8_percent_encode(
        url,
        percent_encoding::NON_ALPHANUMERIC,
    )
    .to_string()
    .replace("%23", "#") // Preserve # for heading anchors
    .replace("%2F", "/") // Preserve / for file paths
}

// Scan a note and return a tuple of references and in-note referenceables
//
// Post-condition: the in-note referenceables are in order
pub fn scan_note(path: &PathBuf) -> (Vec<Reference>, Vec<Referenceable>) {
    let text = fs::read_to_string(path).unwrap();
    if text.is_empty() {
        return (Vec::new(), Vec::new());
    }

    let tree = Tree::new(&text);

    let mut references = Vec::new();
    let mut referenceables = Vec::new();
    extract_reference_and_referenceable(
        &tree.root_node,
        path,
        &mut references,
        &mut referenceables,
    );

    (references, referenceables)
}

/// Extracts all references and referenceables from a node.
fn extract_reference_and_referenceable(
    node: &Node,
    path: &PathBuf,
    references: &mut Vec<Reference>,
    referenceables: &mut Vec<Referenceable>,
) {
    match &node.kind {
        // Reference
        NodeKind::Link {
            link_type,
            dest_url,
            ..
        }
        | NodeKind::Image {
            link_type,
            dest_url,
            ..
        } => {
            match link_type {
                LinkType::WikiLink { has_pothole } => {
                    let display_text = if !has_pothole {
                        dest_url.to_string()
                    } else {
                        // Take out the text from the link's first child
                        assert_eq!(node.child_count(), 1);
                        let text_node = &node.children[0];
                        if let NodeKind::Text(text) = &text_node.kind {
                            text.to_string()
                        } else {
                            unreachable!("Never: Wikilink should have text");
                        }
                    };
                    let reference = Reference {
                        range: node.byte_range().clone(),
                        dest: dest_url.to_string(),
                        kind: ReferenceKind::WikiLink,
                        display_text,
                    };
                    references.push(reference);
                }
                LinkType::Inline => {
                    // Decode the destination URL. Eg, from `Note%201` to `Note 1`
                    let mut dest = percent_decode(dest_url);
                    // `[text]()` points to file `().md`
                    if dest.is_empty() {
                        dest = "()".to_string();
                    }
                    let dest = dest.strip_suffix(".md").unwrap_or(&dest);
                    let dest = dest.strip_suffix(".markdown").unwrap_or(&dest);
                    // Take out the text from the link's first child
                    let display_text = {
                        match node.children.as_slice() {
                            [] => "".to_string(),
                            [text_node] => {
                                if let NodeKind::Text(text) = &text_node.kind {
                                    text.to_string()
                                } else {
                                    unreachable!(
                                        "Never: Markdown link should have text"
                                    );
                                }
                            }
                            [fst, snd, ..] => {
                                unreachable!(
                                    "Never: Markdown link should have at most one child, got \n first: {:?}\n second: {:?}",
                                    fst, snd,
                                );
                            }
                        }
                    };

                    let reference = Reference {
                        range: node.byte_range().clone(),
                        dest: dest.to_string(),
                        kind: ReferenceKind::MarkdownLink,
                        display_text,
                    };
                    references.push(reference);
                }
                _ => {}
            }
        }
        // Referenceable
        NodeKind::Heading { level, .. } => {
            // Extract text from heading's children (can be Text, Code, etc.)
            let text = node
                .children
                .iter()
                .filter_map(|child| match &child.kind {
                    NodeKind::Text(text) => Some(text.as_ref()),
                    NodeKind::Code(code) => Some(code.as_ref()),
                    _ => None,
                })
                .collect::<Vec<&str>>()
                .join("");

            let referenceable = Referenceable::Heading {
                path: path.clone(),
                level: level.clone(),
                text,
                range: node.byte_range().clone(),
            };
            referenceables.push(referenceable);
        }
        NodeKind::List { .. } => {
            // TODO: Block. Not implemented.
        }
        NodeKind::Paragraph { .. } => {
            // TODO: Block. Not implemented.
        }
        _ => {}
    }

    for child in node.children.iter() {
        extract_reference_and_referenceable(
            child,
            path,
            references,
            referenceables,
        );
    }
}

fn scan_dir_for_assets_and_notes(dir: &Path) -> Vec<Referenceable> {
    fn aux<'a>(
        dir: &Path,
        referenceables: &'a mut Vec<Referenceable>,
        ignores: &[&str],
    ) {
        for entry in fs::read_dir(dir)
            .expect("Failed to read directory")
            .flatten()
        {
            let path = entry.path();
            if path.is_dir() {
                if ignores.iter().any(|ignore| {
                    path.file_name().and_then(|n| n.to_str()) == Some(ignore)
                }) {
                    continue;
                }
                aux(&path, referenceables, ignores);
            } else if path.is_file() {
                let item = match path.extension().and_then(|ext| ext.to_str()) {
                    Some("md") => Referenceable::Note {
                        path,
                        children: Vec::new(),
                    },
                    _ => {
                        if ignores.iter().any(|ignore| {
                            path.file_name().and_then(|n| n.to_str())
                                == Some(ignore)
                        }) {
                            continue;
                        }
                        Referenceable::Asset { path }
                    }
                };
                referenceables.push(item);
            }
        }
    }
    let ignores = vec![".obsidian", ".DS_Store"];
    let mut referenceables = Vec::<Referenceable>::new();
    aux(dir, &mut referenceables, &ignores);
    referenceables
}

/// Scan a vault for referenceables and references.
///
/// in-note referenceables stored in note's children
///
/// Returns:
///   - referenceables: all referenceables
///   - references: note references and asset references
fn scan_vault(dir: &Path) -> (Vec<Referenceable>, Vec<Reference>) {
    let mut file_referenceables = scan_dir_for_assets_and_notes(dir);
    let mut all_references = Vec::<Reference>::new();

    let file_referenceables_with_children = file_referenceables
        .into_iter()
        .map(|mut referenceable| match referenceable {
            Referenceable::Note { ref path, .. } => {
                let (references, referenceables) = scan_note(path);
                all_references.extend(references);
                referenceable.add_in_note_referenceables(referenceables);
                referenceable
            }
            asset @ Referenceable::Asset { .. } => asset,
            other => unreachable!(
                "in-note referenceable shouldn't present here, got {:?}",
                other
            ),
        })
        .collect();

    (file_referenceables_with_children, all_references)
}

/// Splits a destination string into two parts: the file name, and nested headings or block identifier
fn split_dest_string(s: &str) -> (&str, Option<Vec<&str>>, Option<&str>) {
    let hash_pos = s.find('#');

    // No hash
    if hash_pos.is_none() {
        return (s, None, None);
    }

    let hash_pos = hash_pos.unwrap();
    let file_name = &s[..hash_pos];
    let after_fst_hash = &s[hash_pos + 1..];

    // Check if it's a block reference
    // A block reference start with `^` and followed by letters and numbers
    if after_fst_hash.starts_with('^') && after_fst_hash.len() > 1 {
        let maybe_identifier = &after_fst_hash[1..];
        if maybe_identifier.chars().all(|c| c.is_ascii_alphanumeric()) {
            return (file_name, None, Some(maybe_identifier));
        }
    }

    (file_name, Some(parse_nested_heading(after_fst_hash)), None)
}

/// Parse a string into a vector of nested headings.
/// ```
/// use markdown_tools::link::parse_nested_heading;
/// assert_eq!(parse_nested_heading("##A###B"), vec!["A", "B"]);
/// assert_eq!(parse_nested_heading("A##B"), vec!["A", "B"]);
/// assert_eq!(parse_nested_heading("A#####C#B"), vec!["A", "C", "B"]);
/// assert_eq!(parse_nested_heading("##"), vec![""]);
/// assert_eq!(parse_nested_heading("##C"), vec!["C"]);
/// ```
pub fn parse_nested_heading(s: &str) -> Vec<&str> {
    if s.chars().all(|c| c == '#') && !s.is_empty() {
        // All hashes case
        return vec![""];
    }

    s.split('#').filter(|part| !part.is_empty()).collect()
}

/// Subsequence check that only matches ancestor-descendant relationship
///
/// Used in file name match and heading match
///
/// Returns the index of last matching item in haystack
///
/// Examples
/// ```
/// use markdown_tools::link::match_subsequence;
/// assert_eq!(match_subsequence(&[1, 2, 3, 4], &[1, 3]), Some(2));
/// assert_eq!(match_subsequence(&[1, 2, 3, 4], &[2, 4]), Some(3));
/// assert_eq!(match_subsequence(&[1, 2, 3, 4], &[1, 2, 3, 4]), Some(3));
/// assert_eq!(match_subsequence(&[1, 2, 3, 4], &[3, 1]), None);
/// assert_eq!(match_subsequence(&[1, 2, 3, 4], &[5]), None);
/// assert_eq!(match_subsequence(&["a", "b", "c"], &["a", "c"]), Some(2));
/// assert_eq!(match_subsequence(&["a", "b", "c"], &["c", "a"]), None);
/// ```
pub fn match_subsequence<T: PartialEq>(
    haystack: &[T],
    needle: &[T],
) -> Option<usize> {
    let mut haystack_idx = 0;
    let mut last_match_idx = 0;

    for needle_item in needle {
        // Find the next occurrence of needle_item in haystack
        let mut found = false;
        while haystack_idx < haystack.len() {
            if &haystack[haystack_idx] == needle_item {
                last_match_idx = haystack_idx;
                haystack_idx += 1; // Move past this match
                found = true;
                break;
            }
            haystack_idx += 1;
        }

        if !found {
            return None;
        }
    }

    Some(last_match_idx)
}

/// Match a file name reference against a list of paths.
///
/// Arguments:
/// - `needle`: the file (note or asset) name to match
/// - `haystack`: a list of paths to match against
///
/// - Trim spaces
/// - Add `.md` if a file has no extension
/// - Exact match first
/// - Try subsequence match if not exact match
///
/// see `test_match_file` for examples.
fn resolve_link(needle: &str, haystack: &Vec<PathBuf>) -> Option<PathBuf> {
    // Remove spaces
    let mut needle = needle.trim().to_string();
    // Add `.md` if a file has no extension
    if !needle.contains('.') {
        needle.push_str(".md");
    }

    let needle = Path::new(needle.as_str());
    // Try exact match first
    for hay in haystack {
        if hay == needle {
            return Some(hay.clone());
        }
    }

    // If not exact match, try to subsequence match
    let needle_components: Vec<_> = needle.components().collect();
    for hay in haystack {
        let hay_components: Vec<_> = hay.components().collect();
        if match_subsequence(&hay_components, &needle_components).is_some() {
            return Some(hay.clone());
        }
    }
    None
}

/// Resolve the path of new note to create when a link is unresolved or when some special commands
/// are used (create new empty note)
///
/// Arguments:
/// - `name`: the name of the note to create
/// - `paths`: the list of existing note paths
///
/// Returns:
/// - `(parent_dir, note_path)`: the parent directory to create if any, the path of note to create
///
/// #LLM: this function is implemented by LLM using TDD
///
/// - Parse dir and file name from the destination string first
/// - remove `\` and `/`
/// - increment the file name until it doesn't exist
///
/// Returns the path of dir to create and the path note to create.
///
/// Post-condition: the path of dir to create, if exists, is a parent of the path of note to create.
pub fn resolve_new_note_path(
    name: &str,
    paths: &Vec<PathBuf>,
) -> (Option<PathBuf>, PathBuf) {
    let name = name.trim();
    let path = Path::new(name);

    // Extract parent directory and file name
    let parent = path.parent().filter(|p| p.as_os_str().len() > 0);
    let mut file_name = if let Some(f) = path.file_name() {
        f.to_string_lossy().to_string()
    } else {
        // No file name (e.g., "dir/"), use the whole name after stripping slashes
        name.replace('\\', "").replace('/', "")
    };

    // Add .md extension if it doesn't already have .md extension
    if !file_name.ends_with(".md") {
        file_name.push_str(".md");
    }

    // Build the initial note path
    let mut note_path = if let Some(p) = parent {
        p.join(&file_name)
    } else {
        PathBuf::from(&file_name)
    };

    // Increment the file name until it doesn't exist in paths
    let mut counter = 1;
    while paths.contains(&note_path) {
        // Extract stem and extension
        let stem = Path::new(&file_name).file_stem().unwrap().to_string_lossy();
        let ext = Path::new(&file_name)
            .extension()
            .map(|e| format!(".{}", e.to_string_lossy()))
            .unwrap_or_default();

        // Create new file name with counter
        let new_file_name = format!("{} {}{}", stem, counter, ext);
        note_path = if let Some(p) = parent {
            p.join(&new_file_name)
        } else {
            PathBuf::from(&new_file_name)
        };

        counter += 1;
    }

    // Return (parent directory to create if any, note path to create)
    (parent.map(|p| p.to_path_buf()), note_path)
}

/// Resolve nested headings using subsequence matching with backtracking.
///
/// Given a list of heading referenceables (in document order) and a sequence of nested
/// heading texts to match, finds the first valid match that forms a proper hierarchy.
///
/// Uses backtracking to try all possible subsequence matches until finding one where
/// each heading level is strictly greater than the previous (forming a valid parent-child
/// hierarchy). This handles cases where identical heading names appear multiple times.
///
/// Returns the matched heading referenceable, or None if no valid match is found.
///
/// Example:
/// If headings in doc are: H2(L2), H4(L4), H3(L3), H2(L2), H4(L3), H3(L4)
/// - ["L2", "L4"] matches the first H4(L4) at index 1
/// - ["L2", "L3", "L4"] matches H3(L4) at index 5 (skips first invalid L2→L4→L3)
/// - ["L2", "L4", "L3"] returns None (no valid hierarchy exists)
fn resolve_nested_headings<'a>(
    headings: &[&'a Referenceable],
    nested_headings: &[&str],
) -> Option<Referenceable> {
    if nested_headings.is_empty() {
        return None;
    }

    // Extract heading texts in order
    let heading_texts: Vec<&str> = headings
        .iter()
        .filter_map(|r| match r {
            Referenceable::Heading { text, .. } => Some(text.as_str()),
            _ => None,
        })
        .collect();

    // Try to find all possible subsequence matches using backtracking
    fn find_valid_match<'a>(
        headings: &[&'a Referenceable],
        heading_texts: &[&str],
        nested_headings: &[&str],
        start_idx: usize,
        depth: usize,
        current_matches: &mut Vec<usize>,
    ) -> Option<usize> {
        // Base case: we've matched all nested headings
        if depth == nested_headings.len() {
            // Verify this match forms a valid hierarchy
            for i in 1..current_matches.len() {
                let prev_idx = current_matches[i - 1];
                let curr_idx = current_matches[i];

                if let (
                    Referenceable::Heading {
                        level: prev_level, ..
                    },
                    Referenceable::Heading {
                        level: curr_level, ..
                    },
                ) = (headings[prev_idx], headings[curr_idx])
                {
                    if (*curr_level as usize) <= (*prev_level as usize) {
                        // Invalid hierarchy - try another match
                        return None;
                    }
                }
            }
            // Valid hierarchy found - return the last matched index
            return Some(*current_matches.last().unwrap());
        }

        let target = nested_headings[depth];

        // Try to find the target heading starting from start_idx
        for i in start_idx..heading_texts.len() {
            if heading_texts[i] == target {
                current_matches.push(i);
                if let Some(result) = find_valid_match(
                    headings,
                    heading_texts,
                    nested_headings,
                    i + 1,
                    depth + 1,
                    current_matches,
                ) {
                    return Some(result);
                }
                current_matches.pop();
            }
        }

        None
    }

    let mut current_matches = Vec::new();
    let last_match_idx = find_valid_match(
        headings,
        &heading_texts,
        nested_headings,
        0,
        0,
        &mut current_matches,
    )?;

    // Return the heading at the last matched index
    headings.get(last_match_idx).map(|r| (*r).clone())
}

/// Builds links from references and referenceables.
/// Return a tuple of matched links and unresolved references.
///
/// - If heading reference is not found, fallback to note / asset reference.
///   - a reference to figure's heading is allowed but the heading part gets ignored.
fn build_links(
    references: Vec<Reference>,
    referenceable: Vec<Referenceable>,
) -> (Vec<Link>, Vec<Reference>) {
    let mut links = Vec::<Link>::new();
    let mut unresolved = Vec::<Reference>::new();

    // Build path-referenceable map
    let mut path_referenceable_map = HashMap::<PathBuf, Referenceable>::new();
    for referenceable in referenceable {
        path_referenceable_map
            .insert(referenceable.path().clone(), referenceable);
    }

    let referenceable_paths =
        path_referenceable_map.keys().cloned().collect::<Vec<_>>();

    for reference in references {
        let dest = reference.dest.as_str();
        let (file_name, nested_headings_opt, block_identifier_opt) =
            split_dest_string(dest);
        let matched_path_opt = resolve_link(file_name, &referenceable_paths);

        // If path is found
        if let Some(matched_path) = matched_path_opt {
            let file_referenceable =
                path_referenceable_map.get(&matched_path).unwrap();

            // If the matched file is a note
            if let Referenceable::Note { children, .. } = file_referenceable {
                // We see if there's heading or block identifier in the reference
                let matched_in_note_child: Option<Referenceable> = match (
                    nested_headings_opt,
                    block_identifier_opt,
                ) {
                    (None, None) => None,
                    (Some(_), Some(_)) => unreachable!(
                        "Never: nested headings and block identifier should not be present at the same time"
                    ),
                    (Some(nested_headings), None) => {
                        let headings = children
                            .iter()
                            .filter(|r| match r {
                                Referenceable::Heading { .. } => true,
                                _ => false,
                            })
                            .collect::<Vec<_>>();
                        resolve_nested_headings(&headings, &nested_headings)
                    }
                    (None, Some(block_identifier)) => {
                        // TODO: resolve block references
                        None
                    }
                };

                if let Some(in_note) = matched_in_note_child {
                    let link = Link {
                        from: reference,
                        to: in_note.clone(),
                    };
                    links.push(link);
                    continue;
                }
            }

            let link = Link {
                from: reference,
                to: file_referenceable.clone(),
            };
            links.push(link);
        } else {
            unresolved.push(reference);
        }
    }

    (links, unresolved)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::{assert_debug_snapshot, assert_snapshot};
    use std::fs;

    fn print_table(titles: &[&str], columns: &[Vec<&str>]) -> String {
        let mut output = String::new();

        // Calculate column widths (including index column and titles)
        let index_width = columns[0].len().to_string().len().max(3); // "Idx" width

        let mut widths: Vec<usize> = titles
            .iter()
            .zip(columns.iter())
            .map(|(title, col)| {
                title
                    .len()
                    .max(col.iter().map(|s| s.len()).max().unwrap_or(0))
            })
            .collect();

        // Print header separator
        output.push_str(&format!("+-{}-+", "-".repeat(index_width)));
        for &width in &widths {
            output.push_str(&format!("-{}-+", "-".repeat(width)));
        }
        output.push('\n');

        // Print header
        output.push_str(&format!("| {:^index_width$} |", "Idx"));
        for (i, &width) in widths.iter().enumerate() {
            output.push_str(&format!(" {:^width$} |", titles[i]));
        }
        output.push('\n');

        // Print header separator
        output.push_str(&format!("+-{}-+", "-".repeat(index_width)));
        for &width in &widths {
            output.push_str(&format!("-{}-+", "-".repeat(width)));
        }
        output.push('\n');

        // Print rows
        for i in 0..columns[0].len() {
            output.push_str(&format!("| {:>index_width$} |", i));
            for (j, &width) in widths.iter().enumerate() {
                output.push_str(&format!(" {:<width$} |", columns[j][i]));
            }
            output.push('\n');
        }

        // Print bottom separator
        output.push_str(&format!("+-{}-+", "-".repeat(index_width)));
        for &width in &widths {
            output.push_str(&format!("-{}-+", "-".repeat(width)));
        }
        output.push('\n');

        output
    }

    #[test]
    fn test_match_file() {
        let paths = vec![
            "Note 1.md",
            "Three laws of motion.md",
            "indir_same_name.md",
            "ww.md",
            "Figure 1.jpg.md",
            "Figure 1.jpg",
            "().md",
            "Figure1#2.jpg",
            "Figure 1.jpg.md.md",
            "dir.md",
            "empty_video.mp4",
            "Hi.txt.md",
            "block note.md",
            "dir/indir_same_name.md",
            "dir/indir2.md",
            "dir/inner_dir/note_in_inner_dir.md",
            "unsupported_text_file.txt",
            "unsupported_text_file.txt.md",
            "unsupported.unsupported",
            "Figure1.md",
            "Figure1^2.jpg",
            "Note 2.md",
            "Figure1|2.jpg",
            "Note 1",
            "Something",
            "a.joiwduvqneoi",
            "a.joiwduvqneoi.md",
        ]
        .iter()
        .map(|s| PathBuf::from(s))
        .collect::<Vec<_>>();

        let assert_match = |input: &str, expected: &str| {
            let matched = resolve_link(input, &paths)
                .and_then(|p| p.as_path().to_str().map(|s| s.to_string()));
            assert_eq!(matched, Some(expected.to_string()));
        };
        let assert_no_match = |input: &str| {
            let matched = resolve_link(input, &paths)
                .and_then(|p| p.as_path().to_str().map(|s| s.to_string()));
            assert_eq!(matched, None);
        };

        // Logic:
        // If the file name has no extension, we add `.md` to it.
        // So, a file called `Note 1` will never be matched as any attempt
        // to match it will understood as `Note 1.md`.
        //
        // Space is stipped
        //
        // Examples:
        // Basic
        assert_match("Note 1.md", "Note 1.md");
        assert_match("Note 1", "Note 1.md");
        // There exist both `Figure 1.jpg` and `Figure 1.jpg.md`, but `Figure 1.jpg` is matched
        assert_match("Figure 1.jpg", "Figure 1.jpg");
        assert_match("Figure 1.jpg.md", "Figure 1.jpg.md");
        assert_match("Figure 1.jpg.md.md", "Figure 1.jpg.md.md");
        assert_match("Figure1^2.jpg", "Figure1^2.jpg");
        assert_no_match("dir/");
        // `indir_same_name.md` at the top level won't be matched
        assert_match("dir/indir_same_name", "dir/indir_same_name.md");
        // matching of nested dirs is subsequence check (only match ancestor-descendant relationship)
        // All of the following 3 will match to `dir/inner_dir/note_in_inner_dir.md`
        assert_match(
            "dir/inner_dir/note_in_inner_dir",
            "dir/inner_dir/note_in_inner_dir.md",
        );
        assert_match(
            "inner_dir/note_in_inner_dir",
            "dir/inner_dir/note_in_inner_dir.md",
        );
        assert_match(
            "dir/note_in_inner_dir",
            "dir/inner_dir/note_in_inner_dir.md",
        );
        // This won't be matched as there's no dir named `random` in the vault
        assert_no_match("random/note_in_inner_dir");
        // Find note in the root-level first, and then fallback to note in subdirectory
        assert_match("dir/indir_same_name.md", "dir/indir_same_name.md");
        assert_match("indir_same_name.md", "indir_same_name.md"); // root, not dir version
        // There exist no note named `indir2.md` in root-level, but there's one in subdirectory
        assert_match("indir2.md", "dir/indir2.md"); // finds in subdirectory
        // File with no extension will never be matched
        assert_no_match("Something"); // no matched although there exist a file named `Something`
        // Points to file although the extension makes no sense
        // Explanation: it doesn't matter if the file is a supported format or not in https://help.obsidian.md/file-formats
        assert_match("a.joiwduvqneoi", "a.joiwduvqneoi");
    }

    #[test]
    fn test_parse_nested_heading() {
        let inputs = vec!["##A###B", "A##B", "A#####C#B", "##", "##C"];
        let outputs = inputs
            .iter()
            .map(|&s| parse_nested_heading(s))
            .map(|v| v.join("#")) // Join with '#'
            .collect::<Vec<_>>();
        let table = print_table(
            &vec!["Input", "Output"],
            &vec![inputs, outputs.iter().map(|s| s.as_str()).collect()],
        );
        assert_snapshot!(table, @r"
        +-----+-----------+--------+
        | Idx |   Input   | Output |
        +-----+-----------+--------+
        |   0 | ##A###B   | A#B    |
        |   1 | A##B      | A#B    |
        |   2 | A#####C#B | A#C#B  |
        |   3 | ##        |        |
        |   4 | ##C       | C      |
        +-----+-----------+--------+
        ");
    }

    #[test]
    fn test_split_dest_string() {
        // Get references from note 1
        let path = PathBuf::from("tests/data/vaults/tt/Note 1.md");
        let (references, _): (Vec<Reference>, Vec<Referenceable>) =
            scan_note(&path);
        let dest_strings: Vec<&str> =
            references.iter().map(|r| r.dest.as_str()).collect();
        let (note_names, nested_headings, block_identifiers): (
            Vec<&str>,
            Vec<Option<Vec<&str>>>,
            Vec<Option<&str>>,
        ) = dest_strings.iter().map(|s| split_dest_string(s)).collect();
        let nested_headings_str: Vec<String> = nested_headings
            .iter()
            .map(|v| match v {
                Some(v) => v.join("#"),
                None => "-".to_string(),
            })
            .collect();
        let table = print_table(
            &vec!["Destination string", "Note", "Headings"],
            &vec![
                dest_strings,
                note_names,
                nested_headings_str.iter().map(|s| s.as_str()).collect(),
            ],
        );
        assert_snapshot!(table, @r"
        +-----+-----------------------------------------+---------------------------------+----------------------------------+
        | Idx |           Destination string            |              Note               |             Headings             |
        +-----+-----------------------------------------+---------------------------------+----------------------------------+
        |   0 | Three laws of motion                    | Three laws of motion            | -                                |
        |   1 | #Level 3 title                          |                                 | Level 3 title                    |
        |   2 | Note 2#Some level 2 title               | Note 2                          | Some level 2 title               |
        |   3 | ()                                      | ()                              | -                                |
        |   4 | ww                                      | ww                              | -                                |
        |   5 | ()                                      | ()                              | -                                |
        |   6 | Three laws of motion                    | Three laws of motion            | -                                |
        |   7 | Three laws of motion                    | Three laws of motion            | -                                |
        |   8 | Three laws of motion.md                 | Three laws of motion.md         | -                                |
        |   9 | Note 2                                  | Note 2                          | -                                |
        |  10 | #Level 3 title                          |                                 | Level 3 title                    |
        |  11 | #Level 4 title                          |                                 | Level 4 title                    |
        |  12 | #random                                 |                                 | random                           |
        |  13 | Note 2#Some level 2 title               | Note 2                          | Some level 2 title               |
        |  14 | Note 2#Some level 2 title#Level 3 title | Note 2                          | Some level 2 title#Level 3 title |
        |  15 | Note 2#random#Level 3 title             | Note 2                          | random#Level 3 title             |
        |  16 | Note 2#Level 3 title                    | Note 2                          | Level 3 title                    |
        |  17 | Note 2#L4                               | Note 2                          | L4                               |
        |  18 | Note 2#Some level 2 title#L4            | Note 2                          | Some level 2 title#L4            |
        |  19 | Non-existing note 4                     | Non-existing note 4             | -                                |
        |  20 | #                                       |                                 |                                  |
        |  21 | Note 2##                                | Note 2                          |                                  |
        |  22 | #######Link to figure                   |                                 | Link to figure                   |
        |  23 | ######Link to figure                    |                                 | Link to figure                   |
        |  24 | ####Link to figure                      |                                 | Link to figure                   |
        |  25 | ###Link to figure                       |                                 | Link to figure                   |
        |  26 | #Link to figure                         |                                 | Link to figure                   |
        |  27 | #L2                                     |                                 | L2                               |
        |  28 | Note 2                                  | Note 2                          | -                                |
        |  29 | ###L2#L4                                |                                 | L2#L4                            |
        |  30 | ##L2######L4                            |                                 | L2#L4                            |
        |  31 | ##L2#####L4                             |                                 | L2#L4                            |
        |  32 | ##L2#####L4#L3                          |                                 | L2#L4#L3                         |
        |  33 | ##L2#####L4#Another L3                  |                                 | L2#L4#Another L3                 |
        |  34 | ##L2######L4                            |                                 | L2#L4                            |
        |  35 | ##L2#####L4                             |                                 | L2#L4                            |
        |  36 | ##L2#####L4#L3                          |                                 | L2#L4#L3                         |
        |  37 | Figure1.jpg                             | Figure1.jpg                     | -                                |
        |  38 | Figure1.jpg#2                           | Figure1.jpg                     | 2                                |
        |  39 | Figure1.jpg                             | Figure1.jpg                     | -                                |
        |  40 | Figure1.jpg.md                          | Figure1.jpg.md                  | -                                |
        |  41 | Figure1.jpg.md.md                       | Figure1.jpg.md.md               | -                                |
        |  42 | Figure1#2.jpg                           | Figure1                         | 2.jpg                            |
        |  43 | Figure1                                 | Figure1                         | -                                |
        |  44 | Figure1^2.jpg                           | Figure1^2.jpg                   | -                                |
        |  45 | dir/                                    | dir/                            | -                                |
        |  46 | dir/inner_dir/note_in_inner_dir         | dir/inner_dir/note_in_inner_dir | -                                |
        |  47 | inner_dir/note_in_inner_dir             | inner_dir/note_in_inner_dir     | -                                |
        |  48 | dir/note_in_inner_dir                   | dir/note_in_inner_dir           | -                                |
        |  49 | random/note_in_inner_dir                | random/note_in_inner_dir        | -                                |
        |  50 | inner_dir/hi                            | inner_dir/hi                    | -                                |
        |  51 | dir/indir_same_name                     | dir/indir_same_name             | -                                |
        |  52 | indir_same_name                         | indir_same_name                 | -                                |
        |  53 | indir2                                  | indir2                          | -                                |
        |  54 | Something                               | Something                       | -                                |
        |  55 | unsupported_text_file.txt               | unsupported_text_file.txt       | -                                |
        |  56 | a.joiwduvqneoi                          | a.joiwduvqneoi                  | -                                |
        |  57 | Note 1                                  | Note 1                          | -                                |
        |  58 | Figure1.jpg                             | Figure1.jpg                     | -                                |
        |  59 | empty_video.mp4                         | empty_video.mp4                 | -                                |
        +-----+-----------------------------------------+---------------------------------+----------------------------------+
        ");
    }

    mod resolve_nested_headings {
        use super::*;

        /// Helper to create a list of headings from (level, text) tuples.
        /// Automatically assigns sequential ranges for each heading.
        fn headings(specs: &[(HeadingLevel, &str)]) -> Vec<Referenceable> {
            specs
                .iter()
                .enumerate()
                .map(|(i, (level, text))| {
                    let start = i * 10;
                    let end = start + 10;
                    Referenceable::Heading {
                        path: PathBuf::from("test.md"),
                        level: *level,
                        text: text.to_string(),
                        range: start..end,
                    }
                })
                .collect()
        }

        #[test]
        fn test_simple_match() {
            let hs = headings(&[
                (HeadingLevel::H1, "Chapter 1"),
                (HeadingLevel::H2, "Section 1.1"),
            ]);
            let refs: Vec<_> = hs.iter().collect();

            let result = super::resolve_nested_headings(
                &refs,
                &["Chapter 1", "Section 1.1"],
            );
            assert_eq!(result, Some(hs[1].clone()));
        }

        #[test]
        fn test_invalid_hierarchy_returns_none() {
            let hs = headings(&[
                (HeadingLevel::H1, "Chapter 1"),
                (HeadingLevel::H3, "Section 1.1.1"),
                (HeadingLevel::H2, "Section 2.1"),
            ]);
            let refs: Vec<_> = hs.iter().collect();

            let result = super::resolve_nested_headings(
                &refs,
                &["Chapter 1", "Section 1.1.1", "Section 2.1"],
            );
            assert_eq!(result, None);
        }

        #[test]
        fn test_duplicate_names_finds_valid_match() {
            // First match attempt: Chapter 1 → Section 1.2 → Section 1.1 (invalid: H4→H3)
            // Second match: Chapter 1 → Section 1.1 → Section 1.1.1 (valid: H2→H3→H4)
            let hs = headings(&[
                (HeadingLevel::H1, "1"),
                (HeadingLevel::H3, "1.1"),
                (HeadingLevel::H2, "1.1.1"),
                (HeadingLevel::H1, "1"),
                (HeadingLevel::H2, "1.1"),
                (HeadingLevel::H3, "1.1.1"),
            ]);
            let refs: Vec<_> = hs.iter().collect();

            let result =
                super::resolve_nested_headings(&refs, &["1", "1.1", "1.1.1"]);
            assert_eq!(result, Some(hs[5].clone()));
        }

        #[test]
        fn test_first_valid_match_is_returned() {
            // Both matches are valid; should return the first one
            let hs = headings(&[
                (HeadingLevel::H1, "Chapter 1"),
                (HeadingLevel::H2, "Section 1.1"),
                (HeadingLevel::H1, "Chapter 1"),
                (HeadingLevel::H2, "Section 1.1"),
            ]);
            let refs: Vec<_> = hs.iter().collect();

            let result = super::resolve_nested_headings(
                &refs,
                &["Chapter 1", "Section 1.1"],
            );
            assert_eq!(result, Some(hs[1].clone()));
        }

        #[test]
        fn test_single_heading() {
            let hs = headings(&[(HeadingLevel::H1, "Chapter 1")]);
            let refs: Vec<_> = hs.iter().collect();

            let result = super::resolve_nested_headings(&refs, &["Chapter 1"]);
            assert_eq!(result, Some(hs[0].clone()));
        }

        #[test]
        fn test_no_match() {
            let hs = headings(&[
                (HeadingLevel::H1, "Chapter 1"),
                (HeadingLevel::H2, "Section 1.1"),
            ]);
            let refs: Vec<_> = hs.iter().collect();

            let result = super::resolve_nested_headings(
                &refs,
                &["Chapter 1", "Section 1.2"],
            );
            assert_eq!(result, None);
        }

        #[test]
        fn test_empty_nested_headings() {
            let hs = headings(&[(HeadingLevel::H1, "Chapter 1")]);
            let refs: Vec<_> = hs.iter().collect();

            let result = super::resolve_nested_headings(&refs, &[]);
            assert_eq!(result, None);
        }

        #[test]
        fn test_deep_hierarchy() {
            let hs = headings(&[
                (HeadingLevel::H1, "Part 1"),
                (HeadingLevel::H2, "Chapter 1.1"),
                (HeadingLevel::H3, "Section 1.1.1"),
                (HeadingLevel::H4, "Subsection 1.1.1.1"),
                (HeadingLevel::H5, "Paragraph 1.1.1.1.1"),
                (HeadingLevel::H6, "Subparagraph 1.1.1.1.1.1"),
            ]);
            let refs: Vec<_> = hs.iter().collect();

            let result = super::resolve_nested_headings(
                &refs,
                &[
                    "Part 1",
                    "Section 1.1.1",
                    "Paragraph 1.1.1.1.1",
                    "Subparagraph 1.1.1.1.1.1",
                ],
            );
            assert_eq!(result, Some(hs[5].clone()));
        }

        #[test]
        fn test_same_level_headings_no_match() {
            let hs = headings(&[
                (HeadingLevel::H1, "Chapter 1"),
                (HeadingLevel::H1, "Chapter 2"),
            ]);
            let refs: Vec<_> = hs.iter().collect();

            let result = super::resolve_nested_headings(
                &refs,
                &["Chapter 1", "Chapter 2"],
            );
            assert_eq!(result, None);
        }

        #[test]
        fn test_skip_multiple_invalid_matches() {
            let hs = headings(&[
                (HeadingLevel::H1, "Chapter 1"),
                (HeadingLevel::H1, "Chapter 2"),
                (HeadingLevel::H2, "Section 2.1"),
                (HeadingLevel::H1, "Chapter 1"),
                (HeadingLevel::H2, "Section 1.1"),
                (HeadingLevel::H3, "Section 1.1.1"),
            ]);
            let refs: Vec<_> = hs.iter().collect();

            let result = super::resolve_nested_headings(
                &refs,
                &["Chapter 1", "Section 1.1", "Section 1.1.1"],
            );
            assert_eq!(result, Some(hs[5].clone()));
        }
    }

    mod resolve_new_note_path {
        use super::*;

        #[test]
        fn simple_name_with_md_extension() {
            let (dir, note) = resolve_new_note_path("Note 1.md", &vec![]);
            assert_eq!(dir, None);
            assert_eq!(note, PathBuf::from("Note 1.md"));
        }

        #[test]
        fn simple_name_without_extension() {
            let (dir, note) = resolve_new_note_path("Something", &vec![]);
            assert_eq!(dir, None);
            assert_eq!(note, PathBuf::from("Something.md"));
        }

        #[test]
        fn with_directory_no_extension() {
            let (dir, note) = resolve_new_note_path("A/B", &vec![]);
            assert_eq!(dir, Some(PathBuf::from("A")));
            assert_eq!(note, PathBuf::from("A/B.md"));
        }

        #[test]
        fn with_directory_and_md_extension() {
            let (dir, note) = resolve_new_note_path("A/B.md", &vec![]);
            assert_eq!(dir, Some(PathBuf::from("A")));
            assert_eq!(note, PathBuf::from("A/B.md"));
        }

        #[test]
        fn increment_when_file_exists() {
            let (dir, note) = resolve_new_note_path(
                "Untitle",
                &vec![PathBuf::from("Untitle.md")],
            );
            assert_eq!(dir, None);
            assert_eq!(note, PathBuf::from("Untitle 1.md"));
        }

        #[test]
        fn increment_multiple_times() {
            let (dir, note) = resolve_new_note_path(
                "Untitle",
                &vec![
                    PathBuf::from("Untitle.md"),
                    PathBuf::from("Untitle 1.md"),
                    PathBuf::from("Untitle 2.md"),
                ],
            );
            assert_eq!(dir, None);
            assert_eq!(note, PathBuf::from("Untitle 3.md"));
        }

        #[test]
        fn trailing_slash() {
            let (dir, note) = resolve_new_note_path("dir/", &vec![]);
            assert_eq!(dir, None);
            assert_eq!(note, PathBuf::from("dir.md"));
        }

        #[test]
        fn trailing_slash_with_existing_file() {
            let (dir, note) =
                resolve_new_note_path("dir/", &vec![PathBuf::from("dir.md")]);
            assert_eq!(dir, None);
            assert_eq!(note, PathBuf::from("dir 1.md"));
        }

        #[test]
        fn non_md_extension() {
            let (dir, note) = resolve_new_note_path("Hi.txt", &vec![]);
            assert_eq!(dir, None);
            assert_eq!(note, PathBuf::from("Hi.txt.md"));
        }

        #[test]
        fn already_has_md_extension() {
            let (dir, note) = resolve_new_note_path("Hi.md", &vec![]);
            assert_eq!(dir, None);
            assert_eq!(note, PathBuf::from("Hi.md"));
        }

        #[test]
        fn nested_directory_path() {
            let (dir, note) = resolve_new_note_path("A/B/C", &vec![]);
            assert_eq!(dir, Some(PathBuf::from("A/B")));
            assert_eq!(note, PathBuf::from("A/B/C.md"));
        }

        #[test]
        fn whitespace_trimming() {
            let (dir, note) =
                resolve_new_note_path("  Note with spaces  ", &vec![]);
            assert_eq!(dir, None);
            assert_eq!(note, PathBuf::from("Note with spaces.md"));
        }

        #[test]
        fn increment_in_subdirectory() {
            let (dir, note) = resolve_new_note_path(
                "dir/note",
                &vec![PathBuf::from("dir/note.md")],
            );
            assert_eq!(dir, Some(PathBuf::from("dir")));
            assert_eq!(note, PathBuf::from("dir/note 1.md"));
        }
    }

    #[test]
    fn test_scan_vault() {
        let dir = PathBuf::from("tests/data/vaults/tt");
        let (referenceables, references) = scan_vault(&dir);
        assert_debug_snapshot!(references, @r########"
        [
            Reference {
                range: 167..223,
                dest: "Three laws of motion",
                kind: MarkdownLink,
                display_text: "Three laws of motion 11",
            },
            Reference {
                range: 254..289,
                dest: "#Level 3 title",
                kind: MarkdownLink,
                display_text: "Level 3 title",
            },
            Reference {
                range: 356..395,
                dest: "Note 2#Some level 2 title",
                kind: MarkdownLink,
                display_text: "22",
            },
            Reference {
                range: 514..521,
                dest: "()",
                kind: MarkdownLink,
                display_text: "www",
            },
            Reference {
                range: 590..596,
                dest: "ww",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 647..651,
                dest: "()",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 700..733,
                dest: "Three laws of motion",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 951..974,
                dest: "Three laws of motion",
                kind: WikiLink,
                display_text: "Three laws of motion",
            },
            Reference {
                range: 1015..1041,
                dest: "Three laws of motion.md",
                kind: WikiLink,
                display_text: "Three laws of motion.md",
            },
            Reference {
                range: 1075..1095,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " Note two",
            },
            Reference {
                range: 1125..1142,
                dest: "#Level 3 title",
                kind: WikiLink,
                display_text: "#Level 3 title",
            },
            Reference {
                range: 1203..1220,
                dest: "#Level 4 title",
                kind: WikiLink,
                display_text: "#Level 4 title",
            },
            Reference {
                range: 1282..1292,
                dest: "#random",
                kind: WikiLink,
                display_text: "#random",
            },
            Reference {
                range: 1358..1386,
                dest: "Note 2#Some level 2 title",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title",
            },
            Reference {
                range: 1457..1499,
                dest: "Note 2#Some level 2 title#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title#Level 3 title",
            },
            Reference {
                range: 1536..1566,
                dest: "Note 2#random#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#random#Level 3 title",
            },
            Reference {
                range: 1645..1668,
                dest: "Note 2#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#Level 3 title",
            },
            Reference {
                range: 1697..1709,
                dest: "Note 2#L4",
                kind: WikiLink,
                display_text: "Note 2#L4",
            },
            Reference {
                range: 1745..1776,
                dest: "Note 2#Some level 2 title#L4",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title#L4",
            },
            Reference {
                range: 1942..1964,
                dest: "Non-existing note 4",
                kind: WikiLink,
                display_text: "Non-existing note 4",
            },
            Reference {
                range: 2040..2044,
                dest: "#",
                kind: WikiLink,
                display_text: "#",
            },
            Reference {
                range: 2094..2105,
                dest: "Note 2##",
                kind: WikiLink,
                display_text: "Note 2##",
            },
            Reference {
                range: 2186..2210,
                dest: "#######Link to figure",
                kind: WikiLink,
                display_text: "#######Link to figure",
            },
            Reference {
                range: 2243..2266,
                dest: "######Link to figure",
                kind: WikiLink,
                display_text: "######Link to figure",
            },
            Reference {
                range: 2297..2318,
                dest: "####Link to figure",
                kind: WikiLink,
                display_text: "####Link to figure",
            },
            Reference {
                range: 2348..2368,
                dest: "###Link to figure",
                kind: WikiLink,
                display_text: "###Link to figure",
            },
            Reference {
                range: 2396..2414,
                dest: "#Link to figure",
                kind: WikiLink,
                display_text: "#Link to figure",
            },
            Reference {
                range: 2447..2459,
                dest: "#L2 ",
                kind: WikiLink,
                display_text: " #L4",
            },
            Reference {
                range: 2554..2571,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " 2 | 3",
            },
            Reference {
                range: 2672..2683,
                dest: "###L2#L4",
                kind: WikiLink,
                display_text: "###L2#L4",
            },
            Reference {
                range: 2743..2758,
                dest: "##L2######L4",
                kind: WikiLink,
                display_text: "##L2######L4",
            },
            Reference {
                range: 2817..2831,
                dest: "##L2#####L4",
                kind: WikiLink,
                display_text: "##L2#####L4",
            },
            Reference {
                range: 2893..2910,
                dest: "##L2#####L4#L3",
                kind: WikiLink,
                display_text: "##L2#####L4#L3",
            },
            Reference {
                range: 2966..2991,
                dest: "##L2#####L4#Another L3",
                kind: WikiLink,
                display_text: "##L2#####L4#Another L3",
            },
            Reference {
                range: 3332..3349,
                dest: "##L2######L4",
                kind: MarkdownLink,
                display_text: "1",
            },
            Reference {
                range: 3408..3424,
                dest: "##L2#####L4",
                kind: MarkdownLink,
                display_text: "2",
            },
            Reference {
                range: 3486..3505,
                dest: "##L2#####L4#L3",
                kind: MarkdownLink,
                display_text: "3",
            },
            Reference {
                range: 3577..3591,
                dest: "Figure1.jpg",
                kind: WikiLink,
                display_text: "Figure1.jpg",
            },
            Reference {
                range: 3700..3716,
                dest: "Figure1.jpg#2",
                kind: WikiLink,
                display_text: "Figure1.jpg#2",
            },
            Reference {
                range: 3762..3780,
                dest: "Figure1.jpg ",
                kind: WikiLink,
                display_text: " 2",
            },
            Reference {
                range: 3867..3884,
                dest: "Figure1.jpg.md",
                kind: WikiLink,
                display_text: "Figure1.jpg.md",
            },
            Reference {
                range: 3975..3995,
                dest: "Figure1.jpg.md.md",
                kind: WikiLink,
                display_text: "Figure1.jpg.md.md",
            },
            Reference {
                range: 4020..4036,
                dest: "Figure1#2.jpg",
                kind: WikiLink,
                display_text: "Figure1#2.jpg",
            },
            Reference {
                range: 4159..4175,
                dest: "Figure1",
                kind: WikiLink,
                display_text: "2.jpg",
            },
            Reference {
                range: 4298..4314,
                dest: "Figure1^2.jpg",
                kind: WikiLink,
                display_text: "Figure1^2.jpg",
            },
            Reference {
                range: 4424..4431,
                dest: "dir/",
                kind: WikiLink,
                display_text: "dir/",
            },
            Reference {
                range: 4861..4895,
                dest: "dir/inner_dir/note_in_inner_dir",
                kind: WikiLink,
                display_text: "dir/inner_dir/note_in_inner_dir",
            },
            Reference {
                range: 4935..4965,
                dest: "inner_dir/note_in_inner_dir",
                kind: WikiLink,
                display_text: "inner_dir/note_in_inner_dir",
            },
            Reference {
                range: 4999..5023,
                dest: "dir/note_in_inner_dir",
                kind: WikiLink,
                display_text: "dir/note_in_inner_dir",
            },
            Reference {
                range: 5095..5122,
                dest: "random/note_in_inner_dir",
                kind: WikiLink,
                display_text: "random/note_in_inner_dir",
            },
            Reference {
                range: 5263..5278,
                dest: "inner_dir/hi",
                kind: WikiLink,
                display_text: "inner_dir/hi",
            },
            Reference {
                range: 5309..5331,
                dest: "dir/indir_same_name",
                kind: WikiLink,
                display_text: "dir/indir_same_name",
            },
            Reference {
                range: 5358..5376,
                dest: "indir_same_name",
                kind: WikiLink,
                display_text: "indir_same_name",
            },
            Reference {
                range: 5446..5455,
                dest: "indir2",
                kind: WikiLink,
                display_text: "indir2",
            },
            Reference {
                range: 5502..5514,
                dest: "Something",
                kind: WikiLink,
                display_text: "Something",
            },
            Reference {
                range: 5631..5659,
                dest: "unsupported_text_file.txt",
                kind: WikiLink,
                display_text: "unsupported_text_file.txt",
            },
            Reference {
                range: 5740..5757,
                dest: "a.joiwduvqneoi",
                kind: WikiLink,
                display_text: "a.joiwduvqneoi",
            },
            Reference {
                range: 5793..5802,
                dest: "Note 1",
                kind: WikiLink,
                display_text: "Note 1",
            },
            Reference {
                range: 5896..5911,
                dest: "Figure1.jpg",
                kind: WikiLink,
                display_text: "Figure1.jpg",
            },
            Reference {
                range: 5936..5954,
                dest: "empty_video.mp4",
                kind: WikiLink,
                display_text: "empty_video.mp4",
            },
            Reference {
                range: 43..58,
                dest: "Note 1#^afon",
                kind: WikiLink,
                display_text: "Note 1#^afon",
            },
            Reference {
                range: 60..71,
                dest: "#^1 dwad",
                kind: WikiLink,
                display_text: "#^1 dwad",
            },
            Reference {
                range: 73..86,
                dest: "#^insidel6",
                kind: WikiLink,
                display_text: "#^insidel6",
            },
            Reference {
                range: 88..104,
                dest: "#L6#^insidel6",
                kind: WikiLink,
                display_text: "#L6#^insidel6",
            },
        ]
        "########);
        assert_debug_snapshot!(referenceables, @r#"
        [
            Note {
                path: "tests/data/vaults/tt/Note 1.md",
                children: [
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H3,
                        text: "Level 3 title",
                        range: 55..73,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H4,
                        text: "Level 4 title",
                        range: 73..92,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H3,
                        text: "Example (level 3)",
                        range: 93..115,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H6,
                        text: "Markdown link: [x](y)",
                        range: 116..147,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H6,
                        text: "Wiki link: [[x#]] | [[x#^block_identifier]]",
                        range: 887..942,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H5,
                        text: "Link to asset",
                        range: 3536..3556,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H2,
                        text: "L2",
                        range: 5957..5963,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H3,
                        text: "L3",
                        range: 5964..5971,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H4,
                        text: "L4",
                        range: 5971..5979,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H3,
                        text: "Another L3",
                        range: 5979..5994,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 1.md",
                        level: H2,
                        text: "",
                        range: 5999..6003,
                    },
                ],
            },
            Asset {
                path: "tests/data/vaults/tt/a.joiwduvqneoi",
            },
            Note {
                path: "tests/data/vaults/tt/Figure1.jpg.md",
                children: [],
            },
            Asset {
                path: "tests/data/vaults/tt/Something",
            },
            Note {
                path: "tests/data/vaults/tt/Three laws of motion.md",
                children: [],
            },
            Note {
                path: "tests/data/vaults/tt/indir_same_name.md",
                children: [],
            },
            Note {
                path: "tests/data/vaults/tt/ww.md",
                children: [],
            },
            Note {
                path: "tests/data/vaults/tt/unsupported_text_file.txt.md",
                children: [],
            },
            Note {
                path: "tests/data/vaults/tt/Figure1.jpg.md.md",
                children: [],
            },
            Note {
                path: "tests/data/vaults/tt/().md",
                children: [],
            },
            Asset {
                path: "tests/data/vaults/tt/Figure1#2.jpg",
            },
            Asset {
                path: "tests/data/vaults/tt/Note 1",
            },
            Note {
                path: "tests/data/vaults/tt/dir.md",
                children: [],
            },
            Note {
                path: "tests/data/vaults/tt/a.joiwduvqneoi.md",
                children: [],
            },
            Asset {
                path: "tests/data/vaults/tt/empty_video.mp4",
            },
            Note {
                path: "tests/data/vaults/tt/Hi.txt.md",
                children: [],
            },
            Note {
                path: "tests/data/vaults/tt/dir/inner_dir/note_in_inner_dir.md",
                children: [],
            },
            Note {
                path: "tests/data/vaults/tt/dir/indir_same_name.md",
                children: [],
            },
            Note {
                path: "tests/data/vaults/tt/dir/indir2.md",
                children: [],
            },
            Asset {
                path: "tests/data/vaults/tt/unsupported_text_file.txt",
            },
            Asset {
                path: "tests/data/vaults/tt/unsupported.unsupported",
            },
            Note {
                path: "tests/data/vaults/tt/Figure1.md",
                children: [],
            },
            Asset {
                path: "tests/data/vaults/tt/Figure1^2.jpg",
            },
            Asset {
                path: "tests/data/vaults/tt/Figure1.jpg",
            },
            Note {
                path: "tests/data/vaults/tt/block ref.md",
                children: [
                    Heading {
                        path: "tests/data/vaults/tt/block ref.md",
                        level: H6,
                        text: "L6",
                        range: 179..189,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/block ref.md",
                        level: H6,
                        text: "^1 dwad",
                        range: 219..233,
                    },
                ],
            },
            Note {
                path: "tests/data/vaults/tt/Note 2.md",
                children: [
                    Heading {
                        path: "tests/data/vaults/tt/Note 2.md",
                        level: H2,
                        text: "Some level 2 title",
                        range: 1..23,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 2.md",
                        level: H4,
                        text: "L4",
                        range: 24..32,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 2.md",
                        level: H3,
                        text: "Level 3 title",
                        range: 33..51,
                    },
                    Heading {
                        path: "tests/data/vaults/tt/Note 2.md",
                        level: H2,
                        text: "Another level 2 title",
                        range: 53..77,
                    },
                ],
            },
            Asset {
                path: "tests/data/vaults/tt/Figure1|2.jpg",
            },
        ]
        "#);
    }

    #[test]
    fn test_exract_references_and_referenceables() {
        let path = PathBuf::from("tests/data/vaults/tt/Note 1.md");
        let (references, referenceables): (Vec<Reference>, Vec<Referenceable>) =
            scan_note(&path);
        assert_debug_snapshot!(references, @r########"
        [
            Reference {
                range: 167..223,
                dest: "Three laws of motion",
                kind: MarkdownLink,
                display_text: "Three laws of motion 11",
            },
            Reference {
                range: 254..289,
                dest: "#Level 3 title",
                kind: MarkdownLink,
                display_text: "Level 3 title",
            },
            Reference {
                range: 356..395,
                dest: "Note 2#Some level 2 title",
                kind: MarkdownLink,
                display_text: "22",
            },
            Reference {
                range: 514..521,
                dest: "()",
                kind: MarkdownLink,
                display_text: "www",
            },
            Reference {
                range: 590..596,
                dest: "ww",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 647..651,
                dest: "()",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 700..733,
                dest: "Three laws of motion",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 951..974,
                dest: "Three laws of motion",
                kind: WikiLink,
                display_text: "Three laws of motion",
            },
            Reference {
                range: 1015..1041,
                dest: "Three laws of motion.md",
                kind: WikiLink,
                display_text: "Three laws of motion.md",
            },
            Reference {
                range: 1075..1095,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " Note two",
            },
            Reference {
                range: 1125..1142,
                dest: "#Level 3 title",
                kind: WikiLink,
                display_text: "#Level 3 title",
            },
            Reference {
                range: 1203..1220,
                dest: "#Level 4 title",
                kind: WikiLink,
                display_text: "#Level 4 title",
            },
            Reference {
                range: 1282..1292,
                dest: "#random",
                kind: WikiLink,
                display_text: "#random",
            },
            Reference {
                range: 1358..1386,
                dest: "Note 2#Some level 2 title",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title",
            },
            Reference {
                range: 1457..1499,
                dest: "Note 2#Some level 2 title#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title#Level 3 title",
            },
            Reference {
                range: 1536..1566,
                dest: "Note 2#random#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#random#Level 3 title",
            },
            Reference {
                range: 1645..1668,
                dest: "Note 2#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#Level 3 title",
            },
            Reference {
                range: 1697..1709,
                dest: "Note 2#L4",
                kind: WikiLink,
                display_text: "Note 2#L4",
            },
            Reference {
                range: 1745..1776,
                dest: "Note 2#Some level 2 title#L4",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title#L4",
            },
            Reference {
                range: 1942..1964,
                dest: "Non-existing note 4",
                kind: WikiLink,
                display_text: "Non-existing note 4",
            },
            Reference {
                range: 2040..2044,
                dest: "#",
                kind: WikiLink,
                display_text: "#",
            },
            Reference {
                range: 2094..2105,
                dest: "Note 2##",
                kind: WikiLink,
                display_text: "Note 2##",
            },
            Reference {
                range: 2186..2210,
                dest: "#######Link to figure",
                kind: WikiLink,
                display_text: "#######Link to figure",
            },
            Reference {
                range: 2243..2266,
                dest: "######Link to figure",
                kind: WikiLink,
                display_text: "######Link to figure",
            },
            Reference {
                range: 2297..2318,
                dest: "####Link to figure",
                kind: WikiLink,
                display_text: "####Link to figure",
            },
            Reference {
                range: 2348..2368,
                dest: "###Link to figure",
                kind: WikiLink,
                display_text: "###Link to figure",
            },
            Reference {
                range: 2396..2414,
                dest: "#Link to figure",
                kind: WikiLink,
                display_text: "#Link to figure",
            },
            Reference {
                range: 2447..2459,
                dest: "#L2 ",
                kind: WikiLink,
                display_text: " #L4",
            },
            Reference {
                range: 2554..2571,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " 2 | 3",
            },
            Reference {
                range: 2672..2683,
                dest: "###L2#L4",
                kind: WikiLink,
                display_text: "###L2#L4",
            },
            Reference {
                range: 2743..2758,
                dest: "##L2######L4",
                kind: WikiLink,
                display_text: "##L2######L4",
            },
            Reference {
                range: 2817..2831,
                dest: "##L2#####L4",
                kind: WikiLink,
                display_text: "##L2#####L4",
            },
            Reference {
                range: 2893..2910,
                dest: "##L2#####L4#L3",
                kind: WikiLink,
                display_text: "##L2#####L4#L3",
            },
            Reference {
                range: 2966..2991,
                dest: "##L2#####L4#Another L3",
                kind: WikiLink,
                display_text: "##L2#####L4#Another L3",
            },
            Reference {
                range: 3332..3349,
                dest: "##L2######L4",
                kind: MarkdownLink,
                display_text: "1",
            },
            Reference {
                range: 3408..3424,
                dest: "##L2#####L4",
                kind: MarkdownLink,
                display_text: "2",
            },
            Reference {
                range: 3486..3505,
                dest: "##L2#####L4#L3",
                kind: MarkdownLink,
                display_text: "3",
            },
            Reference {
                range: 3577..3591,
                dest: "Figure1.jpg",
                kind: WikiLink,
                display_text: "Figure1.jpg",
            },
            Reference {
                range: 3700..3716,
                dest: "Figure1.jpg#2",
                kind: WikiLink,
                display_text: "Figure1.jpg#2",
            },
            Reference {
                range: 3762..3780,
                dest: "Figure1.jpg ",
                kind: WikiLink,
                display_text: " 2",
            },
            Reference {
                range: 3867..3884,
                dest: "Figure1.jpg.md",
                kind: WikiLink,
                display_text: "Figure1.jpg.md",
            },
            Reference {
                range: 3975..3995,
                dest: "Figure1.jpg.md.md",
                kind: WikiLink,
                display_text: "Figure1.jpg.md.md",
            },
            Reference {
                range: 4020..4036,
                dest: "Figure1#2.jpg",
                kind: WikiLink,
                display_text: "Figure1#2.jpg",
            },
            Reference {
                range: 4159..4175,
                dest: "Figure1",
                kind: WikiLink,
                display_text: "2.jpg",
            },
            Reference {
                range: 4298..4314,
                dest: "Figure1^2.jpg",
                kind: WikiLink,
                display_text: "Figure1^2.jpg",
            },
            Reference {
                range: 4424..4431,
                dest: "dir/",
                kind: WikiLink,
                display_text: "dir/",
            },
            Reference {
                range: 4861..4895,
                dest: "dir/inner_dir/note_in_inner_dir",
                kind: WikiLink,
                display_text: "dir/inner_dir/note_in_inner_dir",
            },
            Reference {
                range: 4935..4965,
                dest: "inner_dir/note_in_inner_dir",
                kind: WikiLink,
                display_text: "inner_dir/note_in_inner_dir",
            },
            Reference {
                range: 4999..5023,
                dest: "dir/note_in_inner_dir",
                kind: WikiLink,
                display_text: "dir/note_in_inner_dir",
            },
            Reference {
                range: 5095..5122,
                dest: "random/note_in_inner_dir",
                kind: WikiLink,
                display_text: "random/note_in_inner_dir",
            },
            Reference {
                range: 5263..5278,
                dest: "inner_dir/hi",
                kind: WikiLink,
                display_text: "inner_dir/hi",
            },
            Reference {
                range: 5309..5331,
                dest: "dir/indir_same_name",
                kind: WikiLink,
                display_text: "dir/indir_same_name",
            },
            Reference {
                range: 5358..5376,
                dest: "indir_same_name",
                kind: WikiLink,
                display_text: "indir_same_name",
            },
            Reference {
                range: 5446..5455,
                dest: "indir2",
                kind: WikiLink,
                display_text: "indir2",
            },
            Reference {
                range: 5502..5514,
                dest: "Something",
                kind: WikiLink,
                display_text: "Something",
            },
            Reference {
                range: 5631..5659,
                dest: "unsupported_text_file.txt",
                kind: WikiLink,
                display_text: "unsupported_text_file.txt",
            },
            Reference {
                range: 5740..5757,
                dest: "a.joiwduvqneoi",
                kind: WikiLink,
                display_text: "a.joiwduvqneoi",
            },
            Reference {
                range: 5793..5802,
                dest: "Note 1",
                kind: WikiLink,
                display_text: "Note 1",
            },
            Reference {
                range: 5896..5911,
                dest: "Figure1.jpg",
                kind: WikiLink,
                display_text: "Figure1.jpg",
            },
            Reference {
                range: 5936..5954,
                dest: "empty_video.mp4",
                kind: WikiLink,
                display_text: "empty_video.mp4",
            },
        ]
        "########);
        assert_debug_snapshot!(referenceables, @r#"
        [
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                text: "Level 3 title",
                range: 55..73,
            },
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H4,
                text: "Level 4 title",
                range: 73..92,
            },
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                text: "Example (level 3)",
                range: 93..115,
            },
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H6,
                text: "Markdown link: [x](y)",
                range: 116..147,
            },
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H6,
                text: "Wiki link: [[x#]] | [[x#^block_identifier]]",
                range: 887..942,
            },
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H5,
                text: "Link to asset",
                range: 3536..3556,
            },
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H2,
                text: "L2",
                range: 5957..5963,
            },
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                text: "L3",
                range: 5964..5971,
            },
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H4,
                text: "L4",
                range: 5971..5979,
            },
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                text: "Another L3",
                range: 5979..5994,
            },
            Heading {
                path: "tests/data/vaults/tt/Note 1.md",
                level: H2,
                text: "",
                range: 5999..6003,
            },
        ]
        "#);
    }

    #[test]
    fn test_parse_ast_with_links() {
        let path = "tests/data/vaults/tt/Note 1.md";
        let text = fs::read_to_string(path).unwrap();
        let tree = Tree::new(&text);
        assert_snapshot!(tree.root_node, @r########"
        Document [0..6003]
          List(None) [0..55]
            Item [0..55]
              Text(Borrowed("Note in Obsidian cannot have # ^ ")) [2..35]
              Text(Borrowed("[")) [35..36]
              Text(Borrowed(" ")) [36..37]
              Text(Borrowed("]")) [37..38]
              Text(Borrowed(" | in the title.")) [38..54]
          Heading { level: H3, id: None, classes: [], attrs: [] } [55..73]
            Text(Borrowed("Level 3 title")) [59..72]
          Heading { level: H4, id: None, classes: [], attrs: [] } [73..92]
            Text(Borrowed("Level 4 title")) [78..91]
          Heading { level: H3, id: None, classes: [], attrs: [] } [93..115]
            Text(Borrowed("Example (level 3)")) [97..114]
          Heading { level: H6, id: None, classes: [], attrs: [] } [116..147]
            Text(Borrowed("Markdown link: ")) [123..138]
            Code(Borrowed("[x](y)")) [138..146]
          List(None) [147..887]
            Item [147..224]
              Text(Borrowed("percent encoding: ")) [149..167]
              Link { link_type: Inline, dest_url: Borrowed("Three%20laws%20of%20motion.md"), title: Borrowed(""), id: Borrowed("") } [167..223]
                Text(Borrowed("Three laws of motion 11")) [168..191]
            Item [224..331]
              Text(Borrowed("heading  in the same file:  ")) [226..254]
              Link { link_type: Inline, dest_url: Borrowed("#Level%203%20title"), title: Borrowed(""), id: Borrowed("") } [254..289]
                Text(Borrowed("Level 3 title")) [255..268]
              List(None) [289..331]
                Item [289..331]
                  Code(Borrowed("[Level 3 title](#Level%203%20title)")) [293..330]
            Item [331..499]
              Text(Borrowed("different file heading ")) [333..356]
              Link { link_type: Inline, dest_url: Borrowed("Note%202#Some%20level%202%20title"), title: Borrowed(""), id: Borrowed("") } [356..395]
                Text(Borrowed("22")) [357..359]
              List(None) [395..499]
                Item [395..441]
                  Code(Borrowed("[22](Note%202#Some%20level%202%20title)")) [399..440]
                Item [440..499]
                  Text(Borrowed("the heading is level 2 but we don")) [444..477]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [477..478]
                  Text(Borrowed("t need to specify it")) [478..498]
            Item [499..575]
              Text(Borrowed("empty link 1 ")) [501..514]
              Link { link_type: Inline, dest_url: Borrowed(""), title: Borrowed(""), id: Borrowed("") } [514..521]
                Text(Borrowed("www")) [515..518]
              List(None) [521..575]
                Item [521..575]
                  Text(Borrowed("empty markdown link ")) [525..545]
                  Code(Borrowed("[]()")) [545..551]
                  Text(Borrowed(" points to note ")) [551..567]
                  Code(Borrowed("().md")) [567..574]
            Item [575..632]
              Text(Borrowed("empty link 2 ")) [577..590]
              Link { link_type: Inline, dest_url: Borrowed("ww"), title: Borrowed(""), id: Borrowed("") } [590..596]
              List(None) [596..632]
                Item [596..609]
                  Code(Borrowed("[](ww)")) [600..608]
                Item [608..632]
                  Text(Borrowed("points to note ")) [612..627]
                  Code(Borrowed("ww")) [627..631]
            Item [632..685]
              Text(Borrowed("empty link 3 ")) [634..647]
              Link { link_type: Inline, dest_url: Borrowed(""), title: Borrowed(""), id: Borrowed("") } [647..651]
              List(None) [651..685]
                Item [651..662]
                  Code(Borrowed("[]()")) [655..661]
                Item [661..685]
                  Text(Borrowed("points to note ")) [665..680]
                  Code(Borrowed("()")) [680..684]
            Item [685..887]
              Text(Borrowed("empty link 4 ")) [687..700]
              Link { link_type: Inline, dest_url: Borrowed("Three%20laws%20of%20motion.md"), title: Borrowed(""), id: Borrowed("") } [700..733]
              List(None) [733..887]
                Item [733..773]
                  Code(Borrowed("[](Three%20laws%20of%20motion.md)")) [737..772]
                Item [772..814]
                  Text(Borrowed("points to note ")) [776..791]
                  Code(Borrowed("Three laws of motion")) [791..813]
                Item [813..887]
                  Text(Borrowed("the first part of markdown link is displayed text and doesn")) [817..876]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [876..877]
                  Text(Borrowed("t matter")) [877..885]
          Heading { level: H6, id: None, classes: [], attrs: [] } [887..942]
            Text(Borrowed("Wiki link: ")) [894..905]
            Code(Borrowed("[[x#]]")) [905..913]
            Text(Borrowed(" | ")) [913..916]
            Code(Borrowed("[[x#^block_identifier]]")) [916..941]
          List(None) [942..3536]
            Item [942..976]
              Text(Borrowed("basic: ")) [944..951]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Three laws of motion"), title: Borrowed(""), id: Borrowed("") } [951..974]
                Text(Borrowed("Three laws of motion")) [953..973]
            Item [976..1043]
              Text(Borrowed("explicit markdown extension in name: ")) [978..1015]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Three laws of motion.md"), title: Borrowed(""), id: Borrowed("") } [1015..1041]
                Text(Borrowed("Three laws of motion.md")) [1017..1040]
            Item [1043..1097]
              Text(Borrowed("with pipe for displayed text: ")) [1045..1075]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Note 2 "), title: Borrowed(""), id: Borrowed("") } [1075..1095]
                Text(Borrowed(" Note two")) [1085..1094]
            Item [1097..1168]
              Text(Borrowed("heading in the same note: ")) [1099..1125]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1125..1142]
                Text(Borrowed("#Level 3 title")) [1127..1141]
              List(None) [1143..1168]
                Item [1143..1168]
                  Code(Borrowed("[[#Level 3 title]]")) [1147..1167]
            Item [1168..1246]
              Text(Borrowed("nested heading in the same note: ")) [1170..1203]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 4 title"), title: Borrowed(""), id: Borrowed("") } [1203..1220]
                Text(Borrowed("#Level 4 title")) [1205..1219]
              List(None) [1221..1246]
                Item [1221..1246]
                  Code(Borrowed("[[#Level 4 title]]")) [1225..1245]
            Item [1246..1331]
              Text(Borrowed("invalid heading in the same note: ")) [1248..1282]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#random"), title: Borrowed(""), id: Borrowed("") } [1282..1292]
                Text(Borrowed("#random")) [1284..1291]
              List(None) [1293..1331]
                Item [1293..1311]
                  Code(Borrowed("[[#random]]")) [1297..1310]
                Item [1310..1331]
                  Text(Borrowed("fallback to note")) [1314..1330]
            Item [1331..1423]
              Text(Borrowed("heading in another note: ")) [1333..1358]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title"), title: Borrowed(""), id: Borrowed("") } [1358..1386]
                Text(Borrowed("Note 2#Some level 2 title")) [1360..1385]
              List(None) [1387..1423]
                Item [1387..1423]
                  Code(Borrowed("[[Note 2#Some level 2 title]]")) [1391..1422]
            Item [1423..1501]
              Text(Borrowed("nested heading in another note: ")) [1425..1457]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1457..1499]
                Text(Borrowed("Note 2#Some level 2 title#Level 3 title")) [1459..1498]
            Item [1501..1618]
              Text(Borrowed("invalid heading in another note: ")) [1503..1536]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#random#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1536..1566]
                Text(Borrowed("Note 2#random#Level 3 title")) [1538..1565]
              List(None) [1567..1618]
                Item [1567..1618]
                  Text(Borrowed("fallback to note if the heading doesn")) [1571..1608]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1608..1609]
                  Text(Borrowed("t exist")) [1609..1616]
            Item [1618..1670]
              Text(Borrowed("heading in another note: ")) [1620..1645]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1645..1668]
                Text(Borrowed("Note 2#Level 3 title")) [1647..1667]
            Item [1670..1711]
              Text(Borrowed("heading in another note: ")) [1672..1697]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#L4"), title: Borrowed(""), id: Borrowed("") } [1697..1709]
                Text(Borrowed("Note 2#L4")) [1699..1708]
            Item [1711..1921]
              Text(Borrowed("nested heading in another note: ")) [1713..1745]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#L4"), title: Borrowed(""), id: Borrowed("") } [1745..1776]
                Text(Borrowed("Note 2#Some level 2 title#L4")) [1747..1775]
              List(None) [1777..1921]
                Item [1777..1850]
                  Text(Borrowed("when there")) [1781..1791]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1791..1792]
                  Text(Borrowed("s multiple levels, the level doesn")) [1792..1826]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1826..1827]
                  Text(Borrowed("t need to be specified")) [1827..1849]
                Item [1849..1921]
                  Text(Borrowed("it will match as long as the ancestor-descendant relationship holds")) [1853..1920]
            Item [1921..1966]
              Text(Borrowed("non-existing note: ")) [1923..1942]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Non-existing note 4"), title: Borrowed(""), id: Borrowed("") } [1942..1964]
                Text(Borrowed("Non-existing note 4")) [1944..1963]
            Item [1966..2011]
              Text(Borrowed("empty link: ")) [1968..1980]
              Text(Borrowed("[")) [1980..1981]
              Text(Borrowed("[")) [1981..1982]
              Text(Borrowed("]")) [1982..1983]
              Text(Borrowed("]")) [1983..1984]
              List(None) [1984..2011]
                Item [1984..2011]
                  Text(Borrowed("points to current note")) [1988..2010]
            Item [2011..2128]
              Text(Borrowed("empty heading:")) [2013..2027]
              List(None) [2027..2128]
                Item [2027..2074]
                  Code(Borrowed("[[#]]")) [2031..2038]
                  Text(Borrowed(": ")) [2038..2040]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#"), title: Borrowed(""), id: Borrowed("") } [2040..2044]
                    Text(Borrowed("#")) [2042..2043]
                  List(None) [2047..2074]
                    Item [2047..2074]
                      Text(Borrowed("points to current note")) [2051..2073]
                Item [2073..2128]
                  Code(Borrowed("[[Note 2##]]")) [2077..2091]
                  Text(Borrowed(":  ")) [2091..2094]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2##"), title: Borrowed(""), id: Borrowed("") } [2094..2105]
                    Text(Borrowed("Note 2##")) [2096..2104]
                  List(None) [2107..2128]
                    Item [2107..2128]
                      Text(Borrowed("points to Note 2")) [2111..2127]
            Item [2128..2416]
              Text(Borrowed("incorrect heading level")) [2130..2153]
              List(None) [2153..2416]
                Item [2153..2212]
                  Code(Borrowed("[[#######Link to figure]]")) [2157..2184]
                  Text(Borrowed(": ")) [2184..2186]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#######Link to figure"), title: Borrowed(""), id: Borrowed("") } [2186..2210]
                    Text(Borrowed("#######Link to figure")) [2188..2209]
                Item [2211..2268]
                  Code(Borrowed("[[######Link to figure]]")) [2215..2241]
                  Text(Borrowed(": ")) [2241..2243]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("######Link to figure"), title: Borrowed(""), id: Borrowed("") } [2243..2266]
                    Text(Borrowed("######Link to figure")) [2245..2265]
                Item [2267..2320]
                  Code(Borrowed("[[####Link to figure]]")) [2271..2295]
                  Text(Borrowed(": ")) [2295..2297]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("####Link to figure"), title: Borrowed(""), id: Borrowed("") } [2297..2318]
                    Text(Borrowed("####Link to figure")) [2299..2317]
                Item [2319..2370]
                  Code(Borrowed("[[###Link to figure]]")) [2323..2346]
                  Text(Borrowed(": ")) [2346..2348]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("###Link to figure"), title: Borrowed(""), id: Borrowed("") } [2348..2368]
                    Text(Borrowed("###Link to figure")) [2350..2367]
                Item [2369..2416]
                  Code(Borrowed("[[#Link to figure]]")) [2373..2394]
                  Text(Borrowed(": ")) [2394..2396]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Link to figure"), title: Borrowed(""), id: Borrowed("") } [2396..2414]
                    Text(Borrowed("#Link to figure")) [2398..2413]
            Item [2416..2536]
              Text(Borrowed("ambiguous pipe and heading: ")) [2419..2447]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("#L2 "), title: Borrowed(""), id: Borrowed("") } [2447..2459]
                Text(Borrowed(" #L4")) [2454..2458]
              List(None) [2461..2536]
                Item [2461..2481]
                  Code(Borrowed("[[#L2 | #L4]]")) [2465..2480]
                Item [2481..2498]
                  Text(Borrowed("points to L2")) [2485..2497]
                Item [2498..2536]
                  Text(Borrowed("things after the pipe is escaped")) [2502..2534]
            Item [2536..2624]
              Text(Borrowed("multiple pipe: ")) [2539..2554]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Note 2 "), title: Borrowed(""), id: Borrowed("") } [2554..2571]
                Text(Borrowed(" 2 | 3")) [2564..2570]
              List(None) [2573..2624]
                Item [2573..2598]
                  Code(Borrowed("[[Note 2 | 2 | 3]]")) [2577..2597]
                Item [2598..2624]
                  Text(Borrowed("this points to Note 2")) [2602..2623]
            Item [2624..3117]
              Text(Borrowed("incorrect nested heading")) [2626..2650]
              List(None) [2651..3117]
                Item [2651..2720]
                  Code(Borrowed("[[###L2#L4]]")) [2655..2669]
                  Text(Borrowed(":  ")) [2669..2672]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("###L2#L4"), title: Borrowed(""), id: Borrowed("") } [2672..2683]
                    Text(Borrowed("###L2#L4")) [2674..2682]
                  List(None) [2685..2720]
                    Item [2685..2720]
                      Text(Borrowed("points to L4 heading correctly")) [2689..2719]
                Item [2719..2795]
                  Code(Borrowed("[[##L2######L4]]")) [2723..2741]
                  Text(Borrowed(": ")) [2741..2743]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2######L4"), title: Borrowed(""), id: Borrowed("") } [2743..2758]
                    Text(Borrowed("##L2######L4")) [2745..2757]
                  List(None) [2760..2795]
                    Item [2760..2795]
                      Text(Borrowed("points to L4 heading correctly")) [2764..2794]
                Item [2794..2868]
                  Code(Borrowed("[[##L2#####L4]]")) [2798..2815]
                  Text(Borrowed(": ")) [2815..2817]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4"), title: Borrowed(""), id: Borrowed("") } [2817..2831]
                    Text(Borrowed("##L2#####L4")) [2819..2830]
                  List(None) [2833..2868]
                    Item [2833..2868]
                      Text(Borrowed("points to L4 heading correctly")) [2837..2867]
                Item [2867..2941]
                  Code(Borrowed("[[##L2#####L4#L3]]")) [2871..2891]
                  Text(Borrowed(": ")) [2891..2893]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4#L3"), title: Borrowed(""), id: Borrowed("") } [2893..2910]
                    Text(Borrowed("##L2#####L4#L3")) [2895..2909]
                  List(None) [2912..2941]
                    Item [2912..2941]
                      Text(Borrowed("fallback to current note")) [2916..2940]
                Item [2940..3022]
                  Code(Borrowed("[[##L2#####L4#L3]]")) [2944..2964]
                  Text(Borrowed(": ")) [2964..2966]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4#Another L3"), title: Borrowed(""), id: Borrowed("") } [2966..2991]
                    Text(Borrowed("##L2#####L4#Another L3")) [2968..2990]
                  List(None) [2993..3022]
                    Item [2993..3022]
                      Text(Borrowed("fallback to current note")) [2997..3021]
                Item [3021..3117]
                  Text(Borrowed("for displayed text, the first hash is removed, the subsequent nesting ones are not affected")) [3025..3116]
            Item [3117..3237]
              Text(Borrowed("↳ it looks like whenever there")) [3119..3151]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3151..3152]
              Text(Borrowed("s multiple hash, it")) [3152..3171]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3171..3172]
              Text(Borrowed("s all stripped. only the ancestor-descendant relationship matter")) [3172..3236]
            Item [3237..3536]
              Text(Borrowed("I don")) [3239..3244]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3244..3245]
              Text(Borrowed("t think there")) [3245..3258]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3258..3259]
              Text(Borrowed("s a different between Wikilink and Markdown link")) [3259..3307]
              List(None) [3307..3536]
                Item [3307..3385]
                  Code(Borrowed("[1](##L2######L4)")) [3311..3330]
                  Text(Borrowed(": ")) [3330..3332]
                  Link { link_type: Inline, dest_url: Borrowed("##L2######L4"), title: Borrowed(""), id: Borrowed("") } [3332..3349]
                    Text(Borrowed("1")) [3333..3334]
                  List(None) [3350..3385]
                    Item [3350..3385]
                      Text(Borrowed("points to L4 heading correctly")) [3354..3384]
                Item [3384..3460]
                  Code(Borrowed("[2](##L2#####L4)")) [3388..3406]
                  Text(Borrowed(": ")) [3406..3408]
                  Link { link_type: Inline, dest_url: Borrowed("##L2#####L4"), title: Borrowed(""), id: Borrowed("") } [3408..3424]
                    Text(Borrowed("2")) [3409..3410]
                  List(None) [3425..3460]
                    Item [3425..3460]
                      Text(Borrowed("points to L4 heading correctly")) [3429..3459]
                Item [3459..3536]
                  Code(Borrowed("[3](##L2#####L4#L3)")) [3463..3484]
                  Text(Borrowed(": ")) [3484..3486]
                  Link { link_type: Inline, dest_url: Borrowed("##L2#####L4#L3"), title: Borrowed(""), id: Borrowed("") } [3486..3505]
                    Text(Borrowed("3")) [3487..3488]
                  List(None) [3506..3536]
                    Item [3506..3536]
                      Text(Borrowed("fallback to current note")) [3510..3534]
          Heading { level: H5, id: None, classes: [], attrs: [] } [3536..3556]
            Text(Borrowed("Link to asset")) [3542..3555]
          List(None) [3556..5876]
            Item [3556..3677]
              Code(Borrowed("[[Figure1.jpg]]")) [3558..3575]
              Text(Borrowed(": ")) [3575..3577]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1.jpg"), title: Borrowed(""), id: Borrowed("") } [3577..3591]
                Text(Borrowed("Figure1.jpg")) [3579..3590]
              List(None) [3592..3677]
                Item [3592..3677]
                  Text(Borrowed("even if there exists a note called ")) [3596..3631]
                  Code(Borrowed("Figure1.jpg")) [3631..3644]
                  Text(Borrowed(", the asset will take precedence")) [3644..3676]
            Item [3677..3737]
              Code(Borrowed("[[Figure1.jpg#2]]")) [3679..3698]
              Text(Borrowed(": ")) [3698..3700]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1.jpg#2"), title: Borrowed(""), id: Borrowed("") } [3700..3716]
                Text(Borrowed("Figure1.jpg#2")) [3702..3715]
              List(None) [3717..3737]
                Item [3717..3737]
                  Text(Borrowed("points to image")) [3721..3736]
            Item [3737..3843]
              Code(Borrowed("[[Figure1.jpg | 2]]")) [3739..3760]
              Text(Borrowed(": ")) [3760..3762]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Figure1.jpg "), title: Borrowed(""), id: Borrowed("") } [3762..3780]
                Text(Borrowed(" 2")) [3777..3779]
              List(None) [3781..3843]
                Item [3781..3801]
                  Text(Borrowed("points to image")) [3785..3800]
                Item [3800..3843]
                  Text(Borrowed("leading and ending spaces are stripped")) [3804..3842]
            Item [3843..3948]
              Code(Borrowed("[[Figure1.jpg.md]]")) [3845..3865]
              Text(Borrowed(": ")) [3865..3867]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1.jpg.md"), title: Borrowed(""), id: Borrowed("") } [3867..3884]
                Text(Borrowed("Figure1.jpg.md")) [3869..3883]
              List(None) [3885..3948]
                Item [3885..3948]
                  Text(Borrowed("with explicit ")) [3889..3903]
                  Code(Borrowed(".md")) [3903..3908]
                  Text(Borrowed(" ending, we seek for note ")) [3908..3934]
                  Code(Borrowed("Figure1.jpg")) [3934..3947]
            Item [3948..3997]
              Code(Borrowed("[[Figure1.jpg.md.md]]")) [3950..3973]
              Text(Borrowed(": ")) [3973..3975]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1.jpg.md.md"), title: Borrowed(""), id: Borrowed("") } [3975..3995]
                Text(Borrowed("Figure1.jpg.md.md")) [3977..3994]
            Item [3997..4136]
              Code(Borrowed("[[Figure1#2.jpg]]")) [3999..4018]
              Text(Borrowed(": ")) [4018..4020]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1#2.jpg"), title: Borrowed(""), id: Borrowed("") } [4020..4036]
                Text(Borrowed("Figure1#2.jpg")) [4022..4035]
              List(None) [4037..4136]
                Item [4037..4136]
                  Text(Borrowed("understood as note and points to note Figure1 (fallback to note after failing finding heading)")) [4041..4135]
            Item [4136..4275]
              Code(Borrowed("[[Figure1|2.jpg]]")) [4138..4157]
              Text(Borrowed(": ")) [4157..4159]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Figure1"), title: Borrowed(""), id: Borrowed("") } [4159..4175]
                Text(Borrowed("2.jpg")) [4169..4174]
              List(None) [4176..4275]
                Item [4176..4275]
                  Text(Borrowed("understood as note and points to note Figure1 (fallback to note after failing finding heading)")) [4180..4274]
            Item [4275..4335]
              Code(Borrowed("[[Figure1^2.jpg]]")) [4277..4296]
              Text(Borrowed(": ")) [4296..4298]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1^2.jpg"), title: Borrowed(""), id: Borrowed("") } [4298..4314]
                Text(Borrowed("Figure1^2.jpg")) [4300..4313]
              List(None) [4315..4335]
                Item [4315..4335]
                  Text(Borrowed("points to image")) [4319..4334]
            Item [4335..4410]
              Text(Borrowed("↳ when there")) [4337..4351]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [4351..4352]
              Text(Borrowed("s ")) [4352..4354]
              Code(Borrowed(".md")) [4354..4359]
              Text(Borrowed(", it")) [4359..4363]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [4363..4364]
              Text(Borrowed("s removed and limit to the searching of notes")) [4364..4409]
            Item [4410..4749]
              Code(Borrowed("[[dir/]]")) [4412..4422]
              Text(Borrowed(": ")) [4422..4424]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("dir/"), title: Borrowed(""), id: Borrowed("") } [4424..4431]
                Text(Borrowed("dir/")) [4426..4430]
              List(None) [4432..4749]
                Item [4432..4440]
                  Text(Borrowed("BUG")) [4436..4439]
                Item [4439..4501]
                  Text(Borrowed("when clicking it, it will create ")) [4443..4476]
                  Code(Borrowed("dir")) [4476..4481]
                  Text(Borrowed(" note if not exists")) [4481..4500]
                Item [4500..4538]
                  Text(Borrowed("create ")) [4504..4511]
                  Code(Borrowed("dir 1.md")) [4511..4521]
                  Text(Borrowed(" if ")) [4521..4525]
                  Code(Borrowed("dir")) [4525..4530]
                  Text(Borrowed(" exists")) [4530..4537]
                Item [4537..4586]
                  Text(Borrowed("create ")) [4541..4548]
                  Code(Borrowed("dir {n+1}.md")) [4548..4562]
                  Text(Borrowed(" if ")) [4562..4566]
                  Code(Borrowed("dir {n}.md")) [4566..4578]
                  Text(Borrowed(" exists")) [4578..4585]
                Item [4585..4749]
                  Text(Borrowed("I guess the logic is:")) [4589..4610]
                  List(None) [4611..4749]
                    Item [4611..4675]
                      Text(Borrowed("there")) [4615..4620]
                      Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [4620..4621]
                      Text(Borrowed("s no file named ")) [4621..4637]
                      Code(Borrowed("dir/")) [4637..4643]
                      Text(Borrowed(", Obsidian try to create a note")) [4643..4674]
                    Item [4675..4702]
                      Text(Borrowed("it removes ")) [4679..4690]
                      Code(Borrowed("/")) [4690..4693]
                      Text(Borrowed(" and ")) [4693..4698]
                      Code(Borrowed("\\")) [4698..4701]
                    Item [4702..4749]
                      Text(Borrowed("if there exists one, it add integer suffix")) [4706..4748]
            Item [4749..5280]
              Text(Borrowed("matching of nested dirs only match ancestor-descendant relationship")) [4751..4818]
              List(None) [4818..5280]
                Item [4818..4897]
                  Code(Borrowed("[[dir/inner_dir/note_in_inner_dir]]")) [4822..4859]
                  Text(Borrowed(": ")) [4859..4861]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("dir/inner_dir/note_in_inner_dir"), title: Borrowed(""), id: Borrowed("") } [4861..4895]
                    Text(Borrowed("dir/inner_dir/note_in_inner_dir")) [4863..4894]
                Item [4896..4967]
                  Code(Borrowed("[[inner_dir/note_in_inner_dir]]")) [4900..4933]
                  Text(Borrowed(": ")) [4933..4935]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("inner_dir/note_in_inner_dir"), title: Borrowed(""), id: Borrowed("") } [4935..4965]
                    Text(Borrowed("inner_dir/note_in_inner_dir")) [4937..4964]
                Item [4966..5025]
                  Code(Borrowed("[[dir/note_in_inner_dir]]")) [4970..4997]
                  Text(Borrowed(": ")) [4997..4999]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("dir/note_in_inner_dir"), title: Borrowed(""), id: Borrowed("") } [4999..5023]
                    Text(Borrowed("dir/note_in_inner_dir")) [5001..5022]
                Item [5024..5060]
                  Text(Borrowed("↳ all points to the same note")) [5028..5059]
                Item [5059..5260]
                  Code(Borrowed("[[random/note_in_inner_dir]]")) [5063..5093]
                  Text(Borrowed(": ")) [5093..5095]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("random/note_in_inner_dir"), title: Borrowed(""), id: Borrowed("") } [5095..5122]
                    Text(Borrowed("random/note_in_inner_dir")) [5097..5121]
                  List(None) [5124..5260]
                    Item [5124..5146]
                      Text(Borrowed("this has no match")) [5128..5145]
                    Item [5146..5199]
                      Text(Borrowed("it will try to understand the file name and path")) [5150..5198]
                    Item [5199..5260]
                      Text(Borrowed("mkdir and touch file (in contrast to the case of ")) [5203..5252]
                      Code(Borrowed("dir/")) [5252..5258]
                      Text(Borrowed(")")) [5258..5259]
                Item [5259..5280]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("inner_dir/hi"), title: Borrowed(""), id: Borrowed("") } [5263..5278]
                    Text(Borrowed("inner_dir/hi")) [5265..5277]
            Item [5280..5333]
              Code(Borrowed("[[dir/indir_same_name]]")) [5282..5307]
              Text(Borrowed(": ")) [5307..5309]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("dir/indir_same_name"), title: Borrowed(""), id: Borrowed("") } [5309..5331]
                Text(Borrowed("dir/indir_same_name")) [5311..5330]
            Item [5333..5429]
              Code(Borrowed("[[indir_same_name]]")) [5335..5356]
              Text(Borrowed(": ")) [5356..5358]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("indir_same_name"), title: Borrowed(""), id: Borrowed("") } [5358..5376]
                Text(Borrowed("indir_same_name")) [5360..5375]
              List(None) [5377..5429]
                Item [5377..5429]
                  Text(Borrowed("points to ")) [5381..5391]
                  Code(Borrowed("indir_same_name")) [5391..5408]
                  Text(Borrowed(", not the in dir one")) [5408..5428]
            Item [5429..5483]
              Code(Borrowed("[[indir2]]")) [5432..5444]
              Text(Borrowed(": ")) [5444..5446]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("indir2"), title: Borrowed(""), id: Borrowed("") } [5446..5455]
                Text(Borrowed("indir2")) [5448..5454]
              List(None) [5457..5483]
                Item [5457..5483]
                  Text(Borrowed("points to ")) [5460..5470]
                  Code(Borrowed("dir/indir2")) [5470..5482]
            Item [5483..5596]
              Code(Borrowed("[[Something]]")) [5485..5500]
              Text(Borrowed(": ")) [5500..5502]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Something"), title: Borrowed(""), id: Borrowed("") } [5502..5514]
                Text(Borrowed("Something")) [5504..5513]
              List(None) [5515..5596]
                Item [5515..5596]
                  Text(Borrowed("there exists a ")) [5519..5534]
                  Code(Borrowed("Something")) [5534..5545]
                  Text(Borrowed(" file, but this will points to note ")) [5545..5581]
                  Code(Borrowed("Something.md")) [5581..5595]
            Item [5596..5716]
              Code(Borrowed("[[unsupported_text_file.txt]]")) [5598..5629]
              Text(Borrowed(": ")) [5629..5631]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("unsupported_text_file.txt"), title: Borrowed(""), id: Borrowed("") } [5631..5659]
                Text(Borrowed("unsupported_text_file.txt")) [5633..5658]
              List(None) [5660..5716]
                Item [5660..5716]
                  Text(Borrowed("points to text file, which is of unsupported format")) [5664..5715]
            Item [5716..5777]
              Code(Borrowed("[[a.joiwduvqneoi]]")) [5718..5738]
              Text(Borrowed(": ")) [5738..5740]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("a.joiwduvqneoi"), title: Borrowed(""), id: Borrowed("") } [5740..5757]
                Text(Borrowed("a.joiwduvqneoi")) [5742..5756]
              List(None) [5758..5777]
                Item [5758..5777]
                  Text(Borrowed("points to file")) [5762..5776]
            Item [5777..5876]
              Code(Borrowed("[[Note 1]]")) [5779..5791]
              Text(Borrowed(": ")) [5791..5793]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 1"), title: Borrowed(""), id: Borrowed("") } [5793..5802]
                Text(Borrowed("Note 1")) [5795..5801]
              List(None) [5803..5876]
                Item [5803..5876]
                  Text(Borrowed("even if there exists a file named ")) [5807..5841]
                  Code(Borrowed("Note 1")) [5841..5849]
                  Text(Borrowed(", this points to the note")) [5849..5874]
          Paragraph [5876..5956]
            Code(Borrowed("![[Figure1.jpg]]")) [5876..5894]
            Text(Borrowed(": ")) [5894..5896]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1.jpg"), title: Borrowed(""), id: Borrowed("") } [5896..5911]
              Text(Borrowed("Figure1.jpg")) [5899..5910]
            SoftBreak [5912..5913]
            Code(Borrowed("[[empty_video.mp4]]")) [5913..5934]
            Text(Borrowed(": ")) [5934..5936]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("empty_video.mp4"), title: Borrowed(""), id: Borrowed("") } [5936..5954]
              Text(Borrowed("empty_video.mp4")) [5938..5953]
          Heading { level: H2, id: None, classes: [], attrs: [] } [5957..5963]
            Text(Borrowed("L2")) [5960..5962]
          Heading { level: H3, id: None, classes: [], attrs: [] } [5964..5971]
            Text(Borrowed("L3")) [5968..5970]
          Heading { level: H4, id: None, classes: [], attrs: [] } [5971..5979]
            Text(Borrowed("L4")) [5976..5978]
          Heading { level: H3, id: None, classes: [], attrs: [] } [5979..5994]
            Text(Borrowed("Another L3")) [5983..5993]
          Rule [5995..5999]
          Heading { level: H2, id: None, classes: [], attrs: [] } [5999..6003]
        "########);
    }
}
