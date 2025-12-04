use super::types::{Link, Reference, Referenceable};
use super::utils::is_block_identifier;

use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

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
        if is_block_identifier(maybe_identifier) {
            return (file_name, None, Some(maybe_identifier));
        }
    }

    (file_name, Some(parse_nested_heading(after_fst_hash)), None)
}

/// Parse a string into a vector of nested headings.
/// ```
/// use markdown_tools::link::resolve::parse_nested_heading;
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
/// use markdown_tools::link::resolve::match_subsequence;
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
#[allow(dead_code)]
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
///
/// TODO: if there's a dir that has the same name as the note,
///       we should prioritize referenceables inside it
pub fn build_links(
    references: &[Reference],
    referenceable: &[Referenceable],
) -> (Vec<Link>, Vec<Reference>) {
    let mut links = Vec::<Link>::new();
    let mut unresolved = Vec::<Reference>::new();

    // Build path-referenceable map
    let mut path_referenceable_map = HashMap::<PathBuf, &Referenceable>::new();
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
        let file_name = if file_name.is_empty() {
            // Fallback to current note if the file name is empty
            reference
                .path
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_string()
        } else {
            file_name.to_string()
        };
        let file_name = file_name.as_str();
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
                        let matched_blocks = children
                            .iter()
                            .filter_map(|r| match r {
                                Referenceable::Block { identifier, .. } => {
                                    if identifier == block_identifier {
                                        Some(r)
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            })
                            .collect::<Vec<_>>();
                        if let [matched_block, ..] = matched_blocks[..] {
                            Some(matched_block.clone())
                        } else {
                            None
                        }
                    }
                };

                if let Some(in_note) = matched_in_note_child {
                    let link = Link {
                        from: reference.clone(),
                        to: in_note.clone(),
                    };
                    links.push(link);
                    continue;
                }
            }

            let link = Link {
                from: reference.clone(),
                to: (*file_referenceable).clone(),
            };
            links.push(link);
        } else {
            unresolved.push(reference.clone());
        }
    }

    (links, unresolved)
}

#[cfg(test)]
mod tests {
    use std::cmp::min;

    use super::*;
    use crate::link::extract::{scan_note, scan_vault};
    use insta::assert_snapshot;
    use pulldown_cmark::HeadingLevel;

    fn print_table(titles: &[&str], columns: &[Vec<String>]) -> String {
        let mut output = String::new();

        // Calculate column widths (including index column and titles)
        let index_width = columns[0].len().to_string().len().max(3); // "Idx" width

        let widths: Vec<usize> = titles
            .iter()
            .zip(columns.iter())
            .map(|(title, col)| {
                title.len().max(
                    col.iter()
                        .map(|s| s.replace('\n', "\\n").len())
                        .max()
                        .unwrap_or(0),
                )
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
                let cell = columns[j][i].replace('\n', "\\n");
                output.push_str(&format!(" {:<width$} |", cell));
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
            &vec![inputs.iter().map(|s| s.to_string()).collect(), outputs],
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
        let (_, references, _): (_, Vec<Reference>, Vec<Referenceable>) =
            scan_note(&path);
        let dest_strings: Vec<String> =
            references.iter().map(|r| r.dest.clone()).collect();
        let (note_names, nested_headings, _block_identifiers): (
            Vec<String>,
            Vec<Option<Vec<&str>>>,
            Vec<Option<&str>>,
        ) = dest_strings
            .iter()
            .map(|s| {
                let (n, h, b) = split_dest_string(s);
                (n.to_string(), h, b)
            })
            .collect();
        let nested_headings_str: Vec<String> = nested_headings
            .iter()
            .map(|v| match v {
                Some(v) => v.join("#"),
                None => "-".to_string(),
            })
            .collect();
        let table = print_table(
            &vec!["Destination string", "Note", "Headings"],
            &vec![dest_strings, note_names, nested_headings_str],
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
    fn test_build_links_node_1() {
        let dir = PathBuf::from("tests/data/vaults/tt");
        let root_dir = PathBuf::from("tests/data/vaults/tt");
        let (_, referenceables, references) =
            scan_vault(&dir, &root_dir, false);
        let note_1_references = references
            .into_iter()
            .filter(|r| r.path == PathBuf::from("Note 1.md"))
            .collect::<Vec<_>>();
        let (links_built_from_note_1, unresolved_references_in_note_1) =
            build_links(&note_1_references, &referenceables);
        fn fmt_link(link: &Link) -> String {
            let mut s = String::new();
            s.push_str(link.from.dest.as_str());
            s.push_str(" -> ");
            s.push_str(format!("{}", link.to).as_str());
            s
        }
        let links = links_built_from_note_1
            .into_iter()
            .map(|l| fmt_link(&l))
            .collect::<Vec<_>>()
            .join("\n");
        assert_snapshot!(links, @r"
        Three laws of motion -> Note: Three laws of motion.md
        #Level 3 title -> Heading: Note 1.md level: h3, text: Level 3 title
        Note 2#Some level 2 title -> Heading: Note 2.md level: h2, text: Some level 2 title
        () -> Note: ().md
        ww -> Note: ww.md
        () -> Note: ().md
        Three laws of motion -> Note: Three laws of motion.md
        Three laws of motion -> Note: Three laws of motion.md
        Three laws of motion.md -> Note: Three laws of motion.md
        Note 2 -> Note: Note 2.md
        #Level 3 title -> Heading: Note 1.md level: h3, text: Level 3 title
        #Level 4 title -> Heading: Note 1.md level: h4, text: Level 4 title
        #random -> Note: Note 1.md
        Note 2#Some level 2 title -> Heading: Note 2.md level: h2, text: Some level 2 title
        Note 2#Some level 2 title#Level 3 title -> Heading: Note 2.md level: h3, text: Level 3 title
        Note 2#random#Level 3 title -> Note: Note 2.md
        Note 2#Level 3 title -> Heading: Note 2.md level: h3, text: Level 3 title
        Note 2#L4 -> Heading: Note 2.md level: h4, text: L4
        Note 2#Some level 2 title#L4 -> Heading: Note 2.md level: h4, text: L4
        # -> Note: Note 1.md
        Note 2## -> Note: Note 2.md
        #######Link to figure -> Note: Note 1.md
        ######Link to figure -> Note: Note 1.md
        ####Link to figure -> Note: Note 1.md
        ###Link to figure -> Note: Note 1.md
        #Link to figure -> Note: Note 1.md
        #L2 -> Heading: Note 1.md level: h2, text: L2
        Note 2 -> Note: Note 2.md
        ###L2#L4 -> Heading: Note 1.md level: h4, text: L4
        ##L2######L4 -> Heading: Note 1.md level: h4, text: L4
        ##L2#####L4 -> Heading: Note 1.md level: h4, text: L4
        ##L2#####L4#L3 -> Note: Note 1.md
        ##L2#####L4#Another L3 -> Note: Note 1.md
        ##L2######L4 -> Heading: Note 1.md level: h4, text: L4
        ##L2#####L4 -> Heading: Note 1.md level: h4, text: L4
        ##L2#####L4#L3 -> Note: Note 1.md
        Figure1.jpg -> Asset: Figure1.jpg
        Figure1.jpg#2 -> Asset: Figure1.jpg
        Figure1.jpg -> Asset: Figure1.jpg
        Figure1.jpg.md -> Note: Figure1.jpg.md
        Figure1.jpg.md.md -> Note: Figure1.jpg.md.md
        Figure1#2.jpg -> Note: Figure1.md
        Figure1 -> Note: Figure1.md
        Figure1^2.jpg -> Asset: Figure1^2.jpg
        dir/inner_dir/note_in_inner_dir -> Note: dir/inner_dir/note_in_inner_dir.md
        inner_dir/note_in_inner_dir -> Note: dir/inner_dir/note_in_inner_dir.md
        dir/note_in_inner_dir -> Note: dir/inner_dir/note_in_inner_dir.md
        dir/indir_same_name -> Note: dir/indir_same_name.md
        indir_same_name -> Note: indir_same_name.md
        indir2 -> Note: dir/indir2.md
        unsupported_text_file.txt -> Asset: unsupported_text_file.txt
        a.joiwduvqneoi -> Asset: a.joiwduvqneoi
        Note 1 -> Note: Note 1.md
        Figure1.jpg -> Asset: Figure1.jpg
        empty_video.mp4 -> Asset: empty_video.mp4
        ");

        let unresolved_str = unresolved_references_in_note_1
            .into_iter()
            .map(|r| {
                let mut s = String::new();
                s.push_str(&r.dest);
                s
            })
            .collect::<Vec<_>>()
            .join("\n");
        assert_snapshot!(unresolved_str, @r"
        Non-existing note 4
        dir/
        random/note_in_inner_dir
        inner_dir/hi
        Something
        ");
    }

    #[test]
    fn test_build_links_block_ref() {
        let dir = PathBuf::from("tests/data/vaults/tt");
        let root_dir = PathBuf::from("tests/data/vaults/tt");
        let path = PathBuf::from("block.md");
        let block_md = std::fs::read_to_string(root_dir.join(&path)).unwrap();

        let (_, referenceables, references) =
            scan_vault(&dir, &root_dir, false);

        let block_md_references = references
            .into_iter()
            .filter(|r| r.path == path)
            .collect::<Vec<_>>();

        let _block_md_referenceables = &referenceables
            .iter()
            .filter_map(|r| match r {
                Referenceable::Note { path: p, .. } => {
                    if p == &path {
                        Some(r)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        // assert_debug_snapshot!(block_md_referenceables, @r#""#);

        let (links_built_from_block_md, unresolved_references_in_block_md) =
            build_links(&block_md_references, &referenceables);

        let ref_contents = links_built_from_block_md
            .iter()
            .map(|l| {
                let refe = &l.from;
                block_md[refe.range.clone()].to_string()
            })
            .collect::<Vec<_>>();

        let refable_contents = links_built_from_block_md
            .iter()
            .map(|l| {
                let refable = &l.to;
                let range = match refable {
                    Referenceable::Block { range, .. } => range,
                    Referenceable::Note { .. } => return "Note".to_string(),
                    _ => panic!("Unexpected referenceable"),
                };
                block_md[(range.start)
                    ..(range.start + min(range.end - range.start, 10))]
                    .to_string()
            })
            .collect::<Vec<_>>();

        let identifiers = links_built_from_block_md
            .iter()
            .map(|l| {
                let refable = &l.to;
                match refable {
                    Referenceable::Block { identifier, .. } => {
                        identifier.to_string()
                    }
                    Referenceable::Note { .. } => return "-".to_string(),
                    _ => panic!("Expected block referenceable"),
                }
            })
            .collect::<Vec<_>>();

        let kinds = links_built_from_block_md
            .iter()
            .map(|l| {
                let refable = &l.to;
                match refable {
                    Referenceable::Block { kind, .. } => format!("{:?}", kind),
                    Referenceable::Note { .. } => return "-".to_string(),
                    _ => panic!("Expected block referenceable"),
                }
            })
            .collect::<Vec<_>>();

        let table = print_table(
            &vec!["Reference", "Referenceable content", "Identifier", "Kind"],
            &vec![ref_contents, refable_contents, identifiers, kinds],
        );

        assert_snapshot!(table, @r"
        +-----+------------------+-----------------------+-------------+-----------------+
        | Idx |    Reference     | Referenceable content | Identifier  |      Kind       |
        +-----+------------------+-----------------------+-------------+-----------------+
        |   0 | [[#^quotation]   | > quotatio            | quotation   | BlockQuote      |
        |   1 | [[#^callout]     | > [!info]             | callout     | BlockQuote      |
        |   2 | [[#^paragraph]   | paragraph             | paragraph   | InlineParagraph |
        |   3 | [[#^p-with-code] | paragraph             | p-with-code | InlineParagraph |
        |   4 | [[#^paragraph2]  | paragraph             | paragraph2  | Paragraph       |
        |   5 | [[#^table]       | | Col 1  |            | table       | Table           |
        |   6 | [[#^firstline]   | - a nested            | firstline   | InlineListItem  |
        |   7 | [[#^inneritem]   | \n	-  item\n          | inneritem   | InlineListItem  |
        |   8 | [[#^tableref]    | | Col 1  |            | tableref    | Table           |
        |   9 | [[#^tableref3]   | ^tableref2            | tableref3   | Paragraph       |
        |  10 | [[#^tableref2]   | | Col 1  |            | tableref2   | Table           |
        |  11 | [[#^works]       | this\n^work           | works       | InlineParagraph |
        |  12 | [[#^firstline]   | - a nested            | firstline   | InlineListItem  |
        |  13 | [[#^inneritem]   | \n	-  item\n          | inneritem   | InlineListItem  |
        |  14 | [[#^firstline1]  | a nested l            | firstline1  | InlineParagraph |
        |  15 | [[#^inneritem1]  | Note                  | -           | -               |
        |  16 | [[#^fulllst1]    | Note                  | -           | -               |
        +-----+------------------+-----------------------+-------------+-----------------+
        ");

        let unresolved_str = unresolved_references_in_block_md
            .into_iter()
            .map(|r| {
                let mut s = String::new();
                s.push_str(&r.dest);
                s
            })
            .collect::<Vec<_>>()
            .join("\n");
        assert_snapshot!(unresolved_str, @"");
    }
}
