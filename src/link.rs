#![allow(warnings)] // reason: WIP
/// Extracts references and referenceables from a Markdown AST.
/// Referenceable can be
///     - items in a note: headings, block
///     - notes: markdown files
///     - assets other than notes: images, videos, audios, PDFs, etc.
use crate::ast::{Node, NodeKind, Tree};
use pulldown_cmark::{HeadingLevel, LinkType};
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
    },
    Heading {
        note_path: PathBuf,
        level: HeadingLevel,
        range: Range<usize>,
    },
    Block {
        note_path: PathBuf,
    },
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
                            panic!("Never: Wikilink should have text");
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
                                    panic!("Markdown link should have text");
                                }
                            }
                            [fst, snd, ..] => {
                                panic!(
                                    "Markdown link should have at most one child, got \n first: {:?}\n second: {:?}",
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
            let referenceable = Referenceable::Heading {
                note_path: path.clone(),
                level: level.clone(),
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
                    Some("md") => Referenceable::Note { path },
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

fn scan_vault(dir: &Path) -> (Vec<Referenceable>, Vec<Reference>) {
    let mut file_referenceables = scan_dir_for_assets_and_notes(dir);

    let mut in_note_referenceables = Vec::<Referenceable>::new();
    let mut references = Vec::<Reference>::new();

    for note in file_referenceables.iter() {
        if let Referenceable::Note { path } = note {
            let (note_references, note_referenceables) = scan_note(path);
            references.extend(note_references);
            in_note_referenceables.extend(note_referenceables);
        }
    }

    // Merge referenceables
    file_referenceables.extend(in_note_referenceables);
    (file_referenceables, references)
}

/// Splits a destination string into three parts: the note name and nested headings
fn split_dest_string(s: &str) -> (&str, Vec<&str>) {
    let hash_pos = s.find('#');

    if hash_pos.is_none() {
        return (s, Vec::new());
    }

    let hash_pos = hash_pos.unwrap();
    let note_name = &s[..hash_pos];
    let hs = &s[hash_pos + 1..];

    (note_name, parse_nested_heading(hs))
}

/// Parse a string into a vector of nested headings.
fn parse_nested_heading(s: &str) -> Vec<&str> {
    if s.chars().all(|c| c == '#') && !s.is_empty() {
        // All hashes case
        return vec![""];
    }

    s.split('#').filter(|part| !part.is_empty()).collect()
}

fn parse_destination(dest: &str) -> Referenceable {
    //! If the destination string ends with `.md`, it is targeting a note.
    // TODO:
    // if there's # or |, it's forced to be a note
    // find first # and first |, split into three parts
    // the first part is note name
    //     if it's empty, it's current note
    // the second part is nested heading, only taking ancestor-descendant relationship into account
    if dest.ends_with(".md") {
        Referenceable::Note {
            path: PathBuf::from(dest),
        }
    } else {
        Referenceable::Asset {
            path: PathBuf::from(dest),
        }
    }
}

/// Builds links from references and referenceables.
/// Return a tuple of matched links and unresolved references.
fn build_links(
    references: Vec<Reference>,
    referenceable: Vec<Referenceable>,
) -> (Vec<Link>, Vec<Referenceable>) {
    let mut links = Vec::<Link>::new();
    for reference in references {
        for referenceable in referenceable.iter() {}
    }
    todo!()
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
        let (note_names, nested_headings): (Vec<&str>, Vec<Vec<&str>>) =
            dest_strings.iter().map(|s| split_dest_string(s)).collect();
        let nested_headings_str: Vec<String> =
            nested_headings.iter().map(|v| v.join("#")).collect();
        let table = print_table(
            &vec!["Destination string", "Note", "Headings"],
            &vec![
                dest_strings,
                note_names,
                nested_headings_str.iter().map(|s| s.as_str()).collect(),
            ],
        );
        assert_snapshot!(table, @r"
        +-----+-----------------------------------------+-------------------------+----------------------------------+
        | Idx |           Destination string            |          Note           |             Headings             |
        +-----+-----------------------------------------+-------------------------+----------------------------------+
        |   0 | Three laws of motion                    | Three laws of motion    |                                  |
        |   1 | #Level 3 title                          |                         | Level 3 title                    |
        |   2 | Note 2#Some level 2 title               | Note 2                  | Some level 2 title               |
        |   3 | ()                                      | ()                      |                                  |
        |   4 | ww                                      | ww                      |                                  |
        |   5 | ()                                      | ()                      |                                  |
        |   6 | Three laws of motion                    | Three laws of motion    |                                  |
        |   7 | Three laws of motion                    | Three laws of motion    |                                  |
        |   8 | Three laws of motion.md                 | Three laws of motion.md |                                  |
        |   9 | Note 2                                  | Note 2                  |                                  |
        |  10 | #Level 3 title                          |                         | Level 3 title                    |
        |  11 | #Level 4 title                          |                         | Level 4 title                    |
        |  12 | #random                                 |                         | random                           |
        |  13 | Note 2#Some level 2 title               | Note 2                  | Some level 2 title               |
        |  14 | Note 2#Some level 2 title#Level 3 title | Note 2                  | Some level 2 title#Level 3 title |
        |  15 | Note 2#random#Level 3 title             | Note 2                  | random#Level 3 title             |
        |  16 | Note 2#Level 3 title                    | Note 2                  | Level 3 title                    |
        |  17 | Note 2#L4                               | Note 2                  | L4                               |
        |  18 | Note 2#Some level 2 title#L4            | Note 2                  | Some level 2 title#L4            |
        |  19 | Non-existing note 4                     | Non-existing note 4     |                                  |
        |  20 | #                                       |                         |                                  |
        |  21 | Note 2##                                | Note 2                  |                                  |
        |  22 | #######Link to figure                   |                         | Link to figure                   |
        |  23 | ######Link to figure                    |                         | Link to figure                   |
        |  24 | ####Link to figure                      |                         | Link to figure                   |
        |  25 | ###Link to figure                       |                         | Link to figure                   |
        |  26 | #Link to figure                         |                         | Link to figure                   |
        |  27 | #L2                                     |                         | L2                               |
        |  28 | Note 2                                  | Note 2                  |                                  |
        |  29 | ###L2#L4                                |                         | L2#L4                            |
        |  30 | ##L2######L4                            |                         | L2#L4                            |
        |  31 | ##L2#####L4                             |                         | L2#L4                            |
        |  32 | ##L2#####L4#L3                          |                         | L2#L4#L3                         |
        |  33 | ##L2#####L4#Another L3                  |                         | L2#L4#Another L3                 |
        |  34 | ##L2######L4                            |                         | L2#L4                            |
        |  35 | ##L2#####L4                             |                         | L2#L4                            |
        |  36 | ##L2#####L4#L3                          |                         | L2#L4#L3                         |
        |  37 | Figure 1.jpg                            | Figure 1.jpg            |                                  |
        |  38 | Figure 1.jpg.md                         | Figure 1.jpg.md         |                                  |
        |  39 | Figure 1.jpg.md.md                      | Figure 1.jpg.md.md      |                                  |
        |  40 | Figure1#2.jpg                           | Figure1                 | 2.jpg                            |
        |  41 | Figure1                                 | Figure1                 |                                  |
        |  42 | Figure1^2.jpg                           | Figure1^2.jpg           |                                  |
        |  43 | Figure 1.jpg                            | Figure 1.jpg            |                                  |
        |  44 | empty_video.mp4                         | empty_video.mp4         |                                  |
        +-----+-----------------------------------------+-------------------------+----------------------------------+
        ");
    }

    #[test]
    fn test_scan_vault() {
        let dir = PathBuf::from("tests/data/vaults/tt");
        let (referenceables, references) = scan_vault(&dir);
        assert_debug_snapshot!(references, @r########"
        [
            Reference {
                range: 169..225,
                dest: "Three laws of motion",
                kind: MarkdownLink,
                display_text: "Three laws of motion 11",
            },
            Reference {
                range: 256..291,
                dest: "#Level 3 title",
                kind: MarkdownLink,
                display_text: "Level 3 title",
            },
            Reference {
                range: 358..397,
                dest: "Note 2#Some level 2 title",
                kind: MarkdownLink,
                display_text: "22",
            },
            Reference {
                range: 516..523,
                dest: "()",
                kind: MarkdownLink,
                display_text: "www",
            },
            Reference {
                range: 592..598,
                dest: "ww",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 649..653,
                dest: "()",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 702..735,
                dest: "Three laws of motion",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 953..976,
                dest: "Three laws of motion",
                kind: WikiLink,
                display_text: "Three laws of motion",
            },
            Reference {
                range: 1017..1043,
                dest: "Three laws of motion.md",
                kind: WikiLink,
                display_text: "Three laws of motion.md",
            },
            Reference {
                range: 1077..1097,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " Note two",
            },
            Reference {
                range: 1127..1144,
                dest: "#Level 3 title",
                kind: WikiLink,
                display_text: "#Level 3 title",
            },
            Reference {
                range: 1205..1222,
                dest: "#Level 4 title",
                kind: WikiLink,
                display_text: "#Level 4 title",
            },
            Reference {
                range: 1284..1294,
                dest: "#random",
                kind: WikiLink,
                display_text: "#random",
            },
            Reference {
                range: 1360..1388,
                dest: "Note 2#Some level 2 title",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title",
            },
            Reference {
                range: 1459..1501,
                dest: "Note 2#Some level 2 title#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title#Level 3 title",
            },
            Reference {
                range: 1538..1568,
                dest: "Note 2#random#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#random#Level 3 title",
            },
            Reference {
                range: 1647..1670,
                dest: "Note 2#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#Level 3 title",
            },
            Reference {
                range: 1699..1711,
                dest: "Note 2#L4",
                kind: WikiLink,
                display_text: "Note 2#L4",
            },
            Reference {
                range: 1747..1778,
                dest: "Note 2#Some level 2 title#L4",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title#L4",
            },
            Reference {
                range: 1944..1966,
                dest: "Non-existing note 4",
                kind: WikiLink,
                display_text: "Non-existing note 4",
            },
            Reference {
                range: 2042..2046,
                dest: "#",
                kind: WikiLink,
                display_text: "#",
            },
            Reference {
                range: 2096..2107,
                dest: "Note 2##",
                kind: WikiLink,
                display_text: "Note 2##",
            },
            Reference {
                range: 2188..2212,
                dest: "#######Link to figure",
                kind: WikiLink,
                display_text: "#######Link to figure",
            },
            Reference {
                range: 2245..2268,
                dest: "######Link to figure",
                kind: WikiLink,
                display_text: "######Link to figure",
            },
            Reference {
                range: 2299..2320,
                dest: "####Link to figure",
                kind: WikiLink,
                display_text: "####Link to figure",
            },
            Reference {
                range: 2350..2370,
                dest: "###Link to figure",
                kind: WikiLink,
                display_text: "###Link to figure",
            },
            Reference {
                range: 2398..2416,
                dest: "#Link to figure",
                kind: WikiLink,
                display_text: "#Link to figure",
            },
            Reference {
                range: 2449..2461,
                dest: "#L2 ",
                kind: WikiLink,
                display_text: " #L4",
            },
            Reference {
                range: 2556..2573,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " 2 | 3",
            },
            Reference {
                range: 2674..2685,
                dest: "###L2#L4",
                kind: WikiLink,
                display_text: "###L2#L4",
            },
            Reference {
                range: 2745..2760,
                dest: "##L2######L4",
                kind: WikiLink,
                display_text: "##L2######L4",
            },
            Reference {
                range: 2819..2833,
                dest: "##L2#####L4",
                kind: WikiLink,
                display_text: "##L2#####L4",
            },
            Reference {
                range: 2895..2912,
                dest: "##L2#####L4#L3",
                kind: WikiLink,
                display_text: "##L2#####L4#L3",
            },
            Reference {
                range: 2968..2993,
                dest: "##L2#####L4#Another L3",
                kind: WikiLink,
                display_text: "##L2#####L4#Another L3",
            },
            Reference {
                range: 3334..3351,
                dest: "##L2######L4",
                kind: MarkdownLink,
                display_text: "1",
            },
            Reference {
                range: 3410..3426,
                dest: "##L2#####L4",
                kind: MarkdownLink,
                display_text: "2",
            },
            Reference {
                range: 3488..3507,
                dest: "##L2#####L4#L3",
                kind: MarkdownLink,
                display_text: "3",
            },
            Reference {
                range: 3580..3595,
                dest: "Figure 1.jpg",
                kind: WikiLink,
                display_text: "Figure 1.jpg",
            },
            Reference {
                range: 3707..3725,
                dest: "Figure 1.jpg.md",
                kind: WikiLink,
                display_text: "Figure 1.jpg.md",
            },
            Reference {
                range: 3818..3839,
                dest: "Figure 1.jpg.md.md",
                kind: WikiLink,
                display_text: "Figure 1.jpg.md.md",
            },
            Reference {
                range: 3864..3880,
                dest: "Figure1#2.jpg",
                kind: WikiLink,
                display_text: "Figure1#2.jpg",
            },
            Reference {
                range: 4004..4020,
                dest: "Figure1",
                kind: WikiLink,
                display_text: "2.jpg",
            },
            Reference {
                range: 4144..4160,
                dest: "Figure1^2.jpg",
                kind: WikiLink,
                display_text: "Figure1^2.jpg",
            },
            Reference {
                range: 4278..4294,
                dest: "Figure 1.jpg",
                kind: WikiLink,
                display_text: "Figure 1.jpg",
            },
            Reference {
                range: 4320..4338,
                dest: "empty_video.mp4",
                kind: WikiLink,
                display_text: "empty_video.mp4",
            },
            Reference {
                range: 50..61,
                dest: "#^c93d41",
                kind: WikiLink,
                display_text: "#^c93d41",
            },
            Reference {
                range: 117..126,
                dest: "#^9afo",
                kind: WikiLink,
                display_text: "#^9afo",
            },
        ]
        "########);
        assert_debug_snapshot!(referenceables, @r#"
        [
            Note {
                path: "tests/data/vaults/tt/Note 1.md",
            },
            Note {
                path: "tests/data/vaults/tt/Three laws of motion.md",
            },
            Note {
                path: "tests/data/vaults/tt/ww.md",
            },
            Note {
                path: "tests/data/vaults/tt/Figure 1.jpg.md",
            },
            Asset {
                path: "tests/data/vaults/tt/Figure 1.jpg",
            },
            Note {
                path: "tests/data/vaults/tt/().md",
            },
            Asset {
                path: "tests/data/vaults/tt/Figure1#2.jpg",
            },
            Note {
                path: "tests/data/vaults/tt/Figure 1.jpg.md.md",
            },
            Asset {
                path: "tests/data/vaults/tt/empty_video.mp4",
            },
            Note {
                path: "tests/data/vaults/tt/block note.md",
            },
            Note {
                path: "tests/data/vaults/tt/Figure1.md",
            },
            Asset {
                path: "tests/data/vaults/tt/Figure1^2.jpg",
            },
            Note {
                path: "tests/data/vaults/tt/Note 2.md",
            },
            Asset {
                path: "tests/data/vaults/tt/Figure1|2.jpg",
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 57..75,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H4,
                range: 75..94,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 95..117,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H6,
                range: 118..149,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H6,
                range: 889..944,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H5,
                range: 3538..3558,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H2,
                range: 4341..4347,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 4348..4355,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H4,
                range: 4355..4363,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 4363..4378,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H2,
                range: 4383..4387,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 2.md",
                level: H2,
                range: 1..23,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 2.md",
                level: H4,
                range: 24..32,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 2.md",
                level: H3,
                range: 33..51,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 2.md",
                level: H2,
                range: 53..77,
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
                range: 169..225,
                dest: "Three laws of motion",
                kind: MarkdownLink,
                display_text: "Three laws of motion 11",
            },
            Reference {
                range: 256..291,
                dest: "#Level 3 title",
                kind: MarkdownLink,
                display_text: "Level 3 title",
            },
            Reference {
                range: 358..397,
                dest: "Note 2#Some level 2 title",
                kind: MarkdownLink,
                display_text: "22",
            },
            Reference {
                range: 516..523,
                dest: "()",
                kind: MarkdownLink,
                display_text: "www",
            },
            Reference {
                range: 592..598,
                dest: "ww",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 649..653,
                dest: "()",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 702..735,
                dest: "Three laws of motion",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 953..976,
                dest: "Three laws of motion",
                kind: WikiLink,
                display_text: "Three laws of motion",
            },
            Reference {
                range: 1017..1043,
                dest: "Three laws of motion.md",
                kind: WikiLink,
                display_text: "Three laws of motion.md",
            },
            Reference {
                range: 1077..1097,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " Note two",
            },
            Reference {
                range: 1127..1144,
                dest: "#Level 3 title",
                kind: WikiLink,
                display_text: "#Level 3 title",
            },
            Reference {
                range: 1205..1222,
                dest: "#Level 4 title",
                kind: WikiLink,
                display_text: "#Level 4 title",
            },
            Reference {
                range: 1284..1294,
                dest: "#random",
                kind: WikiLink,
                display_text: "#random",
            },
            Reference {
                range: 1360..1388,
                dest: "Note 2#Some level 2 title",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title",
            },
            Reference {
                range: 1459..1501,
                dest: "Note 2#Some level 2 title#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title#Level 3 title",
            },
            Reference {
                range: 1538..1568,
                dest: "Note 2#random#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#random#Level 3 title",
            },
            Reference {
                range: 1647..1670,
                dest: "Note 2#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#Level 3 title",
            },
            Reference {
                range: 1699..1711,
                dest: "Note 2#L4",
                kind: WikiLink,
                display_text: "Note 2#L4",
            },
            Reference {
                range: 1747..1778,
                dest: "Note 2#Some level 2 title#L4",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title#L4",
            },
            Reference {
                range: 1944..1966,
                dest: "Non-existing note 4",
                kind: WikiLink,
                display_text: "Non-existing note 4",
            },
            Reference {
                range: 2042..2046,
                dest: "#",
                kind: WikiLink,
                display_text: "#",
            },
            Reference {
                range: 2096..2107,
                dest: "Note 2##",
                kind: WikiLink,
                display_text: "Note 2##",
            },
            Reference {
                range: 2188..2212,
                dest: "#######Link to figure",
                kind: WikiLink,
                display_text: "#######Link to figure",
            },
            Reference {
                range: 2245..2268,
                dest: "######Link to figure",
                kind: WikiLink,
                display_text: "######Link to figure",
            },
            Reference {
                range: 2299..2320,
                dest: "####Link to figure",
                kind: WikiLink,
                display_text: "####Link to figure",
            },
            Reference {
                range: 2350..2370,
                dest: "###Link to figure",
                kind: WikiLink,
                display_text: "###Link to figure",
            },
            Reference {
                range: 2398..2416,
                dest: "#Link to figure",
                kind: WikiLink,
                display_text: "#Link to figure",
            },
            Reference {
                range: 2449..2461,
                dest: "#L2 ",
                kind: WikiLink,
                display_text: " #L4",
            },
            Reference {
                range: 2556..2573,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " 2 | 3",
            },
            Reference {
                range: 2674..2685,
                dest: "###L2#L4",
                kind: WikiLink,
                display_text: "###L2#L4",
            },
            Reference {
                range: 2745..2760,
                dest: "##L2######L4",
                kind: WikiLink,
                display_text: "##L2######L4",
            },
            Reference {
                range: 2819..2833,
                dest: "##L2#####L4",
                kind: WikiLink,
                display_text: "##L2#####L4",
            },
            Reference {
                range: 2895..2912,
                dest: "##L2#####L4#L3",
                kind: WikiLink,
                display_text: "##L2#####L4#L3",
            },
            Reference {
                range: 2968..2993,
                dest: "##L2#####L4#Another L3",
                kind: WikiLink,
                display_text: "##L2#####L4#Another L3",
            },
            Reference {
                range: 3334..3351,
                dest: "##L2######L4",
                kind: MarkdownLink,
                display_text: "1",
            },
            Reference {
                range: 3410..3426,
                dest: "##L2#####L4",
                kind: MarkdownLink,
                display_text: "2",
            },
            Reference {
                range: 3488..3507,
                dest: "##L2#####L4#L3",
                kind: MarkdownLink,
                display_text: "3",
            },
            Reference {
                range: 3580..3595,
                dest: "Figure 1.jpg",
                kind: WikiLink,
                display_text: "Figure 1.jpg",
            },
            Reference {
                range: 3707..3725,
                dest: "Figure 1.jpg.md",
                kind: WikiLink,
                display_text: "Figure 1.jpg.md",
            },
            Reference {
                range: 3818..3839,
                dest: "Figure 1.jpg.md.md",
                kind: WikiLink,
                display_text: "Figure 1.jpg.md.md",
            },
            Reference {
                range: 3864..3880,
                dest: "Figure1#2.jpg",
                kind: WikiLink,
                display_text: "Figure1#2.jpg",
            },
            Reference {
                range: 4004..4020,
                dest: "Figure1",
                kind: WikiLink,
                display_text: "2.jpg",
            },
            Reference {
                range: 4144..4160,
                dest: "Figure1^2.jpg",
                kind: WikiLink,
                display_text: "Figure1^2.jpg",
            },
            Reference {
                range: 4278..4294,
                dest: "Figure 1.jpg",
                kind: WikiLink,
                display_text: "Figure 1.jpg",
            },
            Reference {
                range: 4320..4338,
                dest: "empty_video.mp4",
                kind: WikiLink,
                display_text: "empty_video.mp4",
            },
        ]
        "########);
        assert_debug_snapshot!(referenceables, @r#"
        [
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 57..75,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H4,
                range: 75..94,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 95..117,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H6,
                range: 118..149,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H6,
                range: 889..944,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H5,
                range: 3538..3558,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H2,
                range: 4341..4347,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 4348..4355,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H4,
                range: 4355..4363,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 4363..4378,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H2,
                range: 4383..4387,
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
        Document [0..4387]
          List(None) [0..57]
            Item [0..57]
              Text(Borrowed("Note in Obsidian cannot have # ^ ")) [2..35]
              Text(Borrowed("[")) [35..36]
              Text(Borrowed(" ")) [36..37]
              Text(Borrowed("]")) [37..38]
              Text(Borrowed(" | in the heading.")) [38..56]
          Heading { level: H3, id: None, classes: [], attrs: [] } [57..75]
            Text(Borrowed("Level 3 title")) [61..74]
          Heading { level: H4, id: None, classes: [], attrs: [] } [75..94]
            Text(Borrowed("Level 4 title")) [80..93]
          Heading { level: H3, id: None, classes: [], attrs: [] } [95..117]
            Text(Borrowed("Example (level 3)")) [99..116]
          Heading { level: H6, id: None, classes: [], attrs: [] } [118..149]
            Text(Borrowed("Markdown link: ")) [125..140]
            Code(Borrowed("[x](y)")) [140..148]
          List(None) [149..889]
            Item [149..226]
              Text(Borrowed("percent encoding: ")) [151..169]
              Link { link_type: Inline, dest_url: Borrowed("Three%20laws%20of%20motion.md"), title: Borrowed(""), id: Borrowed("") } [169..225]
                Text(Borrowed("Three laws of motion 11")) [170..193]
            Item [226..333]
              Text(Borrowed("heading  in the same file:  ")) [228..256]
              Link { link_type: Inline, dest_url: Borrowed("#Level%203%20title"), title: Borrowed(""), id: Borrowed("") } [256..291]
                Text(Borrowed("Level 3 title")) [257..270]
              List(None) [291..333]
                Item [291..333]
                  Code(Borrowed("[Level 3 title](#Level%203%20title)")) [295..332]
            Item [333..501]
              Text(Borrowed("different file heading ")) [335..358]
              Link { link_type: Inline, dest_url: Borrowed("Note%202#Some%20level%202%20title"), title: Borrowed(""), id: Borrowed("") } [358..397]
                Text(Borrowed("22")) [359..361]
              List(None) [397..501]
                Item [397..443]
                  Code(Borrowed("[22](Note%202#Some%20level%202%20title)")) [401..442]
                Item [442..501]
                  Text(Borrowed("the heading is level 2 but we don")) [446..479]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [479..480]
                  Text(Borrowed("t need to specify it")) [480..500]
            Item [501..577]
              Text(Borrowed("empty link 1 ")) [503..516]
              Link { link_type: Inline, dest_url: Borrowed(""), title: Borrowed(""), id: Borrowed("") } [516..523]
                Text(Borrowed("www")) [517..520]
              List(None) [523..577]
                Item [523..577]
                  Text(Borrowed("empty markdown link ")) [527..547]
                  Code(Borrowed("[]()")) [547..553]
                  Text(Borrowed(" points to note ")) [553..569]
                  Code(Borrowed("().md")) [569..576]
            Item [577..634]
              Text(Borrowed("empty link 2 ")) [579..592]
              Link { link_type: Inline, dest_url: Borrowed("ww"), title: Borrowed(""), id: Borrowed("") } [592..598]
              List(None) [598..634]
                Item [598..611]
                  Code(Borrowed("[](ww)")) [602..610]
                Item [610..634]
                  Text(Borrowed("points to note ")) [614..629]
                  Code(Borrowed("ww")) [629..633]
            Item [634..687]
              Text(Borrowed("empty link 3 ")) [636..649]
              Link { link_type: Inline, dest_url: Borrowed(""), title: Borrowed(""), id: Borrowed("") } [649..653]
              List(None) [653..687]
                Item [653..664]
                  Code(Borrowed("[]()")) [657..663]
                Item [663..687]
                  Text(Borrowed("points to note ")) [667..682]
                  Code(Borrowed("()")) [682..686]
            Item [687..889]
              Text(Borrowed("empty link 4 ")) [689..702]
              Link { link_type: Inline, dest_url: Borrowed("Three%20laws%20of%20motion.md"), title: Borrowed(""), id: Borrowed("") } [702..735]
              List(None) [735..889]
                Item [735..775]
                  Code(Borrowed("[](Three%20laws%20of%20motion.md)")) [739..774]
                Item [774..816]
                  Text(Borrowed("points to note ")) [778..793]
                  Code(Borrowed("Three laws of motion")) [793..815]
                Item [815..889]
                  Text(Borrowed("the first part of markdown link is displayed text and doesn")) [819..878]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [878..879]
                  Text(Borrowed("t matter")) [879..887]
          Heading { level: H6, id: None, classes: [], attrs: [] } [889..944]
            Text(Borrowed("Wiki link: ")) [896..907]
            Code(Borrowed("[[x#]]")) [907..915]
            Text(Borrowed(" | ")) [915..918]
            Code(Borrowed("[[x#^block_identifier]]")) [918..943]
          List(None) [944..3538]
            Item [944..978]
              Text(Borrowed("basic: ")) [946..953]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Three laws of motion"), title: Borrowed(""), id: Borrowed("") } [953..976]
                Text(Borrowed("Three laws of motion")) [955..975]
            Item [978..1045]
              Text(Borrowed("explicit markdown extension in name: ")) [980..1017]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Three laws of motion.md"), title: Borrowed(""), id: Borrowed("") } [1017..1043]
                Text(Borrowed("Three laws of motion.md")) [1019..1042]
            Item [1045..1099]
              Text(Borrowed("with pipe for displayed text: ")) [1047..1077]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Note 2 "), title: Borrowed(""), id: Borrowed("") } [1077..1097]
                Text(Borrowed(" Note two")) [1087..1096]
            Item [1099..1170]
              Text(Borrowed("heading in the same note: ")) [1101..1127]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1127..1144]
                Text(Borrowed("#Level 3 title")) [1129..1143]
              List(None) [1145..1170]
                Item [1145..1170]
                  Code(Borrowed("[[#Level 3 title]]")) [1149..1169]
            Item [1170..1248]
              Text(Borrowed("nested heading in the same note: ")) [1172..1205]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 4 title"), title: Borrowed(""), id: Borrowed("") } [1205..1222]
                Text(Borrowed("#Level 4 title")) [1207..1221]
              List(None) [1223..1248]
                Item [1223..1248]
                  Code(Borrowed("[[#Level 4 title]]")) [1227..1247]
            Item [1248..1333]
              Text(Borrowed("invalid heading in the same note: ")) [1250..1284]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#random"), title: Borrowed(""), id: Borrowed("") } [1284..1294]
                Text(Borrowed("#random")) [1286..1293]
              List(None) [1295..1333]
                Item [1295..1313]
                  Code(Borrowed("[[#random]]")) [1299..1312]
                Item [1312..1333]
                  Text(Borrowed("fallback to note")) [1316..1332]
            Item [1333..1425]
              Text(Borrowed("heading in another note: ")) [1335..1360]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title"), title: Borrowed(""), id: Borrowed("") } [1360..1388]
                Text(Borrowed("Note 2#Some level 2 title")) [1362..1387]
              List(None) [1389..1425]
                Item [1389..1425]
                  Code(Borrowed("[[Note 2#Some level 2 title]]")) [1393..1424]
            Item [1425..1503]
              Text(Borrowed("nested heading in another note: ")) [1427..1459]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1459..1501]
                Text(Borrowed("Note 2#Some level 2 title#Level 3 title")) [1461..1500]
            Item [1503..1620]
              Text(Borrowed("invalid heading in another note: ")) [1505..1538]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#random#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1538..1568]
                Text(Borrowed("Note 2#random#Level 3 title")) [1540..1567]
              List(None) [1569..1620]
                Item [1569..1620]
                  Text(Borrowed("fallback to note if the heading doesn")) [1573..1610]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1610..1611]
                  Text(Borrowed("t exist")) [1611..1618]
            Item [1620..1672]
              Text(Borrowed("heading in another note: ")) [1622..1647]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [1647..1670]
                Text(Borrowed("Note 2#Level 3 title")) [1649..1669]
            Item [1672..1713]
              Text(Borrowed("heading in another note: ")) [1674..1699]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#L4"), title: Borrowed(""), id: Borrowed("") } [1699..1711]
                Text(Borrowed("Note 2#L4")) [1701..1710]
            Item [1713..1923]
              Text(Borrowed("nested heading in another note: ")) [1715..1747]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#L4"), title: Borrowed(""), id: Borrowed("") } [1747..1778]
                Text(Borrowed("Note 2#Some level 2 title#L4")) [1749..1777]
              List(None) [1779..1923]
                Item [1779..1852]
                  Text(Borrowed("when there")) [1783..1793]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1793..1794]
                  Text(Borrowed("s multiple levels, the level doesn")) [1794..1828]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1828..1829]
                  Text(Borrowed("t need to be specified")) [1829..1851]
                Item [1851..1923]
                  Text(Borrowed("it will match as long as the ancestor-descendant relationship holds")) [1855..1922]
            Item [1923..1968]
              Text(Borrowed("non-existing note: ")) [1925..1944]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Non-existing note 4"), title: Borrowed(""), id: Borrowed("") } [1944..1966]
                Text(Borrowed("Non-existing note 4")) [1946..1965]
            Item [1968..2013]
              Text(Borrowed("empty link: ")) [1970..1982]
              Text(Borrowed("[")) [1982..1983]
              Text(Borrowed("[")) [1983..1984]
              Text(Borrowed("]")) [1984..1985]
              Text(Borrowed("]")) [1985..1986]
              List(None) [1986..2013]
                Item [1986..2013]
                  Text(Borrowed("points to current note")) [1990..2012]
            Item [2013..2130]
              Text(Borrowed("empty heading:")) [2015..2029]
              List(None) [2029..2130]
                Item [2029..2076]
                  Code(Borrowed("[[#]]")) [2033..2040]
                  Text(Borrowed(": ")) [2040..2042]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#"), title: Borrowed(""), id: Borrowed("") } [2042..2046]
                    Text(Borrowed("#")) [2044..2045]
                  List(None) [2049..2076]
                    Item [2049..2076]
                      Text(Borrowed("points to current note")) [2053..2075]
                Item [2075..2130]
                  Code(Borrowed("[[Note 2##]]")) [2079..2093]
                  Text(Borrowed(":  ")) [2093..2096]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2##"), title: Borrowed(""), id: Borrowed("") } [2096..2107]
                    Text(Borrowed("Note 2##")) [2098..2106]
                  List(None) [2109..2130]
                    Item [2109..2130]
                      Text(Borrowed("points to Note 2")) [2113..2129]
            Item [2130..2418]
              Text(Borrowed("incorrect heading level")) [2132..2155]
              List(None) [2155..2418]
                Item [2155..2214]
                  Code(Borrowed("[[#######Link to figure]]")) [2159..2186]
                  Text(Borrowed(": ")) [2186..2188]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#######Link to figure"), title: Borrowed(""), id: Borrowed("") } [2188..2212]
                    Text(Borrowed("#######Link to figure")) [2190..2211]
                Item [2213..2270]
                  Code(Borrowed("[[######Link to figure]]")) [2217..2243]
                  Text(Borrowed(": ")) [2243..2245]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("######Link to figure"), title: Borrowed(""), id: Borrowed("") } [2245..2268]
                    Text(Borrowed("######Link to figure")) [2247..2267]
                Item [2269..2322]
                  Code(Borrowed("[[####Link to figure]]")) [2273..2297]
                  Text(Borrowed(": ")) [2297..2299]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("####Link to figure"), title: Borrowed(""), id: Borrowed("") } [2299..2320]
                    Text(Borrowed("####Link to figure")) [2301..2319]
                Item [2321..2372]
                  Code(Borrowed("[[###Link to figure]]")) [2325..2348]
                  Text(Borrowed(": ")) [2348..2350]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("###Link to figure"), title: Borrowed(""), id: Borrowed("") } [2350..2370]
                    Text(Borrowed("###Link to figure")) [2352..2369]
                Item [2371..2418]
                  Code(Borrowed("[[#Link to figure]]")) [2375..2396]
                  Text(Borrowed(": ")) [2396..2398]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Link to figure"), title: Borrowed(""), id: Borrowed("") } [2398..2416]
                    Text(Borrowed("#Link to figure")) [2400..2415]
            Item [2418..2538]
              Text(Borrowed("ambiguous pipe and heading: ")) [2421..2449]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("#L2 "), title: Borrowed(""), id: Borrowed("") } [2449..2461]
                Text(Borrowed(" #L4")) [2456..2460]
              List(None) [2463..2538]
                Item [2463..2483]
                  Code(Borrowed("[[#L2 | #L4]]")) [2467..2482]
                Item [2483..2500]
                  Text(Borrowed("points to L2")) [2487..2499]
                Item [2500..2538]
                  Text(Borrowed("things after the pipe is escaped")) [2504..2536]
            Item [2538..2626]
              Text(Borrowed("multiple pipe: ")) [2541..2556]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Note 2 "), title: Borrowed(""), id: Borrowed("") } [2556..2573]
                Text(Borrowed(" 2 | 3")) [2566..2572]
              List(None) [2575..2626]
                Item [2575..2600]
                  Code(Borrowed("[[Note 2 | 2 | 3]]")) [2579..2599]
                Item [2600..2626]
                  Text(Borrowed("this points to Note 2")) [2604..2625]
            Item [2626..3119]
              Text(Borrowed("incorrect nested heading")) [2628..2652]
              List(None) [2653..3119]
                Item [2653..2722]
                  Code(Borrowed("[[###L2#L4]]")) [2657..2671]
                  Text(Borrowed(":  ")) [2671..2674]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("###L2#L4"), title: Borrowed(""), id: Borrowed("") } [2674..2685]
                    Text(Borrowed("###L2#L4")) [2676..2684]
                  List(None) [2687..2722]
                    Item [2687..2722]
                      Text(Borrowed("points to L4 heading correctly")) [2691..2721]
                Item [2721..2797]
                  Code(Borrowed("[[##L2######L4]]")) [2725..2743]
                  Text(Borrowed(": ")) [2743..2745]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2######L4"), title: Borrowed(""), id: Borrowed("") } [2745..2760]
                    Text(Borrowed("##L2######L4")) [2747..2759]
                  List(None) [2762..2797]
                    Item [2762..2797]
                      Text(Borrowed("points to L4 heading correctly")) [2766..2796]
                Item [2796..2870]
                  Code(Borrowed("[[##L2#####L4]]")) [2800..2817]
                  Text(Borrowed(": ")) [2817..2819]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4"), title: Borrowed(""), id: Borrowed("") } [2819..2833]
                    Text(Borrowed("##L2#####L4")) [2821..2832]
                  List(None) [2835..2870]
                    Item [2835..2870]
                      Text(Borrowed("points to L4 heading correctly")) [2839..2869]
                Item [2869..2943]
                  Code(Borrowed("[[##L2#####L4#L3]]")) [2873..2893]
                  Text(Borrowed(": ")) [2893..2895]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4#L3"), title: Borrowed(""), id: Borrowed("") } [2895..2912]
                    Text(Borrowed("##L2#####L4#L3")) [2897..2911]
                  List(None) [2914..2943]
                    Item [2914..2943]
                      Text(Borrowed("fallback to current note")) [2918..2942]
                Item [2942..3024]
                  Code(Borrowed("[[##L2#####L4#L3]]")) [2946..2966]
                  Text(Borrowed(": ")) [2966..2968]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4#Another L3"), title: Borrowed(""), id: Borrowed("") } [2968..2993]
                    Text(Borrowed("##L2#####L4#Another L3")) [2970..2992]
                  List(None) [2995..3024]
                    Item [2995..3024]
                      Text(Borrowed("fallback to current note")) [2999..3023]
                Item [3023..3119]
                  Text(Borrowed("for displayed text, the first hash is removed, the subsequent nesting ones are not affected")) [3027..3118]
            Item [3119..3239]
              Text(Borrowed("↳ it looks like whenever there")) [3121..3153]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3153..3154]
              Text(Borrowed("s multiple hash, it")) [3154..3173]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3173..3174]
              Text(Borrowed("s all stripped. only the ancestor-descendant relationship matter")) [3174..3238]
            Item [3239..3538]
              Text(Borrowed("I don")) [3241..3246]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3246..3247]
              Text(Borrowed("t think there")) [3247..3260]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3260..3261]
              Text(Borrowed("s a different between Wikilink and Markdown link")) [3261..3309]
              List(None) [3309..3538]
                Item [3309..3387]
                  Code(Borrowed("[1](##L2######L4)")) [3313..3332]
                  Text(Borrowed(": ")) [3332..3334]
                  Link { link_type: Inline, dest_url: Borrowed("##L2######L4"), title: Borrowed(""), id: Borrowed("") } [3334..3351]
                    Text(Borrowed("1")) [3335..3336]
                  List(None) [3352..3387]
                    Item [3352..3387]
                      Text(Borrowed("points to L4 heading correctly")) [3356..3386]
                Item [3386..3462]
                  Code(Borrowed("[2](##L2#####L4)")) [3390..3408]
                  Text(Borrowed(": ")) [3408..3410]
                  Link { link_type: Inline, dest_url: Borrowed("##L2#####L4"), title: Borrowed(""), id: Borrowed("") } [3410..3426]
                    Text(Borrowed("2")) [3411..3412]
                  List(None) [3427..3462]
                    Item [3427..3462]
                      Text(Borrowed("points to L4 heading correctly")) [3431..3461]
                Item [3461..3538]
                  Code(Borrowed("[3](##L2#####L4#L3)")) [3465..3486]
                  Text(Borrowed(": ")) [3486..3488]
                  Link { link_type: Inline, dest_url: Borrowed("##L2#####L4#L3"), title: Borrowed(""), id: Borrowed("") } [3488..3507]
                    Text(Borrowed("3")) [3489..3490]
                  List(None) [3508..3538]
                    Item [3508..3538]
                      Text(Borrowed("fallback to current note")) [3512..3536]
          Heading { level: H5, id: None, classes: [], attrs: [] } [3538..3558]
            Text(Borrowed("Link to asset")) [3544..3557]
          List(None) [3558..4257]
            Item [3558..3682]
              Code(Borrowed("[[Figure 1.jpg]]")) [3560..3578]
              Text(Borrowed(": ")) [3578..3580]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg"), title: Borrowed(""), id: Borrowed("") } [3580..3595]
                Text(Borrowed("Figure 1.jpg")) [3582..3594]
              List(None) [3596..3682]
                Item [3596..3682]
                  Text(Borrowed("even if there exists a note called ")) [3600..3635]
                  Code(Borrowed("Figure 1.jpg")) [3635..3649]
                  Text(Borrowed(", the asset will take precedence")) [3649..3681]
            Item [3682..3790]
              Code(Borrowed("[[Figure 1.jpg.md]]")) [3684..3705]
              Text(Borrowed(": ")) [3705..3707]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg.md"), title: Borrowed(""), id: Borrowed("") } [3707..3725]
                Text(Borrowed("Figure 1.jpg.md")) [3709..3724]
              List(None) [3726..3790]
                Item [3726..3790]
                  Text(Borrowed("with explicit ")) [3730..3744]
                  Code(Borrowed(".md")) [3744..3749]
                  Text(Borrowed(" ending, we seek for note ")) [3749..3775]
                  Code(Borrowed("Figure 1.jpg")) [3775..3789]
            Item [3790..3841]
              Code(Borrowed("[[Figure 1.jpg.md.md]]")) [3792..3816]
              Text(Borrowed(": ")) [3816..3818]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg.md.md"), title: Borrowed(""), id: Borrowed("") } [3818..3839]
                Text(Borrowed("Figure 1.jpg.md.md")) [3820..3838]
            Item [3841..3981]
              Code(Borrowed("[[Figure1#2.jpg]]")) [3843..3862]
              Text(Borrowed(": ")) [3862..3864]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1#2.jpg"), title: Borrowed(""), id: Borrowed("") } [3864..3880]
                Text(Borrowed("Figure1#2.jpg")) [3866..3879]
              List(None) [3881..3981]
                Item [3881..3981]
                  Text(Borrowed("understood as note and points to note Figure 1 (fallback to note after failing finding heading)")) [3885..3980]
            Item [3981..4121]
              Code(Borrowed("[[Figure1|2.jpg]]")) [3983..4002]
              Text(Borrowed(": ")) [4002..4004]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Figure1"), title: Borrowed(""), id: Borrowed("") } [4004..4020]
                Text(Borrowed("2.jpg")) [4014..4019]
              List(None) [4021..4121]
                Item [4021..4121]
                  Text(Borrowed("understood as note and points to note Figure 1 (fallback to note after failing finding heading)")) [4025..4120]
            Item [4121..4181]
              Code(Borrowed("[[Figure1^2.jpg]]")) [4123..4142]
              Text(Borrowed(": ")) [4142..4144]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1^2.jpg"), title: Borrowed(""), id: Borrowed("") } [4144..4160]
                Text(Borrowed("Figure1^2.jpg")) [4146..4159]
              List(None) [4161..4181]
                Item [4161..4181]
                  Text(Borrowed("points to image")) [4165..4180]
            Item [4181..4257]
              Text(Borrowed("↳ when there")) [4183..4197]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [4197..4198]
              Text(Borrowed("s ")) [4198..4200]
              Code(Borrowed(".md")) [4200..4205]
              Text(Borrowed(", it")) [4205..4209]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [4209..4210]
              Text(Borrowed("s removed and limit to the searching of notes")) [4210..4255]
          Paragraph [4257..4296]
            Code(Borrowed("![[Figure 1.jpg]]")) [4257..4276]
            Text(Borrowed(": ")) [4276..4278]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg"), title: Borrowed(""), id: Borrowed("") } [4278..4294]
              Text(Borrowed("Figure 1.jpg")) [4281..4293]
          Paragraph [4297..4340]
            Code(Borrowed("[[empty_video.mp4]]")) [4297..4318]
            Text(Borrowed(": ")) [4318..4320]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("empty_video.mp4"), title: Borrowed(""), id: Borrowed("") } [4320..4338]
              Text(Borrowed("empty_video.mp4")) [4322..4337]
          Heading { level: H2, id: None, classes: [], attrs: [] } [4341..4347]
            Text(Borrowed("L2")) [4344..4346]
          Heading { level: H3, id: None, classes: [], attrs: [] } [4348..4355]
            Text(Borrowed("L3")) [4352..4354]
          Heading { level: H4, id: None, classes: [], attrs: [] } [4355..4363]
            Text(Borrowed("L4")) [4360..4362]
          Heading { level: H3, id: None, classes: [], attrs: [] } [4363..4378]
            Text(Borrowed("Another L3")) [4367..4377]
          Rule [4379..4383]
          Heading { level: H2, id: None, classes: [], attrs: [] } [4383..4387]
        "########);
    }
}
