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

/// Splits a destination string into three parts: the note name and (nested) heading, and the display text.
fn split_dest_string(s: &str) -> (&str, &str, &str) {
    let hash_pos = s.find('#');
    let pipe_pos = s.find('|');

    match (hash_pos, pipe_pos) {
        //! If pipe comes first, we ignore the hash, considering it to be a
        //! part of the display text.
        (Some(h), Some(p)) if p < h => {
            let note = &s[..p];
            let display_text = &s[p + 1..];
            (note, "", display_text)
        }
        (Some(h), Some(p)) => {
            let note = &s[..h];
            let heading = &s[h + 1..p];
            let display_text = &s[p + 1..];
            (note, heading, display_text)
        }
        // No pipe
        (Some(h), None) => {
            let note = &s[..h];
            let heading = &s[h + 1..];
            (note, heading, "")
        }
        // No hash
        (None, Some(p)) => {
            let note = &s[..p];
            let display_text = &s[p + 1..];
            (note, "", display_text)
        }
        // No pipe or hash
        (None, None) => (&s, "", ""),
    }
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
    fn test_split_dest_string() {
        // Get references from note 1
        let path = PathBuf::from("tests/data/vaults/tt/Note 1.md");
        let (references, _): (Vec<Reference>, Vec<Referenceable>) =
            scan_note(&path);
        let dest_strings: Vec<&str> =
            references.iter().map(|r| r.dest.as_str()).collect();
        let (note_names, headings, display_texts): (
            Vec<&str>,
            Vec<&str>,
            Vec<&str>,
        ) = dest_strings.iter().map(|s| split_dest_string(s)).collect();
        let table = print_table(
            &vec!["Destination string", "Note", "Heading", "Display text"],
            &vec![dest_strings, note_names, headings, display_texts],
        );
        assert_snapshot!(table, @r"
        +-----+-----------------------------------------+-------------------------+----------------------------------+--------------+
        | Idx |           Destination string            |          Note           |             Heading              | Display text |
        +-----+-----------------------------------------+-------------------------+----------------------------------+--------------+
        |   0 | Three laws of motion                    | Three laws of motion    |                                  |              |
        |   1 | #Level 3 title                          |                         | Level 3 title                    |              |
        |   2 | Note 2#Some level 2 title               | Note 2                  | Some level 2 title               |              |
        |   3 | ()                                      | ()                      |                                  |              |
        |   4 | ww                                      | ww                      |                                  |              |
        |   5 | ()                                      | ()                      |                                  |              |
        |   6 | Three laws of motion                    | Three laws of motion    |                                  |              |
        |   7 | Three laws of motion                    | Three laws of motion    |                                  |              |
        |   8 | Three laws of motion.md                 | Three laws of motion.md |                                  |              |
        |   9 | Note 2                                  | Note 2                  |                                  |              |
        |  10 | #Level 3 title                          |                         | Level 3 title                    |              |
        |  11 | #Level 4 title                          |                         | Level 4 title                    |              |
        |  12 | #random                                 |                         | random                           |              |
        |  13 | Note 2#Some level 2 title               | Note 2                  | Some level 2 title               |              |
        |  14 | Note 2#Some level 2 title#Level 3 title | Note 2                  | Some level 2 title#Level 3 title |              |
        |  15 | Note 2#random#Level 3 title             | Note 2                  | random#Level 3 title             |              |
        |  16 | Note 2#Level 3 title                    | Note 2                  | Level 3 title                    |              |
        |  17 | Note 2#L4                               | Note 2                  | L4                               |              |
        |  18 | Note 2#Some level 2 title#L4            | Note 2                  | Some level 2 title#L4            |              |
        |  19 | Non-existing note 4                     | Non-existing note 4     |                                  |              |
        |  20 | #                                       |                         |                                  |              |
        |  21 | #######Link to figure                   |                         | ######Link to figure             |              |
        |  22 | ######Link to figure                    |                         | #####Link to figure              |              |
        |  23 | ####Link to figure                      |                         | ###Link to figure                |              |
        |  24 | ###Link to figure                       |                         | ##Link to figure                 |              |
        |  25 | #Link to figure                         |                         | Link to figure                   |              |
        |  26 | #L2                                     |                         | L2                               |              |
        |  27 | Note 2                                  | Note 2                  |                                  |              |
        |  28 | ###L2#L4                                |                         | ##L2#L4                          |              |
        |  29 | ##L2######L4                            |                         | #L2######L4                      |              |
        |  30 | ##L2#####L4                             |                         | #L2#####L4                       |              |
        |  31 | ##L2#####L4#L3                          |                         | #L2#####L4#L3                    |              |
        |  32 | ##L2#####L4#Another L3                  |                         | #L2#####L4#Another L3            |              |
        |  33 | ##L2######L4                            |                         | #L2######L4                      |              |
        |  34 | ##L2#####L4                             |                         | #L2#####L4                       |              |
        |  35 | ##L2#####L4#L3                          |                         | #L2#####L4#L3                    |              |
        |  36 | Figure 1.jpg                            | Figure 1.jpg            |                                  |              |
        |  37 | Figure 1.jpg.md                         | Figure 1.jpg.md         |                                  |              |
        |  38 | Figure 1.jpg.md.md                      | Figure 1.jpg.md.md      |                                  |              |
        |  39 | Figure1#2.jpg                           | Figure1                 | 2.jpg                            |              |
        |  40 | Figure1                                 | Figure1                 |                                  |              |
        |  41 | Figure1^2.jpg                           | Figure1^2.jpg           |                                  |              |
        |  42 | Figure 1.jpg                            | Figure 1.jpg            |                                  |              |
        |  43 | empty_video.mp4                         | empty_video.mp4         |                                  |              |
        +-----+-----------------------------------------+-------------------------+----------------------------------+--------------+
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
                range: 2030..2034,
                dest: "#",
                kind: WikiLink,
                display_text: "#",
            },
            Reference {
                range: 2128..2152,
                dest: "#######Link to figure",
                kind: WikiLink,
                display_text: "#######Link to figure",
            },
            Reference {
                range: 2185..2208,
                dest: "######Link to figure",
                kind: WikiLink,
                display_text: "######Link to figure",
            },
            Reference {
                range: 2239..2260,
                dest: "####Link to figure",
                kind: WikiLink,
                display_text: "####Link to figure",
            },
            Reference {
                range: 2290..2310,
                dest: "###Link to figure",
                kind: WikiLink,
                display_text: "###Link to figure",
            },
            Reference {
                range: 2338..2356,
                dest: "#Link to figure",
                kind: WikiLink,
                display_text: "#Link to figure",
            },
            Reference {
                range: 2389..2401,
                dest: "#L2 ",
                kind: WikiLink,
                display_text: " #L4",
            },
            Reference {
                range: 2496..2513,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " 2 | 3",
            },
            Reference {
                range: 2614..2625,
                dest: "###L2#L4",
                kind: WikiLink,
                display_text: "###L2#L4",
            },
            Reference {
                range: 2685..2700,
                dest: "##L2######L4",
                kind: WikiLink,
                display_text: "##L2######L4",
            },
            Reference {
                range: 2759..2773,
                dest: "##L2#####L4",
                kind: WikiLink,
                display_text: "##L2#####L4",
            },
            Reference {
                range: 2835..2852,
                dest: "##L2#####L4#L3",
                kind: WikiLink,
                display_text: "##L2#####L4#L3",
            },
            Reference {
                range: 2908..2933,
                dest: "##L2#####L4#Another L3",
                kind: WikiLink,
                display_text: "##L2#####L4#Another L3",
            },
            Reference {
                range: 3274..3291,
                dest: "##L2######L4",
                kind: MarkdownLink,
                display_text: "1",
            },
            Reference {
                range: 3350..3366,
                dest: "##L2#####L4",
                kind: MarkdownLink,
                display_text: "2",
            },
            Reference {
                range: 3428..3447,
                dest: "##L2#####L4#L3",
                kind: MarkdownLink,
                display_text: "3",
            },
            Reference {
                range: 3520..3535,
                dest: "Figure 1.jpg",
                kind: WikiLink,
                display_text: "Figure 1.jpg",
            },
            Reference {
                range: 3647..3665,
                dest: "Figure 1.jpg.md",
                kind: WikiLink,
                display_text: "Figure 1.jpg.md",
            },
            Reference {
                range: 3758..3779,
                dest: "Figure 1.jpg.md.md",
                kind: WikiLink,
                display_text: "Figure 1.jpg.md.md",
            },
            Reference {
                range: 3804..3820,
                dest: "Figure1#2.jpg",
                kind: WikiLink,
                display_text: "Figure1#2.jpg",
            },
            Reference {
                range: 3944..3960,
                dest: "Figure1",
                kind: WikiLink,
                display_text: "2.jpg",
            },
            Reference {
                range: 4084..4100,
                dest: "Figure1^2.jpg",
                kind: WikiLink,
                display_text: "Figure1^2.jpg",
            },
            Reference {
                range: 4218..4234,
                dest: "Figure 1.jpg",
                kind: WikiLink,
                display_text: "Figure 1.jpg",
            },
            Reference {
                range: 4260..4278,
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
                range: 3478..3498,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H2,
                range: 4281..4287,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 4288..4295,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H4,
                range: 4295..4303,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 4303..4318,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H2,
                range: 4323..4327,
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
                range: 2030..2034,
                dest: "#",
                kind: WikiLink,
                display_text: "#",
            },
            Reference {
                range: 2128..2152,
                dest: "#######Link to figure",
                kind: WikiLink,
                display_text: "#######Link to figure",
            },
            Reference {
                range: 2185..2208,
                dest: "######Link to figure",
                kind: WikiLink,
                display_text: "######Link to figure",
            },
            Reference {
                range: 2239..2260,
                dest: "####Link to figure",
                kind: WikiLink,
                display_text: "####Link to figure",
            },
            Reference {
                range: 2290..2310,
                dest: "###Link to figure",
                kind: WikiLink,
                display_text: "###Link to figure",
            },
            Reference {
                range: 2338..2356,
                dest: "#Link to figure",
                kind: WikiLink,
                display_text: "#Link to figure",
            },
            Reference {
                range: 2389..2401,
                dest: "#L2 ",
                kind: WikiLink,
                display_text: " #L4",
            },
            Reference {
                range: 2496..2513,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " 2 | 3",
            },
            Reference {
                range: 2614..2625,
                dest: "###L2#L4",
                kind: WikiLink,
                display_text: "###L2#L4",
            },
            Reference {
                range: 2685..2700,
                dest: "##L2######L4",
                kind: WikiLink,
                display_text: "##L2######L4",
            },
            Reference {
                range: 2759..2773,
                dest: "##L2#####L4",
                kind: WikiLink,
                display_text: "##L2#####L4",
            },
            Reference {
                range: 2835..2852,
                dest: "##L2#####L4#L3",
                kind: WikiLink,
                display_text: "##L2#####L4#L3",
            },
            Reference {
                range: 2908..2933,
                dest: "##L2#####L4#Another L3",
                kind: WikiLink,
                display_text: "##L2#####L4#Another L3",
            },
            Reference {
                range: 3274..3291,
                dest: "##L2######L4",
                kind: MarkdownLink,
                display_text: "1",
            },
            Reference {
                range: 3350..3366,
                dest: "##L2#####L4",
                kind: MarkdownLink,
                display_text: "2",
            },
            Reference {
                range: 3428..3447,
                dest: "##L2#####L4#L3",
                kind: MarkdownLink,
                display_text: "3",
            },
            Reference {
                range: 3520..3535,
                dest: "Figure 1.jpg",
                kind: WikiLink,
                display_text: "Figure 1.jpg",
            },
            Reference {
                range: 3647..3665,
                dest: "Figure 1.jpg.md",
                kind: WikiLink,
                display_text: "Figure 1.jpg.md",
            },
            Reference {
                range: 3758..3779,
                dest: "Figure 1.jpg.md.md",
                kind: WikiLink,
                display_text: "Figure 1.jpg.md.md",
            },
            Reference {
                range: 3804..3820,
                dest: "Figure1#2.jpg",
                kind: WikiLink,
                display_text: "Figure1#2.jpg",
            },
            Reference {
                range: 3944..3960,
                dest: "Figure1",
                kind: WikiLink,
                display_text: "2.jpg",
            },
            Reference {
                range: 4084..4100,
                dest: "Figure1^2.jpg",
                kind: WikiLink,
                display_text: "Figure1^2.jpg",
            },
            Reference {
                range: 4218..4234,
                dest: "Figure 1.jpg",
                kind: WikiLink,
                display_text: "Figure 1.jpg",
            },
            Reference {
                range: 4260..4278,
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
                range: 3478..3498,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H2,
                range: 4281..4287,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 4288..4295,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H4,
                range: 4295..4303,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H3,
                range: 4303..4318,
            },
            Heading {
                note_path: "tests/data/vaults/tt/Note 1.md",
                level: H2,
                range: 4323..4327,
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
        Document [0..4327]
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
          List(None) [944..3478]
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
            Item [2013..2070]
              Text(Borrowed("empty heading: ")) [2015..2030]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#"), title: Borrowed(""), id: Borrowed("") } [2030..2034]
                Text(Borrowed("#")) [2032..2033]
              List(None) [2035..2070]
                Item [2035..2070]
                  Code(Borrowed("[[#]]")) [2039..2046]
                  Text(Borrowed(" points to current note")) [2046..2069]
            Item [2070..2358]
              Text(Borrowed("incorrect heading level")) [2072..2095]
              List(None) [2095..2358]
                Item [2095..2154]
                  Code(Borrowed("[[#######Link to figure]]")) [2099..2126]
                  Text(Borrowed(": ")) [2126..2128]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#######Link to figure"), title: Borrowed(""), id: Borrowed("") } [2128..2152]
                    Text(Borrowed("#######Link to figure")) [2130..2151]
                Item [2153..2210]
                  Code(Borrowed("[[######Link to figure]]")) [2157..2183]
                  Text(Borrowed(": ")) [2183..2185]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("######Link to figure"), title: Borrowed(""), id: Borrowed("") } [2185..2208]
                    Text(Borrowed("######Link to figure")) [2187..2207]
                Item [2209..2262]
                  Code(Borrowed("[[####Link to figure]]")) [2213..2237]
                  Text(Borrowed(": ")) [2237..2239]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("####Link to figure"), title: Borrowed(""), id: Borrowed("") } [2239..2260]
                    Text(Borrowed("####Link to figure")) [2241..2259]
                Item [2261..2312]
                  Code(Borrowed("[[###Link to figure]]")) [2265..2288]
                  Text(Borrowed(": ")) [2288..2290]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("###Link to figure"), title: Borrowed(""), id: Borrowed("") } [2290..2310]
                    Text(Borrowed("###Link to figure")) [2292..2309]
                Item [2311..2358]
                  Code(Borrowed("[[#Link to figure]]")) [2315..2336]
                  Text(Borrowed(": ")) [2336..2338]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Link to figure"), title: Borrowed(""), id: Borrowed("") } [2338..2356]
                    Text(Borrowed("#Link to figure")) [2340..2355]
            Item [2358..2478]
              Text(Borrowed("ambiguous pipe and heading: ")) [2361..2389]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("#L2 "), title: Borrowed(""), id: Borrowed("") } [2389..2401]
                Text(Borrowed(" #L4")) [2396..2400]
              List(None) [2403..2478]
                Item [2403..2423]
                  Code(Borrowed("[[#L2 | #L4]]")) [2407..2422]
                Item [2423..2440]
                  Text(Borrowed("points to L2")) [2427..2439]
                Item [2440..2478]
                  Text(Borrowed("things after the pipe is escaped")) [2444..2476]
            Item [2478..2566]
              Text(Borrowed("multiple pipe: ")) [2481..2496]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Note 2 "), title: Borrowed(""), id: Borrowed("") } [2496..2513]
                Text(Borrowed(" 2 | 3")) [2506..2512]
              List(None) [2515..2566]
                Item [2515..2540]
                  Code(Borrowed("[[Note 2 | 2 | 3]]")) [2519..2539]
                Item [2540..2566]
                  Text(Borrowed("this points to Note 2")) [2544..2565]
            Item [2566..3059]
              Text(Borrowed("incorrect nested heading")) [2568..2592]
              List(None) [2593..3059]
                Item [2593..2662]
                  Code(Borrowed("[[###L2#L4]]")) [2597..2611]
                  Text(Borrowed(":  ")) [2611..2614]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("###L2#L4"), title: Borrowed(""), id: Borrowed("") } [2614..2625]
                    Text(Borrowed("###L2#L4")) [2616..2624]
                  List(None) [2627..2662]
                    Item [2627..2662]
                      Text(Borrowed("points to L4 heading correctly")) [2631..2661]
                Item [2661..2737]
                  Code(Borrowed("[[##L2######L4]]")) [2665..2683]
                  Text(Borrowed(": ")) [2683..2685]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2######L4"), title: Borrowed(""), id: Borrowed("") } [2685..2700]
                    Text(Borrowed("##L2######L4")) [2687..2699]
                  List(None) [2702..2737]
                    Item [2702..2737]
                      Text(Borrowed("points to L4 heading correctly")) [2706..2736]
                Item [2736..2810]
                  Code(Borrowed("[[##L2#####L4]]")) [2740..2757]
                  Text(Borrowed(": ")) [2757..2759]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4"), title: Borrowed(""), id: Borrowed("") } [2759..2773]
                    Text(Borrowed("##L2#####L4")) [2761..2772]
                  List(None) [2775..2810]
                    Item [2775..2810]
                      Text(Borrowed("points to L4 heading correctly")) [2779..2809]
                Item [2809..2883]
                  Code(Borrowed("[[##L2#####L4#L3]]")) [2813..2833]
                  Text(Borrowed(": ")) [2833..2835]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4#L3"), title: Borrowed(""), id: Borrowed("") } [2835..2852]
                    Text(Borrowed("##L2#####L4#L3")) [2837..2851]
                  List(None) [2854..2883]
                    Item [2854..2883]
                      Text(Borrowed("fallback to current note")) [2858..2882]
                Item [2882..2964]
                  Code(Borrowed("[[##L2#####L4#L3]]")) [2886..2906]
                  Text(Borrowed(": ")) [2906..2908]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("##L2#####L4#Another L3"), title: Borrowed(""), id: Borrowed("") } [2908..2933]
                    Text(Borrowed("##L2#####L4#Another L3")) [2910..2932]
                  List(None) [2935..2964]
                    Item [2935..2964]
                      Text(Borrowed("fallback to current note")) [2939..2963]
                Item [2963..3059]
                  Text(Borrowed("for displayed text, the first hash is removed, the subsequent nesting ones are not affected")) [2967..3058]
            Item [3059..3179]
              Text(Borrowed("↳ it looks like whenever there")) [3061..3093]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3093..3094]
              Text(Borrowed("s multiple hash, it")) [3094..3113]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3113..3114]
              Text(Borrowed("s all stripped. only the ancestor-descendant relationship matter")) [3114..3178]
            Item [3179..3478]
              Text(Borrowed("I don")) [3181..3186]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3186..3187]
              Text(Borrowed("t think there")) [3187..3200]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [3200..3201]
              Text(Borrowed("s a different between Wikilink and Markdown link")) [3201..3249]
              List(None) [3249..3478]
                Item [3249..3327]
                  Code(Borrowed("[1](##L2######L4)")) [3253..3272]
                  Text(Borrowed(": ")) [3272..3274]
                  Link { link_type: Inline, dest_url: Borrowed("##L2######L4"), title: Borrowed(""), id: Borrowed("") } [3274..3291]
                    Text(Borrowed("1")) [3275..3276]
                  List(None) [3292..3327]
                    Item [3292..3327]
                      Text(Borrowed("points to L4 heading correctly")) [3296..3326]
                Item [3326..3402]
                  Code(Borrowed("[2](##L2#####L4)")) [3330..3348]
                  Text(Borrowed(": ")) [3348..3350]
                  Link { link_type: Inline, dest_url: Borrowed("##L2#####L4"), title: Borrowed(""), id: Borrowed("") } [3350..3366]
                    Text(Borrowed("2")) [3351..3352]
                  List(None) [3367..3402]
                    Item [3367..3402]
                      Text(Borrowed("points to L4 heading correctly")) [3371..3401]
                Item [3401..3478]
                  Code(Borrowed("[3](##L2#####L4#L3)")) [3405..3426]
                  Text(Borrowed(": ")) [3426..3428]
                  Link { link_type: Inline, dest_url: Borrowed("##L2#####L4#L3"), title: Borrowed(""), id: Borrowed("") } [3428..3447]
                    Text(Borrowed("3")) [3429..3430]
                  List(None) [3448..3478]
                    Item [3448..3478]
                      Text(Borrowed("fallback to current note")) [3452..3476]
          Heading { level: H5, id: None, classes: [], attrs: [] } [3478..3498]
            Text(Borrowed("Link to asset")) [3484..3497]
          List(None) [3498..4197]
            Item [3498..3622]
              Code(Borrowed("[[Figure 1.jpg]]")) [3500..3518]
              Text(Borrowed(": ")) [3518..3520]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg"), title: Borrowed(""), id: Borrowed("") } [3520..3535]
                Text(Borrowed("Figure 1.jpg")) [3522..3534]
              List(None) [3536..3622]
                Item [3536..3622]
                  Text(Borrowed("even if there exists a note called ")) [3540..3575]
                  Code(Borrowed("Figure 1.jpg")) [3575..3589]
                  Text(Borrowed(", the asset will take precedence")) [3589..3621]
            Item [3622..3730]
              Code(Borrowed("[[Figure 1.jpg.md]]")) [3624..3645]
              Text(Borrowed(": ")) [3645..3647]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg.md"), title: Borrowed(""), id: Borrowed("") } [3647..3665]
                Text(Borrowed("Figure 1.jpg.md")) [3649..3664]
              List(None) [3666..3730]
                Item [3666..3730]
                  Text(Borrowed("with explicit ")) [3670..3684]
                  Code(Borrowed(".md")) [3684..3689]
                  Text(Borrowed(" ending, we seek for note ")) [3689..3715]
                  Code(Borrowed("Figure 1.jpg")) [3715..3729]
            Item [3730..3781]
              Code(Borrowed("[[Figure 1.jpg.md.md]]")) [3732..3756]
              Text(Borrowed(": ")) [3756..3758]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg.md.md"), title: Borrowed(""), id: Borrowed("") } [3758..3779]
                Text(Borrowed("Figure 1.jpg.md.md")) [3760..3778]
            Item [3781..3921]
              Code(Borrowed("[[Figure1#2.jpg]]")) [3783..3802]
              Text(Borrowed(": ")) [3802..3804]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1#2.jpg"), title: Borrowed(""), id: Borrowed("") } [3804..3820]
                Text(Borrowed("Figure1#2.jpg")) [3806..3819]
              List(None) [3821..3921]
                Item [3821..3921]
                  Text(Borrowed("understood as note and points to note Figure 1 (fallback to note after failing finding heading)")) [3825..3920]
            Item [3921..4061]
              Code(Borrowed("[[Figure1|2.jpg]]")) [3923..3942]
              Text(Borrowed(": ")) [3942..3944]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Figure1"), title: Borrowed(""), id: Borrowed("") } [3944..3960]
                Text(Borrowed("2.jpg")) [3954..3959]
              List(None) [3961..4061]
                Item [3961..4061]
                  Text(Borrowed("understood as note and points to note Figure 1 (fallback to note after failing finding heading)")) [3965..4060]
            Item [4061..4121]
              Code(Borrowed("[[Figure1^2.jpg]]")) [4063..4082]
              Text(Borrowed(": ")) [4082..4084]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure1^2.jpg"), title: Borrowed(""), id: Borrowed("") } [4084..4100]
                Text(Borrowed("Figure1^2.jpg")) [4086..4099]
              List(None) [4101..4121]
                Item [4101..4121]
                  Text(Borrowed("points to image")) [4105..4120]
            Item [4121..4197]
              Text(Borrowed("↳ when there")) [4123..4137]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [4137..4138]
              Text(Borrowed("s ")) [4138..4140]
              Code(Borrowed(".md")) [4140..4145]
              Text(Borrowed(", it")) [4145..4149]
              Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [4149..4150]
              Text(Borrowed("s removed and limit to the searching of notes")) [4150..4195]
          Paragraph [4197..4236]
            Code(Borrowed("![[Figure 1.jpg]]")) [4197..4216]
            Text(Borrowed(": ")) [4216..4218]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg"), title: Borrowed(""), id: Borrowed("") } [4218..4234]
              Text(Borrowed("Figure 1.jpg")) [4221..4233]
          Paragraph [4237..4280]
            Code(Borrowed("[[empty_video.mp4]]")) [4237..4258]
            Text(Borrowed(": ")) [4258..4260]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("empty_video.mp4"), title: Borrowed(""), id: Borrowed("") } [4260..4278]
              Text(Borrowed("empty_video.mp4")) [4262..4277]
          Heading { level: H2, id: None, classes: [], attrs: [] } [4281..4287]
            Text(Borrowed("L2")) [4284..4286]
          Heading { level: H3, id: None, classes: [], attrs: [] } [4288..4295]
            Text(Borrowed("L3")) [4292..4294]
          Heading { level: H4, id: None, classes: [], attrs: [] } [4295..4303]
            Text(Borrowed("L4")) [4300..4302]
          Heading { level: H3, id: None, classes: [], attrs: [] } [4303..4318]
            Text(Borrowed("Another L3")) [4307..4317]
          Rule [4319..4323]
          Heading { level: H2, id: None, classes: [], attrs: [] } [4323..4327]
        "########);
    }
}
