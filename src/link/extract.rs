use super::types::{Reference, ReferenceKind, Referenceable};
use super::utils::{is_block_identifier, percent_decode};
use crate::ast::{Node, NodeKind, Tree};
use crate::link::types::BlockReferenceableKind;
use pulldown_cmark::LinkType;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};

// Scan a note and return a tuple of references and in-note referenceables
//
// Post-condition: the in-note referenceables are in order
pub fn scan_note(path: &PathBuf) -> (Vec<Reference>, Vec<Referenceable>) {
    let text = fs::read_to_string(path).unwrap();
    if text.is_empty() {
        return (Vec::new(), Vec::new());
    }

    let tree = Tree::new(&text);

    let root_children = &tree.root_node.children;

    extract_reference_and_referenceable(root_children, path)

    // let mut references = Vec::new();
    // let mut referenceables = Vec::new();
    // extract_reference_and_referenceable(
    //     &tree.root_node,
    //     path,
    //     &mut references,
    //     &mut referenceables,
    // );
    //
    // (references, referenceables)
}

/// Extracts all references and referenceables from a list of node.
///
/// We operates on a list of node because we need to get previous node
/// when we encounter a block reference.

struct BlockIdentifier {
    identifier: String,
    range: Range<usize>,
}

enum NodeParsedResult {
    Refernce(Reference),
    Referenceable(Referenceable),
    BlockIdentifier(BlockIdentifier),
}

/// Maybe we should return a boolean value indicating whether
/// the node analysis can be ended?
fn extract_node_reference_referenceable_and_identifier(
    node: &Node,
    path: &PathBuf,
) -> Option<NodeParsedResult> {
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
                        path: path.clone(),
                        range: node.byte_range().clone(),
                        dest: dest_url.trim().to_string(),
                        kind: ReferenceKind::WikiLink,
                        display_text,
                    };
                    Some(NodeParsedResult::Refernce(reference))
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
                        path: path.clone(),
                        range: node.byte_range().clone(),
                        dest: dest.to_string(),
                        kind: ReferenceKind::MarkdownLink,
                        display_text,
                    };
                    Some(NodeParsedResult::Refernce(reference))
                }
                _ => None,
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
            Some(NodeParsedResult::Referenceable(referenceable))
        }
        NodeKind::List { .. } => {
            // TODO: Block. Not implemented.
            None
        }
        NodeKind::Paragraph => {
            // If the paragraph has two text nodes
            // with the first one being `^`, and the second one being a
            // valid block identifier, then it's a block reference.
            if node.children.len() == 2 {
                let fst = &node.children[0];
                let snd = &node.children[1];
                if let NodeKind::Text(text) = &fst.kind {
                    if text.as_ref() == "^" {
                        if let NodeKind::Text(text) = &snd.kind {
                            if is_block_identifier(text.as_ref()) {
                                return Some(
                                    NodeParsedResult::BlockIdentifier(
                                        BlockIdentifier {
                                            identifier: text
                                                .as_ref()
                                                .to_string(),
                                            range: (fst.byte_range().start)
                                                ..(snd.byte_range().end),
                                        },
                                    ),
                                );
                            }
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

fn extract_reference_and_referenceable(
    nodes: &[Node],
    path: &PathBuf,
) -> (Vec<Reference>, Vec<Referenceable>) {
    let mut references = Vec::<Reference>::new();
    let mut referenceables = Vec::<Referenceable>::new();

    // iterate over nodes
    for (i, node) in nodes.iter().enumerate() {
        // Parse current node
        if let Some(result) =
            extract_node_reference_referenceable_and_identifier(node, path)
        {
            match result {
                NodeParsedResult::Refernce(reference) => {
                    references.push(reference);
                }
                NodeParsedResult::Referenceable(referenceable) => {
                    referenceables.push(referenceable);
                }
                NodeParsedResult::BlockIdentifier(block_identifier) => {
                    // if it's a block idenfier, its previous node could be a block referenceable
                    if i > 0 {
                        let previous_node = &nodes[i - 1];
                        let block_kind = match &previous_node.kind {
                            NodeKind::Paragraph => {
                                Some(BlockReferenceableKind::Paragraph)
                            }
                            NodeKind::List(..) => {
                                Some(BlockReferenceableKind::List)
                            }
                            NodeKind::Table(..) => {
                                Some(BlockReferenceableKind::Table)
                            }
                            NodeKind::BlockQuote(..) => {
                                Some(BlockReferenceableKind::BlockQuote)
                            }
                            _ => None,
                        };
                        if let Some(block_kind_val) = block_kind {
                            let block_referenceable = Referenceable::Block {
                                path: path.clone(),
                                identifier: block_identifier.identifier,
                                kind: block_kind_val,
                                range: previous_node.byte_range().clone(),
                            };
                            referenceables.push(block_referenceable);
                        }
                    }
                }
            }
        }

        // Parse its children
        let (child_refs, child_refables) =
            extract_reference_and_referenceable(&node.children, path);
        references.extend(child_refs);
        referenceables.extend(child_refables);
    }

    (references, referenceables)
}

fn extract_reference_and_referenceable_(
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
                        path: path.clone(),
                        range: node.byte_range().clone(),
                        dest: dest_url.trim().to_string(),
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
                        path: path.clone(),
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
        NodeKind::Paragraph => {
            // If the paragraph ends with two text nodes
            // with the first one being `^`, and the second one being a
            // valid block identifier, then it's a block reference.
        }
        _ => {}
    }

    for child in node.children.iter() {
        extract_reference_and_referenceable_(
            child,
            path,
            references,
            referenceables,
        );
    }
}

const IGNORED: &[&str] = &[".obsidian", ".DS_Store", ".git"];

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
                    Some("md") | Some("markdown") => Referenceable::Note {
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
    let mut referenceables = Vec::<Referenceable>::new();
    aux(dir, &mut referenceables, IGNORED);
    referenceables
}

/// Convert an absolute path to a path relative to root_dir (in-place).
/// If the path is not under root_dir, leaves it unchanged.
fn make_path_relative(path: &mut PathBuf, root_dir: &Path) {
    if let Ok(relative) = path.strip_prefix(root_dir) {
        *path = relative.to_path_buf();
    }
}

/// Recursively convert all paths in a Referenceable to be relative to root_dir (in-place).
fn make_referenceable_relative(
    referenceable: &mut Referenceable,
    root_dir: &Path,
) {
    match referenceable {
        Referenceable::Asset { path } => {
            make_path_relative(path, root_dir);
        }
        Referenceable::Note { path, children } => {
            make_path_relative(path, root_dir);
            for child in children.iter_mut() {
                make_referenceable_relative(child, root_dir);
            }
        }
        Referenceable::Heading { path, .. } => {
            make_path_relative(path, root_dir);
        }
        Referenceable::Block { path, .. } => {
            make_path_relative(path, root_dir);
        }
    }
}

/// Scan a vault for referenceables and references.
///
/// in-note referenceables are stored in note's children
///
/// Arguments:
/// - `dir`: the directory to scan
/// - `root_dir`: the root directory - all paths will be made relative to this
///
/// Returns:
/// - referenceables: all referenceables (with relative paths)
/// - references: note references and asset references
pub fn scan_vault(
    dir: &Path,
    root_dir: &Path,
) -> (Vec<Referenceable>, Vec<Reference>) {
    let file_referenceables = scan_dir_for_assets_and_notes(dir);
    let mut all_references = Vec::<Reference>::new();

    let file_referenceables_with_children: Vec<Referenceable> =
        file_referenceables
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

    // Convert all paths to be relative to root_dir
    let mut relative_referenceables = file_referenceables_with_children;
    for referenceable in relative_referenceables.iter_mut() {
        make_referenceable_relative(referenceable, root_dir);
    }

    // Convert reference paths to be relative to root_dir
    for reference in all_references.iter_mut() {
        make_path_relative(&mut reference.path, root_dir);
    }

    (relative_referenceables, all_references)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::{assert_debug_snapshot, assert_snapshot};

    #[test]
    fn test_exract_references_and_referenceables() {
        let path = PathBuf::from("tests/data/vaults/tt/block.md");
        let (references, referenceables): (Vec<Reference>, Vec<Referenceable>) =
            scan_note(&path);
        assert_debug_snapshot!(references, @r##"
        [
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 210..224,
                dest: "#^quotation",
                kind: WikiLink,
                display_text: "#^quotation",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 272..284,
                dest: "#^callout",
                kind: WikiLink,
                display_text: "#^callout",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 316..331,
                dest: "#^paragraph1",
                kind: WikiLink,
                display_text: "#^paragraph1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 356..371,
                dest: "#^paragraph2",
                kind: WikiLink,
                display_text: "#^paragraph2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 463..473,
                dest: "#^table",
                kind: WikiLink,
                display_text: "#^table",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 485..496,
                dest: "#^table2",
                kind: WikiLink,
                display_text: "#^table2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 508..523,
                dest: "#^tableagain",
                kind: WikiLink,
                display_text: "#^tableagain",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 700..714,
                dest: "#^firstline",
                kind: WikiLink,
                display_text: "#^firstline",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 746..760,
                dest: "#^inneritem",
                kind: WikiLink,
                display_text: "#^inneritem",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 980..993,
                dest: "#^tableref",
                kind: WikiLink,
                display_text: "#^tableref",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1150..1164,
                dest: "#^tableref3",
                kind: WikiLink,
                display_text: "#^tableref3",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1168..1182,
                dest: "#^tableref2",
                kind: WikiLink,
                display_text: "#^tableref2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1324..1334,
                dest: "#^works",
                kind: WikiLink,
                display_text: "#^works",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1754..1768,
                dest: "#^firstline",
                kind: WikiLink,
                display_text: "#^firstline",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1800..1814,
                dest: "#^inneritem",
                kind: WikiLink,
                display_text: "#^inneritem",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1932..1947,
                dest: "#^firstline1",
                kind: WikiLink,
                display_text: "#^firstline1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 2000..2015,
                dest: "#^inneritem1",
                kind: WikiLink,
                display_text: "#^inneritem1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 2067..2080,
                dest: "#^fulllst1",
                kind: WikiLink,
                display_text: "#^fulllst1",
            },
        ]
        "##);
        assert_debug_snapshot!(referenceables, @r#"
        [
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "paragraph2",
                kind: Paragraph,
                range: 24..36,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "fulllist",
                kind: List,
                range: 52..85,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "table",
                kind: Table,
                range: 96..176,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "quotation",
                kind: BlockQuote,
                range: 185..197,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "callout",
                kind: BlockQuote,
                range: 228..261,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: a later block identifier invalidate previous one",
                range: 801..869,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "tableref",
                kind: Table,
                range: 870..950,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "tableref2",
                kind: Table,
                range: 997..1077,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "tableref3",
                kind: Paragraph,
                range: 1078..1089,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: the number of blank lines before identifier doesn’t matter",
                range: 1232..1310,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: full reference to a list make its inner state not refereceable",
                range: 1602..1684,
            },
        ]
        "#);
    }

    #[test]
    fn test_parse_ast_with_links() {
        let path = "tests/data/vaults/tt/block.md";
        let text = fs::read_to_string(path).unwrap();
        let tree = Tree::new(&text);
        assert_snapshot!(tree.root_node, @r##"
        Document [0..2107]
          Paragraph [0..23]
            Text(Borrowed("paragraph 1 ")) [0..12]
            Text(Borrowed("^")) [12..13]
            Text(Borrowed("paragraph")) [13..22]
          Paragraph [24..36]
            Text(Borrowed("paragraph 2")) [24..35]
          Paragraph [37..49]
            Text(Borrowed("^")) [37..38]
            Text(Borrowed("paragraph2")) [38..48]
          List(None) [52..85]
            Item [52..85]
              Text(Borrowed("some list")) [54..63]
              List(None) [63..85]
                Item [63..74]
                  Text(Borrowed("item 1")) [67..73]
                Item [73..85]
                  Text(Borrowed("item 2")) [77..83]
          Paragraph [85..95]
            Text(Borrowed("^")) [85..86]
            Text(Borrowed("fulllist")) [86..94]
          Table([None, None]) [96..176]
            TableHead [96..116]
              TableCell [97..105]
                Text(Borrowed("Col 1")) [98..103]
              TableCell [106..114]
                Text(Borrowed("Col 2")) [107..112]
            TableRow [136..156]
              TableCell [137..145]
                Text(Borrowed("Cell 1")) [138..144]
              TableCell [146..154]
                Text(Borrowed("Cell 2")) [147..153]
            TableRow [156..176]
              TableCell [157..165]
                Text(Borrowed("Cell 3")) [158..164]
              TableCell [166..174]
                Text(Borrowed("Cell 4")) [167..173]
          Paragraph [177..184]
            Text(Borrowed("^")) [177..178]
            Text(Borrowed("table")) [178..183]
          BlockQuote(None) [185..197]
            Paragraph [187..197]
              Text(Borrowed("quotation")) [187..196]
          Paragraph [198..209]
            Text(Borrowed("^")) [198..199]
            Text(Borrowed("quotation")) [199..208]
          Paragraph [210..226]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^quotation"), title: Borrowed(""), id: Borrowed("") } [210..224]
              Text(Borrowed("#^quotation")) [212..223]
          BlockQuote(None) [228..261]
            Paragraph [230..261]
              Text(Borrowed("[")) [230..231]
              Text(Borrowed("!info")) [231..236]
              Text(Borrowed("]")) [236..237]
              Text(Borrowed(" this is a info callout")) [237..260]
          Paragraph [262..271]
            Text(Borrowed("^")) [262..263]
            Text(Borrowed("callout")) [263..270]
          Paragraph [272..286]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^callout"), title: Borrowed(""), id: Borrowed("") } [272..284]
              Text(Borrowed("#^callout")) [274..283]
          Rule [288..292]
          Paragraph [293..303]
            Text(Borrowed("reference")) [293..302]
          List(None) [303..626]
            Item [303..333]
              Text(Borrowed("paragraph: ")) [305..316]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^paragraph1"), title: Borrowed(""), id: Borrowed("") } [316..331]
                Text(Borrowed("#^paragraph1")) [318..330]
            Item [333..454]
              Text(Borrowed("separate line caret: ")) [335..356]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^paragraph2"), title: Borrowed(""), id: Borrowed("") } [356..371]
                Text(Borrowed("#^paragraph2")) [358..370]
              List(None) [372..454]
                Item [372..454]
                  Text(Borrowed("for paragraph the caret doesn")) [376..405]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [405..406]
                  Text(Borrowed("t need to have a blank line before and after it")) [406..453]
            Item [454..476]
              Text(Borrowed("table: ")) [456..463]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^table"), title: Borrowed(""), id: Borrowed("") } [463..473]
                Text(Borrowed("#^table")) [465..472]
            Item [476..499]
              Text(Borrowed("table: ")) [478..485]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^table2"), title: Borrowed(""), id: Borrowed("") } [485..496]
                Text(Borrowed("#^table2")) [487..495]
            Item [499..626]
              Text(Borrowed("table: ")) [501..508]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableagain"), title: Borrowed(""), id: Borrowed("") } [508..523]
                Text(Borrowed("#^tableagain")) [510..522]
              List(None) [524..626]
                Item [524..626]
                  Text(Borrowed("it looks like the block reference will always point to last non-empty non-block-reference struct")) [528..624]
          Rule [626..630]
          List(None) [631..797]
            Item [631..681]
              Text(Borrowed("a nested list ")) [633..647]
              Text(Borrowed("^")) [647..648]
              Text(Borrowed("firstline")) [648..657]
              List(None) [657..681]
                Item [657..681]
                  Text(Borrowed("item")) [662..666]
                  SoftBreak [666..667]
                  Text(Borrowed("^")) [670..671]
                  Text(Borrowed("inneritem")) [671..680]
            Item [681..797]
              Text(Borrowed("inside a list")) [683..696]
              List(None) [696..797]
                Item [696..742]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline"), title: Borrowed(""), id: Borrowed("") } [700..714]
                    Text(Borrowed("#^firstline")) [702..713]
                  Text(Borrowed(": points to the first line")) [715..741]
                Item [741..797]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem"), title: Borrowed(""), id: Borrowed("") } [746..760]
                    Text(Borrowed("#^inneritem")) [748..759]
                  Text(Borrowed(": points to the first inner item")) [761..793]
          Rule [797..801]
          Heading { level: H6, id: None, classes: [], attrs: [] } [801..869]
            Text(Borrowed("Edge case: a later block identifier invalidate previous one")) [809..868]
          Table([None, None]) [870..950]
            TableHead [870..890]
              TableCell [871..879]
                Text(Borrowed("Col 1")) [872..877]
              TableCell [880..888]
                Text(Borrowed("Col 2")) [881..886]
            TableRow [910..930]
              TableCell [911..919]
                Text(Borrowed("Cell 1")) [912..918]
              TableCell [920..928]
                Text(Borrowed("Cell 2")) [921..927]
            TableRow [930..950]
              TableCell [931..939]
                Text(Borrowed("Cell 3")) [932..938]
              TableCell [940..948]
                Text(Borrowed("Cell 4")) [941..947]
          Paragraph [951..961]
            Text(Borrowed("^")) [951..952]
            Text(Borrowed("tableref")) [952..960]
          List(None) [962..997]
            Item [962..997]
              Text(Borrowed("this works fine ")) [964..980]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref"), title: Borrowed(""), id: Borrowed("") } [980..993]
                Text(Borrowed("#^tableref")) [982..992]
          Table([None, None]) [997..1077]
            TableHead [997..1017]
              TableCell [998..1006]
                Text(Borrowed("Col 1")) [999..1004]
              TableCell [1007..1015]
                Text(Borrowed("Col 2")) [1008..1013]
            TableRow [1037..1057]
              TableCell [1038..1046]
                Text(Borrowed("Cell 1")) [1039..1045]
              TableCell [1047..1055]
                Text(Borrowed("Cell 2")) [1048..1054]
            TableRow [1057..1077]
              TableCell [1058..1066]
                Text(Borrowed("Cell 3")) [1059..1065]
              TableCell [1067..1075]
                Text(Borrowed("Cell 4")) [1068..1074]
          Paragraph [1078..1089]
            Text(Borrowed("^")) [1078..1079]
            Text(Borrowed("tableref2")) [1079..1088]
          Paragraph [1090..1101]
            Text(Borrowed("^")) [1090..1091]
            Text(Borrowed("tableref3")) [1091..1100]
          List(None) [1102..1232]
            Item [1102..1166]
              Text(Borrowed("now the above table can only be referenced by ")) [1104..1150]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref3"), title: Borrowed(""), id: Borrowed("") } [1150..1164]
                Text(Borrowed("#^tableref3")) [1152..1163]
            Item [1166..1232]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref2"), title: Borrowed(""), id: Borrowed("") } [1168..1182]
                Text(Borrowed("#^tableref2")) [1170..1181]
              Text(Borrowed(" is invalid and will fallback to the whole note")) [1183..1230]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1232..1310]
            Text(Borrowed("Edge case: the number of blank lines before identifier doesn")) [1240..1300]
            Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1300..1301]
            Text(Borrowed("t matter")) [1301..1309]
          Paragraph [1311..1323]
            Text(Borrowed("this")) [1311..1315]
            SoftBreak [1315..1316]
            Text(Borrowed("^")) [1316..1317]
            Text(Borrowed("works")) [1317..1322]
          Paragraph [1324..1336]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^works"), title: Borrowed(""), id: Borrowed("") } [1324..1334]
              Text(Borrowed("#^works")) [1326..1333]
          List(None) [1337..1602]
            Item [1337..1385]
              Text(Borrowed("1 blank line after the identifier is required")) [1339..1384]
            Item [1385..1602]
              Text(Borrowed("however, 0-n blank line before the identifier works fine")) [1387..1443]
              List(None) [1443..1602]
                Item [1443..1602]
                  Text(Borrowed("for clarity, we should always require at least 1 blank line before the identifier (so that the identifier won")) [1447..1556]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1556..1557]
                  Text(Borrowed("t be parsed as part of the previous struct)")) [1557..1600]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1602..1684]
            Text(Borrowed("Edge case: full reference to a list make its inner state not refereceable")) [1610..1683]
          List(None) [1685..2107]
            Item [1685..1735]
              Paragraph [1687..1712]
                Text(Borrowed("a nested list ")) [1687..1701]
                Text(Borrowed("^")) [1701..1702]
                Text(Borrowed("firstline")) [1702..1711]
              List(None) [1711..1735]
                Item [1711..1735]
                  Text(Borrowed("item")) [1716..1720]
                  SoftBreak [1720..1721]
                  Text(Borrowed("^")) [1724..1725]
                  Text(Borrowed("inneritem")) [1725..1734]
            Item [1735..1849]
              Paragraph [1737..1751]
                Text(Borrowed("inside a list")) [1737..1750]
              List(None) [1750..1849]
                Item [1750..1796]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline"), title: Borrowed(""), id: Borrowed("") } [1754..1768]
                    Text(Borrowed("#^firstline")) [1756..1767]
                  Text(Borrowed(": points to the first line")) [1769..1795]
                Item [1795..1849]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem"), title: Borrowed(""), id: Borrowed("") } [1800..1814]
                    Text(Borrowed("#^inneritem")) [1802..1813]
                  Text(Borrowed(": points to the first inner item")) [1815..1847]
            Item [1849..1913]
              Paragraph [1851..1877]
                Text(Borrowed("a nested list ")) [1851..1865]
                Text(Borrowed("^")) [1865..1866]
                Text(Borrowed("firstline1")) [1866..1876]
              List(None) [1876..1913]
                Item [1876..1913]
                  Text(Borrowed("item")) [1881..1885]
                  SoftBreak [1885..1886]
                  Text(Borrowed("^")) [1889..1890]
                  Text(Borrowed("inneritem1")) [1890..1900]
                  SoftBreak [1900..1901]
                  Text(Borrowed("^")) [1901..1902]
                  Text(Borrowed("fulllist1")) [1902..1911]
            Item [1913..2107]
              Paragraph [1915..1929]
                Text(Borrowed("inside a list")) [1915..1928]
              List(None) [1928..2107]
                Item [1928..1996]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline1"), title: Borrowed(""), id: Borrowed("") } [1932..1947]
                    Text(Borrowed("#^firstline1")) [1934..1946]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [1948..1995]
                Item [1995..2064]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem1"), title: Borrowed(""), id: Borrowed("") } [2000..2015]
                    Text(Borrowed("#^inneritem1")) [2002..2014]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [2016..2063]
                Item [2063..2107]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^fulllst1"), title: Borrowed(""), id: Borrowed("") } [2067..2080]
                    Text(Borrowed("#^fulllst1")) [2069..2079]
                  Text(Borrowed(": points to the full list")) [2081..2106]
        "##);
    }
}
