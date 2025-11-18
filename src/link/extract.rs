use super::types::{Reference, ReferenceKind, Referenceable};
use super::utils::{is_block_identifier, percent_decode};
use crate::ast::{Node, NodeKind, Tree};
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

    let mut references = Vec::new();
    let mut referenceables = Vec::new();

    let root_children = &tree.root_node.children;
    extract_reference_and_referenceable(
        &tree.root_node,
        path,
        &mut references,
        &mut referenceables,
    );

    (references, referenceables)
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
            todo!()
        }
        _ => None,
    }
}

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
        extract_reference_and_referenceable(
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
                range: 701..715,
                dest: "#^firstline",
                kind: WikiLink,
                display_text: "#^firstline",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 747..761,
                dest: "#^inneritem",
                kind: WikiLink,
                display_text: "#^inneritem",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 946..961,
                dest: "#^firstline1",
                kind: WikiLink,
                display_text: "#^firstline1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1014..1029,
                dest: "#^inneritem1",
                kind: WikiLink,
                display_text: "#^inneritem1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1081..1094,
                dest: "#^fulllst1",
                kind: WikiLink,
                display_text: "#^fulllst1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1306..1319,
                dest: "#^tableref",
                kind: WikiLink,
                display_text: "#^tableref",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1476..1490,
                dest: "#^tableref3",
                kind: WikiLink,
                display_text: "#^tableref3",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1494..1508,
                dest: "#^tableref2",
                kind: WikiLink,
                display_text: "#^tableref2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1650..1660,
                dest: "#^works",
                kind: WikiLink,
                display_text: "#^works",
            },
        ]
        "##);
        assert_debug_snapshot!(referenceables, @r#"
        [
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: a later block identifier invalidate previous one",
                range: 1127..1195,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: the number of blank lines before identifier doesn’t matter",
                range: 1558..1636,
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
        Document [0..1927]
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
          List(None) [631..1123]
            Item [631..682]
              Paragraph [633..658]
                Text(Borrowed("a nested list ")) [633..647]
                Text(Borrowed("^")) [647..648]
                Text(Borrowed("firstline")) [648..657]
              List(None) [657..682]
                Item [657..682]
                  Text(Borrowed("item")) [662..666]
                  SoftBreak [666..667]
                  Text(Borrowed("^")) [670..671]
                  Text(Borrowed("inneritem")) [671..680]
            Item [682..796]
              Paragraph [684..698]
                Text(Borrowed("inside a list")) [684..697]
              List(None) [697..796]
                Item [697..743]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline"), title: Borrowed(""), id: Borrowed("") } [701..715]
                    Text(Borrowed("#^firstline")) [703..714]
                  Text(Borrowed(": points to the first line")) [716..742]
                Item [742..796]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem"), title: Borrowed(""), id: Borrowed("") } [747..761]
                    Text(Borrowed("#^inneritem")) [749..760]
                  Text(Borrowed(": points to the first inner item")) [762..794]
            Item [796..863]
              Paragraph [798..861]
                Text(Borrowed("full reference to a list make its inner state not refereceable")) [798..860]
            Item [863..927]
              Paragraph [865..891]
                Text(Borrowed("a nested list ")) [865..879]
                Text(Borrowed("^")) [879..880]
                Text(Borrowed("firstline1")) [880..890]
              List(None) [890..927]
                Item [890..927]
                  Text(Borrowed("item")) [895..899]
                  SoftBreak [899..900]
                  Text(Borrowed("^")) [903..904]
                  Text(Borrowed("inneritem1")) [904..914]
                  SoftBreak [914..915]
                  Text(Borrowed("^")) [915..916]
                  Text(Borrowed("fulllist1")) [916..925]
            Item [927..1123]
              Paragraph [929..943]
                Text(Borrowed("inside a list")) [929..942]
              List(None) [942..1123]
                Item [942..1010]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline1"), title: Borrowed(""), id: Borrowed("") } [946..961]
                    Text(Borrowed("#^firstline1")) [948..960]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [962..1009]
                Item [1009..1078]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem1"), title: Borrowed(""), id: Borrowed("") } [1014..1029]
                    Text(Borrowed("#^inneritem1")) [1016..1028]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [1030..1077]
                Item [1077..1123]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^fulllst1"), title: Borrowed(""), id: Borrowed("") } [1081..1094]
                    Text(Borrowed("#^fulllst1")) [1083..1093]
                  Text(Borrowed(": points to the full list")) [1095..1120]
          Rule [1123..1127]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1127..1195]
            Text(Borrowed("Edge case: a later block identifier invalidate previous one")) [1135..1194]
          Table([None, None]) [1196..1276]
            TableHead [1196..1216]
              TableCell [1197..1205]
                Text(Borrowed("Col 1")) [1198..1203]
              TableCell [1206..1214]
                Text(Borrowed("Col 2")) [1207..1212]
            TableRow [1236..1256]
              TableCell [1237..1245]
                Text(Borrowed("Cell 1")) [1238..1244]
              TableCell [1246..1254]
                Text(Borrowed("Cell 2")) [1247..1253]
            TableRow [1256..1276]
              TableCell [1257..1265]
                Text(Borrowed("Cell 3")) [1258..1264]
              TableCell [1266..1274]
                Text(Borrowed("Cell 4")) [1267..1273]
          Paragraph [1277..1287]
            Text(Borrowed("^")) [1277..1278]
            Text(Borrowed("tableref")) [1278..1286]
          List(None) [1288..1323]
            Item [1288..1323]
              Text(Borrowed("this works fine ")) [1290..1306]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref"), title: Borrowed(""), id: Borrowed("") } [1306..1319]
                Text(Borrowed("#^tableref")) [1308..1318]
          Table([None, None]) [1323..1403]
            TableHead [1323..1343]
              TableCell [1324..1332]
                Text(Borrowed("Col 1")) [1325..1330]
              TableCell [1333..1341]
                Text(Borrowed("Col 2")) [1334..1339]
            TableRow [1363..1383]
              TableCell [1364..1372]
                Text(Borrowed("Cell 1")) [1365..1371]
              TableCell [1373..1381]
                Text(Borrowed("Cell 2")) [1374..1380]
            TableRow [1383..1403]
              TableCell [1384..1392]
                Text(Borrowed("Cell 3")) [1385..1391]
              TableCell [1393..1401]
                Text(Borrowed("Cell 4")) [1394..1400]
          Paragraph [1404..1415]
            Text(Borrowed("^")) [1404..1405]
            Text(Borrowed("tableref2")) [1405..1414]
          Paragraph [1416..1427]
            Text(Borrowed("^")) [1416..1417]
            Text(Borrowed("tableref3")) [1417..1426]
          List(None) [1428..1558]
            Item [1428..1492]
              Text(Borrowed("now the above table can only be referenced by ")) [1430..1476]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref3"), title: Borrowed(""), id: Borrowed("") } [1476..1490]
                Text(Borrowed("#^tableref3")) [1478..1489]
            Item [1492..1558]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref2"), title: Borrowed(""), id: Borrowed("") } [1494..1508]
                Text(Borrowed("#^tableref2")) [1496..1507]
              Text(Borrowed(" is invalid and will fallback to the whole note")) [1509..1556]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1558..1636]
            Text(Borrowed("Edge case: the number of blank lines before identifier doesn")) [1566..1626]
            Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1626..1627]
            Text(Borrowed("t matter")) [1627..1635]
          Paragraph [1637..1649]
            Text(Borrowed("this")) [1637..1641]
            SoftBreak [1641..1642]
            Text(Borrowed("^")) [1642..1643]
            Text(Borrowed("works")) [1643..1648]
          Paragraph [1650..1662]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^works"), title: Borrowed(""), id: Borrowed("") } [1650..1660]
              Text(Borrowed("#^works")) [1652..1659]
          List(None) [1663..1927]
            Item [1663..1711]
              Text(Borrowed("1 blank line after the identifier is required")) [1665..1710]
            Item [1711..1927]
              Text(Borrowed("however, 0-n blank line before the identifier works fine")) [1713..1769]
              List(None) [1769..1927]
                Item [1769..1927]
                  Text(Borrowed("for clarity, we should always require at least 1 blank line before the identifier (so that the identifier won")) [1773..1882]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1882..1883]
                  Text(Borrowed("t be parsed as part of the previous struct)")) [1883..1926]
        "##);
    }
}
