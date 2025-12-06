use super::types::{Reference, ReferenceKind, Referenceable};
use super::utils::{is_block_identifier, percent_decode};
use crate::ast::{Node, NodeKind, Tree};
use crate::link::types::BlockReferenceableKind;
use pulldown_cmark::LinkType;
use serde_yaml;
use serde_yaml::Value as Y;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};

/// Scan a vault for referenceables and references.
///
/// in-note referenceables are stored in note's children
///
/// Arguments:
/// - `dir`: the directory to scan
/// - `root_dir`: the root directory - all paths will be made relative to this
///
/// Returns:
/// - referenceables: note and asset referenceables
/// - references: references
///
/// Post-condition:
///   - all referenceables are not in-note referenceables
///   - frontmatters match file referenceables
pub fn scan_vault(
    dir: &Path,
    root_dir: &Path,
    filter_publish: bool,
) -> (Vec<Option<Y>>, Vec<Referenceable>, Vec<Reference>) {
    let file_referenceables = scan_dir_for_assets_and_notes(dir);

    // Collect
    let (
        fm_by_file,
        mut referenceables_with_children_by_file,
        references_by_file,
    ): (Vec<Option<Y>>, Vec<Referenceable>, Vec<Vec<Reference>>) =
        file_referenceables
            .into_iter()
            .filter_map(|mut file_refable| match file_refable {
                Referenceable::Note { ref path, .. } => {
                    let (fm, references, innote_refables) = scan_note(path);
                    let publish = if !filter_publish {
                        true
                    } else {
                        fm.as_ref()
                            .and_then(|fm_val| fm_val.as_mapping())
                            .and_then(|fm_val| fm_val.get("publish"))
                            .and_then(|publish| publish.as_bool())
                            .unwrap_or(false)
                    };
                    if !publish {
                        None
                    } else {
                        file_refable
                            .add_in_note_referenceables(innote_refables);
                        Some((fm, file_refable, references))
                    }
                }
                asset @ Referenceable::Asset { .. } => {
                    Some((None, asset, Vec::new()))
                }
                other => unreachable!(
                    "in-note referenceable shouldn't present here, got {:?}",
                    other
                ),
            })
            .collect();

    // TODO(maybe): we don't flatten references and just return a Vec of Vec?
    let mut references =
        references_by_file.into_iter().flatten().collect::<Vec<_>>();

    // Make all referenceables relative to the root directory
    referenceables_with_children_by_file
        .iter_mut()
        .for_each(|referenceable| {
            make_referenceable_relative(referenceable, root_dir);
        });
    // Make all references relative to the root directory as well
    references.iter_mut().for_each(|reference| {
        if let Ok(relative) = reference.path.strip_prefix(root_dir) {
            reference.path = relative.to_path_buf();
        }
    });

    (fm_by_file, referenceables_with_children_by_file, references)
}

// Scan a note and return a tuple of references and in-note referenceables
//
// Post-condition: the in-note referenceables are in order
pub fn scan_note(
    path: &PathBuf,
) -> (Option<Y>, Vec<Reference>, Vec<Referenceable>) {
    let text = fs::read_to_string(path).unwrap();
    if text.is_empty() {
        return (None, Vec::new(), Vec::new());
    }

    let tree = Tree::new(&text);

    // Frontmatter
    let fm_node = tree.root_node.children.first();
    let fm_content = if let Some(fm_node) = fm_node {
        if fm_node.kind
            == NodeKind::MetadataBlock(
                pulldown_cmark::MetadataBlockKind::YamlStyle,
            )
        {
            let content: String = fm_node
                .children
                .iter()
                .map(|child| match &child.kind {
                    NodeKind::Text(text) => text.as_ref(),
                    _ => unreachable!(
                        "Never: Yaml style should only have text children"
                    ),
                })
                .collect();
            let fm: Option<Y> = serde_yaml::from_str(&content).unwrap_or(None);
            fm
        } else {
            None
        }
    } else {
        None
    };

    // Extra link src and tgt
    let root_children = &tree.root_node.children;
    let (references, referenceables) =
        extract_reference_and_referenceable(root_children, path);

    (fm_content, references, referenceables)
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
            // Note: anything looks like `![[]]` will considered as `Image` event
            let is_embed = matches!(&node.kind, NodeKind::Image { .. });
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
                    let kind = if !is_embed {
                        ReferenceKind::WikiLink
                    } else {
                        ReferenceKind::Embed
                    };
                    let reference = Reference {
                        path: path.clone(),
                        range: node.byte_range().clone(),
                        dest: dest_url.trim().to_string(),
                        kind,
                        display_text,
                    };
                    Some(NodeParsedResult::Refernce(reference))
                }
                LinkType::Inline => {
                    // Markdown link like `[text](Note.md)`
                    // The destination in parentheses is percent-encoded
                    // Decode the destination URL. Eg, from `Note%201` to `Note 1`
                    let mut dest = percent_decode(dest_url);
                    // `[text]()` points to file `().md`
                    if dest.is_empty() {
                        dest = "()".to_string();
                    }
                    let dest = dest.strip_suffix(".md").unwrap_or(&dest);
                    let dest = dest.strip_suffix(".markdown").unwrap_or(dest);
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
                level: *level,
                text,
                range: node.byte_range().clone(),
            };
            Some(NodeParsedResult::Referenceable(referenceable))
        }
        NodeKind::Paragraph => {
            if node.children.len() < 2 {
                return None;
            }

            let snd_last_child = &node.children[node.children.len() - 2];
            let last_child = &node.children[node.children.len() - 1];

            let block_identifier: Option<BlockIdentifier> =
                match (&snd_last_child.kind, &last_child.kind) {
                    (NodeKind::Text(fst), NodeKind::Text(snd)) => {
                        if fst.as_ref() == "^"
                            && is_block_identifier(snd.as_ref())
                        {
                            Some(BlockIdentifier {
                                identifier: snd.as_ref().to_string(),
                                range: (snd_last_child.byte_range().start
                                    ..last_child.byte_range().end),
                            })
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

            block_identifier.as_ref()?;

            if let Some(block_identifer_val) = block_identifier {
                if node.children.len() == 2 {
                    // If the paragraph has exactly two text nodes as block identifier,
                    // it's a block identifier
                    return Some(NodeParsedResult::BlockIdentifier(
                        block_identifer_val,
                    ));
                } else {
                    // If the paragraph has more than two text nodes as block identifier,
                    // it's a block referenceable itself
                    let refable = Referenceable::Block {
                        path: path.clone(),
                        identifier: block_identifer_val.identifier,
                        kind: BlockReferenceableKind::InlineParagraph,
                        range: node.byte_range().clone(),
                    };
                    return Some(NodeParsedResult::Referenceable(refable));
                }
            }

            None
        }
        NodeKind::Item => {
            // Find last two Text children (skip non-Text children like nested Lists)
            let text_children: Vec<&Node> = node
                .children
                .iter()
                .filter(|child| matches!(child.kind, NodeKind::Text(_)))
                .collect();

            if text_children.len() < 2 {
                return None;
            }

            let second_last_text = text_children[text_children.len() - 2];
            let last_text = text_children[text_children.len() - 1];

            let block_identifier: Option<BlockIdentifier> =
                match (&second_last_text.kind, &last_text.kind) {
                    (NodeKind::Text(caret), NodeKind::Text(identifier)) => {
                        if caret.as_ref() == "^"
                            && is_block_identifier(identifier.as_ref())
                        {
                            Some(BlockIdentifier {
                                identifier: identifier.as_ref().to_string(),
                                range: (second_last_text.byte_range().start
                                    ..last_text.byte_range().end),
                            })
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

            if let Some(block_identifer_val) = block_identifier {
                let refable = Referenceable::Block {
                    path: path.clone(),
                    identifier: block_identifer_val.identifier,
                    kind: BlockReferenceableKind::InlineListItem,
                    range: node.byte_range().clone(),
                };
                return Some(NodeParsedResult::Referenceable(refable));
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
                            NodeKind::Callout { .. } => {
                                Some(BlockReferenceableKind::Callout)
                            }
                            NodeKind::BlockQuote => {
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

const IGNORED: &[&str] = &[".obsidian", ".DS_Store", ".git"];

fn scan_dir_for_assets_and_notes(dir: &Path) -> Vec<Referenceable> {
    fn aux(
        dir: &Path,
        referenceables: &mut Vec<Referenceable>,
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

/// Recursively apply a map to all paths in a Referenceable
pub fn mut_transform_referenceable_path<M>(
    referenceable: &mut Referenceable,
    path_map: &M,
) where
    M: Fn(&mut PathBuf),
{
    match referenceable {
        Referenceable::Note { path, children } => {
            path_map(path);
            for child in children.iter_mut() {
                mut_transform_referenceable_path(child, path_map);
            }
        }
        Referenceable::Asset { path }
        | Referenceable::Heading { path, .. }
        | Referenceable::Block { path, .. } => {
            path_map(path);
        }
    }
}

/// Recursively convert all paths in a Referenceable to be relative to root_dir (in-place).
fn make_referenceable_relative(
    referenceable: &mut Referenceable,
    root_dir: &Path,
) {
    let make_path_relative = |path: &mut PathBuf| {
        if let Ok(relative) = path.strip_prefix(root_dir) {
            *path = relative.to_path_buf();
        }
    };
    mut_transform_referenceable_path(referenceable, &make_path_relative);
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::{assert_debug_snapshot, assert_snapshot};

    #[test]
    fn test_exract_references_and_referenceables() {
        let path = PathBuf::from("tests/data/vaults/tt/block.md");
        let (_, references, referenceables): (
            _,
            Vec<Reference>,
            Vec<Referenceable>,
        ) = scan_note(&path);
        assert_debug_snapshot!(references, @r##"
        [
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 251..265,
                dest: "#^quotation",
                display_text: "#^quotation",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 313..325,
                dest: "#^callout",
                display_text: "#^callout",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 357..371,
                dest: "#^paragraph",
                display_text: "#^paragraph",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 396..412,
                dest: "#^p-with-code",
                display_text: "#^p-with-code",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 437..452,
                dest: "#^paragraph2",
                display_text: "#^paragraph2",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 544..554,
                dest: "#^table",
                display_text: "#^table",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 632..646,
                dest: "#^firstline",
                display_text: "#^firstline",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 678..692,
                dest: "#^inneritem",
                display_text: "#^inneritem",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 912..925,
                dest: "#^tableref",
                display_text: "#^tableref",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 1082..1096,
                dest: "#^tableref3",
                display_text: "#^tableref3",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 1100..1114,
                dest: "#^tableref2",
                display_text: "#^tableref2",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 1256..1266,
                dest: "#^works",
                display_text: "#^works",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 1686..1700,
                dest: "#^firstline",
                display_text: "#^firstline",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 1732..1746,
                dest: "#^inneritem",
                display_text: "#^inneritem",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 1864..1879,
                dest: "#^firstline1",
                display_text: "#^firstline1",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 1932..1947,
                dest: "#^inneritem1",
                display_text: "#^inneritem1",
            },
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/tt/block.md",
                range: 1999..2012,
                dest: "#^fulllst1",
                display_text: "#^fulllst1",
            },
        ]
        "##);
        assert_debug_snapshot!(referenceables, @r#"
        [
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "paragraph",
                kind: InlineParagraph,
                range: 0..23,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "p-with-code",
                kind: InlineParagraph,
                range: 24..66,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "paragraph2",
                kind: Paragraph,
                range: 67..79,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "fulllist",
                kind: List,
                range: 93..126,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "table",
                kind: Table,
                range: 137..217,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "quotation",
                kind: BlockQuote,
                range: 226..238,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "callout",
                kind: Callout,
                range: 269..302,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "firstline",
                kind: InlineListItem,
                range: 563..613,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "inneritem",
                kind: InlineListItem,
                range: 589..613,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: a later block identifier invalidate previous one",
                range: 733..801,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "tableref",
                kind: Table,
                range: 802..882,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "tableref2",
                kind: Table,
                range: 929..1009,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "tableref3",
                kind: Paragraph,
                range: 1010..1021,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: the number of blank lines before identifier doesn’t matter",
                range: 1164..1242,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "works",
                kind: InlineParagraph,
                range: 1243..1255,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: full reference to a list make its inner state not refereceable",
                range: 1534..1616,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "firstline",
                kind: InlineParagraph,
                range: 1619..1644,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "inneritem",
                kind: InlineListItem,
                range: 1643..1667,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "firstline1",
                kind: InlineParagraph,
                range: 1783..1809,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "fulllist1",
                kind: InlineListItem,
                range: 1808..1845,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: When there are more than one identical identifiers",
                range: 2041..2111,
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
        Document [0..2166]
          Paragraph [0..23]
            Text(Borrowed("paragraph 1 ")) [0..12]
            Text(Borrowed("^")) [12..13]
            Text(Borrowed("paragraph")) [13..22]
          Paragraph [24..66]
            Text(Borrowed("paragraph with ")) [24..39]
            Code(Borrowed("code")) [39..45]
            Text(Borrowed(" inside ")) [45..53]
            Text(Borrowed("^")) [53..54]
            Text(Borrowed("p-with-code")) [54..65]
          Paragraph [67..79]
            Text(Borrowed("paragraph 2")) [67..78]
          Paragraph [80..92]
            Text(Borrowed("^")) [80..81]
            Text(Borrowed("paragraph2")) [81..91]
          List(None) [93..126]
            Item [93..126]
              Text(Borrowed("some list")) [95..104]
              List(None) [104..126]
                Item [104..115]
                  Text(Borrowed("item 1")) [108..114]
                Item [114..126]
                  Text(Borrowed("item 2")) [118..124]
          Paragraph [126..136]
            Text(Borrowed("^")) [126..127]
            Text(Borrowed("fulllist")) [127..135]
          Table([None, None]) [137..217]
            TableHead [137..157]
              TableCell [138..146]
                Text(Borrowed("Col 1")) [139..144]
              TableCell [147..155]
                Text(Borrowed("Col 2")) [148..153]
            TableRow [177..197]
              TableCell [178..186]
                Text(Borrowed("Cell 1")) [179..185]
              TableCell [187..195]
                Text(Borrowed("Cell 2")) [188..194]
            TableRow [197..217]
              TableCell [198..206]
                Text(Borrowed("Cell 3")) [199..205]
              TableCell [207..215]
                Text(Borrowed("Cell 4")) [208..214]
          Paragraph [218..225]
            Text(Borrowed("^")) [218..219]
            Text(Borrowed("table")) [219..224]
          BlockQuote [226..238]
            Paragraph [228..238]
              Text(Borrowed("quotation")) [228..237]
          Paragraph [239..250]
            Text(Borrowed("^")) [239..240]
            Text(Borrowed("quotation")) [240..249]
          Paragraph [251..267]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^quotation"), title: Borrowed(""), id: Borrowed("") } [251..265]
              Text(Borrowed("#^quotation")) [253..264]
          Callout { kind: Obsidian(Info), title: Some("this is a info callout"), foldable: None, content_start_byte: 302 } [269..302]
            Paragraph [271..302]
              Text(Borrowed("[")) [271..272]
              Text(Borrowed("!info")) [272..277]
              Text(Borrowed("]")) [277..278]
              Text(Borrowed(" this is a info callout")) [278..301]
          Paragraph [303..312]
            Text(Borrowed("^")) [303..304]
            Text(Borrowed("callout")) [304..311]
          Paragraph [313..327]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^callout"), title: Borrowed(""), id: Borrowed("") } [313..325]
              Text(Borrowed("#^callout")) [315..324]
          Rule [329..333]
          Paragraph [334..344]
            Text(Borrowed("reference")) [334..343]
          List(None) [344..558]
            Item [344..373]
              Text(Borrowed("paragraph: ")) [346..357]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^paragraph"), title: Borrowed(""), id: Borrowed("") } [357..371]
                Text(Borrowed("#^paragraph")) [359..370]
            Item [373..414]
              Text(Borrowed("paragraph with code: ")) [375..396]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^p-with-code"), title: Borrowed(""), id: Borrowed("") } [396..412]
                Text(Borrowed("#^p-with-code")) [398..411]
            Item [414..535]
              Text(Borrowed("separate line caret: ")) [416..437]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^paragraph2"), title: Borrowed(""), id: Borrowed("") } [437..452]
                Text(Borrowed("#^paragraph2")) [439..451]
              List(None) [453..535]
                Item [453..535]
                  Text(Borrowed("for paragraph the caret doesn")) [457..486]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [486..487]
                  Text(Borrowed("t need to have a blank line before and after it")) [487..534]
            Item [535..558]
              Text(Borrowed("table: ")) [537..544]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^table"), title: Borrowed(""), id: Borrowed("") } [544..554]
                Text(Borrowed("#^table")) [546..553]
          Rule [558..562]
          List(None) [563..729]
            Item [563..613]
              Text(Borrowed("a nested list ")) [565..579]
              Text(Borrowed("^")) [579..580]
              Text(Borrowed("firstline")) [580..589]
              List(None) [589..613]
                Item [589..613]
                  Text(Borrowed("item")) [594..598]
                  SoftBreak [598..599]
                  Text(Borrowed("^")) [602..603]
                  Text(Borrowed("inneritem")) [603..612]
            Item [613..729]
              Text(Borrowed("inside a list")) [615..628]
              List(None) [628..729]
                Item [628..674]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline"), title: Borrowed(""), id: Borrowed("") } [632..646]
                    Text(Borrowed("#^firstline")) [634..645]
                  Text(Borrowed(": points to the first line")) [647..673]
                Item [673..729]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem"), title: Borrowed(""), id: Borrowed("") } [678..692]
                    Text(Borrowed("#^inneritem")) [680..691]
                  Text(Borrowed(": points to the first inner item")) [693..725]
          Rule [729..733]
          Heading { level: H6, id: None, classes: [], attrs: [] } [733..801]
            Text(Borrowed("Edge case: a later block identifier invalidate previous one")) [741..800]
          Table([None, None]) [802..882]
            TableHead [802..822]
              TableCell [803..811]
                Text(Borrowed("Col 1")) [804..809]
              TableCell [812..820]
                Text(Borrowed("Col 2")) [813..818]
            TableRow [842..862]
              TableCell [843..851]
                Text(Borrowed("Cell 1")) [844..850]
              TableCell [852..860]
                Text(Borrowed("Cell 2")) [853..859]
            TableRow [862..882]
              TableCell [863..871]
                Text(Borrowed("Cell 3")) [864..870]
              TableCell [872..880]
                Text(Borrowed("Cell 4")) [873..879]
          Paragraph [883..893]
            Text(Borrowed("^")) [883..884]
            Text(Borrowed("tableref")) [884..892]
          List(None) [894..929]
            Item [894..929]
              Text(Borrowed("this works fine ")) [896..912]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref"), title: Borrowed(""), id: Borrowed("") } [912..925]
                Text(Borrowed("#^tableref")) [914..924]
          Table([None, None]) [929..1009]
            TableHead [929..949]
              TableCell [930..938]
                Text(Borrowed("Col 1")) [931..936]
              TableCell [939..947]
                Text(Borrowed("Col 2")) [940..945]
            TableRow [969..989]
              TableCell [970..978]
                Text(Borrowed("Cell 1")) [971..977]
              TableCell [979..987]
                Text(Borrowed("Cell 2")) [980..986]
            TableRow [989..1009]
              TableCell [990..998]
                Text(Borrowed("Cell 3")) [991..997]
              TableCell [999..1007]
                Text(Borrowed("Cell 4")) [1000..1006]
          Paragraph [1010..1021]
            Text(Borrowed("^")) [1010..1011]
            Text(Borrowed("tableref2")) [1011..1020]
          Paragraph [1022..1033]
            Text(Borrowed("^")) [1022..1023]
            Text(Borrowed("tableref3")) [1023..1032]
          List(None) [1034..1164]
            Item [1034..1098]
              Text(Borrowed("now the above table can only be referenced by ")) [1036..1082]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref3"), title: Borrowed(""), id: Borrowed("") } [1082..1096]
                Text(Borrowed("#^tableref3")) [1084..1095]
            Item [1098..1164]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref2"), title: Borrowed(""), id: Borrowed("") } [1100..1114]
                Text(Borrowed("#^tableref2")) [1102..1113]
              Text(Borrowed(" is invalid and will fallback to the whole note")) [1115..1162]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1164..1242]
            Text(Borrowed("Edge case: the number of blank lines before identifier doesn")) [1172..1232]
            Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1232..1233]
            Text(Borrowed("t matter")) [1233..1241]
          Paragraph [1243..1255]
            Text(Borrowed("this")) [1243..1247]
            SoftBreak [1247..1248]
            Text(Borrowed("^")) [1248..1249]
            Text(Borrowed("works")) [1249..1254]
          Paragraph [1256..1268]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^works"), title: Borrowed(""), id: Borrowed("") } [1256..1266]
              Text(Borrowed("#^works")) [1258..1265]
          List(None) [1269..1534]
            Item [1269..1317]
              Text(Borrowed("1 blank line after the identifier is required")) [1271..1316]
            Item [1317..1534]
              Text(Borrowed("however, 0-n blank line before the identifier works fine")) [1319..1375]
              List(None) [1375..1534]
                Item [1375..1534]
                  Text(Borrowed("for clarity, we should always require at least 1 blank line before the identifier (so that the identifier won")) [1379..1488]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1488..1489]
                  Text(Borrowed("t be parsed as part of the previous struct)")) [1489..1532]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1534..1616]
            Text(Borrowed("Edge case: full reference to a list make its inner state not refereceable")) [1542..1615]
          List(None) [1617..2041]
            Item [1617..1667]
              Paragraph [1619..1644]
                Text(Borrowed("a nested list ")) [1619..1633]
                Text(Borrowed("^")) [1633..1634]
                Text(Borrowed("firstline")) [1634..1643]
              List(None) [1643..1667]
                Item [1643..1667]
                  Text(Borrowed("item")) [1648..1652]
                  SoftBreak [1652..1653]
                  Text(Borrowed("^")) [1656..1657]
                  Text(Borrowed("inneritem")) [1657..1666]
            Item [1667..1781]
              Paragraph [1669..1683]
                Text(Borrowed("inside a list")) [1669..1682]
              List(None) [1682..1781]
                Item [1682..1728]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline"), title: Borrowed(""), id: Borrowed("") } [1686..1700]
                    Text(Borrowed("#^firstline")) [1688..1699]
                  Text(Borrowed(": points to the first line")) [1701..1727]
                Item [1727..1781]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem"), title: Borrowed(""), id: Borrowed("") } [1732..1746]
                    Text(Borrowed("#^inneritem")) [1734..1745]
                  Text(Borrowed(": points to the first inner item")) [1747..1779]
            Item [1781..1845]
              Paragraph [1783..1809]
                Text(Borrowed("a nested list ")) [1783..1797]
                Text(Borrowed("^")) [1797..1798]
                Text(Borrowed("firstline1")) [1798..1808]
              List(None) [1808..1845]
                Item [1808..1845]
                  Text(Borrowed("item")) [1813..1817]
                  SoftBreak [1817..1818]
                  Text(Borrowed("^")) [1821..1822]
                  Text(Borrowed("inneritem1")) [1822..1832]
                  SoftBreak [1832..1833]
                  Text(Borrowed("^")) [1833..1834]
                  Text(Borrowed("fulllist1")) [1834..1843]
            Item [1845..2041]
              Paragraph [1847..1861]
                Text(Borrowed("inside a list")) [1847..1860]
              List(None) [1860..2041]
                Item [1860..1928]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline1"), title: Borrowed(""), id: Borrowed("") } [1864..1879]
                    Text(Borrowed("#^firstline1")) [1866..1878]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [1880..1927]
                Item [1927..1996]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem1"), title: Borrowed(""), id: Borrowed("") } [1932..1947]
                    Text(Borrowed("#^inneritem1")) [1934..1946]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [1948..1995]
                Item [1995..2041]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^fulllst1"), title: Borrowed(""), id: Borrowed("") } [1999..2012]
                    Text(Borrowed("#^fulllst1")) [2001..2011]
                  Text(Borrowed(": points to the full list")) [2013..2038]
          Heading { level: H6, id: None, classes: [], attrs: [] } [2041..2111]
            Text(Borrowed("Edge case: When there are more than one identical identifiers")) [2049..2110]
          Paragraph [2112..2166]
            Text(Borrowed("Obsidian doesn")) [2112..2126]
            Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [2126..2127]
            Text(Borrowed("t guarantee to points to the first one")) [2127..2165]
        "##);
    }
}
