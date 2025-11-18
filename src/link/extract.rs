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
            if node.children.len() < 2 {
                return None;
            }

            let snd_last_child = &node.children[node.children.len() - 1];
            let last_child = &node.children[node.children.len() - 2];

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

            if block_identifier.is_none() {
                return None;
            }

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
                range: 251..265,
                dest: "#^quotation",
                kind: WikiLink,
                display_text: "#^quotation",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 313..325,
                dest: "#^callout",
                kind: WikiLink,
                display_text: "#^callout",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 357..372,
                dest: "#^paragraph1",
                kind: WikiLink,
                display_text: "#^paragraph1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 397..413,
                dest: "#^p-with-code",
                kind: WikiLink,
                display_text: "#^p-with-code",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 438..453,
                dest: "#^paragraph2",
                kind: WikiLink,
                display_text: "#^paragraph2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 545..555,
                dest: "#^table",
                kind: WikiLink,
                display_text: "#^table",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 567..578,
                dest: "#^table2",
                kind: WikiLink,
                display_text: "#^table2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 590..605,
                dest: "#^tableagain",
                kind: WikiLink,
                display_text: "#^tableagain",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 782..796,
                dest: "#^firstline",
                kind: WikiLink,
                display_text: "#^firstline",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 828..842,
                dest: "#^inneritem",
                kind: WikiLink,
                display_text: "#^inneritem",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1062..1075,
                dest: "#^tableref",
                kind: WikiLink,
                display_text: "#^tableref",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1232..1246,
                dest: "#^tableref3",
                kind: WikiLink,
                display_text: "#^tableref3",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1250..1264,
                dest: "#^tableref2",
                kind: WikiLink,
                display_text: "#^tableref2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1406..1416,
                dest: "#^works",
                kind: WikiLink,
                display_text: "#^works",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1836..1850,
                dest: "#^firstline",
                kind: WikiLink,
                display_text: "#^firstline",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1882..1896,
                dest: "#^inneritem",
                kind: WikiLink,
                display_text: "#^inneritem",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 2014..2029,
                dest: "#^firstline1",
                kind: WikiLink,
                display_text: "#^firstline1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 2082..2097,
                dest: "#^inneritem1",
                kind: WikiLink,
                display_text: "#^inneritem1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 2149..2162,
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
                kind: BlockQuote,
                range: 269..302,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: a later block identifier invalidate previous one",
                range: 883..951,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "tableref",
                kind: Table,
                range: 952..1032,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "tableref2",
                kind: Table,
                range: 1079..1159,
            },
            Block {
                path: "tests/data/vaults/tt/block.md",
                identifier: "tableref3",
                kind: Paragraph,
                range: 1160..1171,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: the number of blank lines before identifier doesn’t matter",
                range: 1314..1392,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: full reference to a list make its inner state not refereceable",
                range: 1684..1766,
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
        Document [0..2189]
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
          BlockQuote(None) [226..238]
            Paragraph [228..238]
              Text(Borrowed("quotation")) [228..237]
          Paragraph [239..250]
            Text(Borrowed("^")) [239..240]
            Text(Borrowed("quotation")) [240..249]
          Paragraph [251..267]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^quotation"), title: Borrowed(""), id: Borrowed("") } [251..265]
              Text(Borrowed("#^quotation")) [253..264]
          BlockQuote(None) [269..302]
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
          List(None) [344..708]
            Item [344..374]
              Text(Borrowed("paragraph: ")) [346..357]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^paragraph1"), title: Borrowed(""), id: Borrowed("") } [357..372]
                Text(Borrowed("#^paragraph1")) [359..371]
            Item [374..415]
              Text(Borrowed("paragraph with code: ")) [376..397]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^p-with-code"), title: Borrowed(""), id: Borrowed("") } [397..413]
                Text(Borrowed("#^p-with-code")) [399..412]
            Item [415..536]
              Text(Borrowed("separate line caret: ")) [417..438]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^paragraph2"), title: Borrowed(""), id: Borrowed("") } [438..453]
                Text(Borrowed("#^paragraph2")) [440..452]
              List(None) [454..536]
                Item [454..536]
                  Text(Borrowed("for paragraph the caret doesn")) [458..487]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [487..488]
                  Text(Borrowed("t need to have a blank line before and after it")) [488..535]
            Item [536..558]
              Text(Borrowed("table: ")) [538..545]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^table"), title: Borrowed(""), id: Borrowed("") } [545..555]
                Text(Borrowed("#^table")) [547..554]
            Item [558..581]
              Text(Borrowed("table: ")) [560..567]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^table2"), title: Borrowed(""), id: Borrowed("") } [567..578]
                Text(Borrowed("#^table2")) [569..577]
            Item [581..708]
              Text(Borrowed("table: ")) [583..590]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableagain"), title: Borrowed(""), id: Borrowed("") } [590..605]
                Text(Borrowed("#^tableagain")) [592..604]
              List(None) [606..708]
                Item [606..708]
                  Text(Borrowed("it looks like the block reference will always point to last non-empty non-block-reference struct")) [610..706]
          Rule [708..712]
          List(None) [713..879]
            Item [713..763]
              Text(Borrowed("a nested list ")) [715..729]
              Text(Borrowed("^")) [729..730]
              Text(Borrowed("firstline")) [730..739]
              List(None) [739..763]
                Item [739..763]
                  Text(Borrowed("item")) [744..748]
                  SoftBreak [748..749]
                  Text(Borrowed("^")) [752..753]
                  Text(Borrowed("inneritem")) [753..762]
            Item [763..879]
              Text(Borrowed("inside a list")) [765..778]
              List(None) [778..879]
                Item [778..824]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline"), title: Borrowed(""), id: Borrowed("") } [782..796]
                    Text(Borrowed("#^firstline")) [784..795]
                  Text(Borrowed(": points to the first line")) [797..823]
                Item [823..879]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem"), title: Borrowed(""), id: Borrowed("") } [828..842]
                    Text(Borrowed("#^inneritem")) [830..841]
                  Text(Borrowed(": points to the first inner item")) [843..875]
          Rule [879..883]
          Heading { level: H6, id: None, classes: [], attrs: [] } [883..951]
            Text(Borrowed("Edge case: a later block identifier invalidate previous one")) [891..950]
          Table([None, None]) [952..1032]
            TableHead [952..972]
              TableCell [953..961]
                Text(Borrowed("Col 1")) [954..959]
              TableCell [962..970]
                Text(Borrowed("Col 2")) [963..968]
            TableRow [992..1012]
              TableCell [993..1001]
                Text(Borrowed("Cell 1")) [994..1000]
              TableCell [1002..1010]
                Text(Borrowed("Cell 2")) [1003..1009]
            TableRow [1012..1032]
              TableCell [1013..1021]
                Text(Borrowed("Cell 3")) [1014..1020]
              TableCell [1022..1030]
                Text(Borrowed("Cell 4")) [1023..1029]
          Paragraph [1033..1043]
            Text(Borrowed("^")) [1033..1034]
            Text(Borrowed("tableref")) [1034..1042]
          List(None) [1044..1079]
            Item [1044..1079]
              Text(Borrowed("this works fine ")) [1046..1062]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref"), title: Borrowed(""), id: Borrowed("") } [1062..1075]
                Text(Borrowed("#^tableref")) [1064..1074]
          Table([None, None]) [1079..1159]
            TableHead [1079..1099]
              TableCell [1080..1088]
                Text(Borrowed("Col 1")) [1081..1086]
              TableCell [1089..1097]
                Text(Borrowed("Col 2")) [1090..1095]
            TableRow [1119..1139]
              TableCell [1120..1128]
                Text(Borrowed("Cell 1")) [1121..1127]
              TableCell [1129..1137]
                Text(Borrowed("Cell 2")) [1130..1136]
            TableRow [1139..1159]
              TableCell [1140..1148]
                Text(Borrowed("Cell 3")) [1141..1147]
              TableCell [1149..1157]
                Text(Borrowed("Cell 4")) [1150..1156]
          Paragraph [1160..1171]
            Text(Borrowed("^")) [1160..1161]
            Text(Borrowed("tableref2")) [1161..1170]
          Paragraph [1172..1183]
            Text(Borrowed("^")) [1172..1173]
            Text(Borrowed("tableref3")) [1173..1182]
          List(None) [1184..1314]
            Item [1184..1248]
              Text(Borrowed("now the above table can only be referenced by ")) [1186..1232]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref3"), title: Borrowed(""), id: Borrowed("") } [1232..1246]
                Text(Borrowed("#^tableref3")) [1234..1245]
            Item [1248..1314]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref2"), title: Borrowed(""), id: Borrowed("") } [1250..1264]
                Text(Borrowed("#^tableref2")) [1252..1263]
              Text(Borrowed(" is invalid and will fallback to the whole note")) [1265..1312]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1314..1392]
            Text(Borrowed("Edge case: the number of blank lines before identifier doesn")) [1322..1382]
            Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1382..1383]
            Text(Borrowed("t matter")) [1383..1391]
          Paragraph [1393..1405]
            Text(Borrowed("this")) [1393..1397]
            SoftBreak [1397..1398]
            Text(Borrowed("^")) [1398..1399]
            Text(Borrowed("works")) [1399..1404]
          Paragraph [1406..1418]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^works"), title: Borrowed(""), id: Borrowed("") } [1406..1416]
              Text(Borrowed("#^works")) [1408..1415]
          List(None) [1419..1684]
            Item [1419..1467]
              Text(Borrowed("1 blank line after the identifier is required")) [1421..1466]
            Item [1467..1684]
              Text(Borrowed("however, 0-n blank line before the identifier works fine")) [1469..1525]
              List(None) [1525..1684]
                Item [1525..1684]
                  Text(Borrowed("for clarity, we should always require at least 1 blank line before the identifier (so that the identifier won")) [1529..1638]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1638..1639]
                  Text(Borrowed("t be parsed as part of the previous struct)")) [1639..1682]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1684..1766]
            Text(Borrowed("Edge case: full reference to a list make its inner state not refereceable")) [1692..1765]
          List(None) [1767..2189]
            Item [1767..1817]
              Paragraph [1769..1794]
                Text(Borrowed("a nested list ")) [1769..1783]
                Text(Borrowed("^")) [1783..1784]
                Text(Borrowed("firstline")) [1784..1793]
              List(None) [1793..1817]
                Item [1793..1817]
                  Text(Borrowed("item")) [1798..1802]
                  SoftBreak [1802..1803]
                  Text(Borrowed("^")) [1806..1807]
                  Text(Borrowed("inneritem")) [1807..1816]
            Item [1817..1931]
              Paragraph [1819..1833]
                Text(Borrowed("inside a list")) [1819..1832]
              List(None) [1832..1931]
                Item [1832..1878]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline"), title: Borrowed(""), id: Borrowed("") } [1836..1850]
                    Text(Borrowed("#^firstline")) [1838..1849]
                  Text(Borrowed(": points to the first line")) [1851..1877]
                Item [1877..1931]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem"), title: Borrowed(""), id: Borrowed("") } [1882..1896]
                    Text(Borrowed("#^inneritem")) [1884..1895]
                  Text(Borrowed(": points to the first inner item")) [1897..1929]
            Item [1931..1995]
              Paragraph [1933..1959]
                Text(Borrowed("a nested list ")) [1933..1947]
                Text(Borrowed("^")) [1947..1948]
                Text(Borrowed("firstline1")) [1948..1958]
              List(None) [1958..1995]
                Item [1958..1995]
                  Text(Borrowed("item")) [1963..1967]
                  SoftBreak [1967..1968]
                  Text(Borrowed("^")) [1971..1972]
                  Text(Borrowed("inneritem1")) [1972..1982]
                  SoftBreak [1982..1983]
                  Text(Borrowed("^")) [1983..1984]
                  Text(Borrowed("fulllist1")) [1984..1993]
            Item [1995..2189]
              Paragraph [1997..2011]
                Text(Borrowed("inside a list")) [1997..2010]
              List(None) [2010..2189]
                Item [2010..2078]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline1"), title: Borrowed(""), id: Borrowed("") } [2014..2029]
                    Text(Borrowed("#^firstline1")) [2016..2028]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [2030..2077]
                Item [2077..2146]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem1"), title: Borrowed(""), id: Borrowed("") } [2082..2097]
                    Text(Borrowed("#^inneritem1")) [2084..2096]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [2098..2145]
                Item [2145..2189]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^fulllst1"), title: Borrowed(""), id: Borrowed("") } [2149..2162]
                    Text(Borrowed("#^fulllst1")) [2151..2161]
                  Text(Borrowed(": points to the full list")) [2163..2188]
        "##);
    }
}
