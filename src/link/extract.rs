use super::types::{Reference, ReferenceKind, Referenceable};
use super::utils::percent_decode;
use crate::ast::{Node, NodeKind, Tree};
use pulldown_cmark::LinkType;
use std::fs;
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
                range: 207..221,
                dest: "#^quotation",
                kind: WikiLink,
                display_text: "#^quotation",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 269..281,
                dest: "#^callout",
                kind: WikiLink,
                display_text: "#^callout",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 313..328,
                dest: "#^paragraph1",
                kind: WikiLink,
                display_text: "#^paragraph1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 353..368,
                dest: "#^paragraph2",
                kind: WikiLink,
                display_text: "#^paragraph2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 460..470,
                dest: "#^table",
                kind: WikiLink,
                display_text: "#^table",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 482..493,
                dest: "#^table2",
                kind: WikiLink,
                display_text: "#^table2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 505..520,
                dest: "#^tableagain",
                kind: WikiLink,
                display_text: "#^tableagain",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 698..712,
                dest: "#^firstline",
                kind: WikiLink,
                display_text: "#^firstline",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 744..758,
                dest: "#^inneritem",
                kind: WikiLink,
                display_text: "#^inneritem",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 943..958,
                dest: "#^firstline1",
                kind: WikiLink,
                display_text: "#^firstline1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1011..1026,
                dest: "#^inneritem1",
                kind: WikiLink,
                display_text: "#^inneritem1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1078..1091,
                dest: "#^fulllst1",
                kind: WikiLink,
                display_text: "#^fulllst1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1303..1316,
                dest: "#^tableref",
                kind: WikiLink,
                display_text: "#^tableref",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1473..1487,
                dest: "#^tableref3",
                kind: WikiLink,
                display_text: "#^tableref3",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1491..1505,
                dest: "#^tableref2",
                kind: WikiLink,
                display_text: "#^tableref2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1647..1657,
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
                range: 1124..1192,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: the number of blank lines before identifier doesn’t matter",
                range: 1555..1633,
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
        Document [0..1924]
          Paragraph [0..23]
            Text(Borrowed("paragraph 1 ")) [0..12]
            Text(Borrowed("^")) [12..13]
            Text(Borrowed("paragraph")) [13..22]
          Paragraph [24..48]
            Text(Borrowed("paragraph 2")) [24..35]
            SoftBreak [35..36]
            Text(Borrowed("^")) [36..37]
            Text(Borrowed("paragraph2")) [37..47]
          List(None) [49..82]
            Item [49..82]
              Text(Borrowed("some list")) [51..60]
              List(None) [60..82]
                Item [60..71]
                  Text(Borrowed("item 1")) [64..70]
                Item [70..82]
                  Text(Borrowed("item 2")) [74..80]
          Paragraph [82..92]
            Text(Borrowed("^")) [82..83]
            Text(Borrowed("fulllist")) [83..91]
          Table([None, None]) [93..173]
            TableHead [93..113]
              TableCell [94..102]
                Text(Borrowed("Col 1")) [95..100]
              TableCell [103..111]
                Text(Borrowed("Col 2")) [104..109]
            TableRow [133..153]
              TableCell [134..142]
                Text(Borrowed("Cell 1")) [135..141]
              TableCell [143..151]
                Text(Borrowed("Cell 2")) [144..150]
            TableRow [153..173]
              TableCell [154..162]
                Text(Borrowed("Cell 3")) [155..161]
              TableCell [163..171]
                Text(Borrowed("Cell 4")) [164..170]
          Paragraph [174..181]
            Text(Borrowed("^")) [174..175]
            Text(Borrowed("table")) [175..180]
          BlockQuote(None) [182..194]
            Paragraph [184..194]
              Text(Borrowed("quotation")) [184..193]
          Paragraph [195..206]
            Text(Borrowed("^")) [195..196]
            Text(Borrowed("quotation")) [196..205]
          Paragraph [207..223]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^quotation"), title: Borrowed(""), id: Borrowed("") } [207..221]
              Text(Borrowed("#^quotation")) [209..220]
          BlockQuote(None) [225..258]
            Paragraph [227..258]
              Text(Borrowed("[")) [227..228]
              Text(Borrowed("!info")) [228..233]
              Text(Borrowed("]")) [233..234]
              Text(Borrowed(" this is a info callout")) [234..257]
          Paragraph [259..268]
            Text(Borrowed("^")) [259..260]
            Text(Borrowed("callout")) [260..267]
          Paragraph [269..283]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^callout"), title: Borrowed(""), id: Borrowed("") } [269..281]
              Text(Borrowed("#^callout")) [271..280]
          Rule [285..289]
          Paragraph [290..300]
            Text(Borrowed("reference")) [290..299]
          List(None) [300..623]
            Item [300..330]
              Text(Borrowed("paragraph: ")) [302..313]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^paragraph1"), title: Borrowed(""), id: Borrowed("") } [313..328]
                Text(Borrowed("#^paragraph1")) [315..327]
            Item [330..451]
              Text(Borrowed("separate line caret: ")) [332..353]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^paragraph2"), title: Borrowed(""), id: Borrowed("") } [353..368]
                Text(Borrowed("#^paragraph2")) [355..367]
              List(None) [369..451]
                Item [369..451]
                  Text(Borrowed("for paragraph the caret doesn")) [373..402]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [402..403]
                  Text(Borrowed("t need to have a blank line before and after it")) [403..450]
            Item [451..473]
              Text(Borrowed("table: ")) [453..460]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^table"), title: Borrowed(""), id: Borrowed("") } [460..470]
                Text(Borrowed("#^table")) [462..469]
            Item [473..496]
              Text(Borrowed("table: ")) [475..482]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^table2"), title: Borrowed(""), id: Borrowed("") } [482..493]
                Text(Borrowed("#^table2")) [484..492]
            Item [496..623]
              Text(Borrowed("table: ")) [498..505]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableagain"), title: Borrowed(""), id: Borrowed("") } [505..520]
                Text(Borrowed("#^tableagain")) [507..519]
              List(None) [521..623]
                Item [521..623]
                  Text(Borrowed("it looks like the block reference will always point to last non-empty non-block-reference struct")) [525..621]
          Rule [623..627]
          List(None) [628..1120]
            Item [628..679]
              Paragraph [630..655]
                Text(Borrowed("a nested list ")) [630..644]
                Text(Borrowed("^")) [644..645]
                Text(Borrowed("firstline")) [645..654]
              List(None) [654..679]
                Item [654..679]
                  Text(Borrowed("item")) [659..663]
                  SoftBreak [663..664]
                  Text(Borrowed("^")) [667..668]
                  Text(Borrowed("inneritem")) [668..677]
            Item [679..793]
              Paragraph [681..695]
                Text(Borrowed("inside a list")) [681..694]
              List(None) [694..793]
                Item [694..740]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline"), title: Borrowed(""), id: Borrowed("") } [698..712]
                    Text(Borrowed("#^firstline")) [700..711]
                  Text(Borrowed(": points to the first line")) [713..739]
                Item [739..793]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem"), title: Borrowed(""), id: Borrowed("") } [744..758]
                    Text(Borrowed("#^inneritem")) [746..757]
                  Text(Borrowed(": points to the first inner item")) [759..791]
            Item [793..860]
              Paragraph [795..858]
                Text(Borrowed("full reference to a list make its inner state not refereceable")) [795..857]
            Item [860..924]
              Paragraph [862..888]
                Text(Borrowed("a nested list ")) [862..876]
                Text(Borrowed("^")) [876..877]
                Text(Borrowed("firstline1")) [877..887]
              List(None) [887..924]
                Item [887..924]
                  Text(Borrowed("item")) [892..896]
                  SoftBreak [896..897]
                  Text(Borrowed("^")) [900..901]
                  Text(Borrowed("inneritem1")) [901..911]
                  SoftBreak [911..912]
                  Text(Borrowed("^")) [912..913]
                  Text(Borrowed("fulllist1")) [913..922]
            Item [924..1120]
              Paragraph [926..940]
                Text(Borrowed("inside a list")) [926..939]
              List(None) [939..1120]
                Item [939..1007]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline1"), title: Borrowed(""), id: Borrowed("") } [943..958]
                    Text(Borrowed("#^firstline1")) [945..957]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [959..1006]
                Item [1006..1075]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem1"), title: Borrowed(""), id: Borrowed("") } [1011..1026]
                    Text(Borrowed("#^inneritem1")) [1013..1025]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [1027..1074]
                Item [1074..1120]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^fulllst1"), title: Borrowed(""), id: Borrowed("") } [1078..1091]
                    Text(Borrowed("#^fulllst1")) [1080..1090]
                  Text(Borrowed(": points to the full list")) [1092..1117]
          Rule [1120..1124]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1124..1192]
            Text(Borrowed("Edge case: a later block identifier invalidate previous one")) [1132..1191]
          Table([None, None]) [1193..1273]
            TableHead [1193..1213]
              TableCell [1194..1202]
                Text(Borrowed("Col 1")) [1195..1200]
              TableCell [1203..1211]
                Text(Borrowed("Col 2")) [1204..1209]
            TableRow [1233..1253]
              TableCell [1234..1242]
                Text(Borrowed("Cell 1")) [1235..1241]
              TableCell [1243..1251]
                Text(Borrowed("Cell 2")) [1244..1250]
            TableRow [1253..1273]
              TableCell [1254..1262]
                Text(Borrowed("Cell 3")) [1255..1261]
              TableCell [1263..1271]
                Text(Borrowed("Cell 4")) [1264..1270]
          Paragraph [1274..1284]
            Text(Borrowed("^")) [1274..1275]
            Text(Borrowed("tableref")) [1275..1283]
          List(None) [1285..1320]
            Item [1285..1320]
              Text(Borrowed("this works fine ")) [1287..1303]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref"), title: Borrowed(""), id: Borrowed("") } [1303..1316]
                Text(Borrowed("#^tableref")) [1305..1315]
          Table([None, None]) [1320..1400]
            TableHead [1320..1340]
              TableCell [1321..1329]
                Text(Borrowed("Col 1")) [1322..1327]
              TableCell [1330..1338]
                Text(Borrowed("Col 2")) [1331..1336]
            TableRow [1360..1380]
              TableCell [1361..1369]
                Text(Borrowed("Cell 1")) [1362..1368]
              TableCell [1370..1378]
                Text(Borrowed("Cell 2")) [1371..1377]
            TableRow [1380..1400]
              TableCell [1381..1389]
                Text(Borrowed("Cell 3")) [1382..1388]
              TableCell [1390..1398]
                Text(Borrowed("Cell 4")) [1391..1397]
          Paragraph [1401..1412]
            Text(Borrowed("^")) [1401..1402]
            Text(Borrowed("tableref2")) [1402..1411]
          Paragraph [1413..1424]
            Text(Borrowed("^")) [1413..1414]
            Text(Borrowed("tableref3")) [1414..1423]
          List(None) [1425..1555]
            Item [1425..1489]
              Text(Borrowed("now the above table can only be referenced by ")) [1427..1473]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref3"), title: Borrowed(""), id: Borrowed("") } [1473..1487]
                Text(Borrowed("#^tableref3")) [1475..1486]
            Item [1489..1555]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref2"), title: Borrowed(""), id: Borrowed("") } [1491..1505]
                Text(Borrowed("#^tableref2")) [1493..1504]
              Text(Borrowed(" is invalid and will fallback to the whole note")) [1506..1553]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1555..1633]
            Text(Borrowed("Edge case: the number of blank lines before identifier doesn")) [1563..1623]
            Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1623..1624]
            Text(Borrowed("t matter")) [1624..1632]
          Paragraph [1634..1646]
            Text(Borrowed("this")) [1634..1638]
            SoftBreak [1638..1639]
            Text(Borrowed("^")) [1639..1640]
            Text(Borrowed("works")) [1640..1645]
          Paragraph [1647..1659]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^works"), title: Borrowed(""), id: Borrowed("") } [1647..1657]
              Text(Borrowed("#^works")) [1649..1656]
          List(None) [1660..1924]
            Item [1660..1708]
              Text(Borrowed("1 blank line after the identifier is required")) [1662..1707]
            Item [1708..1924]
              Text(Borrowed("however, 0-n blank line before the identifier works fine")) [1710..1766]
              List(None) [1766..1924]
                Item [1766..1924]
                  Text(Borrowed("for clarity, we should always require at least 1 blank line before the identifier (so that the identifier won")) [1770..1879]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1879..1880]
                  Text(Borrowed("t be parsed as part of the previous struct)")) [1880..1923]
        "##);
    }
}
