use super::types::{Reference, ReferenceKind, Referenceable};
use super::utils::percent_decode;
use crate::ast::{Node, NodeKind, Tree};
use pulldown_cmark::LinkType;
use std::fs;
use std::path::{Path, PathBuf};

//
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
        Referenceable::Block { path } => {
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
                range: 204..218,
                dest: "#^quotation",
                kind: WikiLink,
                display_text: "#^quotation",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 265..277,
                dest: "#^callout",
                kind: WikiLink,
                display_text: "#^callout",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 309..324,
                dest: "#^paragraph1",
                kind: WikiLink,
                display_text: "#^paragraph1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 349..364,
                dest: "#^paragraph2",
                kind: WikiLink,
                display_text: "#^paragraph2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 456..466,
                dest: "#^table",
                kind: WikiLink,
                display_text: "#^table",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 478..489,
                dest: "#^table2",
                kind: WikiLink,
                display_text: "#^table2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 501..516,
                dest: "#^tableagain",
                kind: WikiLink,
                display_text: "#^tableagain",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 694..708,
                dest: "#^firstline",
                kind: WikiLink,
                display_text: "#^firstline",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 740..754,
                dest: "#^inneritem",
                kind: WikiLink,
                display_text: "#^inneritem",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 939..954,
                dest: "#^firstline1",
                kind: WikiLink,
                display_text: "#^firstline1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1007..1022,
                dest: "#^inneritem1",
                kind: WikiLink,
                display_text: "#^inneritem1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1074..1087,
                dest: "#^fulllst1",
                kind: WikiLink,
                display_text: "#^fulllst1",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1299..1312,
                dest: "#^tableref",
                kind: WikiLink,
                display_text: "#^tableref",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1469..1483,
                dest: "#^tableref3",
                kind: WikiLink,
                display_text: "#^tableref3",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1487..1501,
                dest: "#^tableref2",
                kind: WikiLink,
                display_text: "#^tableref2",
            },
            Reference {
                path: "tests/data/vaults/tt/block.md",
                range: 1643..1653,
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
                range: 1120..1188,
            },
            Heading {
                path: "tests/data/vaults/tt/block.md",
                level: H6,
                text: "Edge case: the number of blank lines before identifier doesn’t matter",
                range: 1551..1629,
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
        Document [0..1758]
          Paragraph [0..23]
            Text(Borrowed("paragraph 1 ")) [0..12]
            Text(Borrowed("^")) [12..13]
            Text(Borrowed("paragraph")) [13..22]
          Paragraph [24..48]
            Text(Borrowed("paragraph 2")) [24..35]
            SoftBreak [35..36]
            Text(Borrowed("^")) [36..37]
            Text(Borrowed("paragraph2")) [37..47]
          List(None) [49..92]
            Item [49..92]
              Text(Borrowed("some list")) [51..60]
              List(None) [60..92]
                Item [60..71]
                  Text(Borrowed("item 1")) [64..70]
                Item [70..92]
                  Text(Borrowed("item 2")) [74..80]
                  SoftBreak [80..81]
                  Text(Borrowed("^")) [81..82]
                  Text(Borrowed("fulllist")) [82..90]
          Table([None, None]) [92..179]
            TableHead [92..112]
              TableCell [93..101]
                Text(Borrowed("Col 1")) [94..99]
              TableCell [102..110]
                Text(Borrowed("Col 2")) [103..108]
            TableRow [132..152]
              TableCell [133..141]
                Text(Borrowed("Cell 1")) [134..140]
              TableCell [142..150]
                Text(Borrowed("Cell 2")) [143..149]
            TableRow [152..172]
              TableCell [153..161]
                Text(Borrowed("Cell 3")) [154..160]
              TableCell [162..170]
                Text(Borrowed("Cell 4")) [163..169]
            TableRow [172..179]
              TableCell [172..178]
                Text(Borrowed("^")) [172..173]
                Text(Borrowed("table")) [173..178]
              TableCell [179..179]
          BlockQuote(None) [180..203]
            Paragraph [182..203]
              Text(Borrowed("quotation")) [182..191]
              SoftBreak [191..192]
              Text(Borrowed("^")) [192..193]
              Text(Borrowed("quotation")) [193..202]
          Paragraph [204..220]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^quotation"), title: Borrowed(""), id: Borrowed("") } [204..218]
              Text(Borrowed("#^quotation")) [206..217]
          BlockQuote(None) [222..264]
            Paragraph [224..264]
              Text(Borrowed("[")) [224..225]
              Text(Borrowed("!info")) [225..230]
              Text(Borrowed("]")) [230..231]
              Text(Borrowed(" this is a info callout")) [231..254]
              SoftBreak [254..255]
              Text(Borrowed("^")) [255..256]
              Text(Borrowed("callout")) [256..263]
          Paragraph [265..279]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^callout"), title: Borrowed(""), id: Borrowed("") } [265..277]
              Text(Borrowed("#^callout")) [267..276]
          Rule [281..285]
          Paragraph [286..296]
            Text(Borrowed("reference")) [286..295]
          List(None) [296..619]
            Item [296..326]
              Text(Borrowed("paragraph: ")) [298..309]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^paragraph1"), title: Borrowed(""), id: Borrowed("") } [309..324]
                Text(Borrowed("#^paragraph1")) [311..323]
            Item [326..447]
              Text(Borrowed("separate line caret: ")) [328..349]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^paragraph2"), title: Borrowed(""), id: Borrowed("") } [349..364]
                Text(Borrowed("#^paragraph2")) [351..363]
              List(None) [365..447]
                Item [365..447]
                  Text(Borrowed("for paragraph the caret doesn")) [369..398]
                  Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [398..399]
                  Text(Borrowed("t need to have a blank line before and after it")) [399..446]
            Item [447..469]
              Text(Borrowed("table: ")) [449..456]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^table"), title: Borrowed(""), id: Borrowed("") } [456..466]
                Text(Borrowed("#^table")) [458..465]
            Item [469..492]
              Text(Borrowed("table: ")) [471..478]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^table2"), title: Borrowed(""), id: Borrowed("") } [478..489]
                Text(Borrowed("#^table2")) [480..488]
            Item [492..619]
              Text(Borrowed("table: ")) [494..501]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableagain"), title: Borrowed(""), id: Borrowed("") } [501..516]
                Text(Borrowed("#^tableagain")) [503..515]
              List(None) [517..619]
                Item [517..619]
                  Text(Borrowed("it looks like the block reference will always point to last non-empty non-block-reference struct")) [521..617]
          Rule [619..623]
          List(None) [624..1116]
            Item [624..675]
              Paragraph [626..651]
                Text(Borrowed("a nested list ")) [626..640]
                Text(Borrowed("^")) [640..641]
                Text(Borrowed("firstline")) [641..650]
              List(None) [650..675]
                Item [650..675]
                  Text(Borrowed("item")) [655..659]
                  SoftBreak [659..660]
                  Text(Borrowed("^")) [663..664]
                  Text(Borrowed("inneritem")) [664..673]
            Item [675..789]
              Paragraph [677..691]
                Text(Borrowed("inside a list")) [677..690]
              List(None) [690..789]
                Item [690..736]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline"), title: Borrowed(""), id: Borrowed("") } [694..708]
                    Text(Borrowed("#^firstline")) [696..707]
                  Text(Borrowed(": points to the first line")) [709..735]
                Item [735..789]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem"), title: Borrowed(""), id: Borrowed("") } [740..754]
                    Text(Borrowed("#^inneritem")) [742..753]
                  Text(Borrowed(": points to the first inner item")) [755..787]
            Item [789..856]
              Paragraph [791..854]
                Text(Borrowed("full reference to a list make its inner state not refereceable")) [791..853]
            Item [856..920]
              Paragraph [858..884]
                Text(Borrowed("a nested list ")) [858..872]
                Text(Borrowed("^")) [872..873]
                Text(Borrowed("firstline1")) [873..883]
              List(None) [883..920]
                Item [883..920]
                  Text(Borrowed("item")) [888..892]
                  SoftBreak [892..893]
                  Text(Borrowed("^")) [896..897]
                  Text(Borrowed("inneritem1")) [897..907]
                  SoftBreak [907..908]
                  Text(Borrowed("^")) [908..909]
                  Text(Borrowed("fulllist1")) [909..918]
            Item [920..1116]
              Paragraph [922..936]
                Text(Borrowed("inside a list")) [922..935]
              List(None) [935..1116]
                Item [935..1003]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^firstline1"), title: Borrowed(""), id: Borrowed("") } [939..954]
                    Text(Borrowed("#^firstline1")) [941..953]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [955..1002]
                Item [1002..1071]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^inneritem1"), title: Borrowed(""), id: Borrowed("") } [1007..1022]
                    Text(Borrowed("#^inneritem1")) [1009..1021]
                  Text(Borrowed(": this now breaks and fallback to the full note")) [1023..1070]
                Item [1070..1116]
                  Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^fulllst1"), title: Borrowed(""), id: Borrowed("") } [1074..1087]
                    Text(Borrowed("#^fulllst1")) [1076..1086]
                  Text(Borrowed(": points to the full list")) [1088..1113]
          Rule [1116..1120]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1120..1188]
            Text(Borrowed("Edge case: a later block identifier invalidate previous one")) [1128..1187]
          Table([None, None]) [1189..1269]
            TableHead [1189..1209]
              TableCell [1190..1198]
                Text(Borrowed("Col 1")) [1191..1196]
              TableCell [1199..1207]
                Text(Borrowed("Col 2")) [1200..1205]
            TableRow [1229..1249]
              TableCell [1230..1238]
                Text(Borrowed("Cell 1")) [1231..1237]
              TableCell [1239..1247]
                Text(Borrowed("Cell 2")) [1240..1246]
            TableRow [1249..1269]
              TableCell [1250..1258]
                Text(Borrowed("Cell 3")) [1251..1257]
              TableCell [1259..1267]
                Text(Borrowed("Cell 4")) [1260..1266]
          Paragraph [1270..1280]
            Text(Borrowed("^")) [1270..1271]
            Text(Borrowed("tableref")) [1271..1279]
          List(None) [1281..1316]
            Item [1281..1316]
              Text(Borrowed("this works fine ")) [1283..1299]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref"), title: Borrowed(""), id: Borrowed("") } [1299..1312]
                Text(Borrowed("#^tableref")) [1301..1311]
          Table([None, None]) [1316..1396]
            TableHead [1316..1336]
              TableCell [1317..1325]
                Text(Borrowed("Col 1")) [1318..1323]
              TableCell [1326..1334]
                Text(Borrowed("Col 2")) [1327..1332]
            TableRow [1356..1376]
              TableCell [1357..1365]
                Text(Borrowed("Cell 1")) [1358..1364]
              TableCell [1366..1374]
                Text(Borrowed("Cell 2")) [1367..1373]
            TableRow [1376..1396]
              TableCell [1377..1385]
                Text(Borrowed("Cell 3")) [1378..1384]
              TableCell [1386..1394]
                Text(Borrowed("Cell 4")) [1387..1393]
          Paragraph [1397..1408]
            Text(Borrowed("^")) [1397..1398]
            Text(Borrowed("tableref2")) [1398..1407]
          Paragraph [1409..1420]
            Text(Borrowed("^")) [1409..1410]
            Text(Borrowed("tableref3")) [1410..1419]
          List(None) [1421..1551]
            Item [1421..1485]
              Text(Borrowed("now the above table can only be referenced by ")) [1423..1469]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref3"), title: Borrowed(""), id: Borrowed("") } [1469..1483]
                Text(Borrowed("#^tableref3")) [1471..1482]
            Item [1485..1551]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^tableref2"), title: Borrowed(""), id: Borrowed("") } [1487..1501]
                Text(Borrowed("#^tableref2")) [1489..1500]
              Text(Borrowed(" is invalid and will fallback to the whole note")) [1502..1549]
          Heading { level: H6, id: None, classes: [], attrs: [] } [1551..1629]
            Text(Borrowed("Edge case: the number of blank lines before identifier doesn")) [1559..1619]
            Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [1619..1620]
            Text(Borrowed("t matter")) [1620..1628]
          Paragraph [1630..1642]
            Text(Borrowed("this")) [1630..1634]
            SoftBreak [1634..1635]
            Text(Borrowed("^")) [1635..1636]
            Text(Borrowed("works")) [1636..1641]
          Paragraph [1643..1655]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#^works"), title: Borrowed(""), id: Borrowed("") } [1643..1653]
              Text(Borrowed("#^works")) [1645..1652]
          Paragraph [1656..1758]
            Text(Borrowed("1 blank line after the identifier is required, however")) [1656..1710]
            SoftBreak [1710..1711]
            Text(Borrowed("0-n blank line before the identifier works fine")) [1711..1758]
        "##);
    }
}
