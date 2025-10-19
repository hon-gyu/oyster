#![allow(dead_code)] // reason: WIP
use crate::ast::{Node, NodeKind, Tree};
use pulldown_cmark::{HeadingLevel, LinkType};
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub enum InNoteReferenceable {
    Heading {
        level: HeadingLevel,
        range: Range<usize>,
    },
    // TODO: figure out what exactly blocks can be referenced
    Block,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Referenceable {
    Asset {
        path: String,
    },
    Note {
        path: String,
    },
    Heading {
        note_path: String,
        level: HeadingLevel,
        range: Range<usize>,
    },
    Block {
        note_path: String,
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

fn extract_reference_and_referenceable(
    node: &Node,
) -> (Vec<Reference>, Vec<InNoteReferenceable>) {
    let mut references = Vec::new();
    let mut referenceables = Vec::new();

    extract_reference_and_referenceable_helper(
        node,
        &mut references,
        &mut referenceables,
    );

    (references, referenceables)
}

fn extract_reference_and_referenceable_helper(
    node: &Node,
    references: &mut Vec<Reference>,
    referenceables: &mut Vec<InNoteReferenceable>,
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
                            panic!("Wikilink should have text");
                        }
                    };
                    let reference = Reference {
                        range: node.range.clone(),
                        dest: dest_url.to_string(),
                        kind: ReferenceKind::WikiLink,
                        display_text,
                    };
                    references.push(reference);
                }
                LinkType::Inline => {
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
                        range: node.range.clone(),
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
            let referenceable = InNoteReferenceable::Heading {
                level: level.clone(),
                range: node.range.clone(),
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
        extract_reference_and_referenceable_helper(
            child,
            references,
            referenceables,
        );
    }
}

fn build_links(
    references: Vec<Reference>,
    referenceable: Vec<Referenceable>,
) -> Vec<Link> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::{assert_debug_snapshot, assert_snapshot};

    #[test]
    fn test_parse_ast_with_links() {
        use std::fs;
        let path = "tests/tt/Note 1.md";
        let text = fs::read_to_string(path).unwrap();
        let tree = Tree::new(&text);
        assert_snapshot!(tree.root_node, @r##"
        Document [1..990]
          Heading { level: H3, id: None, classes: [], attrs: [] } [1..19]
            Text(Borrowed("Level 3 title")) [5..18]
          Heading { level: H4, id: None, classes: [], attrs: [] } [19..38]
            Text(Borrowed("Level 4 title")) [24..37]
          Heading { level: H3, id: None, classes: [], attrs: [] } [39..61]
            Text(Borrowed("Example (level 3)")) [43..60]
          Heading { level: H6, id: None, classes: [], attrs: [] } [62..83]
            Text(Borrowed("Markdown link")) [69..82]
          List(None) [83..377]
            Item [83..139]
              Link { link_type: Inline, dest_url: Borrowed("Three%20laws%20of%20motion.md"), title: Borrowed(""), id: Borrowed("") } [85..138]
                Text(Borrowed("Three laws of motion")) [86..106]
            Item [139..197]
              Text(Borrowed("same file heading:  ")) [141..161]
              Link { link_type: Inline, dest_url: Borrowed("#Level%203%20title"), title: Borrowed(""), id: Borrowed("") } [161..196]
                Text(Borrowed("Level 3 title")) [162..175]
            Item [197..262]
              Text(Borrowed("different file heading ")) [199..222]
              Link { link_type: Inline, dest_url: Borrowed("Note%202#Some%20level%202%20title"), title: Borrowed(""), id: Borrowed("") } [222..261]
                Text(Borrowed("22")) [223..225]
            Item [262..285]
              Text(Borrowed("empty link 1 ")) [264..277]
              Link { link_type: Inline, dest_url: Borrowed(""), title: Borrowed(""), id: Borrowed("") } [277..284]
                Text(Borrowed("www")) [278..281]
            Item [285..307]
              Text(Borrowed("empty link 2 ")) [287..300]
              Link { link_type: Inline, dest_url: Borrowed("ww"), title: Borrowed(""), id: Borrowed("") } [300..306]
            Item [307..327]
              Text(Borrowed("empty link 3 ")) [309..322]
              Link { link_type: Inline, dest_url: Borrowed(""), title: Borrowed(""), id: Borrowed("") } [322..326]
            Item [327..377]
              Text(Borrowed("empty link 4 ")) [329..342]
              Link { link_type: Inline, dest_url: Borrowed("Three%20laws%20of%20motion.md"), title: Borrowed(""), id: Borrowed("") } [342..375]
          Heading { level: H6, id: None, classes: [], attrs: [] } [377..394]
            Text(Borrowed("Wiki link")) [384..393]
          List(None) [394..891]
            Item [394..421]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Three laws of motion"), title: Borrowed(""), id: Borrowed("") } [396..419]
                Text(Borrowed("Three laws of motion")) [398..418]
            Item [421..451]
              Text(Borrowed("pipe: ")) [423..429]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Note 2 "), title: Borrowed(""), id: Borrowed("") } [429..449]
                Text(Borrowed(" Note two")) [439..448]
            Item [451..498]
              Text(Borrowed("heading in the same file: ")) [453..479]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [479..496]
                Text(Borrowed("#Level 3 title")) [481..495]
            Item [498..552]
              Text(Borrowed("nested heading in the same file: ")) [500..533]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 4 title"), title: Borrowed(""), id: Borrowed("") } [533..550]
                Text(Borrowed("#Level 4 title")) [535..549]
            Item [552..609]
              Text(Borrowed("heading in another file: ")) [554..579]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title"), title: Borrowed(""), id: Borrowed("") } [579..607]
                Text(Borrowed("Note 2#Some level 2 title")) [581..606]
            Item [609..680]
              Text(Borrowed("heading in another file: ")) [611..636]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [636..678]
                Text(Borrowed("Note 2#Some level 2 title#Level 3 title")) [638..677]
            Item [680..732]
              Text(Borrowed("heading in another file: ")) [682..707]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [707..730]
                Text(Borrowed("Note 2#Level 3 title")) [709..729]
            Item [732..773]
              Text(Borrowed("heading in another file: ")) [734..759]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#L4"), title: Borrowed(""), id: Borrowed("") } [759..771]
                Text(Borrowed("Note 2#L4")) [761..770]
            Item [773..833]
              Text(Borrowed("heading in another file: ")) [775..800]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#L4"), title: Borrowed(""), id: Borrowed("") } [800..831]
                Text(Borrowed("Note 2#Some level 2 title#L4")) [802..830]
            Item [833..872]
              Text(Borrowed("broken link: ")) [835..848]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Non-existing note 4"), title: Borrowed(""), id: Borrowed("") } [848..870]
                Text(Borrowed("Non-existing note 4")) [850..869]
            Item [872..891]
              Text(Borrowed("empty link ")) [874..885]
              Text(Borrowed("[")) [885..886]
              Text(Borrowed("[")) [886..887]
              Text(Borrowed("]")) [887..888]
              Text(Borrowed("]")) [888..889]
          Heading { level: H6, id: None, classes: [], attrs: [] } [891..913]
            Text(Borrowed("Link to figure")) [898..912]
          Paragraph [913..930]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg"), title: Borrowed(""), id: Borrowed("") } [913..928]
              Text(Borrowed("Figure 1.jpg")) [915..927]
          Paragraph [931..949]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg"), title: Borrowed(""), id: Borrowed("") } [931..947]
              Text(Borrowed("Figure 1.jpg")) [934..946]
          Paragraph [950..970]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("empty_video.mp4"), title: Borrowed(""), id: Borrowed("") } [950..968]
              Text(Borrowed("empty_video.mp4")) [952..967]
          Paragraph [972..987]
            Text(Borrowed("empty heading?")) [972..986]
          Heading { level: H2, id: None, classes: [], attrs: [] } [987..990]
        "##);
    }

    #[test]
    fn test_exract_references_and_referenceables() {
        use std::fs;
        let path = "tests/tt/Note 1.md";
        let text = fs::read_to_string(path).unwrap();
        let tree = Tree::new(&text);
        let (references, referenceables) =
            extract_reference_and_referenceable(&tree.root_node);
        assert_debug_snapshot!(references, @r##"
        [
            Reference {
                range: 85..138,
                dest: "Three laws of motion",
                kind: MarkdownLink,
                display_text: "Three laws of motion",
            },
            Reference {
                range: 161..196,
                dest: "#Level 3 title",
                kind: MarkdownLink,
                display_text: "Level 3 title",
            },
            Reference {
                range: 222..261,
                dest: "Note 2#Some level 2 title",
                kind: MarkdownLink,
                display_text: "22",
            },
            Reference {
                range: 277..284,
                dest: "()",
                kind: MarkdownLink,
                display_text: "www",
            },
            Reference {
                range: 300..306,
                dest: "ww",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 322..326,
                dest: "()",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 342..375,
                dest: "Three laws of motion",
                kind: MarkdownLink,
                display_text: "",
            },
            Reference {
                range: 396..419,
                dest: "Three laws of motion",
                kind: WikiLink,
                display_text: "Three laws of motion",
            },
            Reference {
                range: 429..449,
                dest: "Note 2 ",
                kind: WikiLink,
                display_text: " Note two",
            },
            Reference {
                range: 479..496,
                dest: "#Level 3 title",
                kind: WikiLink,
                display_text: "#Level 3 title",
            },
            Reference {
                range: 533..550,
                dest: "#Level 4 title",
                kind: WikiLink,
                display_text: "#Level 4 title",
            },
            Reference {
                range: 579..607,
                dest: "Note 2#Some level 2 title",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title",
            },
            Reference {
                range: 636..678,
                dest: "Note 2#Some level 2 title#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title#Level 3 title",
            },
            Reference {
                range: 707..730,
                dest: "Note 2#Level 3 title",
                kind: WikiLink,
                display_text: "Note 2#Level 3 title",
            },
            Reference {
                range: 759..771,
                dest: "Note 2#L4",
                kind: WikiLink,
                display_text: "Note 2#L4",
            },
            Reference {
                range: 800..831,
                dest: "Note 2#Some level 2 title#L4",
                kind: WikiLink,
                display_text: "Note 2#Some level 2 title#L4",
            },
            Reference {
                range: 848..870,
                dest: "Non-existing note 4",
                kind: WikiLink,
                display_text: "Non-existing note 4",
            },
            Reference {
                range: 913..928,
                dest: "Figure 1.jpg",
                kind: WikiLink,
                display_text: "Figure 1.jpg",
            },
            Reference {
                range: 931..947,
                dest: "Figure 1.jpg",
                kind: WikiLink,
                display_text: "Figure 1.jpg",
            },
            Reference {
                range: 950..968,
                dest: "empty_video.mp4",
                kind: WikiLink,
                display_text: "empty_video.mp4",
            },
        ]
        "##);
        assert_debug_snapshot!(referenceables, @r"
        [
            Heading {
                level: H3,
                range: 1..19,
            },
            Heading {
                level: H4,
                range: 19..38,
            },
            Heading {
                level: H3,
                range: 39..61,
            },
            Heading {
                level: H6,
                range: 62..83,
            },
            Heading {
                level: H6,
                range: 377..394,
            },
            Heading {
                level: H6,
                range: 891..913,
            },
            Heading {
                level: H2,
                range: 987..990,
            },
        ]
        ");
    }
}
