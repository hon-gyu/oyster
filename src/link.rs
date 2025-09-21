use crate::ast::{Node, NodeKind, Tree};
use crate::parse::parse;
use pulldown_cmark::{HeadingLevel, LinkType};
use std::ops::Range;

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
    // TODO: figure out what exactly blocks can be referenced
    Block,
}

pub enum ReferenceKind {
    WikiLink,
    MarkdownLink,
}

pub struct Reference {
    range: Range<usize>,
    dest: String,
    kind: ReferenceKind,
    display_text: Option<String>,
}

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
) -> (Vec<Reference>, Vec<Referenceable>) {
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
    referenceables: &mut Vec<Referenceable>,
) {
    // Sometimes we know that a node's children cannot contains more references
    // or referenceables.
    let skip_children = false;

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
                    let display_text = {
                        if !has_pothole {
                            None
                        } else {
                            // Take out the text from the link's first child
                            assert_eq!(node.child_count(), 1);
                            let text_node = &node.children[0];
                            let text =
                                if let NodeKind::Text(text) = &text_node.kind {
                                    Some(text.to_string())
                                } else {
                                    None
                                };
                            Some(text.expect("Wikilink should have text"))
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
                    let dest = percent_decode(dest_url);
                    todo!()
                }
                _ => {}
            }
            todo!()
        }
        // Referenceable
        NodeKind::Heading { level, .. } => {
            todo!()
        }
        NodeKind::List { .. } => {
            todo!("Block")
        }
        NodeKind::Paragraph { .. } => {
            todo!("Block")
        }
        _ => {}
    }

    if !skip_children {
        for child in node.children.iter() {
            extract_reference_and_referenceable_helper(
                child,
                references,
                referenceables,
            );
        }
    }
    todo!()
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
    use insta::assert_snapshot;

    #[test]
    fn test_parse_ast_with_links() {
        use std::fs;
        let path = "tests/tt/Note 1.md";
        let text = fs::read_to_string(path).unwrap();

        let tree = parse(&text);
        assert_snapshot!(tree.root_node, @r##"
        Document [1..941]
          Heading { level: H3, id: None, classes: [], attrs: [] } [1..19]
            Text(Borrowed("Level 3 title")) [5..18]
          Heading { level: H4, id: None, classes: [], attrs: [] } [19..38]
            Text(Borrowed("Level 4 title")) [24..37]
          Heading { level: H3, id: None, classes: [], attrs: [] } [39..61]
            Text(Borrowed("Example (level 3)")) [43..60]
          Heading { level: H6, id: None, classes: [], attrs: [] } [62..83]
            Text(Borrowed("Markdown link")) [69..82]
          List(None) [83..328]
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
            Item [307..328]
              Text(Borrowed("empty link 3 ")) [309..322]
              Link { link_type: Inline, dest_url: Borrowed(""), title: Borrowed(""), id: Borrowed("") } [322..326]
          Heading { level: H6, id: None, classes: [], attrs: [] } [328..345]
            Text(Borrowed("Wiki link")) [335..344]
          List(None) [345..842]
            Item [345..372]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Three laws of motion"), title: Borrowed(""), id: Borrowed("") } [347..370]
                Text(Borrowed("Three laws of motion")) [349..369]
            Item [372..402]
              Text(Borrowed("pipe: ")) [374..380]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Note 2 "), title: Borrowed(""), id: Borrowed("") } [380..400]
                Text(Borrowed(" Note two")) [390..399]
            Item [402..449]
              Text(Borrowed("heading in the same file: ")) [404..430]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [430..447]
                Text(Borrowed("#Level 3 title")) [432..446]
            Item [449..503]
              Text(Borrowed("nested heading in the same file: ")) [451..484]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 4 title"), title: Borrowed(""), id: Borrowed("") } [484..501]
                Text(Borrowed("#Level 4 title")) [486..500]
            Item [503..560]
              Text(Borrowed("heading in another file: ")) [505..530]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title"), title: Borrowed(""), id: Borrowed("") } [530..558]
                Text(Borrowed("Note 2#Some level 2 title")) [532..557]
            Item [560..631]
              Text(Borrowed("heading in another file: ")) [562..587]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [587..629]
                Text(Borrowed("Note 2#Some level 2 title#Level 3 title")) [589..628]
            Item [631..683]
              Text(Borrowed("heading in another file: ")) [633..658]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [658..681]
                Text(Borrowed("Note 2#Level 3 title")) [660..680]
            Item [683..724]
              Text(Borrowed("heading in another file: ")) [685..710]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#L4"), title: Borrowed(""), id: Borrowed("") } [710..722]
                Text(Borrowed("Note 2#L4")) [712..721]
            Item [724..784]
              Text(Borrowed("heading in another file: ")) [726..751]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#L4"), title: Borrowed(""), id: Borrowed("") } [751..782]
                Text(Borrowed("Note 2#Some level 2 title#L4")) [753..781]
            Item [784..823]
              Text(Borrowed("broken link: ")) [786..799]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Non-existing note 4"), title: Borrowed(""), id: Borrowed("") } [799..821]
                Text(Borrowed("Non-existing note 4")) [801..820]
            Item [823..842]
              Text(Borrowed("empty link ")) [825..836]
              Text(Borrowed("[")) [836..837]
              Text(Borrowed("[")) [837..838]
              Text(Borrowed("]")) [838..839]
              Text(Borrowed("]")) [839..840]
          Heading { level: H6, id: None, classes: [], attrs: [] } [842..864]
            Text(Borrowed("Link to figure")) [849..863]
          Paragraph [864..881]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg"), title: Borrowed(""), id: Borrowed("") } [864..879]
              Text(Borrowed("Figure 1.jpg")) [866..878]
          Paragraph [882..900]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg"), title: Borrowed(""), id: Borrowed("") } [882..898]
              Text(Borrowed("Figure 1.jpg")) [885..897]
          Paragraph [901..921]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("empty_video.mp4"), title: Borrowed(""), id: Borrowed("") } [901..919]
              Text(Borrowed("empty_video.mp4")) [903..918]
          Paragraph [923..938]
            Text(Borrowed("empty heading?")) [923..937]
          Heading { level: H2, id: None, classes: [], attrs: [] } [938..941]
        "##);
    }
}
