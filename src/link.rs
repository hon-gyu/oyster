use crate::ast::{Node, Tree};
use crate::parse::parse;
use pulldown_cmark::HeadingLevel;
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

fn decode_url(url: &str) -> String {}

fn encode_url(url: &str) -> String {}

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
        Document [1..821]
          Heading { level: H3, id: None, classes: [], attrs: [] } [1..19]
            Text(Borrowed("Level 3 title")) [5..18]
          Heading { level: H4, id: None, classes: [], attrs: [] } [19..38]
            Text(Borrowed("Level 4 title")) [24..37]
          Heading { level: H3, id: None, classes: [], attrs: [] } [39..61]
            Text(Borrowed("Example (level 3)")) [43..60]
          Heading { level: H6, id: None, classes: [], attrs: [] } [62..83]
            Text(Borrowed("Markdown link")) [69..82]
          List(None) [83..263]
            Item [83..139]
              Link { link_type: Inline, dest_url: Borrowed("Three%20laws%20of%20motion.md"), title: Borrowed(""), id: Borrowed("") } [85..138]
                Text(Borrowed("Three laws of motion")) [86..106]
            Item [139..197]
              Text(Borrowed("same file heading:  ")) [141..161]
              Link { link_type: Inline, dest_url: Borrowed("#Level%203%20title"), title: Borrowed(""), id: Borrowed("") } [161..196]
                Text(Borrowed("Level 3 title")) [162..175]
            Item [197..263]
              Text(Borrowed("different file heading ")) [199..222]
              Link { link_type: Inline, dest_url: Borrowed("Note%202#Some%20level%202%20title"), title: Borrowed(""), id: Borrowed("") } [222..261]
                Text(Borrowed("22")) [223..225]
          Heading { level: H6, id: None, classes: [], attrs: [] } [263..280]
            Text(Borrowed("Wiki link")) [270..279]
          List(None) [280..759]
            Item [280..307]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Three laws of motion"), title: Borrowed(""), id: Borrowed("") } [282..305]
                Text(Borrowed("Three laws of motion")) [284..304]
            Item [307..337]
              Text(Borrowed("pipe: ")) [309..315]
              Link { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("Note 2 "), title: Borrowed(""), id: Borrowed("") } [315..335]
                Text(Borrowed(" Note two")) [325..334]
            Item [337..384]
              Text(Borrowed("heading in the same file: ")) [339..365]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [365..382]
                Text(Borrowed("#Level 3 title")) [367..381]
            Item [384..438]
              Text(Borrowed("nested heading in the same file: ")) [386..419]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("#Level 4 title"), title: Borrowed(""), id: Borrowed("") } [419..436]
                Text(Borrowed("#Level 4 title")) [421..435]
            Item [438..495]
              Text(Borrowed("heading in another file: ")) [440..465]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title"), title: Borrowed(""), id: Borrowed("") } [465..493]
                Text(Borrowed("Note 2#Some level 2 title")) [467..492]
            Item [495..566]
              Text(Borrowed("heading in another file: ")) [497..522]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [522..564]
                Text(Borrowed("Note 2#Some level 2 title#Level 3 title")) [524..563]
            Item [566..618]
              Text(Borrowed("heading in another file: ")) [568..593]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Level 3 title"), title: Borrowed(""), id: Borrowed("") } [593..616]
                Text(Borrowed("Note 2#Level 3 title")) [595..615]
            Item [618..659]
              Text(Borrowed("heading in another file: ")) [620..645]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#L4"), title: Borrowed(""), id: Borrowed("") } [645..657]
                Text(Borrowed("Note 2#L4")) [647..656]
            Item [659..719]
              Text(Borrowed("heading in another file: ")) [661..686]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Note 2#Some level 2 title#L4"), title: Borrowed(""), id: Borrowed("") } [686..717]
                Text(Borrowed("Note 2#Some level 2 title#L4")) [688..716]
            Item [719..759]
              Text(Borrowed("broken link: ")) [721..734]
              Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Non-existing note 4"), title: Borrowed(""), id: Borrowed("") } [734..756]
                Text(Borrowed("Non-existing note 4")) [736..755]
          Heading { level: H6, id: None, classes: [], attrs: [] } [759..781]
            Text(Borrowed("Link to figure")) [766..780]
          Paragraph [781..798]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg"), title: Borrowed(""), id: Borrowed("") } [781..796]
              Text(Borrowed("Figure 1.jpg")) [783..795]
          Paragraph [799..817]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("Figure 1.jpg"), title: Borrowed(""), id: Borrowed("") } [799..815]
              Text(Borrowed("Figure 1.jpg")) [802..814]
          Rule [817..821]
        "##);
    }
}
