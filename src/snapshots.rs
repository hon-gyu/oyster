#[allow(dead_code, unused_imports)]
#[cfg(test)]
mod tests {
    use crate::ast::Tree;
    use crate::export::content::render_content;
    use crate::link::Referenceable;
    use crate::link::build_links;

    use super::*;
    use crate::link::extract::scan_note;
    use insta::{assert_debug_snapshot, assert_snapshot};

    #[test]
    fn embed_parse() {
        let path =
            std::path::PathBuf::from("tests/data/vaults/embed_file/note_II.md");
        let md_src = std::fs::read_to_string(&path).unwrap();
        let tree = Tree::new(&md_src);
        assert_snapshot!(&tree.root_node, @r#"
        Document [0..14]
          Paragraph [0..14]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("note_III"), title: Borrowed(""), id: Borrowed("") } [0..12]
              Text(Borrowed("note_III")) [3..11]
        "#);
    }

    #[test]
    fn mermaid_parse() {
        let path =
            std::path::PathBuf::from("tests/data/vaults/mermaid/note.md");
        let md_src = std::fs::read_to_string(&path).unwrap();
        let tree = Tree::new(&md_src);
        assert_snapshot!(&tree.root_node, @r#"
        Document [0..197]
          CodeBlock(Fenced(Borrowed("mermaid"))) [0..43]
            Text(Borrowed("graph TD\n    A-->B\n    B-->C\n")) [11..40]
          CodeBlock(Fenced(Borrowed("mermaid"))) [46..177]
            Text(Borrowed("sequenceDiagram\n    Alice->>John: Hello John, how are you?\n    John-->>Alice: Great!\n    Alice-)John: See you later!\n")) [57..174]
          CodeBlock(Fenced(Borrowed("mermaid"))) [179..197]
            Text(Borrowed("err\n")) [190..194]
        "#);
    }

    #[test]
    fn latex_parse() {
        let path =
            std::path::PathBuf::from("tests/data/vaults/latex/basic-math.md");
        let md_src = std::fs::read_to_string(&path).unwrap();
        let tree = Tree::new(&md_src);
        assert_snapshot!(&tree.root_node, @r#"
        Document [0..363]
          Heading { level: H1, id: None, classes: [], attrs: [] } [0..22]
            Text(Borrowed("Basic Math Examples")) [2..21]
          Heading { level: H2, id: None, classes: [], attrs: [] } [23..38]
            Text(Borrowed("Inline Math")) [26..37]
          Paragraph [39..86]
            Text(Borrowed("Einstein")) [39..47]
            Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [47..48]
            Text(Borrowed("s ")) [48..50]
            InlineMath(Borrowed("E = mc^2")) [50..60]
            Text(Borrowed(" changed physics forever.")) [60..85]
          Paragraph [87..148]
            Text(Borrowed("Quadratic formula: ")) [87..106]
            InlineMath(Borrowed("x = \\frac{-b \\pm \\sqrt{b^2 - 4ac}}{2a}")) [106..146]
            Text(Borrowed(".")) [146..147]
          Paragraph [149..181]
            Text(Borrowed("Simple variables: ")) [149..167]
            InlineMath(Borrowed("x")) [167..170]
            Text(Borrowed(", ")) [170..172]
            InlineMath(Borrowed("y")) [172..175]
            Text(Borrowed(", ")) [175..177]
            InlineMath(Borrowed("z")) [177..180]
          Heading { level: H2, id: None, classes: [], attrs: [] } [182..198]
            Text(Borrowed("Display Math")) [185..197]
          Paragraph [199..222]
            Text(Borrowed("The Gaussian integral:")) [199..221]
          Paragraph [223..278]
            DisplayMath(Boxed("\n\\int_{-\\infty}^{\\infty} e^{-x^2} dx = \\sqrt{\\pi}\n")) [223..277]
          Paragraph [279..297]
            Text(Borrowed("Euler")) [279..284]
            Text(Inlined(InlineStr { inner: [226, 128, 153, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], len: 3 })) [284..285]
            Text(Borrowed("s identity:")) [285..296]
          Paragraph [298..321]
            DisplayMath(Boxed("\ne^{i\\pi} + 1 = 0\n")) [298..320]
          Paragraph [322..344]
            Text(Borrowed("The area of a circle:")) [322..343]
          Paragraph [345..363]
            DisplayMath(Boxed("\nA = \\pi r^2\n")) [345..362]
        "#);
    }

    #[test]
    fn callout() {
        let path = std::path::PathBuf::from("tests/data/notes/callout.md");
        let md_src = std::fs::read_to_string(&path).unwrap();
        let tree = Tree::new(&md_src);
        assert_snapshot!(&tree.root_node, @r#"
        Document [0..442]
          BlockQuote [0..17]
            Paragraph [2..17]
              Text(Borrowed("This is a note")) [2..16]
          Callout { kind: GFM(Tip), title: None, foldable: None, content_start_byte: 27 } [18..48]
            Paragraph [20..48]
              Text(Borrowed("[")) [20..21]
              Text(Borrowed("!Tip")) [21..25]
              Text(Borrowed("]")) [25..26]
              SoftBreak [26..27]
              Text(Borrowed("tip with one space")) [29..47]
          Callout { kind: GFM(Tip), title: None, foldable: None, content_start_byte: 58 } [50..80]
            Paragraph [51..80]
              Text(Borrowed("[")) [51..52]
              Text(Borrowed("!Tip")) [52..56]
              Text(Borrowed("]")) [56..57]
              SoftBreak [57..58]
              Text(Borrowed("tip with zero space")) [60..79]
          BlockQuote [82..145]
            Paragraph [85..145]
              Text(Borrowed("[")) [85..86]
              Text(Borrowed("!Tip")) [86..90]
              Text(Borrowed("]")) [90..91]
              SoftBreak [91..92]
              Text(Borrowed("tip with 2 space -> not a callout, just blockquote")) [94..144]
          Callout { kind: GFM(Tip), title: Some("title"), foldable: None, content_start_byte: 164 } [147..189]
            Paragraph [149..189]
              Text(Borrowed("[")) [149..150]
              Text(Borrowed("!Tip")) [150..154]
              Text(Borrowed("]")) [154..155]
              Text(Borrowed(" title")) [155..161]
              SoftBreak [161..162]
              Text(Borrowed("This is a tip with title")) [164..188]
          Callout { kind: GFM(Tip), title: Some("title"), foldable: None, content_start_byte: 206 } [191..206]
            Paragraph [193..206]
              Text(Borrowed("[")) [193..194]
              Text(Borrowed("!Tip")) [194..198]
              Text(Borrowed("]")) [198..199]
              Text(Borrowed(" title")) [199..205]
          BlockQuote [207..240]
            Paragraph [209..240]
              Text(Borrowed("This is a separate block quote")) [209..239]
          Callout { kind: GFM(Warning), title: None, foldable: None, content_start_byte: 254 } [241..274]
            Paragraph [243..274]
              Text(Borrowed("[")) [243..244]
              Text(Borrowed("!WARNING")) [244..252]
              Text(Borrowed("]")) [252..253]
              SoftBreak [253..254]
              Text(Borrowed("This is a warning")) [256..273]
          Callout { kind: Custom(Llm), title: None, foldable: None, content_start_byte: 284 } [275..311]
            Paragraph [277..311]
              Text(Borrowed("[")) [277..278]
              Text(Borrowed("!LLM")) [278..282]
              Text(Borrowed("]")) [282..283]
              SoftBreak [283..284]
              Text(Borrowed("This is generated by LLM")) [286..310]
          Callout { kind: Obsidian(Question), title: Some("Can callouts be nested?"), foldable: None, content_start_byte: 351 } [313..442]
            Paragraph [315..351]
              Text(Borrowed("[")) [315..316]
              Text(Borrowed("!question")) [316..325]
              Text(Borrowed("]")) [325..326]
              Text(Borrowed(" Can callouts be nested?")) [326..350]
            Callout { kind: Obsidian(Todo), title: Some("Yes!, they can."), foldable: None, content_start_byte: 379 } [353..442]
              Paragraph [355..379]
                Text(Borrowed("[")) [355..356]
                Text(Borrowed("!todo")) [356..361]
                Text(Borrowed("]")) [361..362]
                Text(Borrowed(" Yes!, they can.")) [362..378]
              Callout { kind: Obsidian(Example), title: Some("You can even use multiple layers of nesting."), foldable: None, content_start_byte: 442 } [383..442]
                Paragraph [385..442]
                  Text(Borrowed("[")) [385..386]
                  Text(Borrowed("!example")) [386..394]
                  Text(Borrowed("]")) [394..395]
                  Text(Borrowed("  You can even use multiple layers of nesting.")) [395..441]
        "#);
    }

    #[test]
    fn embed_file_extract() {
        let path =
            std::path::PathBuf::from("tests/data/vaults/embed_file/note.md");
        let (_fm_opt, references, _referenceables) = scan_note(&path);
        assert_debug_snapshot!(references, @r#"
        [
            Reference {
                kind: WikiLink,
                path: "tests/data/vaults/embed_file/note.md",
                range: 7..24,
                dest: "blue-image.png",
                display_text: "blue-image.png",
            },
            Reference {
                kind: Embed,
                path: "tests/data/vaults/embed_file/note.md",
                range: 42..60,
                dest: "blue-image.png",
                display_text: "blue-image.png",
            },
            Reference {
                kind: Embed,
                path: "tests/data/vaults/embed_file/note.md",
                range: 111..135,
                dest: "blue-image.png",
                display_text: " 200",
            },
            Reference {
                kind: Embed,
                path: "tests/data/vaults/embed_file/note.md",
                range: 167..195,
                dest: "blue-image.png",
                display_text: " 100x150",
            },
            Reference {
                kind: Embed,
                path: "tests/data/vaults/embed_file/note.md",
                range: 224..233,
                dest: "note2",
                display_text: "note2",
            },
            Reference {
                kind: Embed,
                path: "tests/data/vaults/embed_file/note.md",
                range: 283..310,
                dest: "note2#Heading in note 2",
                display_text: "note2#Heading in note 2",
            },
            Reference {
                kind: Embed,
                path: "tests/data/vaults/embed_file/note.md",
                range: 357..374,
                dest: "note2#^60a916",
                display_text: "note2#^60a916",
            },
            Reference {
                kind: Embed,
                path: "tests/data/vaults/embed_file/note.md",
                range: 427..444,
                dest: "note2#^7e162c",
                display_text: "note2#^7e162c",
            },
            Reference {
                kind: Embed,
                path: "tests/data/vaults/embed_file/note.md",
                range: 492..510,
                dest: "note2#^warning",
                display_text: "note2#^warning",
            },
        ]
        "#);
    }

    #[test]
    fn embed_file_parse() {
        let path =
            std::path::PathBuf::from("tests/data/vaults/embed_file/note.md");
        let md_src = std::fs::read_to_string(&path).unwrap();
        let tree = Tree::new(&md_src);
        assert_snapshot!(&tree.root_node, @r#"
        Document [1..511]
          Paragraph [1..26]
            Text(Borrowed("Image")) [1..6]
            SoftBreak [6..7]
            Link { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("blue-image.png"), title: Borrowed(""), id: Borrowed("") } [7..24]
              Text(Borrowed("blue-image.png")) [9..23]
          Paragraph [28..62]
            Text(Borrowed("Embeded Image")) [28..41]
            SoftBreak [41..42]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("blue-image.png"), title: Borrowed(""), id: Borrowed("") } [42..60]
              Text(Borrowed("blue-image.png")) [45..59]
          Paragraph [64..137]
            Text(Borrowed("Scale width according to original aspect ratio")) [64..110]
            SoftBreak [110..111]
            Image { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("blue-image.png "), title: Borrowed(""), id: Borrowed("") } [111..135]
              Text(Borrowed(" 200")) [130..134]
          Paragraph [138..197]
            Text(Borrowed("Resize with width and height")) [138..166]
            SoftBreak [166..167]
            Image { link_type: WikiLink { has_pothole: true }, dest_url: Borrowed("blue-image.png "), title: Borrowed(""), id: Borrowed("") } [167..195]
              Text(Borrowed(" 100x150")) [186..194]
          Paragraph [199..235]
            Text(Borrowed("Embed note: ")) [199..211]
            Code(Borrowed("![[note2]]")) [211..223]
            SoftBreak [223..224]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("note2"), title: Borrowed(""), id: Borrowed("") } [224..233]
              Text(Borrowed("note2")) [227..232]
          Paragraph [237..312]
            Text(Borrowed("Embed heading: ")) [237..252]
            Code(Borrowed("![[note2#Heading in note 2]]")) [252..282]
            SoftBreak [282..283]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("note2#Heading in note 2"), title: Borrowed(""), id: Borrowed("") } [283..310]
              Text(Borrowed("note2#Heading in note 2")) [286..309]
          Paragraph [314..376]
            Text(Borrowed("Embed block - a list: ")) [314..336]
            Code(Borrowed("![[note2#^60a916]]")) [336..356]
            SoftBreak [356..357]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("note2#^60a916"), title: Borrowed(""), id: Borrowed("") } [357..374]
              Text(Borrowed("note2#^60a916")) [360..373]
          Paragraph [378..446]
            Text(Borrowed("Embed block - a paragraph : ")) [378..406]
            Code(Borrowed("![[note2#^7e162c]]")) [406..426]
            SoftBreak [426..427]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("note2#^7e162c"), title: Borrowed(""), id: Borrowed("") } [427..444]
              Text(Borrowed("note2#^7e162c")) [430..443]
          Paragraph [447..511]
            Text(Borrowed("Embed block - callout: ")) [447..470]
            Code(Borrowed("![[note2#^warning]]")) [470..491]
            SoftBreak [491..492]
            Image { link_type: WikiLink { has_pothole: false }, dest_url: Borrowed("note2#^warning"), title: Borrowed(""), id: Borrowed("") } [492..510]
              Text(Borrowed("note2#^warning")) [495..509]
        "#);
    }

    #[test]
    fn frontmatter() {
        let path = std::path::PathBuf::from(
            "tests/data/vaults/frontmatter/article.md",
        );
        let md_src = std::fs::read_to_string(&path).unwrap();

        let tree = Tree::new(&md_src);
        assert_snapshot!(&tree.root_node, @r#"
        Document [0..241]
          MetadataBlock(YamlStyle) [0..133]
            Text(Borrowed("title: Getting Started\nauthor: Jane Doe\ndate: 2024-03-15\ntags:\n  - programming\n  - tutorial\ncategory: tutorials\npublish: true\n")) [4..130]
          Heading { level: H1, id: None, classes: [], attrs: [] } [135..153]
            Text(Borrowed("Getting Started")) [137..152]
          Paragraph [154..208]
            Text(Borrowed("This is a comprehensive guide to getting with started")) [154..207]
          Heading { level: H2, id: None, classes: [], attrs: [] } [209..225]
            Text(Borrowed("Installation")) [212..224]
          Paragraph [226..241]
            Text(Borrowed("First, install")) [226..240]
        "#);

        let (fm_opt, _, _) = scan_note(&path);

        let fm = fm_opt.unwrap();

        assert_debug_snapshot!(fm, @r#"
        Mapping {
            "title": String("Getting Started"),
            "author": String("Jane Doe"),
            "date": String("2024-03-15"),
            "tags": Sequence [
                String("programming"),
                String("tutorial"),
            ],
            "category": String("tutorials"),
            "publish": Bool(true),
        }
        "#);
    }
}
