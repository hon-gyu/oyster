use pulldown_cmark::{Parser, Tag};

#[cfg(test)]
mod tests {
    use super::*;
    use insta::{assert_debug_snapshot, assert_snapshot};
    use pulldown_cmark::{Event, Options, Parser};
    use std::fs;

    /// Default options for parsing markdown
    /// Enable all features except old footnotes
    fn default_opts() -> Options {
        let mut opts = Options::empty();
        opts.insert(Options::ENABLE_TABLES);
        opts.insert(Options::ENABLE_FOOTNOTES);
        opts.insert(Options::ENABLE_STRIKETHROUGH);
        opts.insert(Options::ENABLE_TASKLISTS);
        opts.insert(Options::ENABLE_SMART_PUNCTUATION);
        opts.insert(Options::ENABLE_HEADING_ATTRIBUTES);
        opts.insert(Options::ENABLE_YAML_STYLE_METADATA_BLOCKS);
        opts.insert(Options::ENABLE_PLUSES_DELIMITED_METADATA_BLOCKS);
        // opts.insert(Options::ENABLE_OLD_FOOTNOTES);
        opts.insert(Options::ENABLE_MATH);
        opts.insert(Options::ENABLE_GFM);
        opts.insert(Options::ENABLE_DEFINITION_LIST);
        opts.insert(Options::ENABLE_SUPERSCRIPT);
        opts.insert(Options::ENABLE_SUBSCRIPT);
        opts.insert(Options::ENABLE_WIKILINKS);
        opts
    }

    fn basic_data() -> String {
        // A markdown file with multiple features enabled in options
        let markdown = fs::read_to_string("src/basic.md").unwrap();
        markdown.to_owned()
    }

    fn extended_data() -> String {
        let more = fs::read_to_string("src/extended.md").unwrap();
        basic_data() + more.as_str()
    }

    #[test]
    fn test_parse_markdown() {
        let text = basic_data();
        let opts = default_opts();
        let parser = Parser::new_ext(&text, opts);

        let events = parser.into_offset_iter().collect::<Vec<_>>();
        let event_strs: Vec<String> = events.iter().map(|e| format!("{:?}", e)).collect();
        let events_str = event_strs.join("\n");
        assert_snapshot!(events_str, @r#"
        (Start(MetadataBlock(YamlStyle)), 0..52)
        (Text(Borrowed("title: \"Test Document\"\nauthor: \"Test Author\"\n")), 4..49)
        (End(MetadataBlock(YamlStyle)), 0..52)
        (Start(Heading { level: H1, id: None, classes: [], attrs: [] }), 54..66)
        (Text(Borrowed("Heading 1")), 56..65)
        (End(Heading(H1)), 54..66)
        (Start(Paragraph), 67..117)
        (Text(Borrowed("Basic text with ")), 67..83)
        (Start(Strong), 83..91)
        (Text(Borrowed("bold")), 85..89)
        (End(Strong), 83..91)
        (Text(Borrowed(" and ")), 91..96)
        (Start(Emphasis), 96..104)
        (Text(Borrowed("italic")), 97..103)
        (End(Emphasis), 96..104)
        (Text(Borrowed(" formatting.")), 104..116)
        (End(Paragraph), 67..117)
        (Start(Heading { level: H2, id: None, classes: [], attrs: [] }), 118..128)
        (Text(Borrowed("A List")), 121..127)
        (End(Heading(H2)), 118..128)
        (Start(List(None)), 129..187)
        (Start(Item), 129..164)
        (Text(Borrowed("item 1")), 131..137)
        (Start(List(None)), 140..164)
        (Start(Item), 140..151)
        (Text(Borrowed("item 1.1")), 142..150)
        (End(Item), 140..151)
        (Start(Item), 153..164)
        (Text(Borrowed("item 1.2")), 155..163)
        (End(Item), 153..164)
        (End(List(false)), 140..164)
        (End(Item), 129..164)
        (Start(Item), 164..187)
        (Text(Borrowed("item 2")), 166..172)
        (Start(List(None)), 175..187)
        (Start(Item), 175..187)
        (Text(Borrowed("item 2.1")), 177..185)
        (End(Item), 175..187)
        (End(List(false)), 175..187)
        (End(Item), 164..187)
        (End(List(false)), 129..187)
        (Start(Heading { level: H2, id: None, classes: [], attrs: [] }), 187..206)
        (Text(Borrowed("Task Lists")), 190..200)
        (End(Heading(H2)), 187..206)
        (Start(List(None)), 207..302)
        (Start(Item), 207..228)
        (TaskListMarker(true), 209..212)
        (Text(Borrowed("Completed task")), 213..227)
        (End(Item), 207..228)
        (Start(Item), 228..302)
        (TaskListMarker(false), 230..233)
        (Text(Borrowed("Incomplete task")), 234..249)
        (Start(List(None)), 252..302)
        (Start(Item), 252..275)
        (TaskListMarker(true), 254..257)
        (Text(Borrowed("Nested completed")), 258..274)
        (End(Item), 252..275)
        (Start(Item), 277..302)
        (TaskListMarker(false), 279..282)
        (Text(Borrowed("Nested incomplete")), 283..300)
        (End(Item), 277..302)
        (End(List(false)), 252..302)
        (End(Item), 228..302)
        (End(List(false)), 207..302)
        (Start(Heading { level: H2, id: None, classes: [], attrs: [] }), 302..319)
        (Text(Borrowed("Strikethrough")), 305..318)
        (End(Heading(H2)), 302..319)
        (Start(Paragraph), 320..349)
        (Start(Strikethrough), 320..348)
        (Text(Borrowed("This text is crossed out")), 322..346)
        (End(Strikethrough), 320..348)
        (End(Paragraph), 320..349)
        (Start(Heading { level: H2, id: None, classes: [], attrs: [] }), 350..364)
        (Text(Borrowed("Code Block")), 353..363)
        (End(Heading(H2)), 350..364)
        (Start(CodeBlock(Fenced(Borrowed("python")))), 365..402)
        (Text(Borrowed("def foo():\n    return 1\n")), 375..399)
        (End(CodeBlock), 365..402)
        (Start(Heading { level: H2, id: None, classes: [], attrs: [] }), 404..412)
        (Text(Borrowed("Math")), 407..411)
        (End(Heading(H2)), 404..412)
        (Start(Paragraph), 413..437)
        (Text(Borrowed("Inline math: ")), 413..426)
        (InlineMath(Borrowed("E = mc^2")), 426..436)
        (End(Paragraph), 413..437)
        (Start(Paragraph), 438..505)
        (Text(Borrowed("Block math:")), 438..449)
        (SoftBreak, 449..450)
        (DisplayMath(Boxed("\n\\int_{-\\infty}^{\\infty} e^{-x^2} dx = \\sqrt{\\pi}\n")), 450..504)
        (End(Paragraph), 438..505)
        (Start(Heading { level: H3, id: None, classes: [], attrs: [] }), 506..522)
        (Text(Borrowed("Heading 3.1")), 510..521)
        (End(Heading(H3)), 506..522)
        "#);

        let text_len = text.len();
        assert_snapshot!(text_len, @"522");
    }
}
