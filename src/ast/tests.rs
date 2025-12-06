use super::*;
use insta::assert_snapshot;
use std::fs;

fn data() -> String {
    
    fs::read_to_string("tests/data/notes/basic.md").unwrap()
}

#[test]
fn test_positions_match_editor() {
    // Line 0: "First line"
    // Line 1: "Second line"
    // Line 2: "Third line"
    let text = r#"First line
Second line
Third line"#;
    let tree = Tree::new(text);

    // Root should span from (0,0) to (2,10)
    assert_eq!(tree.root_node.start_point.row, 0);
    assert_eq!(tree.root_node.start_point.column, 0);
    assert_eq!(tree.root_node.end_point.row, 2);
    assert_eq!(tree.root_node.end_point.column, 10);

    // Test with escaped newline - it should still count as a new line
    let text = r#"line 1\
line 2"#;
    let tree = Tree::new(text);

    // The \n creates a new line in the source, so positions should reflect that
    // Line 0: "line 1\"
    // Line 1: "line 2"
    assert_eq!(tree.root_node.start_point.row, 0);
    assert_eq!(tree.root_node.end_point.row, 1);
}

#[test]
fn test_build_ast() {
    let md = data();
    let ast = Tree::new(&md);
    assert_snapshot!(ast.root_node, @r#"
    Document [0..535]
      MetadataBlock(YamlStyle) [0..52]
        Text(Borrowed("title: \"Test Document\"\nauthor: \"Test Author\"\n")) [4..49]
      Heading { level: H1, id: None, classes: [], attrs: [] } [54..66]
        Text(Borrowed("Heading 1")) [56..65]
      Paragraph [67..117]
        Text(Borrowed("Basic text with ")) [67..83]
        Strong [83..91]
          Text(Borrowed("bold")) [85..89]
        Text(Borrowed(" and ")) [91..96]
        Emphasis [96..104]
          Text(Borrowed("italic")) [97..103]
        Text(Borrowed(" formatting.")) [104..116]
      Heading { level: H2, id: None, classes: [], attrs: [] } [118..128]
        Text(Borrowed("A List")) [121..127]
      List(None) [129..187]
        Item [129..164]
          Text(Borrowed("item 1")) [131..137]
          List(None) [140..164]
            Item [140..151]
              Text(Borrowed("item 1.1")) [142..150]
            Item [153..164]
              Text(Borrowed("item 1.2")) [155..163]
        Item [164..187]
          Text(Borrowed("item 2")) [166..172]
          List(None) [175..187]
            Item [175..187]
              Text(Borrowed("item 2.1")) [177..185]
      Heading { level: H2, id: None, classes: [], attrs: [] } [187..206]
        Text(Borrowed("Task Lists")) [190..200]
      List(None) [207..302]
        Item [207..228]
          TaskListMarker(true) [209..212]
          Text(Borrowed("Completed task")) [213..227]
        Item [228..302]
          TaskListMarker(false) [230..233]
          Text(Borrowed("Incomplete task")) [234..249]
          List(None) [252..302]
            Item [252..275]
              TaskListMarker(true) [254..257]
              Text(Borrowed("Nested completed")) [258..274]
            Item [277..302]
              TaskListMarker(false) [279..282]
              Text(Borrowed("Nested incomplete")) [283..300]
      Heading { level: H2, id: None, classes: [], attrs: [] } [302..319]
        Text(Borrowed("Strikethrough")) [305..318]
      Paragraph [320..349]
        Strikethrough [320..348]
          Text(Borrowed("This text is crossed out")) [322..346]
      Heading { level: H2, id: None, classes: [], attrs: [] } [350..364]
        Text(Borrowed("Code Block")) [353..363]
      CodeBlock(Fenced(Borrowed("python"))) [365..402]
        Text(Borrowed("def foo():\n    return 1\n")) [375..399]
      Heading { level: H2, id: None, classes: [], attrs: [] } [404..412]
        Text(Borrowed("Math")) [407..411]
      Paragraph [413..437]
        Text(Borrowed("Inline math: ")) [413..426]
        InlineMath(Borrowed("E = mc^2")) [426..436]
      Paragraph [438..505]
        Text(Borrowed("Block math:")) [438..449]
        SoftBreak [449..450]
        DisplayMath(Boxed("\n\\int_{-\\infty}^{\\infty} e^{-x^2} dx = \\sqrt{\\pi}\n")) [450..504]
      Heading { level: H3, id: None, classes: [], attrs: [] } [506..522]
        Text(Borrowed("Heading 3.1")) [510..521]
      Paragraph [523..535]
        Text(Borrowed("emoji: 😀")) [523..534]
    "#);
}
