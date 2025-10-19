//! Table of contents based on heading levels
use crate::ast::Tree;
use crate::parse::parse;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use insta::assert_snapshot;
    use pulldown_cmark::Parser;
    use std::fs;

    fn data() -> String {
        // let md = fs::read_to_string("src/basic.md").unwrap();
        let md = r#######"
# Heading

some text

## Heading 2

some text

### Heading 3

some text

#### Heading 4

some text

##### Heading 5

some text

###### Heading 6

some text

# Heading

some text
"#######;
        md.to_string()
    }

    #[test]
    fn test_doc() {
        let md = data();
        let tree = parse(&md);
        assert_snapshot!(tree.root_node, @r#"
        Document [1..179]
          Heading { level: H1, id: None, classes: [], attrs: [] } [1..11]
            Text(Borrowed("Heading")) [3..10]
          Paragraph [12..22]
            Text(Borrowed("some text")) [12..21]
          Heading { level: H2, id: None, classes: [], attrs: [] } [23..36]
            Text(Borrowed("Heading 2")) [26..35]
          Paragraph [37..47]
            Text(Borrowed("some text")) [37..46]
          Heading { level: H3, id: None, classes: [], attrs: [] } [48..62]
            Text(Borrowed("Heading 3")) [52..61]
          Paragraph [63..73]
            Text(Borrowed("some text")) [63..72]
          Heading { level: H4, id: None, classes: [], attrs: [] } [74..89]
            Text(Borrowed("Heading 4")) [79..88]
          Paragraph [90..100]
            Text(Borrowed("some text")) [90..99]
          Heading { level: H5, id: None, classes: [], attrs: [] } [101..117]
            Text(Borrowed("Heading 5")) [107..116]
          Paragraph [118..128]
            Text(Borrowed("some text")) [118..127]
          Heading { level: H6, id: None, classes: [], attrs: [] } [129..146]
            Text(Borrowed("Heading 6")) [136..145]
          Paragraph [147..157]
            Text(Borrowed("some text")) [147..156]
          Heading { level: H1, id: None, classes: [], attrs: [] } [158..168]
            Text(Borrowed("Heading")) [160..167]
          Paragraph [169..179]
            Text(Borrowed("some text")) [169..178]
        "#);
    }
}
