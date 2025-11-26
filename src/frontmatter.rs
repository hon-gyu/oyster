use crate::ast::Tree;
use crate::link::Referenceable;
use crate::link::build_links;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::link::extract::scan_note;
    use insta::{assert_debug_snapshot, assert_snapshot};

    #[test]
    fn test_parse() {
        let path = std::path::PathBuf::from(
            "tests/data/vaults/frontmatter/article.md",
        );
        let md_src = std::fs::read_to_string(&path).unwrap();

        let tree = Tree::new(&md_src);
        assert_snapshot!(&tree.root_node, @r#"
        Document [0..243]
          MetadataBlock(YamlStyle) [0..135]
            Text(Borrowed("title: Getting Started\nauthor: Jane Doe\ndate: 2024-03-15\ntags:\n  - programming\n  - tutorial\ncategory: tutorials\npublished: true\n")) [4..132]
          Heading { level: H1, id: None, classes: [], attrs: [] } [137..155]
            Text(Borrowed("Getting Started")) [139..154]
          Paragraph [156..210]
            Text(Borrowed("This is a comprehensive guide to getting with started")) [156..209]
          Heading { level: H2, id: None, classes: [], attrs: [] } [211..227]
            Text(Borrowed("Installation")) [214..226]
          Paragraph [228..243]
            Text(Borrowed("First, install")) [228..242]
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
            "published": Bool(true),
        }
        "#);
    }
}
