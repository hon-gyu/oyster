//! Tests for the query module.

use super::types::Markdown;
use insta::assert_snapshot;

mod eval_tests {
    use insta::assert_debug_snapshot;

    use super::*;
    use crate::query::query::{Expr, eval};

    fn test_doc_src() -> String {
        r#"---
title: Test Document
tags:
- rust
- markdown
---

Preamble content.

# Introduction

Intro content here.

## Details

More details.

## Summary

Summary content.

# Conclusion

Final thoughts.
"#
        .to_string()
    }

    fn test_doc() -> Markdown {
        Markdown::new(&test_doc_src())
    }

    fn assert_eq_up_to_newlines(left: &str, right: &str) {
        assert_eq!(left.trim_end_matches('\n'), right.trim_end_matches('\n'));
    }

    #[test]
    fn test_eval_identity() {
        let md = test_doc();
        let md2 = eval(Expr::Identity, &md).unwrap();
        assert_eq!(md2.len(), 1);
        let md2_src = md2[0].to_src();
        // Compare up to trailing newlines
        assert_eq_up_to_newlines(&md2_src, &test_doc_src());
    }

    #[test]
    fn test_eval_body() {
        let md = test_doc();
        let result = eval(Expr::Body, &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_string(), @r"
        (root)
        Preamble content.
        ├─[1] # Introduction
        │   Intro content here.
        │   ├─[1.1] ## Details
        │   │   More details.
        │   └─[1.2] ## Summary
        │       Summary content.
        └─[2] # Conclusion
            Final thoughts.
        ");
    }

    #[test]
    fn test_eval_preface() {
        let md = test_doc();
        let result = eval(Expr::Preface, &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_src(), @"Preamble content.");
    }

    #[test]
    fn test_eval_index() {
        let md = test_doc();
        // First child (Introduction)
        let result = eval(Expr::Index(0), &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_string(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        └─[1] # Introduction
            Intro content here.
            ├─[1.1] ## Details
            │   More details.
            └─[1.2] ## Summary
                Summary content.
        ");
    }

    #[test]
    fn test_eval_index_negative() {
        let md = test_doc();
        // Last child (Conclusion)
        let result = eval(Expr::Index(-1), &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_string(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        └─[1] # Conclusion
            Final thoughts.
        ");
    }

    #[test]
    fn test_eval_index_out_of_bounds() {
        let md = test_doc();
        let result = eval(Expr::Index(100), &md);
        assert!(result.is_err());
        assert_debug_snapshot!(result.unwrap_err(), @r"
        IndexOutOfBounds(
            100,
        )
        ")
    }

    #[test]
    fn test_eval_field() {
        let md = test_doc();
        let result = eval(Expr::Field("Details".to_string()), &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_string(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        └─(1)
            └─[1.1] ## Details
                More details.
        ");
    }

    #[test]
    fn test_eval_field_not_found() {
        let md = test_doc();
        let result = eval(Expr::Field("Nonexistent".to_string()), &md);
        assert!(result.is_err());
        assert_debug_snapshot!(result.unwrap_err(), @r#"
        FieldNotFound(
            "Nonexistent",
        )
        "#);
    }

    #[test]
    fn test_eval_slice() {
        let md = test_doc();
        // Get first child only [0:1]
        let result = eval(Expr::Slice(Some(0), Some(1)), &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_string(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        ├─[1] # Introduction
        │   Intro content here.
        │   ├─[1.1] ## Details
        │   │   More details.
        │   └─[1.2] ## Summary
        │       Summary content.
        └─[2] # Conclusion
            Final thoughts.
        ");
    }

    #[test]
    fn test_eval_slice_open_end() {
        let md = test_doc();
        // Get all children [0:] - returns single Markdown with all children
        let result = eval(Expr::Slice(Some(0), None), &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_string(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        ├─[1] # Introduction
        │   Intro content here.
        │   ├─[1.1] ## Details
        │   │   More details.
        │   └─[1.2] ## Summary
        │       Summary content.
        └─[2] # Conclusion
            Final thoughts.
        ");
    }

    #[test]
    fn test_eval_pipe() {
        let md = test_doc();
        // Get first child, then its title
        let expr =
            Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Summary));
        let result = eval(expr, &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_src(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        └─[1] # Introduction
            Intro content here.
            ├─[1.1] ## Details
            │   More details.
            └─[1.2] ## Summary
                Summary content.
        ");
    }

    #[test]
    fn test_eval_comma() {
        let md = test_doc();
        // Get both children's summaries
        let expr = Expr::Comma(vec![
            Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Summary)),
            Expr::Pipe(Box::new(Expr::Index(1)), Box::new(Expr::Summary)),
        ]);
        let result = eval(expr, &md).unwrap();
        assert_eq!(result.len(), 2);
        let output: String = result
            .iter()
            .map(|m| m.to_src().trim().to_string())
            .collect::<Vec<_>>()
            .join("\n\n<><><><>TEST SEPARATOR<><><><>\n\n");
        assert_snapshot!(output, @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        └─[1] # Introduction
            Intro content here.
            ├─[1.1] ## Details
            │   More details.
            └─[1.2] ## Summary
                Summary content.

        <><><><>TEST SEPARATOR<><><><>

        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        └─[1] # Conclusion
            Final thoughts.
        ");
    }

    #[test]
    fn test_eval_title() {
        let md = test_doc();
        // Get title of first child
        let expr = Expr::Title(0);
        let result = eval(expr, &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_src(), @"Introduction");
    }

    #[test]
    fn test_eval_nchildren() {
        let md = test_doc();
        let result = eval(Expr::NChildren, &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_src(), @"2");
    }

    #[test]
    fn test_eval_frontmatter() {
        let md = test_doc();
        let result = eval(Expr::Frontmatter, &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_src(), @r"
        title: Test Document
        tags:
        - rust
        - markdown
        ");
    }

    #[test]
    fn test_eval_has_true() {
        let md = test_doc();
        let result = eval(Expr::Has("Details".to_string()), &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].to_src().trim(), "true");
    }

    #[test]
    fn test_eval_has_false() {
        let md = test_doc();
        let result = eval(Expr::Has("Nonexistent".to_string()), &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].to_src().trim(), "false");
    }

    #[test]
    fn test_eval_del() {
        let md = test_doc();
        let result = eval(Expr::Del("Details".to_string()), &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_string(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        └─[1] # Conclusion
            Final thoughts.
        ");
    }

    // #[test]
    // fn test_eval_inc() {
    //     let md = test_doc();
    //     let result = eval(Expr::Inc, &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_src(), @"");
    // }

    // #[test]
    // fn test_eval_dec() {
    //     let md = test_doc();
    //     // First get a child section (which has H1), then decrement
    //     let expr = Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Dec));
    //     let result = eval(expr, &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_src(), @"");
    // }

    // #[test]
    // fn test_eval_range() {
    //     let md = test_doc();
    //     // Get range of first child
    //     let expr = Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Range));
    //     let result = eval(expr, &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     // Range format: "line:col-line:col"
    //     let range_str = result[0].to_src();
    //     assert!(range_str.contains(':'), "Range should contain colons");
    //     assert!(range_str.contains('-'), "Range should contain dash");
    // }
}
