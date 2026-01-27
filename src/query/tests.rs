//! Tests for the query module.

use super::types::Markdown;
use insta::assert_snapshot;

mod eval_tests {
    use insta::assert_debug_snapshot;

    use super::*;
    use crate::query::{
        CodeBlock,
        query::{Expr, eval},
    };

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
        assert_snapshot!(result[0].to_src(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        Preamble content.

        # Introduction

        Intro content here.

        ## Summary

        Summary content.

        # Conclusion

        Final thoughts.
        ");
    }

    #[test]
    fn test_eval_inc() {
        let md = test_doc();
        let result = eval(Expr::Inc(1), &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_string(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        Preamble content.
        └─(1)
            ├─[1.1] ## Introduction
            │   Intro content here.
            │   ├─[1.1.1] ### Details
            │   │   More details.
            │   └─[1.1.2] ### Summary
            │       Summary content.
            └─[1.2] ## Conclusion
                Final thoughts.
        ");
    }

    #[test]
    fn test_eval_inc_saturated() {
        let md = test_doc();
        let result = eval(Expr::Inc(10), &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_string(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        Preamble content.
        └─(1)
            └─(1.1)
                └─(1.1.1)
                    └─(1.1.1.1)
                        └─(1.1.1.1.1)
                            ├─[1.1.1.1.1.1] ###### Introduction
                            │   Intro content here.
                            ├─[1.1.1.1.1.2] ###### Details
                            │   More details.
                            ├─[1.1.1.1.1.3] ###### Summary
                            │   Summary content.
                            └─[1.1.1.1.1.4] ###### Conclusion
                                Final thoughts.
        ");
    }

    #[test]
    fn test_eval_dec() {
        let md = test_doc();
        let expr = Expr::Dec(1);
        let result = eval(expr, &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_string(), @r"
        ---
        title: Test Document
        tags:
        - rust
        - markdown
        ---

        (root)
        Preamble content.
        ├─[1] # Introduction
        │   Intro content here.
        ├─[2] # Details
        │   More details.
        ├─[3] # Summary
        │   Summary content.
        └─[4] # Conclusion
            Final thoughts.
        ");
    }

    // Code block tests
    // --------------------

    // TODO(critical): add codeblock that is indented
    // TODO(critical): add codeblock that is imbalancely indented
    // TODO(critical): add codeblock that starts with more than 3 backticks
    // TODO(critical): add codeblock that has more richful info string
    fn code_doc() -> Markdown {
        Markdown::new(
            r#"# Setup

```rust
fn main() {
    println!("hello");
}
```

Some text.

```python {version: 3.12}
print("hi")
```

````py
print("B")
````

All indent before print should be stripped.
   ````py
  print("C")
  ````

All but one indent before print should be stripped.
  ````py
   print("C")
 ````

## Details

```
plain block
```
"#,
        )
    }

    fn fmt_codeblocks(code_blocks: &Vec<CodeBlock>) -> String {
        code_blocks
            .iter()
            .map(|cb| cb.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[test]
    fn test_code_block_extraction() {
        let md = code_doc();
        let cbs1 = fmt_codeblocks(&md.sections.code_blocks);
        let cbs2 = fmt_codeblocks(&md.sections.children[0].code_blocks);
        assert_snapshot!(cbs1, @"");
        assert_snapshot!(cbs2, @r#"
        CodeBlock
        - language: rust
        - extra: None
        - range: {"loc":"3:1-7:4","bytes":[9,57]}

        CodeBlock
        - language: python
        - extra: {version: 3.12}
        - range: {"loc":"11:1-13:4","bytes":[71,112]}

        CodeBlock
        - language: py
        - extra: None
        - range: {"loc":"15:1-17:5","bytes":[114,136]}

        CodeBlock
        - language: py
        - extra: None
        - range: {"loc":"20:4-22:7","bytes":[185,211]}

        CodeBlock
        - language: py
        - extra: None
        - range: {"loc":"25:3-27:6","bytes":[267,293]}
        "#);
    }

    #[test]
    fn test_eval_code() {
        let md = code_doc();
        // Navigate to # Setup, then get first code block
        let expr =
            Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Code(0)));
        let result = eval(expr, &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_src(), @r#"
        fn main() {
            println!("hello");
        }
        "#);
    }

    #[test]
    fn test_eval_code_second_block() {
        let md = code_doc();
        let expr =
            Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Code(1)));
        let result = eval(expr, &md).unwrap();
        assert_eq!(result.len(), 1);
        assert_snapshot!(result[0].to_src(), @r#"
        print("hi")
        "#);
    }

    #[test]
    fn test_eval_code_out_of_bounds() {
        let md = code_doc();
        let expr =
            Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Code(10)));
        let result = eval(expr, &md);
        assert!(result.is_err());
    }

    #[test]
    fn test_eval_codemeta() {
        let md = code_doc();
        let expr = Expr::Pipe(
            Box::new(Expr::Del("Details".to_string())),
            Box::new(Expr::CodeMeta(1)),
        );
        let result = eval(expr, &md).unwrap();
        assert_eq!(result.len(), 1);
        let src = result[0].to_src();
        assert_snapshot!(src, @r#"
        {
          "content": "print(\"hi\")\n",
          "language": "python",
          "language_extra": "{version: 3.12}",
          "range": {
            "bytes": [
              71,
              112
            ],
            "loc": "11:1-13:4"
          }
        }
        "#);
    }

    #[test]
    fn test_eval_codemeta_all_indent_stripped() {
        let md = code_doc();
        let expr = Expr::Pipe(
            Box::new(Expr::Del("Details".to_string())),
            Box::new(Expr::CodeMeta(-2)),
        );
        let result = eval(expr, &md).unwrap();
        assert_eq!(result.len(), 1);
        let src = result[0].to_src();
        assert_snapshot!(src, @r#"
        {
          "content": "  print(\"C\")\n",
          "language": "py",
          "language_extra": null,
          "range": {
            "bytes": [
              185,
              211
            ],
            "loc": "20:4-22:7"
          }
        }
        "#);
    }

    #[test]
    fn test_eval_codemeta_all_but_one_indent_stripped() {
        let md = code_doc();
        let expr = Expr::Pipe(
            Box::new(Expr::Del("Details".to_string())),
            Box::new(Expr::CodeMeta(-1)),
        );
        let result = eval(expr, &md).unwrap();
        assert_eq!(result.len(), 1);
        let src = result[0].to_src();
        assert_snapshot!(src, @r#"
        {
          "content": "   print(\"C\")\n",
          "language": "py",
          "language_extra": null,
          "range": {
            "bytes": [
              267,
              293
            ],
            "loc": "25:3-27:6"
          }
        }
        "#);
    }
}
