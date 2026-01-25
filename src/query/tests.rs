//! Tests for the query module.

use super::parser::{Boundary, build_sections};
use super::types::Markdown;
use crate::ast::Tree;
use insta::assert_snapshot;

#[test]
fn test_build_sections_basic() {
    let source = r#"# Title

Some intro content.

## Section A

Content of section A.

### Subsection A.1

Details here.

## Section B

Final thoughts.
"#;
    let tree = Tree::new(source, false);
    let sections =
        build_sections(&tree.root_node, source, Boundary::zero()).unwrap();
    assert_snapshot!(sections.to_string(), @r"
    [root]
    └─[1] # Title
        Some intro content.
        ├─[1.1] ## Section A
        │   Content of section A.
        │   └─[1.1.1] ### Subsection A.1
        │       Details here.
        └─[1.2] ## Section B
            Final thoughts.
    ");
}

#[test]
fn test_build_sections_with_preamble() {
    let source = r#"This is content before any heading.

# First Heading

Some content.
"#;
    let tree = Tree::new(source, false);
    let sections =
        build_sections(&tree.root_node, source, Boundary::zero()).unwrap();
    assert_snapshot!(sections.to_string(), @r"
    [root]
    This is content before any heading.
    └─[1] # First Heading
        Some content.
    ");
}

#[test]
fn test_build_sections_with_frontmatter() {
    let source = r#"---
title: Test Document
---

Some preamble.

## Introduction

Intro content.
"#;
    let tree = Tree::new(source, false);
    let sections =
        build_sections(&tree.root_node, source, Boundary::zero()).unwrap();
    assert_snapshot!(sections.to_string(), @r"
    [root]
    └─[0]
        └─[0.1] ## Introduction
            Intro content.
    ");
}

#[test]
fn test_build_sections_gap_filling() {
    let source = r#"# Title

### Deep Section

Content.
"#;
    let tree = Tree::new(source, false);
    let sections =
        build_sections(&tree.root_node, source, Boundary::zero()).unwrap();
    assert_snapshot!(sections.to_string(), @r"
    [root]
    └─[1] # Title
        └─[1.0]
            └─[1.0.1] ### Deep Section
                Content.
    ");
}

#[test]
fn test_query_file() {
    use std::io::Write;
    use tempfile::NamedTempFile;

    let source = r#"---
title: Test Document
tags:
  - rust
  - markdown
---

Some preamble.

# Introduction

Intro content here.

## Details

More details.
"#;
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(source.as_bytes()).unwrap();

    let result = Markdown::from_path(file.path()).unwrap();
    let json = serde_json::to_string_pretty(&result).unwrap();
    assert_snapshot!(json, @r###"
    {
      "frontmatter": {
        "value": {
          "title": "Test Document",
          "tags": [
            "rust",
            "markdown"
          ]
        },
        "range": {
          "loc": "1:1-6:4",
          "bytes": [
            0,
            56
          ]
        }
      },
      "sections": {
        "heading": null,
        "path": "root",
        "content": "Some preamble.",
        "range": {
          "loc": "6:4-17:1",
          "bytes": [
            56,
            137
          ]
        },
        "children": [
          {
            "heading": {
              "level": 1,
              "text": "# Introduction\n",
              "range": {
                "loc": "10:1-11:1",
                "bytes": [
                  74,
                  89
                ]
              }
            },
            "path": "1",
            "content": "Intro content here.",
            "range": {
              "loc": "10:1-17:1",
              "bytes": [
                74,
                137
              ]
            },
            "children": [
              {
                "heading": {
                  "level": 2,
                  "text": "## Details\n",
                  "range": {
                    "loc": "14:1-15:1",
                    "bytes": [
                      111,
                      122
                    ]
                  }
                },
                "path": "1.1",
                "content": "More details.",
                "range": {
                  "loc": "14:1-17:1",
                  "bytes": [
                    111,
                    137
                  ]
                },
                "children": []
              }
            ]
          }
        ]
      }
    }
    "###);
}

fn text_sparse_sections() -> String {
    r#"---
title: Test Document
tags:
  - rust
  - markdown
---

Some preamble.

# Introduction

Intro content here.

### Details

More details.

# Another L1 Heading

vdiqoj

#### Another Level 4

More content.

"#
    .to_string()
}

#[test]
fn test_query_file_padded() {
    use std::io::Write;
    use tempfile::NamedTempFile;

    let source = text_sparse_sections();
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(source.as_bytes()).unwrap();

    let result = Markdown::from_path(file.path()).unwrap();
    let json = serde_json::to_string_pretty(&result).unwrap();
    assert_snapshot!(json, @r#####"
    {
      "frontmatter": {
        "value": {
          "title": "Test Document",
          "tags": [
            "rust",
            "markdown"
          ]
        },
        "range": {
          "loc": "1:1-6:4",
          "bytes": [
            0,
            56
          ]
        }
      },
      "sections": {
        "heading": null,
        "path": "root",
        "content": "Some preamble.",
        "range": {
          "loc": "6:4-25:1",
          "bytes": [
            56,
            205
          ]
        },
        "children": [
          {
            "heading": {
              "level": 1,
              "text": "# Introduction\n",
              "range": {
                "loc": "10:1-11:1",
                "bytes": [
                  74,
                  89
                ]
              }
            },
            "path": "1",
            "content": "",
            "range": {
              "loc": "10:1-18:1",
              "bytes": [
                74,
                139
              ]
            },
            "children": [
              {
                "heading": {
                  "level": 2,
                  "text": "",
                  "range": {
                    "loc": "1:1-1:1",
                    "bytes": [
                      0,
                      0
                    ]
                  }
                },
                "path": "1.0",
                "content": "",
                "range": {
                  "loc": "14:1-18:1",
                  "bytes": [
                    111,
                    139
                  ]
                },
                "children": [
                  {
                    "heading": {
                      "level": 3,
                      "text": "### Details\n",
                      "range": {
                        "loc": "14:1-15:1",
                        "bytes": [
                          111,
                          123
                        ]
                      }
                    },
                    "path": "1.0.1",
                    "content": "More details.",
                    "range": {
                      "loc": "14:1-18:1",
                      "bytes": [
                        111,
                        139
                      ]
                    },
                    "children": []
                  }
                ]
              }
            ]
          },
          {
            "heading": {
              "level": 1,
              "text": "# Another L1 Heading\n",
              "range": {
                "loc": "18:1-19:1",
                "bytes": [
                  139,
                  160
                ]
              }
            },
            "path": "2",
            "content": "",
            "range": {
              "loc": "18:1-25:1",
              "bytes": [
                139,
                205
              ]
            },
            "children": [
              {
                "heading": {
                  "level": 2,
                  "text": "",
                  "range": {
                    "loc": "1:1-1:1",
                    "bytes": [
                      0,
                      0
                    ]
                  }
                },
                "path": "2.0",
                "content": "",
                "range": {
                  "loc": "1:1-25:1",
                  "bytes": [
                    0,
                    205
                  ]
                },
                "children": [
                  {
                    "heading": {
                      "level": 3,
                      "text": "",
                      "range": {
                        "loc": "1:1-1:1",
                        "bytes": [
                          0,
                          0
                        ]
                      }
                    },
                    "path": "2.0.0",
                    "content": "",
                    "range": {
                      "loc": "22:1-25:1",
                      "bytes": [
                        169,
                        205
                      ]
                    },
                    "children": [
                      {
                        "heading": {
                          "level": 4,
                          "text": "#### Another Level 4\n",
                          "range": {
                            "loc": "22:1-23:1",
                            "bytes": [
                              169,
                              190
                            ]
                          }
                        },
                        "path": "2.0.0.1",
                        "content": "More content.",
                        "range": {
                          "loc": "22:1-25:1",
                          "bytes": [
                            169,
                            205
                          ]
                        },
                        "children": []
                      }
                    ]
                  }
                ]
              }
            ]
          }
        ]
      }
    }
    "#####);
}

#[test]
fn test_json_roundtrip() {
    use std::io::Write;
    use tempfile::NamedTempFile;

    let source = r#"---
title: Roundtrip Test
---

Preamble content.

# Introduction

Some intro.

## Details

More details.
"#;
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(source.as_bytes()).unwrap();

    // Parse markdown to struct
    let original = Markdown::from_path(file.path()).unwrap();

    // Serialize to JSON
    let json = serde_json::to_string_pretty(&original).unwrap();

    // Deserialize back
    let roundtripped: Markdown = serde_json::from_str(&json).unwrap();

    // Re-serialize and compare
    let json2 = serde_json::to_string_pretty(&roundtripped).unwrap();

    assert_eq!(
        json, json2,
        "JSON roundtrip should produce identical output"
    );
}

#[test]
fn test_struct_roundtrip() {
    use std::io::Write;
    use tempfile::NamedTempFile;

    let source = r#"---
title: Struct Roundtrip
---

Preamble.

# Heading One

Content one.

## Subheading

Subcontent.
"#;
    let mut file = NamedTempFile::new().unwrap();
    file.write_all(source.as_bytes()).unwrap();

    // Parse markdown to struct
    let original = Markdown::from_path(file.path()).unwrap();

    // Serialize to JSON and back
    let json = serde_json::to_string(&original).unwrap();
    let roundtripped: Markdown = serde_json::from_str(&json).unwrap();

    // Compare structs directly
    assert_eq!(
        original, roundtripped,
        "Struct roundtrip should produce identical struct"
    );
}

#[test]
fn test_markdown_display_sparse_heading() {
    use std::io::Write;
    use tempfile::NamedTempFile;

    let source = text_sparse_sections();

    let mut file = NamedTempFile::new().unwrap();
    file.write_all(source.as_bytes()).unwrap();

    let result = Markdown::from_path(file.path()).unwrap();
    assert_snapshot!(result.to_string(), @r"
    ---
    title: Test Document
    tags:
    - rust
    - markdown
    ---

    [root]
    Some preamble.
    ├─[1] # Introduction
    │   └─[1.0]
    │       └─[1.0.1] ### Details
    │           More details.
    └─[2] # Another L1 Heading
        └─[2.0]
            └─[2.0.0]
                └─[2.0.0.1] #### Another Level 4
                    More content.
    ");
}

#[test]
fn test_markdown_reconstruction() {
    let source = r#"---
title: Reconstruction Test
tags:
  - markdown
---

Preamble content here.

# Introduction

Intro content here.

## Details

More details.

# Second Section

Final content.
"#;
    let result = Markdown::new(source);
    let reconstructed = result.to_src();

    assert_snapshot!(reconstructed, @r"
    ---
    title: Reconstruction Test
    tags:
    - markdown
    ---

    Preamble content here.

    # Introduction

    Intro content here.

    ## Details

    More details.

    # Second Section

    Final content.
    ");
}

#[test]
fn test_roundtrip_markdown_to_struct_to_markdown() {
    let original_source = r#"---
title: Roundtrip Test
---

Preamble.

# Heading 1

Content 1.

## Heading 2

Content 2.
"#;

    let parsed = Markdown::new(original_source);

    // struct -> markdown
    let reconstructed = parsed.to_src();

    // markdown -> struct (again)
    let reparsed = Markdown::new(reconstructed.as_str());

    // Compare the two structs
    assert_eq!(
        parsed, reparsed,
        "markdown -> struct -> markdown -> struct should be idempotent"
    );
}

#[test]
fn test_roundtrip_struct_to_markdown_to_struct() {
    let source = r#"---
title: Struct Roundtrip
tags:
  - test
---

Root content.

# Section 1

Section 1 content.

### Deep Section

Deep content.

# Section 2

Section 2 content.
"#;
    // markdown -> struct
    let original_struct = Markdown::new(source);

    // struct -> markdown
    let markdown = original_struct.to_src();

    // markdown -> struct
    let roundtripped_struct = Markdown::new(markdown.as_str());

    // struct -> markdown (again)
    let markdown2 = roundtripped_struct.to_src();

    // The markdown should be identical after roundtrip
    assert_eq!(
        markdown, markdown2,
        "struct -> markdown -> struct -> markdown should be idempotent"
    );
}

// Tests for query::eval
// =============================================================================

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
        [root]
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

        [root]
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

        [root]
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

        [root]
        └─[1] # Introduction
            Intro content here.
            ├─[1.1] ## Details
            │   More details.
            └─[1.2] ## Summary
                Summary content.
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

    // #[test]
    // fn test_eval_slice() {
    //     let md = test_doc();
    //     // Get first child only [0:1]
    //     let result = eval(Expr::Slice(Some(0), Some(1)), &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_string());
    // }

    // #[test]
    // fn test_eval_slice_open_end() {
    //     let md = test_doc();
    //     // Get all children [0:] - returns single Markdown with all children
    //     let result = eval(Expr::Slice(Some(0), None), &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_string());
    // }

    // #[test]
    // fn test_eval_pipe() {
    //     let md = test_doc();
    //     // Get first child, then its title
    //     let expr =
    //         Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Summary));
    //     let result = eval(expr, &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_src());
    // }

    // #[test]
    // fn test_eval_comma() {
    //     let md = test_doc();
    //     // Get both children's summaries
    //     let expr = Expr::Comma(vec![
    //         Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Summary)),
    //         Expr::Pipe(Box::new(Expr::Index(1)), Box::new(Expr::Summary)),
    //     ]);
    //     let result = eval(expr, &md).unwrap();
    //     assert_eq!(result.len(), 2);
    //     let output: String = result
    //         .iter()
    //         .map(|m| m.to_src().trim().to_string())
    //         .collect::<Vec<_>>()
    //         .join("\n");
    //     assert_snapshot!(output);
    // }

    // #[test]
    // fn test_eval_title() {
    //     let md = test_doc();
    //     // Get title of first child
    //     let expr = Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Title));
    //     let result = eval(expr, &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_src());
    // }

    // #[test]
    // fn test_eval_summary() {
    //     let md = test_doc();
    //     // Get summary (title without #) of first child
    //     let expr =
    //         Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Summary));
    //     let result = eval(expr, &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_src());
    // }

    // #[test]
    // fn test_eval_nchildren() {
    //     let md = test_doc();
    //     let result = eval(Expr::NChildren, &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_src());
    // }

    // #[test]
    // fn test_eval_frontmatter() {
    //     let md = test_doc();
    //     let result = eval(Expr::Frontmatter, &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_src());
    // }

    // #[test]
    // fn test_eval_has_true() {
    //     let md = test_doc();
    //     let result = eval(Expr::Has("Details".to_string()), &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_eq!(result[0].to_src().trim(), "true");
    // }

    // #[test]
    // fn test_eval_has_false() {
    //     let md = test_doc();
    //     let result = eval(Expr::Has("Nonexistent".to_string()), &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_eq!(result[0].to_src().trim(), "false");
    // }

    // #[test]
    // fn test_eval_del() {
    //     let md = test_doc();
    //     let result = eval(Expr::Del("Details".to_string()), &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_string());
    // }

    // #[test]
    // fn test_eval_inc() {
    //     let md = test_doc();
    //     let result = eval(Expr::Inc, &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_src());
    // }

    // #[test]
    // fn test_eval_dec() {
    //     let md = test_doc();
    //     // First get a child section (which has H1), then decrement
    //     let expr = Expr::Pipe(Box::new(Expr::Index(0)), Box::new(Expr::Dec));
    //     let result = eval(expr, &md).unwrap();
    //     assert_eq!(result.len(), 1);
    //     assert_snapshot!(result[0].to_src());
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
