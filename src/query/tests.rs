//! Tests for the query module.

use super::parser::{build_sections, query_file, Boundary};
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

    let result = query_file(file.path()).unwrap();
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

    let result = query_file(file.path()).unwrap();
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
    let original = query_file(file.path()).unwrap();

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
    let original = query_file(file.path()).unwrap();

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
fn test_markdown_display() {
    use std::io::Write;
    use tempfile::NamedTempFile;

    let source = text_sparse_sections();

    let mut file = NamedTempFile::new().unwrap();
    file.write_all(source.as_bytes()).unwrap();

    let result = query_file(file.path()).unwrap();
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
    let result = Markdown::new(source).unwrap();
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

    let parsed = Markdown::new(original_source).unwrap();

    // struct -> markdown
    let reconstructed = parsed.to_src();

    // markdown -> struct (again)
    let reparsed = Markdown::new(reconstructed.as_str()).unwrap();

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
    let original_struct = Markdown::new(source).unwrap();

    // struct -> markdown
    let markdown = original_struct.to_src();

    // markdown -> struct
    let roundtripped_struct = Markdown::new(markdown.as_str()).unwrap();

    // struct -> markdown (again)
    let markdown2 = roundtripped_struct.to_src();

    // The markdown should be identical after roundtrip
    assert_eq!(
        markdown, markdown2,
        "struct -> markdown -> struct -> markdown should be idempotent"
    );
}
