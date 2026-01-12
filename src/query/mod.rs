//! ...
//!
//! We build AST first, then trim the AST to the things of our interest.
//! - Frontmatter
//! - Headings -> Sections
//!
//! All the query logic are handled by `jq`
use crate::ast::{Node, NodeKind, Tree};
use crate::link::extract::scan_note;
use pulldown_cmark::HeadingLevel;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use serde_yaml::Value as YamlValue;
mod heading;

use heading::Heading;

/// A section of content under a heading
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Section {
    /// The heading that starts this section
    pub heading: Heading,
    /// Hierarchy number (e.g., "0.1", "0.1.0.1")
    pub number: String,
    /// Raw content of this section (excluding child sections)
    pub content: String,
    /// Child sections (headings at deeper levels)
    pub children: Vec<Section>,
}

/// Build hierarchy numbers for headings
///
/// The numbering scheme works as follows:
/// - Start with "0" as the implicit document root
/// - Each heading level gets a counter
/// - When a heading appears, increment the counter for its level
/// - Reset all deeper level counters
///
/// Example: ## A, #### B, ### B1, ## C, ### D, ### E, #### F
/// Results: 0.1, 0.1.0.1, 0.1.1, 0.2, 0.2.1, 0.2.2, 0.2.2.1
fn build_heading_numbers(headings: &[Heading]) -> Vec<String> {
    if headings.is_empty() {
        return Vec::new();
    }

    // Track counters for each level (0-6, where 0 is document root)
    let mut counters: [usize; 7] = [0; 7];
    let mut numbers = Vec::new();

    for heading in headings {
        let level = heading.level as usize;

        // Increment counter for this level
        counters[level] += 1;

        // Reset deeper level counters
        for i in (level + 1)..7 {
            counters[i] = 0;
        }

        // Build the number string from level 0 to current level
        let number: String = (0..=level)
            .map(|l| counters[l].to_string())
            .collect::<Vec<_>>()
            .join(".");

        numbers.push(number);
    }

    numbers
}

/// Build hierarchical sections from headings
#[cfg(any())]
fn build_sections(
    headings: &[Heading],
    root: &Node,
    source: &str,
) -> Vec<Section> {
    if headings.is_empty() {
        return Vec::new();
    }

    // Build sections recursively using a stack-based approach
    let mut sections: Vec<Section> = Vec::new();
    let mut stack: Vec<(Section, u8)> = Vec::new(); // (section, level)

    for (i, heading) in headings.iter().enumerate() {
        // Determine content range: from after heading to before next heading (or end of doc)
        let content_start = heading.end_byte;
        let content_end = if i + 1 < headings.len() {
            headings[i + 1].start_byte
        } else {
            root.end_byte
        };

        let content = source[content_start..content_end].trim().to_string();

        let section = Section {
            heading: heading.clone(),
            content,
            children: Vec::new(),
        };

        // Pop sections from stack that are at same or deeper level
        while let Some((_, stack_level)) = stack.last() {
            if *stack_level >= heading.level {
                let (completed, _) = stack.pop().unwrap();
                if let Some((parent, _)) = stack.last_mut() {
                    parent.children.push(completed);
                } else {
                    sections.push(completed);
                }
            } else {
                break;
            }
        }

        stack.push((section, heading.level));
    }

    // Pop remaining sections from stack
    while let Some((section, _)) = stack.pop() {
        if let Some((parent, _)) = stack.last_mut() {
            parent.children.push(section);
        } else {
            sections.push(section);
        }
    }

    sections
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_markdown_basic() {
        let source = r#"---
title: Test Document
tags:
  - test
  - example
---

## Introduction

Some intro content.

### Details

More details here.

## Conclusion

Final thoughts.
"#;
    }

    #[test]
    fn test_query_no_frontmatter() {
        let source = r#"# Title

Some content.
"#;
    }
}
