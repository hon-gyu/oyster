#![allow(dead_code)]
//! Query expression evaluation.
//!
//! TODO: most of the helper functions should be provided by the types for
//! better encapsulation.
use crate::query::SectionHeading;

use super::types::{Markdown, Section};

// Note: we don't have array construct
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    // Primitives
    Identity,                            // .
    Field(String),                       // .field
    Index(isize),                        // [0]
    Slice(Option<isize>, Option<isize>), // [0:2]
    Pipe(Box<Expr>, Box<Expr>),          // expr1 | expr2
    Comma(Vec<Expr>),                    // expr1, expr2, expr3

    // Functions
    Title(isize), // title: section title of the given index
    Summary,      // summary: section summary
    NChildren,    // nchildren: number of children
    Frontmatter, // frontmatter: frontmatter as pure text (no section structure)
    Body,        // body: strip the frontmatter
    Preface,     // content before the first section
    Has(String), // has: has title. Output a boolean string
    Del(String), // del: remove a section by title or by index
    Inc(isize),  // inc: increment all heading levels
    Dec(isize),  // dec: decrement all heading
                 // TOC
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvalError {
    IndexOutOfBounds(isize),
    FieldNotFound(String),
    General(String),
}

/// Get the title text from a section heading (without # prefix)
fn get_heading_title(heading: &SectionHeading) -> Option<String> {
    match heading {
        SectionHeading::Root => None,
        SectionHeading::Heading(h) => {
            // Strip the leading #s and whitespace
            let text = h.text.trim();
            let title = text.trim_start_matches('#').trim_start();
            Some(title.to_string())
        }
    }
}

/// Find a child section by title (case-sensitive exact match)
///
/// Uses breadth-first search so higher-level sections win over deeper ones.
/// When `recursive` is false, only searches immediate children.
fn find_child_by_title<'a>(
    section: &'a Section,
    title: &str,
    recursive: bool,
) -> Option<(usize, &'a Section)> {
    // First pass: check all immediate children (higher level wins)
    for (idx, child) in section.children.iter().enumerate() {
        if let Some(child_title) = get_heading_title(&child.heading) {
            if child_title == title {
                return Some((idx, child));
            }
        }
    }

    // Second pass: recurse into children (breadth-first)
    if recursive {
        for child in &section.children {
            if let Some(found) = find_child_by_title(child, title, true) {
                return Some(found);
            }
        }
    }

    None
}

/// Normalize index (handle negative indices)
fn normalize_index(index: isize, len: usize) -> Option<usize> {
    if index >= 0 {
        let idx = index as usize;
        if idx < len { Some(idx) } else { None }
    } else {
        // Negative index: -1 is last element
        let abs_idx = (-index) as usize;
        if abs_idx <= len {
            Some(len - abs_idx)
        } else {
            None
        }
    }
}

/// Remove a section by title from the section tree.
/// Returns a new section tree without the matching section.
///
/// Note: this breaks the continuity the range. Need re-parsing afterwards.
fn remove_section_by_title(section: &Section, title: &str) -> Section {
    // Check if any immediate child matches the title
    let filtered_children: Vec<Section> = section
        .children
        .iter()
        .filter_map(|child| {
            // Check if this child matches the title
            if let Some(child_title) = get_heading_title(&child.heading) {
                if child_title == title {
                    // Skip this child (remove it)
                    return None;
                }
            }
            // Recursively process this child's children
            let new_child = remove_section_by_title(child, title);
            Some(new_child)
        })
        .collect();

    // Return a new section with filtered children
    Section {
        heading: section.heading.clone(),
        path: section.path.clone(),
        content: section.content.clone(),
        range: section.range.clone(),
        implicit: section.implicit,
        children: filtered_children,
    }
}

pub fn eval(expr: Expr, md: &Markdown) -> Result<Vec<Markdown>, EvalError> {
    match expr {
        Expr::Identity => Ok(vec![md.clone()]),
        Expr::Body => Ok(vec![Markdown::new(&md.sections.to_src())]),
        Expr::Preface => {
            let preface = &md.sections.content;
            Ok(vec![Markdown::new(&preface)])
        }
        Expr::Field(title) => {
            // Find a section by title
            match find_child_by_title(&md.sections, &title, true) {
                Some((_, tgt_sec)) => {
                    let fm_src = md
                        .frontmatter
                        .as_ref()
                        .map_or("".to_string(), |fm| fm.to_src());
                    let tgt_sec_src = tgt_sec.to_src();
                    let src = fm_src + &tgt_sec_src;
                    Ok(vec![Markdown::new(&src)])
                }
                None => Err(EvalError::FieldNotFound(title)),
            }
        }

        Expr::Index(idx) => {
            let children = &md.sections.children;
            match normalize_index(idx, children.len()) {
                Some(i) => Ok(vec![md.slice_sections_inclusive(i, i)]),
                None => Err(EvalError::IndexOutOfBounds(idx)),
            }
        }

        Expr::Slice(start, end) => {
            let children = &md.sections.children;
            let len = children.len();

            if len == 0 {
                return Err(EvalError::General(
                    "Cannot slice a section with no children".to_string(),
                ));
            }

            let start_idx = match start {
                Some(s) => normalize_index(s, len).unwrap_or(0),
                None => 0,
            };

            let end_idx = match end {
                Some(e) => {
                    if e >= 0 {
                        std::cmp::min(e as usize, len)
                    } else {
                        normalize_index(e, len).unwrap_or(0)
                    }
                }
                None => len - 1,
            };

            if start_idx >= end_idx || start_idx >= len {
                return Ok(vec![]);
            }

            Ok(vec![md.slice_sections_inclusive(start_idx, end_idx)])
        }

        Expr::Pipe(left, right) => {
            // Evaluate left, then apply right to each result
            let left_results = eval(*left, md)?;
            let mut results = Vec::new();
            for left_md in &left_results {
                let right_results = eval((*right).clone(), left_md)?;
                results.extend(right_results);
            }
            Ok(results)
        }

        Expr::Comma(exprs) => {
            // Evaluate each expression and collect all results
            let mut results = Vec::new();
            for expr in exprs {
                let expr_results = eval(expr, md)?;
                results.extend(expr_results);
            }
            Ok(results)
        }

        Expr::Title(idx) => {
            let children = &md.sections.children;
            match normalize_index(idx, children.len()) {
                Some(i) => {
                    let title = get_heading_title(&children[i].heading);
                    if let Some(title) = title {
                        Ok(vec![Markdown::new(&title)])
                    } else {
                        Err(EvalError::General("No title found".to_string()))
                    }
                }
                None => Err(EvalError::IndexOutOfBounds(idx)),
            }
        }

        Expr::Summary => Ok(vec![Markdown::new(&md.to_string())]),

        Expr::NChildren => {
            let count = md.sections.children.len();
            Ok(vec![Markdown::new(&count.to_string())])
        }

        Expr::Frontmatter => match &md.frontmatter {
            Some(fm) => {
                let yaml_str = serde_yaml::to_string(&fm.value)
                    .unwrap_or_else(|_| String::new());
                Ok(vec![Markdown::new(yaml_str.trim())])
            }
            None => Err(EvalError::General("No frontmatter".to_string())),
        },

        Expr::Has(title) => {
            let result = {
                let section: &Section = &md.sections;
                let title: &str = &title;
                find_child_by_title(section, title, true).is_some()
            };
            Ok(vec![Markdown::new(if result { "true" } else { "false" })])
        }

        Expr::Del(title) => {
            if let Some(_) = find_child_by_title(&md.sections, &title, true) {
                // Clone the section tree and remove the target section
                let new_sections =
                    remove_section_by_title(&md.sections, &title);

                // Reconstruct markdown from the modified tree
                let mut new_md_src = String::new();
                if let Some(fm) = &md.frontmatter {
                    new_md_src.push_str(&fm.to_src());
                }
                new_md_src.push_str(&new_sections.to_src());

                Ok(vec![Markdown::new(&new_md_src)])
            } else {
                Err(EvalError::General(
                    "Did not find the section to delete".to_string(),
                ))
            }
        }

        Expr::Inc(delta) => Ok(vec![md.with_shifted_heading_levels(delta)]),
        Expr::Dec(delta) => Ok(vec![md.with_shifted_heading_levels(-delta)]),
    }
}
