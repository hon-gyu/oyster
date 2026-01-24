use crate::query::SectionHeading;
use pulldown_cmark::HeadingLevel;

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
    Title,       // title: section title
    Summary,     // summary: section summary
    Range,       // range: section range
    NChildren,   // nchildren: number of children
    Frontmatter, // frontmatter: frontmatter as pure text (no section structure)
    Body, // body: alias of Identify as by default we strip the frontmatter
    Has(String), // has: has title. Output a boolean string
    Del(String), // del: remove a section by title or by index
    Inc,  // incheading: increment all headings by one
    Dec,  // decheading: decrement all headings by one
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvalError {
    IndexOutOfBounds(isize),
    FieldNotFound(String),
    General(String),
}

/// Convert a Section to a Markdown (without frontmatter)
fn section_to_markdown(section: &Section) -> Markdown {
    Markdown {
        frontmatter: None,
        sections: section.clone(),
    }
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
fn find_child_by_title<'a>(
    section: &'a Section,
    title: &str,
) -> Option<&'a Section> {
    for child in &section.children {
        if let Some(child_title) = get_heading_title(&child.heading) {
            if child_title == title {
                return Some(child);
            }
        }
        // Recurse into children
        if let Some(found) = find_child_by_title(child, title) {
            return Some(found);
        }
    }
    None
}

/// Check if a section contains a child with the given title
fn has_child_with_title(section: &Section, title: &str) -> bool {
    find_child_by_title(section, title).is_some()
}

/// Delete a section by title (returns a new section with the matching child removed)
fn delete_by_title(section: &Section, title: &str) -> Section {
    let new_children: Vec<Section> = section
        .children
        .iter()
        .filter_map(|child| {
            if let Some(child_title) = get_heading_title(&child.heading) {
                if child_title == title {
                    return None; // Remove this child
                }
            }
            // Recurse into children
            Some(delete_by_title(child, title))
        })
        .collect();

    Section {
        heading: section.heading.clone(),
        path: section.path.clone(),
        content: section.content.clone(),
        range: section.range.clone(),
        children: new_children,
    }
}

/// Increment heading level by 1 (max H6)
fn increment_heading(section: &Section) -> Section {
    let new_heading = match &section.heading {
        SectionHeading::Root => SectionHeading::Root,
        SectionHeading::Heading(h) => {
            let new_level = match h.level {
                HeadingLevel::H1 => HeadingLevel::H2,
                HeadingLevel::H2 => HeadingLevel::H3,
                HeadingLevel::H3 => HeadingLevel::H4,
                HeadingLevel::H4 => HeadingLevel::H5,
                HeadingLevel::H5 => HeadingLevel::H6,
                HeadingLevel::H6 => HeadingLevel::H6, // Already at max
            };
            // Update heading text to reflect new level
            let title = get_heading_title(&section.heading).unwrap_or_default();
            let new_text =
                format!("{} {}\n", "#".repeat(new_level as usize), title);
            SectionHeading::Heading(super::Heading {
                level: new_level,
                text: new_text,
                range: h.range.clone(),
                id: h.id.clone(),
                classes: h.classes.clone(),
                attrs: h.attrs.clone(),
            })
        }
    };

    Section {
        heading: new_heading,
        path: section.path.clone(),
        content: section.content.clone(),
        range: section.range.clone(),
        children: section.children.iter().map(increment_heading).collect(),
    }
}

/// Decrement heading level by 1 (min H1)
fn decrement_heading(section: &Section) -> Section {
    let new_heading = match &section.heading {
        SectionHeading::Root => SectionHeading::Root,
        SectionHeading::Heading(h) => {
            let new_level = match h.level {
                HeadingLevel::H1 => HeadingLevel::H1, // Already at min
                HeadingLevel::H2 => HeadingLevel::H1,
                HeadingLevel::H3 => HeadingLevel::H2,
                HeadingLevel::H4 => HeadingLevel::H3,
                HeadingLevel::H5 => HeadingLevel::H4,
                HeadingLevel::H6 => HeadingLevel::H5,
            };
            // Update heading text to reflect new level
            let title = get_heading_title(&section.heading).unwrap_or_default();
            let new_text =
                format!("{} {}\n", "#".repeat(new_level as usize), title);
            SectionHeading::Heading(super::Heading {
                level: new_level,
                text: new_text,
                range: h.range.clone(),
                id: h.id.clone(),
                classes: h.classes.clone(),
                attrs: h.attrs.clone(),
            })
        }
    };

    Section {
        heading: new_heading,
        path: section.path.clone(),
        content: section.content.clone(),
        range: section.range.clone(),
        children: section.children.iter().map(decrement_heading).collect(),
    }
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

pub fn eval(expr: Expr, md: &Markdown) -> Result<Vec<Markdown>, EvalError> {
    match expr {
        Expr::Identity => Ok(vec![md.clone()]),
        Expr::Body => Ok(vec![md.clone()]),

        Expr::Field(title) => {
            // Find a section by title
            match find_child_by_title(&md.sections, &title) {
                Some(section) => Ok(vec![section_to_markdown(section)]),
                None => Err(EvalError::FieldNotFound(title)),
            }
        }

        Expr::Index(idx) => {
            let children = &md.sections.children;
            match normalize_index(idx, children.len()) {
                Some(i) => Ok(vec![section_to_markdown(&children[i])]),
                None => Err(EvalError::IndexOutOfBounds(idx)),
            }
        }

        Expr::Slice(start, end) => {
            let children = &md.sections.children;
            let len = children.len();

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
                None => len,
            };

            if start_idx >= end_idx || start_idx >= len {
                return Ok(vec![]);
            }

            Ok(children[start_idx..end_idx]
                .iter()
                .map(section_to_markdown)
                .collect())
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

        Expr::Title => match &md.sections.heading {
            SectionHeading::Root => {
                Err(EvalError::General("No title for root".to_string()))
            }
            SectionHeading::Heading(h) => {
                // Return raw heading text (includes # prefix)
                Ok(vec![Markdown::new(h.text.as_str())])
            }
        },

        Expr::Summary => {
            // Return title without # prefix
            match get_heading_title(&md.sections.heading) {
                Some(title) => Ok(vec![Markdown::new(&title)]),
                None => {
                    Err(EvalError::General("No title for root".to_string()))
                }
            }
        }

        Expr::Range => {
            // Return range as "start_line:start_col-end_line:end_col"
            let range = &md.sections.range;
            let range_str = format!(
                "{}:{}-{}:{}",
                range.start.0 + 1, // 1-indexed for display
                range.start.1 + 1,
                range.end.0 + 1,
                range.end.1 + 1
            );
            Ok(vec![Markdown::new(&range_str)])
        }

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
            let result = has_child_with_title(&md.sections, &title);
            Ok(vec![Markdown::new(if result { "true" } else { "false" })])
        }

        Expr::Del(title) => {
            let new_sections = delete_by_title(&md.sections, &title);
            Ok(vec![Markdown {
                frontmatter: md.frontmatter.clone(),
                sections: new_sections,
            }])
        }

        Expr::Inc => {
            let new_sections = increment_heading(&md.sections);
            Ok(vec![Markdown {
                frontmatter: md.frontmatter.clone(),
                sections: new_sections,
            }])
        }

        Expr::Dec => {
            let new_sections = decrement_heading(&md.sections);
            Ok(vec![Markdown {
                frontmatter: md.frontmatter.clone(),
                sections: new_sections,
            }])
        }
    }
}
