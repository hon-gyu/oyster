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
    WIP,
}

pub fn eval(expr: Expr, md: &Markdown) -> Result<Vec<Markdown>, EvalError> {
    match expr {
        Expr::Identity => Ok(vec![md.clone()]),
        _ => Err(EvalError::WIP),
    }
}
