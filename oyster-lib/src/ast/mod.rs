//! Define the AST for the Markdown

pub mod callout;
mod node;
mod tree;

#[cfg(test)]
mod tests;

// Re-export public types
pub use node::{InvalidNode, Node, NodeKind};
pub use tree::Tree;
