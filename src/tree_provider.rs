use crate::ast::Tree;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub trait TreeProvider {
    fn get_tree(&self, path: &Path) -> Option<&Tree>;
}

pub struct PreloadedTrees<'a> {
    trees: HashMap<PathBuf, Tree<'a>>,
}

impl<'a> TreeProvider for PreloadedTrees<'a> {
    fn get_tree(&self, path: &Path) -> Option<&Tree> {
        self.trees.get(path)
    }
}

impl Default for PreloadedTrees<'_> {
    fn default() -> Self {
        Self {
            trees: HashMap::new(),
        }
    }
}

// ====================

pub struct LazyTreeCache {
    vault_root: PathBuf,
    cache: (), // TODO
    max_cache_size: usize,
}

impl LazyTreeCache {
    fn get_tree(&self, path: &Path) -> Option<&Tree> {
        todo!()
    }
}
