use serde::Serialize;
use std::path::PathBuf;

/// Configuration for the static site generator
#[derive(Debug, Clone)]
pub struct SiteConfig {
    /// Site title
    pub title: String,
    /// Base URL for the site (e.g., "https://example.com" or "/" for relative)
    pub base_url: String,
    /// Output directory for generated site
    pub output_dir: PathBuf,
}

impl Default for SiteConfig {
    fn default() -> Self {
        Self {
            title: "".to_string(),
            base_url: "/".to_string(),
            output_dir: PathBuf::from("dist"),
        }
    }
}

/// Context data passed to templates for rendering pages
#[derive(Debug, Clone, Serialize)]
pub struct PageContext {
    /// Site-level configuration
    pub site: SiteContext,
    /// Current page data
    pub page: PageData,
    /// Links to this page
    #[serde(skip_serializing_if = "Option::is_none")]
    pub backlinks: Option<Vec<LinkInfo>>,
    // /// Table of contents generated from headings
    // #[serde(skip_serializing_if = "Option::is_none")]
    // pub toc: Option<Vec<TocEntry>>,
}

/// Individual page data
#[derive(Debug, Clone, Serialize)]
pub struct PageData {
    /// Page title (from frontmatter or first heading or filename)
    pub title: String,
    /// HTML content of the page
    pub content: String,
    /// Relative path for this page in the output
    pub path: String,
}

/// Site-level context for templates
#[derive(Debug, Clone, Serialize)]
pub struct SiteContext {
    pub title: String,
    pub base_url: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct LinkInfo {
    pub title: String,
    pub path: String,
}
