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
    /// Whether to generate backlinks
    pub generate_backlinks: bool,
}

impl Default for SiteConfig {
    fn default() -> Self {
        Self {
            title: "My Knowledge Base".to_string(),
            base_url: "/".to_string(),
            output_dir: PathBuf::from("dist"),
            generate_backlinks: true,
        }
    }
}

/// Context data passed to templates for rendering pages
#[derive(Debug, Clone, serde::Serialize)]
pub struct PageContext {
    /// Site-level configuration
    pub site: SiteContext,
    /// Current page data
    pub page: PageData,
    /// Links related to this page
    #[serde(skip_serializing_if = "Option::is_none")]
    pub links: Option<LinkContext>,
    /// Table of contents generated from headings
    #[serde(skip_serializing_if = "Option::is_none")]
    pub toc: Option<Vec<TocEntry>>,
}

/// Site-level context for templates
#[derive(Debug, Clone, serde::Serialize)]
pub struct SiteContext {
    pub title: String,
    pub base_url: String,
}

/// Individual page data
#[derive(Debug, Clone, serde::Serialize)]
pub struct PageData {
    /// Page title (from frontmatter or first heading or filename)
    pub title: String,
    /// HTML content of the page
    pub content: String,
    /// Relative path for this page in the output
    pub path: String,
}

/// Link information for a page
#[derive(Debug, Clone, serde::Serialize)]
pub struct LinkContext {
    /// Pages that link to this page
    pub backlinks: Vec<BacklinkInfo>,
    /// Pages this page links to
    pub outgoing_links: Vec<OutgoingLinkInfo>,
}

/// Information about a backlink
#[derive(Debug, Clone, serde::Serialize)]
pub struct BacklinkInfo {
    /// Title of the page that links here
    pub title: String,
    /// Path to the page
    pub path: String,
}

/// Information about an outgoing link
#[derive(Debug, Clone, serde::Serialize)]
pub struct OutgoingLinkInfo {
    /// Title of the linked page
    pub title: String,
    /// Path to the page
    pub path: String,
}

/// Table of contents entry
#[derive(Debug, Clone, serde::Serialize)]
pub struct TocEntry {
    /// Heading level (1-6)
    pub level: u8,
    /// Heading text
    pub text: String,
    /// Anchor ID for linking
    pub id: String,
}
