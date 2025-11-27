#[allow(dead_code)]
/// Color scheme for a theme
struct ThemeColors {
    bg_primary: &'static str,
    bg_secondary: &'static str,
    bg_tertiary: &'static str,
    text_primary: &'static str,
    text_secondary: &'static str,
    accent_primary: &'static str,
    accent_secondary: &'static str,
    link_primary: &'static str,
    link_hover: &'static str,
    code_text: &'static str,
    border_primary: &'static str,
    border_secondary: &'static str,
    blockquote_border: &'static str,
    backlinks_heading: &'static str,
    heading_primary: &'static str,
    summary_hover: &'static str,
}

impl ThemeColors {
    fn dracula() -> Self {
        Self {
            bg_primary: "#282a36",
            bg_secondary: "#44475a",
            bg_tertiary: "#44475a",
            text_primary: "#f8f8f2",
            text_secondary: "#6272a4",
            accent_primary: "#bd93f9",
            accent_secondary: "#ff79c6",
            link_primary: "#8be9fd",
            link_hover: "#50fa7b",
            code_text: "#50fa7b",
            border_primary: "#44475a",
            border_secondary: "#6272a4",
            blockquote_border: "#bd93f9",
            backlinks_heading: "#ffb86c",
            heading_primary: "#bd93f9",
            summary_hover: "#ff79c6",
        }
    }

    fn gruvbox() -> Self {
        Self {
            bg_primary: "#282828",
            bg_secondary: "#3c3836",
            bg_tertiary: "#3c3836",
            text_primary: "#ebdbb2",
            text_secondary: "#a89984",
            accent_primary: "#fabd2f",
            accent_secondary: "#fe8019",
            link_primary: "#83a598",
            link_hover: "#8ec07c",
            code_text: "#b8bb26",
            border_primary: "#3c3836",
            border_secondary: "#504945",
            blockquote_border: "#d79921",
            backlinks_heading: "#fe8019",
            heading_primary: "#fabd2f",
            summary_hover: "#fe8019",
        }
    }

    fn tokyonight() -> Self {
        Self {
            bg_primary: "#1a1b26",
            bg_secondary: "#24283b",
            bg_tertiary: "#24283b",
            text_primary: "#c0caf5",
            text_secondary: "#565f89",
            accent_primary: "#7aa2f7",
            accent_secondary: "#bb9af7",
            link_primary: "#2ac3de",
            link_hover: "#9ece6a",
            code_text: "#9ece6a",
            border_primary: "#24283b",
            border_secondary: "#414868",
            blockquote_border: "#bb9af7",
            backlinks_heading: "#ff9e64",
            heading_primary: "#7aa2f7",
            summary_hover: "#bb9af7",
        }
    }

    /// Generate CSS for the given theme colors
    fn to_css(&self) -> String {
        format!(
            r#"
        body {{
            max-width: 800px;
            margin: 0 auto;
            padding: 2rem;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            background: {};
            color: {};
        }}
        h1, h2, h3, h4, h5, h6 {{
            margin-top: 1.5em;
            margin-bottom: 0.5em;
            line-height: 1.3;
            color: {};
        }}
        h1 {{ font-size: 2em; border-bottom: 2px solid {}; padding-bottom: 0.3em; }}
        h2 {{ font-size: 1.5em; }}
        h3 {{ font-size: 1.25em; }}
        h4 {{ font-size: 1.15em; }}
        h5 {{ font-size: 1.1em; }}
        h6 {{ font-size: 1.05em; }}
        a {{ color: {}; text-decoration: none; }}
        a:hover {{ text-decoration: underline; color: {}; }}
        code {{
            background: {};
            padding: 0.2em 0.4em;
            border-radius: 3px;
            font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
            font-size: 0.9em;
            color: {};
        }}
        pre {{
            background: {};
            padding: 1em;
            border-radius: 5px;
            overflow-x: auto;
            border: 1px solid {};
        }}
        pre code {{
            background: none;
            padding: 0;
        }}
        blockquote {{
            border-left: 4px solid {};
            padding-left: 1em;
            margin-left: 0;
            color: {};
        }}
        hr {{
            border: none;
            border-top: 2px solid {};
            margin: 2em 0;
        }}
        .backlinks {{
            margin-top: 2em;
            padding: 1em;
            background: {};
            border-radius: 5px;
            border: 1px solid {};
        }}
        .backlinks h5 {{
            margin-top: 0;
            font-size: 1em;
            color: {};
            text-transform: uppercase;
            letter-spacing: 0.05em;
        }}
        .backlinks ul {{
            list-style: none;
            padding: 0;
        }}
        .backlinks li {{
            margin: 0.5em 0;
        }}
        .top-nav {{
            margin-bottom: 1em;
            padding-bottom: 0.5em;
            border-bottom: 1px solid {};
        }}
        .home-link {{
            font-size: 1.2em;
            opacity: 0.8;
        }}
        .home-link:hover {{
            opacity: 1;
        }}
        .file-tree ul {{
            list-style: none;
            padding-left: 1.5em;
        }}
        .file-tree > ul {{
            padding-left: 0;
        }}
        .file-tree li {{
            margin: 0.3em 0;
        }}
        .file-tree summary {{
            cursor: pointer;
            user-select: none;
            color: {};
        }}
        .file-tree summary:hover {{
            color: {};
        }}
        .file-tree .file a {{
            color: {};
        }}
        .file-tree .file a:hover {{
            color: {};
        }}
    "#,
            self.bg_primary,        // body background
            self.text_primary,      // body color
            self.heading_primary,   // h1-h6 color
            self.border_primary,    // h1 border-bottom
            self.link_primary,      // a color
            self.link_hover,        // a:hover color
            self.bg_secondary,      // code background
            self.code_text,         // code color
            self.bg_tertiary,       // pre background
            self.border_secondary,  // pre border
            self.blockquote_border, // blockquote border-left
            self.text_secondary,    // blockquote color
            self.border_primary,    // hr border-top
            self.bg_secondary,      // .backlinks background
            self.border_secondary,  // .backlinks border
            self.backlinks_heading, // .backlinks h5 color
            self.border_primary,    // .top-nav border-bottom
            self.heading_primary,   // .file-tree summary color
            self.summary_hover,     // .file-tree summary:hover color
            self.text_primary,      // .file-tree .file a color
            self.link_hover,        // .file-tree .file a:hover color
        )
    }
}

pub fn get_style(name: &str) -> &'static str {
    match name {
        "dracula" => get_dracula_theme(),
        "gruvbox" => get_gruvbox_theme(),
        "tokyonight" => get_tokyonight_theme(),
        _ => get_gruvbox_theme(),
    }
}

fn get_dracula_theme() -> &'static str {
    Box::leak(ThemeColors::dracula().to_css().into_boxed_str())
}

fn get_gruvbox_theme() -> &'static str {
    Box::leak(ThemeColors::gruvbox().to_css().into_boxed_str())
}

fn get_tokyonight_theme() -> &'static str {
    Box::leak(ThemeColors::tokyonight().to_css().into_boxed_str())
}
