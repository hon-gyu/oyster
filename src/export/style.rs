use std::fs;
use std::path::Path;

/// Base CSS file (structural, theme-independent)
const BASE_CSS: &str = include_str!("styles/base.css");

/// Component CSS files
const CALLOUT_CSS: &str = include_str!("styles/callout.css");

/// Theme CSS files
const DRACULA_CSS: &str = include_str!("styles/themes/dracula.css");
const GRUVBOX_CSS: &str = include_str!("styles/themes/gruvbox.css");
const TOKYONIGHT_CSS: &str = include_str!("styles/themes/tokyonight.css");

/// Get the CSS content for a theme
fn get_theme_css(name: &str) -> &'static str {
    match name {
        "dracula" => DRACULA_CSS,
        "gruvbox" => GRUVBOX_CSS,
        "tokyonight" => TOKYONIGHT_CSS,
        _ => GRUVBOX_CSS,
    }
}

/// Copy CSS files to the output directory
/// Returns the relative paths to the CSS files
pub fn setup_styles(
    output_dir: &Path,
    theme: &str,
    custom_callout_css: Option<&Path>,
) -> Result<Vec<String>, std::io::Error> {
    // Create styles directory in output
    let styles_dir = output_dir.join("styles");
    let themes_dir = styles_dir.join("themes");

    fs::create_dir_all(&themes_dir)?;

    // Write base CSS
    let base_path = styles_dir.join("base.css");
    fs::write(&base_path, BASE_CSS)?;

    // Write component CSS
    let callout_path = styles_dir.join("callout.css");
    fs::write(&callout_path, CALLOUT_CSS)?;

    // Write theme CSS
    let theme_filename = format!("{}.css", theme);
    let theme_path = themes_dir.join(&theme_filename);
    fs::write(&theme_path, get_theme_css(theme))?;

    // Copy custom callout CSS if provided
    if let Some(custom_css_path) = custom_callout_css {
        let custom_css_dest = styles_dir.join("custom-callout.css");
        fs::copy(custom_css_path, custom_css_dest)?;
    }

    // Return relative paths
    let mut paths = vec![
        "styles/base.css".to_string(),
        "styles/callout.css".to_string(),
        format!("styles/themes/{}", theme_filename),
    ];

    if custom_callout_css.is_some() {
        paths.push("styles/custom-callout.css".to_string());
    }

    Ok(paths)
}

/// Get CSS file paths relative to a page
/// page_path: the path to the HTML page (e.g., "output/notes/page.html")
/// output_dir: the root output directory (e.g., "output")
/// theme: the theme name
/// has_custom_callout_css: whether custom callout CSS was provided
pub fn get_style_paths(
    page_path: &Path,
    output_dir: &Path,
    theme: &str,
    has_custom_callout_css: bool,
) -> Vec<String> {
    use crate::export::utils::get_relative_dest;

    let base_path = output_dir.join("styles/base.css");
    let callout_path = output_dir.join("styles/callout.css");
    let theme_path = output_dir.join(format!("styles/themes/{}.css", theme));

    let mut paths = vec![
        get_relative_dest(page_path, &base_path),
        get_relative_dest(page_path, &callout_path),
        get_relative_dest(page_path, &theme_path),
    ];

    if has_custom_callout_css {
        let custom_callout_path = output_dir.join("styles/custom-callout.css");
        paths.push(get_relative_dest(page_path, &custom_callout_path));
    }

    paths
}
