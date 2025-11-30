use std::fs;
use std::path::Path;

/// Base CSS file (structural, theme-independent)
const BASE_CSS: &str = include_str!("styles/base.css");

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
pub fn setup_styles(output_dir: &Path, theme: &str) -> Result<Vec<String>, std::io::Error> {
    // Create styles directory in output
    let styles_dir = output_dir.join("styles");
    let themes_dir = styles_dir.join("themes");

    fs::create_dir_all(&themes_dir)?;

    // Write base CSS
    let base_path = styles_dir.join("base.css");
    fs::write(&base_path, BASE_CSS)?;

    // Write theme CSS
    let theme_filename = format!("{}.css", theme);
    let theme_path = themes_dir.join(&theme_filename);
    fs::write(&theme_path, get_theme_css(theme))?;

    // Return relative paths
    Ok(vec![
        "styles/base.css".to_string(),
        format!("styles/themes/{}", theme_filename),
    ])
}

/// Get CSS file paths relative to a page
/// page_path: the path to the HTML page (e.g., "output/notes/page.html")
/// output_dir: the root output directory (e.g., "output")
/// theme: the theme name
pub fn get_style_paths(page_path: &Path, output_dir: &Path, theme: &str) -> Vec<String> {
    use crate::export::utils::get_relative_dest;

    let base_path = output_dir.join("styles/base.css");
    let theme_path = output_dir.join(format!("styles/themes/{}.css", theme));

    vec![
        get_relative_dest(page_path, &base_path),
        get_relative_dest(page_path, &theme_path),
    ]
}
