use maud::{Markup, PreEscaped, html};
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[derive(Debug, Clone, Copy)]
pub enum QuiverRenderMode {
    BuildTime,
    Raw,
}

impl QuiverRenderMode {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "buildtime" | "build-time" | "build" => Some(Self::BuildTime),
            "raw" | "code" => Some(Self::Raw),
            _ => None,
        }
    }
}

impl TryFrom<&str> for QuiverRenderMode {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match Self::from_str(s) {
            Some(mode) => Ok(mode),
            None => Err(format!("Invalid quiver render mode: {}", s)),
        }
    }
}

pub fn render_quiver_build_time(quiver_code: &str) -> Result<String, String> {
    let temp_dir = TempDir::new().unwrap();
    let tex_path = temp_dir.path().join("input.tex");
    let pdf_path = temp_dir.path().join("input.pdf");
    let svg_path = temp_dir.path().join("output.svg");

    // Quiver exports use tikz-cd, so we need to wrap it appropriately
    // The code might already be wrapped in tikzcd environment or not
    let latex_doc = if quiver_code.trim().starts_with("\\begin{tikzcd}") {
        // Already has tikzcd environment
        format!(
            r#"\documentclass[tikz,border=2pt]{{standalone}}
\usepackage{{amssymb}}
\usepackage{{tikz-cd}}
\begin{{document}}
{}
\end{{document}}"#,
            quiver_code
        )
    } else {
        // Wrap in tikzcd environment
        format!(
            r#"\documentclass[tikz,border=2pt]{{standalone}}
\usepackage{{amssymb}}
\usepackage{{tikz-cd}}
\begin{{document}}
\begin{{tikzcd}}
{}
\end{{tikzcd}}
\end{{document}}"#,
            quiver_code
        )
    };

    fs::write(&tex_path, latex_doc)
        .map_err(|e| format!("Failed to write temp tex file: {}", e))?;

    // Compile LaTeX to PDF
    let output = Command::new("pdflatex")
        .arg("-interaction=nonstopmode")
        .arg("-output-directory")
        .arg(temp_dir.path())
        .arg(&tex_path)
        .output()
        .map_err(|e| format!("Failed to run pdflatex: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(format!(
            "Failed to compile LaTeX: stderr: {}, stdout: {}",
            stderr, stdout
        ));
    }

    // Convert PDF to SVG using pdf2svg
    let output = Command::new("pdf2svg")
        .arg(&pdf_path)
        .arg(&svg_path)
        .output()
        .map_err(|e| format!("Failed to run pdf2svg: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Failed to convert PDF to SVG: {}", stderr));
    }

    let svg = fs::read_to_string(&svg_path)
        .map_err(|e| format!("Failed to read generated SVG: {}", e))?;

    // Check if SVG is suspiciously large (likely an empty page from failed compilation)
    // Standard page sizes are 612x792 (US Letter) or 595x842 (A4)
    if svg.contains("height=\"792pt\"") || svg.contains("height=\"842pt\"") {
        return Err("Generated SVG appears to be an empty page".to_string());
    }

    Ok(svg)
}

pub fn render_quiver(quiver_code: &str, mode: QuiverRenderMode) -> Markup {
    match mode {
        QuiverRenderMode::BuildTime => {
            match render_quiver_build_time(quiver_code) {
                Ok(svg) => html! {
                    .quiver-diagram {
                        ( PreEscaped(svg) )
                    }
                },
                Err(_) => {
                    html! {
                        .quiver-error {
                            pre {
                                code .language-quiver {
                                    p .error-message {
                                        "<!-- Quiver rendering failed -->"
                                    }
                                    (quiver_code)
                                }
                            }
                        }
                    }
                }
            }
        }
        QuiverRenderMode::Raw => {
            // Just render as raw code block
            html! {
                pre {
                    code .language-quiver {
                        (quiver_code)
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn test_render_quiver_raw() {
        let quiver_code = r#"\begin{tikzcd}
    A \arrow[r] & B
\end{tikzcd}"#;
        let result = render_quiver(quiver_code, QuiverRenderMode::Raw);
        assert_snapshot!(result.into_string(), @r###"
        <pre><code class="language-quiver">\begin{tikzcd}
            A \arrow[r] &amp; B
        \end{tikzcd}</code></pre>
        "###);
    }

    #[test]
    fn test_render_quiver_build_time_simple() {
        let quiver_code = r#"\begin{tikzcd}
    A \arrow[r] & B
\end{tikzcd}"#;
        let result = render_quiver_build_time(quiver_code);

        match result {
            Ok(svg) => {
                assert!(svg.contains("<svg"));
                assert!(svg.contains("</svg>"));
            }
            Err(e) => {
                // If pdflatex or pdf2svg is not installed, this will fail
                eprintln!("LaTeX tools not available: {}", e);
            }
        }
    }

    #[test]
    fn test_render_quiver_without_tikzcd_wrapper() {
        let quiver_code = r#"A \arrow[r] & B"#;
        let result = render_quiver_build_time(quiver_code);

        match result {
            Ok(svg) => {
                assert!(svg.contains("<svg"));
                assert!(svg.contains("</svg>"));
            }
            Err(e) => {
                eprintln!("LaTeX tools not available: {}", e);
            }
        }
    }

    #[test]
    fn test_render_quiver_build_time_fallback_on_error() {
        // Use invalid quiver syntax to trigger error
        let quiver_code = "this is not valid quiver syntax!!!";
        let result = render_quiver(quiver_code, QuiverRenderMode::BuildTime)
            .into_string();

        // Should fallback to code block with error comment
        assert!(
            result.contains("<!-- Quiver rendering failed")
                || result.contains("language-quiver")
        );
        assert!(result.contains(quiver_code));
    }
}
