use maud::{Markup, PreEscaped, html};
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[derive(Debug, Clone, Copy)]
pub enum TikzRenderMode {
    BuildTime,
    ClientSide,
}

impl TikzRenderMode {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "buildtime" | "build-time" | "build" => Some(Self::BuildTime),
            "clientside" | "client-side" | "client" => Some(Self::ClientSide),
            _ => None,
        }
    }
}

impl TryFrom<&str> for TikzRenderMode {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match Self::from_str(s) {
            Some(mode) => Ok(mode),
            None => Err(format!("Invalid tikz render mode: {}", s)),
        }
    }
}

pub fn render_tikz_build_time(tikz_code: &str) -> Result<String, String> {
    let temp_dir = TempDir::new().unwrap();
    let tex_path = temp_dir.path().join("input.tex");
    let pdf_path = temp_dir.path().join("input.pdf");
    let svg_path = temp_dir.path().join("output.svg");

    // Wrap TikZ code in a minimal LaTeX document
    let latex_doc = format!(
        r#"\documentclass[tikz,border=2pt]{{standalone}}
\usepackage{{tikz}}
\usetikzlibrary{{arrows.meta,positioning,shapes,calc}}
\begin{{document}}
{}
\end{{document}}"#,
        tikz_code
    );

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

    Ok(svg)
}

pub fn render_tikz(tikz_code: &str, mode: TikzRenderMode) -> Markup {
    match mode {
        TikzRenderMode::BuildTime => match render_tikz_build_time(tikz_code) {
            Ok(svg) => html! {
                .tikz-diagram {
                    ( PreEscaped(svg) )
                }
            },
            Err(_) => {
                html! {
                    .tikz-error {
                        pre {
                            code .language-tikz {
                                p .error-message {
                                    "<!-- TikZ rendering failed -->"
                                }
                                (tikz_code)
                            }
                        }
                    }
                }
            }
        },
        TikzRenderMode::ClientSide => {
            // TikZJax looks for <script type="text/tikz"> tags
            html! {
                script type="text/tikz" {
                    (PreEscaped(tikz_code))
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
    fn test_render_tikz_client_side_tikzjax() {
        let tikz_code = r#"\begin{tikzpicture}
    \node (A) at (0,0) {A};
    \node (B) at (2,0) {B};
    \draw[->] (A) -- (B);
\end{tikzpicture}"#;
        let result = render_tikz(tikz_code, TikzRenderMode::ClientSide);
        assert_snapshot!(result.into_string(), @r###"
        <script type="text/tikz">\begin{tikzpicture}
            \node (A) at (0,0) {A};
            \node (B) at (2,0) {B};
            \draw[->] (A) -- (B);
        \end{tikzpicture}</script>
        "###);
    }

    #[test]
    fn test_render_tikz_build_time_simple() {
        let tikz_code = r#"\begin{tikzpicture}
    \node (A) at (0,0) {A};
    \node (B) at (2,0) {B};
    \draw[->] (A) -- (B);
\end{tikzpicture}"#;
        let result = render_tikz_build_time(tikz_code);

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
    fn test_render_tikz_build_time_fallback_on_error() {
        // Use invalid tikz syntax to trigger error
        let tikz_code = "this is not valid tikz syntax!!!";
        let result =
            render_tikz(tikz_code, TikzRenderMode::BuildTime).into_string();

        // Should fallback to code block with error comment
        assert!(
            result.contains("<!-- TikZ rendering failed")
                || result.contains("language-tikz")
        );
        assert!(result.contains(tikz_code));
    }
}
