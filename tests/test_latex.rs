use insta::assert_snapshot;
use markdown_tools::ast::Tree;
use markdown_tools::export::latex::render_latex;
use std::fs;

#[test]
fn test_latex_inline_basic() {
    let latex = "E = mc^2";
    let rendered = render_latex(latex, false);
    // KaTeX renders to HTML, just check it contains the LaTeX content
    assert!(rendered.contains("E"));
    assert!(rendered.contains("mc"));
}

#[test]
fn test_latex_display_basic() {
    let latex = r"\int_{-\infty}^{\infty} e^{-x^2} dx = \sqrt{\pi}";
    let rendered = render_latex(latex, true);
    // KaTeX renders to HTML with display mode
    assert!(rendered.contains("katex-display") || rendered.contains("katex"));
}

#[test]
fn test_latex_error_handling() {
    let invalid_latex = r"\invalid{command}";
    let rendered = render_latex(invalid_latex, false);
    // Should contain error class or the original LaTeX
    assert!(rendered.contains("math-error") || rendered.contains("invalid"));
}

#[test]
fn test_parse_latex_vault_basic() {
    let path = "tests/data/vaults/latex/basic-math.md";
    let text = fs::read_to_string(path).unwrap();
    let tree = Tree::new(&text);

    // Check that math nodes are parsed
    let content = format!("{:?}", tree.root_node);
    assert!(content.contains("InlineMath"));
    assert!(content.contains("DisplayMath"));
}

#[test]
fn test_parse_latex_vault_advanced() {
    let path = "tests/data/vaults/latex/advanced-math.md";
    let text = fs::read_to_string(path).unwrap();
    let tree = Tree::new(&text);

    // Check that complex math is parsed
    let content = format!("{:?}", tree.root_node);
    assert!(content.contains("InlineMath") || content.contains("DisplayMath"));
}

#[test]
fn test_render_latex_inline_snapshot() {
    let latex = r"x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}";
    let rendered = render_latex(latex, false);

    // Snapshot test for inline math rendering
    assert_snapshot!(rendered);
}

#[test]
fn test_render_latex_display_snapshot() {
    let latex = r"\sum_{i=1}^{n} i = \frac{n(n+1)}{2}";
    let rendered = render_latex(latex, true);

    // Snapshot test for display math rendering
    assert_snapshot!(rendered);
}
