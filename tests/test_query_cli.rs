//! Integration tests for query CLI expression flags.

use std::io::Write;
use std::process::Command;

fn oyster() -> Command {
    Command::new(env!("CARGO_BIN_EXE_oyster"))
}

/// Create a temp markdown file and return its path.
fn write_fixture(name: &str, content: &str) -> std::path::PathBuf {
    let path = std::env::temp_dir().join(format!("oyster_test_{name}.md"));
    let mut f = std::fs::File::create(&path).unwrap();
    f.write_all(content.as_bytes()).unwrap();
    path
}

const FIXTURE: &str = "\
---
title: Test
---

Preamble text.

# Introduction

Some intro text.

## Details

Detail content here.

## Summary

Summary content here.

# Conclusion

Final thoughts.
";

// Single expression
// ====================

#[test]
fn no_expressions_returns_identity() {
    let path = write_fixture("identity", FIXTURE);
    let out = oyster()
        .args(["query", path.to_str().unwrap(), "-f", "markdown"])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(stdout.trim(), FIXTURE.trim());
}

#[test]
fn delete_removes_section() {
    let path = write_fixture("delete", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--delete",
            "Details",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(
        !stdout.contains("## Details"),
        "Details section should be removed"
    );
    assert!(stdout.contains("# Introduction"));
    assert!(stdout.contains("# Conclusion"));
}

#[test]
fn field_selects_section() {
    let path = write_fixture("field", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--field",
            "Introduction",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("# Introduction"));
    assert!(stdout.contains("## Details"));
    assert!(
        !stdout.contains("# Conclusion"),
        "Conclusion should not be in the output"
    );
}

#[test]
fn index_selects_child() {
    let path = write_fixture("index", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--index",
            "1",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("# Conclusion"));
    assert!(
        !stdout.contains("# Introduction"),
        "Introduction should not be in the output"
    );
}

#[test]
fn has_returns_true() {
    let path = write_fixture("has_true", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--has",
            "Details",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(stdout.trim(), "true");
}

#[test]
fn has_returns_false() {
    let path = write_fixture("has_false", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--has",
            "Nonexistent",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(stdout.trim(), "false");
}

#[test]
fn inc_increments_headings() {
    let path = write_fixture("inc", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--inc",
            "1",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("## Introduction"), "h1 should become h2");
    assert!(stdout.contains("### Details"), "h2 should become h3");
}

#[test]
fn body_strips_frontmatter() {
    let path = write_fixture("body", FIXTURE);
    let out = oyster()
        .args(["query", path.to_str().unwrap(), "--body", "-f", "markdown"])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(!stdout.contains("---"), "frontmatter should be stripped");
    assert!(!stdout.contains("title: Test"));
    assert!(stdout.contains("# Introduction"));
}

#[test]
fn frontmatter_extracts_yaml() {
    let path = write_fixture("fm", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--frontmatter",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("title: Test"));
}

#[test]
fn nchildren_counts_top_level() {
    let path = write_fixture("nchildren", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--nchildren",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(stdout.trim(), "2");
}

#[test]
fn preface_extracts_preamble() {
    let path = write_fixture("preface", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--preface",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("Preamble text."));
    assert!(!stdout.contains("# Introduction"));
}

// Piped expressions
// ====================

#[test]
fn pipe_body_then_nchildren() {
    let path = write_fixture("pipe_body_n", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--body",
            "--nchildren",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert_eq!(stdout.trim(), "2");
}

#[test]
fn pipe_field_then_nchildren() {
    let path = write_fixture("pipe_field_n", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--field",
            "Introduction",
            "--nchildren",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    // Root has 1 child: the Introduction section itself
    assert_eq!(stdout.trim(), "1");
}

#[test]
fn pipe_field_then_delete() {
    let path = write_fixture("pipe_field_del", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--field",
            "Introduction",
            "--delete",
            "Details",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("# Introduction"));
    assert!(!stdout.contains("## Details"));
    assert!(stdout.contains("## Summary"));
}

// Repeated flags
// ====================

#[test]
fn repeated_inc() {
    let path = write_fixture("rep_inc", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--inc",
            "1",
            "--inc",
            "1",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    // h1 → h2 → h3, h2 → h3 → h4
    assert!(stdout.contains("### Introduction"), "h1 should become h3");
    assert!(stdout.contains("#### Details"), "h2 should become h4");
}

#[test]
fn repeated_delete() {
    let path = write_fixture("rep_del", FIXTURE);
    let out = oyster()
        .args([
            "query",
            path.to_str().unwrap(),
            "--delete",
            "Details",
            "--delete",
            "Summary",
            "-f",
            "markdown",
        ])
        .output()
        .unwrap();
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(!stdout.contains("## Details"));
    assert!(!stdout.contains("## Summary"));
    assert!(stdout.contains("# Introduction"));
    assert!(stdout.contains("# Conclusion"));
}
