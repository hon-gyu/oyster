use crate::query::Expr;
use clap::ArgMatches;

/// Extract expressions from CLI arguments in the order they were provided
pub fn extract_ordered_exprs(matches: &ArgMatches) -> Vec<Expr> {
    let mut indexed: Vec<(usize, Expr)> = Vec::new();

    // Value-taking repeated args
    collect_string_arg(matches, "field", &mut indexed, Expr::Field);
    collect_i64_arg(matches, "index", &mut indexed, |n| {
        Expr::Index(n as isize)
    });
    collect_slice_arg(matches, &mut indexed);
    collect_i64_arg(matches, "title", &mut indexed, |n| {
        Expr::Title(n as isize)
    });
    collect_string_arg(matches, "has", &mut indexed, Expr::Has);
    collect_string_arg(matches, "delete", &mut indexed, Expr::Del);
    collect_i64_arg(matches, "inc", &mut indexed, |n| Expr::Inc(n as isize));
    collect_i64_arg(matches, "dec", &mut indexed, |n| Expr::Dec(n as isize));
    collect_i64_arg(matches, "code", &mut indexed, |n| Expr::Code(n as isize));
    collect_i64_arg(matches, "codemeta", &mut indexed, |n| {
        Expr::CodeMeta(n as isize)
    });

    // Boolean flags
    collect_flag(matches, "summary", &mut indexed, Expr::Summary);
    collect_flag(matches, "nchildren", &mut indexed, Expr::NChildren);
    collect_flag(matches, "frontmatter", &mut indexed, Expr::Frontmatter);
    collect_flag(matches, "body", &mut indexed, Expr::Body);
    collect_flag(matches, "preface", &mut indexed, Expr::Preface);

    indexed.sort_by_key(|(idx, _)| *idx);
    indexed.into_iter().map(|(_, expr)| expr).collect()
}

fn collect_string_arg(
    matches: &ArgMatches,
    name: &str,
    out: &mut Vec<(usize, Expr)>,
    f: fn(String) -> Expr,
) {
    if let (Some(indices), Some(values)) =
        (matches.indices_of(name), matches.get_many::<String>(name))
    {
        for (idx, val) in indices.zip(values) {
            out.push((idx, f(val.clone())));
        }
    }
}

fn collect_i64_arg(
    matches: &ArgMatches,
    name: &str,
    out: &mut Vec<(usize, Expr)>,
    f: fn(i64) -> Expr,
) {
    if let (Some(indices), Some(values)) =
        (matches.indices_of(name), matches.get_many::<i64>(name))
    {
        for (idx, val) in indices.zip(values) {
            out.push((idx, f(*val)));
        }
    }
}

fn collect_flag(
    matches: &ArgMatches,
    name: &str,
    out: &mut Vec<(usize, Expr)>,
    expr: Expr,
) {
    if matches.get_count(name) > 0 {
        if let Some(indices) = matches.indices_of(name) {
            for idx in indices {
                out.push((idx, expr.clone()));
            }
        }
    }
}

fn collect_slice_arg(matches: &ArgMatches, out: &mut Vec<(usize, Expr)>) {
    if let (Some(indices), Some(values)) = (
        matches.indices_of("slice"),
        matches.get_many::<String>("slice"),
    ) {
        for (idx, val) in indices.zip(values) {
            out.push((idx, parse_slice(val)));
        }
    }
}

fn parse_slice(s: &str) -> Expr {
    let parts: Vec<&str> = s.splitn(2, ':').collect();
    let start = parts.first().and_then(|p| {
        let trimmed = p.trim();
        if trimmed.is_empty() {
            None
        } else {
            trimmed.parse::<isize>().ok()
        }
    });
    let end = parts.get(1).and_then(|p| {
        let trimmed = p.trim();
        if trimmed.is_empty() {
            None
        } else {
            trimmed.parse::<isize>().ok()
        }
    });
    Expr::Slice(start, end)
}

/// Combine multiple expressions into a pipeline
pub fn pipe_exprs(exprs: Vec<Expr>) -> Expr {
    let mut iter = exprs.into_iter();
    let first = iter.next().unwrap();
    iter.fold(first, |acc, expr| Expr::Pipe(Box::new(acc), Box::new(expr)))
}
