(** A theme controls the full HTML page layout around rendered markdown content. *)

open Core

type page =
  { title : string
  ; body : string
  ; url_path : string
  ; nav : string
  ; sidebar : string
  }

type t = page -> string

type colors =
  { bg : string
  ; bg_alt : string
  ; fg : string
  ; fg_dim : string
  ; accent : string
  ; accent2 : string
  ; green : string
  ; orange : string
  ; red : string
  ; border : string
  ; blockquote_accent : string
  ; frontmatter_label : string
  }

let css_of_colors (c : colors) : string =
  {%string|:root {
  --bg: %{c.bg};
  --bg-alt: %{c.bg_alt};
  --fg: %{c.fg};
  --fg-dim: %{c.fg_dim};
  --accent: %{c.accent};
  --accent2: %{c.accent2};
  --green: %{c.green};
  --orange: %{c.orange};
  --red: %{c.red};
  --border: %{c.border};
}
*, *::before, *::after { box-sizing: border-box; }
html { font-size: 16px; }
body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
  background: var(--bg);
  color: var(--fg);
  line-height: 1.7;
  margin: 0;
  padding: 0;
}
header {
  border-bottom: 1px solid var(--border);
  padding: 0.5rem 1.5rem;
  background: var(--bg);
}
footer {
  border-top: 1px solid var(--border);
  padding: 0.5rem 1.5rem;
  background: var(--bg);
  font-size: 0.85em;
  color: var(--fg-dim);
}
.layout {
  display: flex;
  min-height: calc(100vh - 3rem);
}
.sidebar {
  width: 10rem;
  flex-shrink: 0;
  padding: 1.5rem 1rem;
  background: var(--bg);
  overflow-y: auto;
  font-size: 0.85em;
  transition: width 0.2s, padding 0.2s;
}
.sidebar ul { list-style: none; padding-left: 1em; margin: 0.2em 0; }
.sidebar > ul { padding-left: 0; }
.sidebar a { color: var(--fg-dim); }
.sidebar a:hover { color: var(--accent2); }
.sidebar details > summary { cursor: pointer; }
.sidebar-handle {
  width: 1px;
  flex-shrink: 0;
  cursor: col-resize;
  background: var(--border);
  position: relative;
  transition: width 0.15s, background 0.15s;
}
.sidebar-handle:hover { width: 10px; background: var(--accent); }
.sidebar-collapsed .sidebar { width: 0; padding: 0; overflow: hidden; }
.sidebar-collapsed .sidebar-handle { width: 6px; cursor: pointer; }
.sidebar-collapsed .sidebar-handle:hover { width: 8px; }
main {
  max-width: 48rem;
  flex: 1;
  margin: 0 auto 0 2rem;
  padding: 2rem 1.5rem;
}
h1, h2, h3, h4, h5, h6 {
  color: var(--accent);
  margin-top: 1.8em;
  margin-bottom: 0.6em;
  line-height: 1.3;
}
h1.page-title { font-size: 2.0em; margin-top: 0; color: var(--fg); border-bottom: none; }
h1 { font-size: 1.8em; border-bottom: 1px solid var(--border); padding-bottom: 0.3em; }
h2 { font-size: 1.6em; }
h3 { font-size: 1.35em; }
h4 { font-size: 1.2em; }
h5 { font-size: 1.1em; }
h6 { font-size: 1.05em; }
a { color: var(--accent2); text-decoration: none; }
a:hover { text-decoration: underline; }
a.unresolved { color: var(--accent2); opacity: 0.7; text-decoration: none; }
a.unresolved:hover { text-decoration: line-through; }
code {
  font-family: "JetBrains Mono", "Fira Code", monospace;
  background: var(--bg-alt);
  padding: 0.15em 0.35em;
  border-radius: 4px;
  font-size: 0.9em;
}
pre {
  background: var(--bg-alt);
  border: 1px solid var(--border);
  border-radius: 6px;
  padding: 1em;
  overflow-x: auto;
}
pre code { background: none; padding: 0; }
pre code.hljs { background: transparent; padding: 0; }
.katex-display { overflow-x: auto; overflow-y: hidden; }
blockquote {
  border-left: 3px solid %{c.blockquote_accent};
  margin-left: 0;
  padding-left: 1em;
  color: var(--fg-dim);
}
table { border-collapse: collapse; width: 100%; }
th, td {
  border: 1px solid var(--border);
  padding: 0.5em 0.75em;
  text-align: left;
}
th { background: var(--bg-alt); }
hr { border: none; border-top: 1px solid var(--border); margin: 2em 0; }
img, video, iframe { max-width: 100%; border-radius: 6px; }
.frontmatter {
  background: var(--bg-alt);
  border: 1px solid var(--border);
  border-radius: 6px;
  padding: 0.5em 1em;
  margin-bottom: 1.5em;
  font-size: 0.9em;
}
.frontmatter table { margin: 0; }
.frontmatter th, .frontmatter td { border: none; padding: 0.1em 0 0.1em 0; vertical-align: top; text-align: left; }
.frontmatter th { width: 1%; padding-right: 1em; }
.frontmatter ul { margin: 0; padding-left: 1.2em; list-style-type: "- "; }
.frontmatter th { color: %{c.frontmatter_label}; background: none; white-space: nowrap; }
.backlinks {
  background: var(--bg-alt);
  border: 1px solid var(--border);
  border-radius: 6px;
  padding: 0.5em 1em;
  margin-top: 2em;
  font-size: 0.9em;
}
.backlinks h2 { margin-top: 0.5em; font-size: 1.2em; }
.backlink-context { list-style: none; margin: 0.3em 0; }
.backlink-context p { margin: 0; }
nav.breadcrumb { font-size: 0.85em; color: var(--fg-dim); margin-bottom: 0.5em; }
nav.breadcrumb a { color: var(--fg-dim); }
nav.breadcrumb a:hover { color: var(--accent2); }
nav.breadcrumb .sep { margin: 0 0.3em; }|}
;;

let wrap ~(css : string) (page : page) : string =
  {%string|<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>%{page.title}</title>
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github-dark.min.css">
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css">
<style>
%{css}
</style>
<script defer src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js"></script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js"></script>
</head>
<body>
<header>
%{page.nav}</header>
<div class="layout">
<aside class="sidebar">
%{page.sidebar}</aside>
<div class="sidebar-handle" onclick="document.body.classList.toggle('sidebar-collapsed')" aria-label="Toggle sidebar"></div>
<main>
<h1 class="page-title">%{page.title}</h1>
%{page.body}</main>
</div>
<footer></footer>
<script>
document.addEventListener("DOMContentLoaded", function() {
  hljs.highlightAll();
  renderMathInElement(document.body, {
    delimiters: [
      {left: "$$", right: "$$", display: true},
      {left: "$", right: "$", display: false},
      {left: "\\(", right: "\\)", display: false},
      {left: "\\[", right: "\\]", display: true}
    ],
    throwOnError: false
  });
});
</script>
</body>
</html>
|}
;;

let of_colors (colors : colors) : t = wrap ~css:(css_of_colors colors)

let none : t =
  fun page ->
  {%string|<!DOCTYPE html>
<html>
<head><meta charset="UTF-8"></head>
<body>
%{page.nav}%{page.sidebar}%{page.body}</body>
</html>
|}
;;

let tokyonight_colors : colors =
  { bg = "#1a1b26"
  ; bg_alt = "#24283b"
  ; fg = "#c0caf5"
  ; fg_dim = "#565f89"
  ; accent = "#7aa2f7"
  ; accent2 = "#bb9af7"
  ; green = "#9ece6a"
  ; orange = "#ff9e64"
  ; red = "#f7768e"
  ; border = "#3b4261"
  ; blockquote_accent = "var(--accent)"
  ; frontmatter_label = "var(--accent)"
  }
;;

let gruvbox_colors : colors =
  { bg = "#282828"
  ; bg_alt = "#3c3836"
  ; fg = "#ebdbb2"
  ; fg_dim = "#928374"
  ; accent = "#fabd2f"
  ; accent2 = "#83a598"
  ; green = "#b8bb26"
  ; orange = "#fe8019"
  ; red = "#fb4934"
  ; border = "#504945"
  ; blockquote_accent = "var(--orange)"
  ; frontmatter_label = "var(--orange)"
  }
;;

let tokyonight : t = of_colors tokyonight_colors
let gruvbox : t = of_colors gruvbox_colors
let default : t = gruvbox
