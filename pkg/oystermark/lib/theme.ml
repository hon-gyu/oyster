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

let theme_css : string = [%blob "static/theme.css"]

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
  --blockquote-accent: %{c.blockquote_accent};
  --frontmatter-label: %{c.frontmatter_label};
}|}
  ^ "\n"
  ^ theme_css
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
<div class="lightbox" id="lightbox">
<button class="lightbox-close" aria-label="Close">&times;</button>
<img id="lightbox-img" src="" alt="">
</div>
<script>
document.addEventListener("DOMContentLoaded", function() {
  /* Lightbox — runs first so it's not blocked by CDN script failures */
  var lb = document.getElementById("lightbox");
  var lbImg = document.getElementById("lightbox-img");
  document.querySelectorAll("main a > img, main a > video").forEach(function(img) {
    var link = img.closest("a");
    if (!link) return;
    link.addEventListener("click", function(e) {
      e.preventDefault();
      e.stopPropagation();
      lbImg.src = img.src || link.href;
      lbImg.alt = img.alt || "";
      lb.classList.add("active");
    });
  });
  lb.addEventListener("click", function(e) {
    if (e.target !== lbImg) lb.classList.remove("active");
  });
  document.addEventListener("keydown", function(e) {
    if (e.key === "Escape") lb.classList.remove("active");
  });
  /* Syntax highlighting & math */
  if (typeof hljs !== "undefined") hljs.highlightAll();
  if (typeof renderMathInElement !== "undefined") renderMathInElement(document.body, {
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

let atom_one_dark_colors : colors =
  { bg = "#282c34"
  ; bg_alt = "#21252b"
  ; fg = "#abb2bf"
  ; fg_dim = "#5c6370"
  ; accent = "#61afef"
  ; accent2 = "#c678dd"
  ; green = "#98c379"
  ; orange = "#d19a66"
  ; red = "#e06c75"
  ; border = "#3e4452"
  ; blockquote_accent = "var(--accent)"
  ; frontmatter_label = "var(--accent)"
  }
;;

let atom_one_light_colors : colors =
  { bg = "#fafafa"
  ; bg_alt = "#f0f0f0"
  ; fg = "#383a42"
  ; fg_dim = "#a0a1a7"
  ; accent = "#4078f2"
  ; accent2 = "#a626a4"
  ; green = "#50a14f"
  ; orange = "#c18401"
  ; red = "#e45649"
  ; border = "#d3d3d3"
  ; blockquote_accent = "var(--accent)"
  ; frontmatter_label = "var(--accent)"
  }
;;

let bluloco_dark_colors : colors =
  { bg = "#282c34"
  ; bg_alt = "#21252b"
  ; fg = "#abb2bf"
  ; fg_dim = "#636d83"
  ; accent = "#3691ff"
  ; accent2 = "#ce9887"
  ; green = "#3fc56b"
  ; orange = "#f9c859"
  ; red = "#ff6480"
  ; border = "#3b4048"
  ; blockquote_accent = "var(--accent)"
  ; frontmatter_label = "var(--accent)"
  }
;;

let bluloco_light_colors : colors =
  { bg = "#f9f9f9"
  ; bg_alt = "#ededec"
  ; fg = "#383a42"
  ; fg_dim = "#a0a1a7"
  ; accent = "#275fe4"
  ; accent2 = "#7c4dff"
  ; green = "#23974a"
  ; orange = "#df631c"
  ; red = "#d52753"
  ; border = "#d3d3d3"
  ; blockquote_accent = "var(--accent)"
  ; frontmatter_label = "var(--accent)"
  }
;;

let default : t = of_colors bluloco_dark_colors

let of_name = function
  | Config.Tokyonight -> of_colors tokyonight_colors
  | Config.Gruvbox -> of_colors gruvbox_colors
  | Config.Atom_one_light -> of_colors atom_one_light_colors
  | Config.Atom_one_dark -> of_colors atom_one_dark_colors
  | Config.Bluloco_light -> of_colors bluloco_light_colors
  | Config.Bluloco_dark -> of_colors bluloco_dark_colors
  | Config.No_theme -> none
