(** A theme controls the full HTML page layout around rendered markdown content. *)

open Core

type page =
  { body : string
  ; url_path : string
  }

type t = page -> string

let wrap ~(css : string) (page : page) : string =
  String.concat
    [ "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n<style>\n"
    ; css
    ; "\n</style>\n</head>\n<body>\n<main>\n"
    ; page.body
    ; "</main>\n</body>\n</html>\n"
    ]
;;

let none : t =
  fun page ->
  String.concat
    [ "<!DOCTYPE html>\n<html>\n<head><meta charset=\"UTF-8\"></head>\n<body>\n"
    ; page.body
    ; "</body>\n</html>\n"
    ]
;;

let tokyonight : t =
  wrap
    ~css:
      {|:root {
  --bg: #1a1b26;
  --bg-alt: #24283b;
  --fg: #c0caf5;
  --fg-dim: #565f89;
  --accent: #7aa2f7;
  --accent2: #bb9af7;
  --green: #9ece6a;
  --orange: #ff9e64;
  --red: #f7768e;
  --border: #3b4261;
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
main {
  max-width: 48rem;
  margin: 2rem auto;
  padding: 0 1.5rem;
}
h1, h2, h3, h4, h5, h6 {
  color: var(--accent);
  margin-top: 1.8em;
  margin-bottom: 0.6em;
  line-height: 1.3;
}
h1 { font-size: 2em; border-bottom: 1px solid var(--border); padding-bottom: 0.3em; }
h2 { font-size: 1.6em; }
h3 { font-size: 1.35em; }
h4 { font-size: 1.2em; }
h5 { font-size: 1.1em; }
h6 { font-size: 1.05em; }
a { color: var(--accent2); text-decoration: none; }
a:hover { text-decoration: underline; }
a.unresolved { color: var(--red); text-decoration: underline wavy; }
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
blockquote {
  border-left: 3px solid var(--accent);
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
.frontmatter th { color: var(--accent); background: none; border: none; }
.frontmatter td { border: none; }|}
;;

let gruvbox : t =
  wrap
    ~css:
      {|:root {
  --bg: #282828;
  --bg-alt: #3c3836;
  --fg: #ebdbb2;
  --fg-dim: #928374;
  --accent: #fabd2f;
  --accent2: #83a598;
  --green: #b8bb26;
  --orange: #fe8019;
  --red: #fb4934;
  --border: #504945;
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
main {
  max-width: 48rem;
  margin: 2rem auto;
  padding: 0 1.5rem;
}
h1, h2, h3, h4, h5, h6 {
  color: var(--accent);
  margin-top: 1.8em;
  margin-bottom: 0.6em;
  line-height: 1.3;
}
h1 { font-size: 2em; border-bottom: 1px solid var(--border); padding-bottom: 0.3em; }
h2 { font-size: 1.6em; }
h3 { font-size: 1.35em; }
h4 { font-size: 1.2em; }
h5 { font-size: 1.1em; }
h6 { font-size: 1.05em; }
a { color: var(--accent2); text-decoration: none; }
a:hover { text-decoration: underline; }
a.unresolved { color: var(--red); text-decoration: underline wavy; }
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
blockquote {
  border-left: 3px solid var(--orange);
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
.frontmatter th { color: var(--orange); background: none; border: none; }
.frontmatter td { border: none; }|}
;;

let default : t = gruvbox
