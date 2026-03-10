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
  width: 8rem;
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
  /* background: var(--border); */
  background: var(--bg);
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
nav.breadcrumb { font-size: 1em; color: var(--fg-dim); margin-bottom: 0.5em; }
nav.breadcrumb a { color: var(--fg-dim); }
nav.breadcrumb a:hover { color: var(--accent2); }
nav.breadcrumb .sep { margin: 0 0.3em; }

/* Callouts */
.callout {
  border-radius: 6px;
  padding: 0.75em 1em;
  margin: 1em 0;
  border: 0;
  border-left: 3px solid var(--callout-color, var(--fg-dim));
  background: color-mix(in srgb, var(--callout-color, var(--fg-dim)) 8%, var(--bg));
}
.callout-title {
  display: flex;
  align-items: center;
  gap: 0.5em;
  font-weight: 600;
  color: var(--callout-color, var(--fg));
  margin-bottom: 0.25em;
}
.callout-title::before {
  content: "";
  display: inline-block;
  width: 1.2em;
  height: 1.2em;
  flex-shrink: 0;
  background-color: var(--callout-color, var(--fg-dim));
  -webkit-mask-size: contain;
  mask-size: contain;
  -webkit-mask-repeat: no-repeat;
  mask-repeat: no-repeat;
  -webkit-mask-position: center;
  mask-position: center;
}
.callout-content > :first-child { margin-top: 0; }
.callout-content > :last-child { margin-bottom: 0; }
details.callout > summary { list-style: none; cursor: pointer; }
details.callout > summary::-webkit-details-marker { display: none; }
details.callout > summary::after {
  content: "";
  display: inline-block;
  width: 1em;
  height: 1em;
  margin-left: auto;
  transition: transform 0.2s;
  background-color: var(--callout-color, var(--fg-dim));
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpolyline points='6 9 12 15 18 9'%3E%3C/polyline%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpolyline points='6 9 12 15 18 9'%3E%3C/polyline%3E%3C/svg%3E");
  -webkit-mask-size: contain;
  mask-size: contain;
}
details.callout:not([open]) > summary::after { transform: rotate(-90deg); }

/* Callout type icons (Lucide SVGs as masks) */
/* note (pencil) */
.callout[data-callout="note"] { --callout-color: var(--accent); }
.callout[data-callout="note"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M12 20h9'/%3E%3Cpath d='M16.376 3.622a1 1 0 0 1 3.002 3.002L7.368 18.635a2 2 0 0 1-.855.506l-2.872.838a.5.5 0 0 1-.62-.62l.838-2.872a2 2 0 0 1 .506-.854z'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M12 20h9'/%3E%3Cpath d='M16.376 3.622a1 1 0 0 1 3.002 3.002L7.368 18.635a2 2 0 0 1-.855.506l-2.872.838a.5.5 0 0 1-.62-.62l.838-2.872a2 2 0 0 1 .506-.854z'/%3E%3C/svg%3E");
}
/* info */
.callout[data-callout="info"] { --callout-color: var(--accent); }
.callout[data-callout="info"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Ccircle cx='12' cy='12' r='10'/%3E%3Cpath d='M12 16v-4'/%3E%3Cpath d='M12 8h.01'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Ccircle cx='12' cy='12' r='10'/%3E%3Cpath d='M12 16v-4'/%3E%3Cpath d='M12 8h.01'/%3E%3C/svg%3E");
}
/* tip/hint/important (flame) */
.callout[data-callout="tip"],
.callout[data-callout="hint"],
.callout[data-callout="important"] { --callout-color: #10b981; }
.callout[data-callout="tip"] .callout-title::before,
.callout[data-callout="hint"] .callout-title::before,
.callout[data-callout="important"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M8.5 14.5A2.5 2.5 0 0 0 11 12c0-1.38-.5-2-1-3-1.072-2.143-.224-4.054 2-6 .5 2.5 2 4.9 4 6.5 2 1.6 3 3.5 3 5.5a7 7 0 1 1-14 0c0-1.153.433-2.294 1-3a2.5 2.5 0 0 0 2.5 2.5z'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M8.5 14.5A2.5 2.5 0 0 0 11 12c0-1.38-.5-2-1-3-1.072-2.143-.224-4.054 2-6 .5 2.5 2 4.9 4 6.5 2 1.6 3 3.5 3 5.5a7 7 0 1 1-14 0c0-1.153.433-2.294 1-3a2.5 2.5 0 0 0 2.5 2.5z'/%3E%3C/svg%3E");
}
/* abstract/summary/tldr (clipboard-list) */
.callout[data-callout="abstract"],
.callout[data-callout="summary"],
.callout[data-callout="tldr"] { --callout-color: #22d3ee; }
.callout[data-callout="abstract"] .callout-title::before,
.callout[data-callout="summary"] .callout-title::before,
.callout[data-callout="tldr"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Crect width='8' height='4' x='8' y='2' rx='1' ry='1'/%3E%3Cpath d='M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2'/%3E%3Cpath d='M12 11h4'/%3E%3Cpath d='M12 16h4'/%3E%3Cpath d='M8 11h.01'/%3E%3Cpath d='M8 16h.01'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Crect width='8' height='4' x='8' y='2' rx='1' ry='1'/%3E%3Cpath d='M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2'/%3E%3Cpath d='M12 11h4'/%3E%3Cpath d='M12 16h4'/%3E%3Cpath d='M8 11h.01'/%3E%3Cpath d='M8 16h.01'/%3E%3C/svg%3E");
}
/* todo (check-circle) */
.callout[data-callout="todo"] { --callout-color: var(--accent); }
.callout[data-callout="todo"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Ccircle cx='12' cy='12' r='10'/%3E%3Cpath d='m9 12 2 2 4-4'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Ccircle cx='12' cy='12' r='10'/%3E%3Cpath d='m9 12 2 2 4-4'/%3E%3C/svg%3E");
}
/* success/check/done */
.callout[data-callout="success"],
.callout[data-callout="check"],
.callout[data-callout="done"] { --callout-color: var(--green); }
.callout[data-callout="success"] .callout-title::before,
.callout[data-callout="check"] .callout-title::before,
.callout[data-callout="done"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M20 6 9 17l-5-5'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M20 6 9 17l-5-5'/%3E%3C/svg%3E");
}
/* question/help/faq */
.callout[data-callout="question"],
.callout[data-callout="help"],
.callout[data-callout="faq"] { --callout-color: var(--orange); }
.callout[data-callout="question"] .callout-title::before,
.callout[data-callout="help"] .callout-title::before,
.callout[data-callout="faq"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Ccircle cx='12' cy='12' r='10'/%3E%3Cpath d='M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3'/%3E%3Cpath d='M12 17h.01'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Ccircle cx='12' cy='12' r='10'/%3E%3Cpath d='M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3'/%3E%3Cpath d='M12 17h.01'/%3E%3C/svg%3E");
}
/* warning/caution/attention (triangle-alert) */
.callout[data-callout="warning"],
.callout[data-callout="caution"],
.callout[data-callout="attention"] { --callout-color: var(--orange); }
.callout[data-callout="warning"] .callout-title::before,
.callout[data-callout="caution"] .callout-title::before,
.callout[data-callout="attention"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='m21.73 18-8-14a2 2 0 0 0-3.48 0l-8 14A2 2 0 0 0 4 21h16a2 2 0 0 0 1.73-3'/%3E%3Cpath d='M12 9v4'/%3E%3Cpath d='M12 17h.01'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='m21.73 18-8-14a2 2 0 0 0-3.48 0l-8 14A2 2 0 0 0 4 21h16a2 2 0 0 0 1.73-3'/%3E%3Cpath d='M12 9v4'/%3E%3Cpath d='M12 17h.01'/%3E%3C/svg%3E");
}
/* failure/fail/missing (x) */
.callout[data-callout="failure"],
.callout[data-callout="fail"],
.callout[data-callout="missing"] { --callout-color: var(--red); }
.callout[data-callout="failure"] .callout-title::before,
.callout[data-callout="fail"] .callout-title::before,
.callout[data-callout="missing"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M18 6 6 18'/%3E%3Cpath d='m6 6 12 12'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M18 6 6 18'/%3E%3Cpath d='m6 6 12 12'/%3E%3C/svg%3E");
}
/* danger/error (zap) */
.callout[data-callout="danger"],
.callout[data-callout="error"] { --callout-color: var(--red); }
.callout[data-callout="danger"] .callout-title::before,
.callout[data-callout="error"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M4 14a1 1 0 0 1-.78-1.63l9.9-10.2a.5.5 0 0 1 .86.46l-1.92 6.02A1 1 0 0 0 13 10h7a1 1 0 0 1 .78 1.63l-9.9 10.2a.5.5 0 0 1-.86-.46l1.92-6.02A1 1 0 0 0 11 14z'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M4 14a1 1 0 0 1-.78-1.63l9.9-10.2a.5.5 0 0 1 .86.46l-1.92 6.02A1 1 0 0 0 13 10h7a1 1 0 0 1 .78 1.63l-9.9 10.2a.5.5 0 0 1-.86-.46l1.92-6.02A1 1 0 0 0 11 14z'/%3E%3C/svg%3E");
}
/* bug */
.callout[data-callout="bug"] { --callout-color: var(--red); }
.callout[data-callout="bug"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='m8 2 1.88 1.88'/%3E%3Cpath d='M14.12 3.88 16 2'/%3E%3Cpath d='M9 7.13v-1a3.003 3.003 0 1 1 6 0v1'/%3E%3Cpath d='M12 20c-3.3 0-6-2.7-6-6v-3a4 4 0 0 1 4-4h4a4 4 0 0 1 4 4v3c0 3.3-2.7 6-6 6'/%3E%3Cpath d='M12 20v-9'/%3E%3Cpath d='M6.53 9C4.6 8.8 3 7.1 3 5'/%3E%3Cpath d='M6 13H2'/%3E%3Cpath d='M3 21c0-2.1 1.7-3.9 3.8-4'/%3E%3Cpath d='M20.97 5c0 2.1-1.6 3.8-3.5 4'/%3E%3Cpath d='M22 13h-4'/%3E%3Cpath d='M17.2 17c2.1.1 3.8 1.9 3.8 4'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='m8 2 1.88 1.88'/%3E%3Cpath d='M14.12 3.88 16 2'/%3E%3Cpath d='M9 7.13v-1a3.003 3.003 0 1 1 6 0v1'/%3E%3Cpath d='M12 20c-3.3 0-6-2.7-6-6v-3a4 4 0 0 1 4-4h4a4 4 0 0 1 4 4v3c0 3.3-2.7 6-6 6'/%3E%3Cpath d='M12 20v-9'/%3E%3Cpath d='M6.53 9C4.6 8.8 3 7.1 3 5'/%3E%3Cpath d='M6 13H2'/%3E%3Cpath d='M3 21c0-2.1 1.7-3.9 3.8-4'/%3E%3Cpath d='M20.97 5c0 2.1-1.6 3.8-3.5 4'/%3E%3Cpath d='M22 13h-4'/%3E%3Cpath d='M17.2 17c2.1.1 3.8 1.9 3.8 4'/%3E%3C/svg%3E");
}
/* example (list) */
.callout[data-callout="example"] { --callout-color: #a78bfa; }
.callout[data-callout="example"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cline x1='8' x2='21' y1='6' y2='6'/%3E%3Cline x1='8' x2='21' y1='12' y2='12'/%3E%3Cline x1='8' x2='21' y1='18' y2='18'/%3E%3Cline x1='3' x2='3.01' y1='6' y2='6'/%3E%3Cline x1='3' x2='3.01' y1='12' y2='12'/%3E%3Cline x1='3' x2='3.01' y1='18' y2='18'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cline x1='8' x2='21' y1='6' y2='6'/%3E%3Cline x1='8' x2='21' y1='12' y2='12'/%3E%3Cline x1='8' x2='21' y1='18' y2='18'/%3E%3Cline x1='3' x2='3.01' y1='6' y2='6'/%3E%3Cline x1='3' x2='3.01' y1='12' y2='12'/%3E%3Cline x1='3' x2='3.01' y1='18' y2='18'/%3E%3C/svg%3E");
}
/* quote/cite */
.callout[data-callout="quote"],
.callout[data-callout="cite"] { --callout-color: var(--fg-dim); }
.callout[data-callout="quote"] .callout-title::before,
.callout[data-callout="cite"] .callout-title::before {
  -webkit-mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M6 21 3 9h4l3-6'/%3E%3Cpath d='M17 21 14 9h4l3-6'/%3E%3C/svg%3E");
  mask-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M6 21 3 9h4l3-6'/%3E%3Cpath d='M17 21 14 9h4l3-6'/%3E%3C/svg%3E");
}|}
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

let tokyonight : t = of_colors tokyonight_colors
let gruvbox : t = of_colors gruvbox_colors
let atom_one_dark : t = of_colors atom_one_dark_colors
let atom_one_light : t = of_colors atom_one_light_colors
let bluloco_dark : t = of_colors bluloco_dark_colors
let bluloco_light : t = of_colors bluloco_light_colors
let default : t = bluloco_dark
