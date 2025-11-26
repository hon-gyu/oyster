pub fn get_style(name: &str) -> &'static str {
    match name {
        "dracula" => get_dracula_theme(),
        "gruvbox" => get_gruvbox_theme(),
        "tokyonight" => get_tokyonight_theme(),
        _ => get_gruvbox_theme(),
    }
}

fn get_dracula_theme() -> &'static str {
    r#"
        body {
            max-width: 800px;
            margin: 0 auto;
            padding: 2rem;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            background: #282a36;
            color: #f8f8f2;
        }
        h1, h2, h3, h4, h5, h6 {
            margin-top: 1.5em;
            margin-bottom: 0.5em;
            line-height: 1.3;
            color: #bd93f9;
        }
        h1 { font-size: 2em; border-bottom: 2px solid #44475a; padding-bottom: 0.3em; }
        h2 { font-size: 1.5em; }
        h3 { font-size: 1.25em; }
        h4 { font-size: 1.15em; }
        h5 { font-size: 1.1em; }
        h6 { font-size: 1.05em; }
        a { color: #8be9fd; text-decoration: none; }
        a:hover { text-decoration: underline; color: #50fa7b; }
        code {
            background: #44475a;
            padding: 0.2em 0.4em;
            border-radius: 3px;
            font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
            font-size: 0.9em;
            color: #50fa7b;
        }
        pre {
            background: #44475a;
            padding: 1em;
            border-radius: 5px;
            overflow-x: auto;
            border: 1px solid #6272a4;
        }
        pre code {
            background: none;
            padding: 0;
        }
        blockquote {
            border-left: 4px solid #bd93f9;
            padding-left: 1em;
            margin-left: 0;
            color: #6272a4;
        }
        hr {
            border: none;
            border-top: 2px solid #44475a;
            margin: 2em 0;
        }
        .backlinks {
            margin-top: 2em;
            padding: 1em;
            background: #44475a;
            border-radius: 5px;
            border: 1px solid #6272a4;
        }
        .backlinks h5 {
            margin-top: 0;
            font-size: 1em;
            color: #ffb86c;
            text-transform: uppercase;
            letter-spacing: 0.05em;
        }
        .backlinks ul {
            list-style: none;
            padding: 0;
        }
        .backlinks li {
            margin: 0.5em 0;
        }
        .top-nav {
            margin-bottom: 1em;
            padding-bottom: 0.5em;
            border-bottom: 1px solid #44475a;
        }
        .home-link {
            font-size: 0.9em;
            opacity: 0.8;
        }
        .home-link:hover {
            opacity: 1;
        }
        .file-tree ul {
            list-style: none;
            padding-left: 1.5em;
        }
        .file-tree > ul {
            padding-left: 0;
        }
        .file-tree li {
            margin: 0.3em 0;
        }
        .file-tree summary {
            cursor: pointer;
            user-select: none;
            color: #bd93f9;
        }
        .file-tree summary:hover {
            color: #ff79c6;
        }
        .file-tree .file a {
            color: #f8f8f2;
        }
        .file-tree .file a:hover {
            color: #50fa7b;
        }
    "#
}

fn get_gruvbox_theme() -> &'static str {
    r#"
        body {
            max-width: 800px;
            margin: 0 auto;
            padding: 2rem;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            background: #282828;
            color: #ebdbb2;
        }
        h1, h2, h3, h4, h5, h6 {
            margin-top: 1.5em;
            margin-bottom: 0.5em;
            line-height: 1.3;
            color: #fabd2f;
        }
        h1 { font-size: 2em; border-bottom: 2px solid #3c3836; padding-bottom: 0.3em; }
        h2 { font-size: 1.5em; }
        h3 { font-size: 1.25em; }
        h4 { font-size: 1.15em; }
        h5 { font-size: 1.1em; }
        h6 { font-size: 1.05em; }
        a { color: #83a598; text-decoration: none; }
        a:hover { text-decoration: underline; color: #8ec07c; }
        code {
            background: #3c3836;
            padding: 0.2em 0.4em;
            border-radius: 3px;
            font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
            font-size: 0.9em;
            color: #b8bb26;
        }
        pre {
            background: #3c3836;
            padding: 1em;
            border-radius: 5px;
            overflow-x: auto;
            border: 1px solid #504945;
        }
        pre code {
            background: none;
            padding: 0;
        }
        blockquote {
            border-left: 4px solid #d79921;
            padding-left: 1em;
            margin-left: 0;
            color: #a89984;
        }
        hr {
            border: none;
            border-top: 2px solid #3c3836;
            margin: 2em 0;
        }
        .backlinks {
            margin-top: 2em;
            padding: 1em;
            background: #3c3836;
            border-radius: 5px;
            border: 1px solid #504945;
        }
        .backlinks h5 {
            margin-top: 0;
            font-size: 1em;
            color: #fe8019;
            text-transform: uppercase;
            letter-spacing: 0.05em;
        }
        .backlinks ul {
            list-style: none;
            padding: 0;
        }
        .backlinks li {
            margin: 0.5em 0;
        }
        .top-nav {
            margin-bottom: 1em;
            padding-bottom: 0.5em;
            border-bottom: 1px solid #3c3836;
        }
        .home-link {
            font-size: 0.9em;
            opacity: 0.8;
        }
        .home-link:hover {
            opacity: 1;
        }
        .file-tree ul {
            list-style: none;
            padding-left: 1.5em;
        }
        .file-tree > ul {
            padding-left: 0;
        }
        .file-tree li {
            margin: 0.3em 0;
        }
        .file-tree summary {
            cursor: pointer;
            user-select: none;
            color: #fabd2f;
        }
        .file-tree summary:hover {
            color: #fe8019;
        }
        .file-tree .file a {
            color: #ebdbb2;
        }
        .file-tree .file a:hover {
            color: #8ec07c;
        }
    "#
}

fn get_tokyonight_theme() -> &'static str {
    r#"
        body {
            max-width: 800px;
            margin: 0 auto;
            padding: 2rem;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            background: #1a1b26;
            color: #c0caf5;
        }
        h1, h2, h3, h4, h5, h6 {
            margin-top: 1.5em;
            margin-bottom: 0.5em;
            line-height: 1.3;
            color: #7aa2f7;
        }
        h1 { font-size: 2em; border-bottom: 2px solid #24283b; padding-bottom: 0.3em; }
        h2 { font-size: 1.5em; }
        h3 { font-size: 1.25em; }
        h4 { font-size: 1.15em; }
        h5 { font-size: 1.1em; }
        h6 { font-size: 1.05em; }
        a { color: #2ac3de; text-decoration: none; }
        a:hover { text-decoration: underline; color: #9ece6a; }
        code {
            background: #24283b;
            padding: 0.2em 0.4em;
            border-radius: 3px;
            font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
            font-size: 0.9em;
            color: #9ece6a;
        }
        pre {
            background: #24283b;
            padding: 1em;
            border-radius: 5px;
            overflow-x: auto;
            border: 1px solid #414868;
        }
        pre code {
            background: none;
            padding: 0;
        }
        blockquote {
            border-left: 4px solid #bb9af7;
            padding-left: 1em;
            margin-left: 0;
            color: #565f89;
        }
        hr {
            border: none;
            border-top: 2px solid #24283b;
            margin: 2em 0;
        }
        .backlinks {
            margin-top: 2em;
            padding: 1em;
            background: #24283b;
            border-radius: 5px;
            border: 1px solid #414868;
        }
        .backlinks h5 {
            margin-top: 0;
            font-size: 1em;
            color: #ff9e64;
            text-transform: uppercase;
            letter-spacing: 0.05em;
        }
        .backlinks ul {
            list-style: none;
            padding: 0;
        }
        .backlinks li {
            margin: 0.5em 0;
        }
        .top-nav {
            margin-bottom: 1em;
            padding-bottom: 0.5em;
            border-bottom: 1px solid #24283b;
        }
        .home-link {
            font-size: 0.9em;
            opacity: 0.8;
        }
        .home-link:hover {
            opacity: 1;
        }
        .file-tree ul {
            list-style: none;
            padding-left: 1.5em;
        }
        .file-tree > ul {
            padding-left: 0;
        }
        .file-tree li {
            margin: 0.3em 0;
        }
        .file-tree summary {
            cursor: pointer;
            user-select: none;
            color: #7aa2f7;
        }
        .file-tree summary:hover {
            color: #bb9af7;
        }
        .file-tree .file a {
            color: #c0caf5;
        }
        .file-tree .file a:hover {
            color: #9ece6a;
        }
    "#
}
