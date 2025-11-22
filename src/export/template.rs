/// Template rendering using minijinja
use super::types::PageContext;
use minijinja::Environment;

/// Default base HTML template
const BASE_TEMPLATE: &str = r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{{ page.title }} - {{ site.title }}</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            line-height: 1.6;
            color: #e4e4e7;
            max-width: 900px;
            margin: 0 auto;
            padding: 2rem;
            background: #18181b;
        }

        header {
            margin-bottom: 2rem;
            padding-bottom: 1rem;
            border-bottom: 2px solid #3f3f46;
        }

        header h1 {
            font-size: 1.5rem;
            color: #fafafa;
        }

        header a {
            text-decoration: none;
            color: #fafafa;
        }

        main {
            background: #27272a;
            padding: 2rem;
            border-radius: 8px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.3);
        }

        article h1 {
            font-size: 2rem;
            margin-bottom: 1rem;
            color: #fafafa;
        }

        article h2 {
            font-size: 1.5rem;
            margin-top: 2rem;
            margin-bottom: 0.75rem;
            color: #e4e4e7;
        }

        article h3 {
            font-size: 1.25rem;
            margin-top: 1.5rem;
            margin-bottom: 0.5rem;
            color: #d4d4d8;
        }

        article p {
            margin-bottom: 1rem;
        }

        article a {
            color: #60a5fa;
            text-decoration: none;
        }

        article a:hover {
            text-decoration: underline;
        }

        article ul, article ol {
            margin-left: 2rem;
            margin-bottom: 1rem;
        }

        article code {
            background: #3f3f46;
            padding: 0.2rem 0.4rem;
            border-radius: 3px;
            font-size: 0.9em;
            font-family: 'Monaco', 'Courier New', monospace;
            color: #fca5a5;
        }

        article pre {
            background: #3f3f46;
            padding: 1rem;
            border-radius: 5px;
            overflow-x: auto;
            margin-bottom: 1rem;
        }

        article pre code {
            background: none;
            padding: 0;
            color: #e4e4e7;
        }

        article blockquote {
            border-left: 4px solid #52525b;
            padding-left: 1rem;
            margin: 1rem 0;
            color: #a1a1aa;
        }

        .backlinks {
            margin-top: 3rem;
            padding-top: 2rem;
            border-top: 1px solid #3f3f46;
        }

        .backlinks h2 {
            font-size: 1.25rem;
            margin-bottom: 1rem;
            color: #a1a1aa;
        }

        .backlinks ul {
            list-style: none;
            margin-left: 0;
        }

        .backlinks li {
            margin-bottom: 0.5rem;
        }

        .backlinks a {
            color: #60a5fa;
            text-decoration: none;
        }

        .backlinks a:hover {
            text-decoration: underline;
        }

        footer {
            margin-top: 2rem;
            padding-top: 1rem;
            border-top: 1px solid #3f3f46;
            text-align: center;
            color: #a1a1aa;
            font-size: 0.9rem;
        }
    </style>
</head>
<body>
    <header>
        <h1><a href="/">{{ site.title }}</a></h1>
    </header>

    <main>
        <article>
            <h1>{{ page.title }}</h1>
            {{ page.content | safe }}
        </article>

        {% if links and links.backlinks %}
        <div class="backlinks">
            <h2>Backlinks</h2>
            <ul>
            {% for backlink in links.backlinks %}
                <li><a href="{{ backlink.path }}">{{ backlink.title }}</a></li>
            {% endfor %}
            </ul>
        </div>
        {% endif %}
    </main>

    <footer>
        <p>footer</p>
    </footer>
</body>
</html>
"#;
// TODO: backlink component should be configurable
// TODO: backlink should contains in-note references as well
// TODO: make CSS configurable
// TODO: make TOC configurable

/// Renders a page using the default template
pub fn render_page(context: &PageContext) -> Result<String, minijinja::Error> {
    let mut env = Environment::new();
    env.add_template("base.html", BASE_TEMPLATE)?;

    let template = env.get_template("base.html")?;
    template.render(context)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::export::types::{PageData, SiteContext};
    use insta::assert_snapshot;

    #[test]
    fn test_render_basic_page() {
        let context = PageContext {
            site: SiteContext {
                title: "Test Site".to_string(),
                base_url: "/".to_string(),
            },
            page: PageData {
                title: "Test Page".to_string(),
                content: "<p>Hello, world!</p>".to_string(),
                path: "/test.html".to_string(),
            },
            links: None,
            toc: None,
        };

        let result = render_page(&context);
        assert!(result.is_ok());

        let html = result.unwrap();
        assert_snapshot!(html, @r#"
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>Test Page - Test Site</title>
            <style>
                * {
                    margin: 0;
                    padding: 0;
                    box-sizing: border-box;
                }

                body {
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
                    line-height: 1.6;
                    color: #333;
                    max-width: 900px;
                    margin: 0 auto;
                    padding: 2rem;
                    background: #fafafa;
                }

                header {
                    margin-bottom: 2rem;
                    padding-bottom: 1rem;
                    border-bottom: 2px solid #eee;
                }

                header h1 {
                    font-size: 1.5rem;
                    color: #111;
                }

                header a {
                    text-decoration: none;
                    color: #111;
                }

                main {
                    background: white;
                    padding: 2rem;
                    border-radius: 8px;
                    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
                }

                article h1 {
                    font-size: 2rem;
                    margin-bottom: 1rem;
                    color: #111;
                }

                article h2 {
                    font-size: 1.5rem;
                    margin-top: 2rem;
                    margin-bottom: 0.75rem;
                    color: #222;
                }

                article h3 {
                    font-size: 1.25rem;
                    margin-top: 1.5rem;
                    margin-bottom: 0.5rem;
                    color: #333;
                }

                article p {
                    margin-bottom: 1rem;
                }

                article a {
                    color: #0066cc;
                    text-decoration: none;
                }

                article a:hover {
                    text-decoration: underline;
                }

                article ul, article ol {
                    margin-left: 2rem;
                    margin-bottom: 1rem;
                }

                article code {
                    background: #f5f5f5;
                    padding: 0.2rem 0.4rem;
                    border-radius: 3px;
                    font-size: 0.9em;
                    font-family: 'Monaco', 'Courier New', monospace;
                }

                article pre {
                    background: #f5f5f5;
                    padding: 1rem;
                    border-radius: 5px;
                    overflow-x: auto;
                    margin-bottom: 1rem;
                }

                article pre code {
                    background: none;
                    padding: 0;
                }

                article blockquote {
                    border-left: 4px solid #ddd;
                    padding-left: 1rem;
                    margin: 1rem 0;
                    color: #666;
                }

                .backlinks {
                    margin-top: 3rem;
                    padding-top: 2rem;
                    border-top: 1px solid #eee;
                }

                .backlinks h2 {
                    font-size: 1.25rem;
                    margin-bottom: 1rem;
                    color: #666;
                }

                .backlinks ul {
                    list-style: none;
                    margin-left: 0;
                }

                .backlinks li {
                    margin-bottom: 0.5rem;
                }

                .backlinks a {
                    color: #0066cc;
                    text-decoration: none;
                }

                .backlinks a:hover {
                    text-decoration: underline;
                }

                footer {
                    margin-top: 2rem;
                    padding-top: 1rem;
                    border-top: 1px solid #eee;
                    text-align: center;
                    color: #666;
                    font-size: 0.9rem;
                }
            </style>
        </head>
        <body>
            <header>
                <h1><a href="/">Test Site</a></h1>
            </header>

            <main>
                <article>
                    <h1>Test Page</h1>
                    <p>Hello, world!</p>
                </article>


            </main>

            <footer>
                <p>footer</p>
            </footer>
        </body>
        </html>
        "#);
    }
}
