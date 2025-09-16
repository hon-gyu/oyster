use crate::validate;
use nonempty;
/// AST is tree-sitter like
use pulldown_cmark::{
    Alignment, BlockQuoteKind, CodeBlockKind, CowStr, Event, HeadingLevel,
    LinkType, MetadataBlockKind, Options, Tag, TagEnd,
};
use std::ops::Range;

/// A tree that represents the syntactic structure of a source code file.
#[derive(Clone, Debug, PartialEq)]
pub struct Tree<'a> {
    root_node: Node<'a>,
    /// the options that were used to parse the syntax tree.
    opts: Options,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node<'a> {
    children: Vec<Node<'a>>,
    range: Range<usize>,
    kind: NodeKind<'a>,
}

impl Tree<'_> {
    /// Copied from tree-sitter's Tree struct
    /// We may want something in other format
    fn print_dot_graph(&self) {
        todo!()
    }

    fn into_string(&self) -> String {
        todo!()
    }
}

/// including Tag + Event that is not start or end
#[derive(Clone, Debug, PartialEq)]
enum NodeKind<'a> {
    /// Root node
    Document,
    /// Tag
    Paragraph,
    Heading {
        level: HeadingLevel,
        id: Option<CowStr<'a>>,
        classes: Vec<CowStr<'a>>,
        attrs: Vec<(CowStr<'a>, Option<CowStr<'a>>)>,
    },
    BlockQuote(Option<BlockQuoteKind>),
    CodeBlock(CodeBlockKind<'a>),
    HtmlBlock,
    List(Option<u64>),
    Item,
    FootnoteDefinition(CowStr<'a>),
    DefinitionList,
    DefinitionListTitle,
    DefinitionListDefinition,
    Table(Vec<Alignment>),
    TableHead,
    TableRow,
    TableCell,
    Emphasis,
    Strong,
    Strikethrough,
    Superscript,
    Subscript,
    Link {
        link_type: LinkType,
        dest_url: CowStr<'a>,
        title: CowStr<'a>,
        id: CowStr<'a>,
    },
    Image {
        link_type: LinkType,
        dest_url: CowStr<'a>,
        title: CowStr<'a>,
        id: CowStr<'a>,
    },
    MetadataBlock(MetadataBlockKind),

    /// inline Events
    Text(CowStr<'a>),
    Code(CowStr<'a>),
    InlineMath(CowStr<'a>),
    DisplayMath(CowStr<'a>),
    Html(CowStr<'a>),
    InlineHtml(CowStr<'a>),
    FootnoteReference(CowStr<'a>),
    SoftBreak,
    HardBreak,
    Rule,
    TaskListMarker(bool),
}

impl<'a> NodeKind<'a> {
    fn from_tag(tag: Tag<'a>) -> Self {
        match tag {
            Tag::Paragraph => Self::Paragraph,
            Tag::Heading {
                level,
                id,
                classes,
                attrs,
            } => Self::Heading {
                level,
                id,
                classes,
                attrs,
            },
            Tag::BlockQuote(x) => Self::BlockQuote(x),
            Tag::CodeBlock(x) => Self::CodeBlock(x),
            Tag::HtmlBlock => Self::HtmlBlock,
            Tag::List(x) => Self::List(x),
            Tag::Item => Self::Item,
            Tag::FootnoteDefinition(x) => Self::FootnoteDefinition(x),
            Tag::DefinitionList => Self::DefinitionList,
            Tag::DefinitionListTitle => Self::DefinitionListTitle,
            Tag::DefinitionListDefinition => Self::DefinitionListDefinition,
            Tag::Table(x) => Self::Table(x),
            Tag::TableHead => Self::TableHead,
            Tag::TableRow => Self::TableRow,
            Tag::TableCell => Self::TableCell,
            Tag::Emphasis => Self::Emphasis,
            Tag::Strong => Self::Strong,
            Tag::Strikethrough => Self::Strikethrough,
            Tag::Superscript => Self::Superscript,
            Tag::Subscript => Self::Subscript,
            Tag::Link {
                link_type,
                dest_url,
                title,
                id,
            } => Self::Link {
                link_type,
                dest_url,
                title,
                id,
            },
            Tag::Image {
                link_type,
                dest_url,
                title,
                id,
            } => Self::Image {
                link_type,
                dest_url,
                title,
                id,
            },
            Tag::MetadataBlock(x) => Self::MetadataBlock(x),
        }
    }

    fn from_event(event: Event<'a>) -> Self {
        match event {
            Event::Text(x) => Self::Text(x),
            Event::Code(x) => Self::Code(x),
            Event::InlineMath(x) => Self::InlineMath(x),
            Event::DisplayMath(x) => Self::DisplayMath(x),
            Event::Html(x) => Self::Html(x),
            Event::InlineHtml(x) => Self::InlineHtml(x),
            Event::FootnoteReference(x) => Self::FootnoteReference(x),
            Event::SoftBreak => Self::SoftBreak,
            Event::HardBreak => Self::HardBreak,
            Event::Rule => Self::Rule,
            Event::TaskListMarker(x) => Self::TaskListMarker(x),
            Event::Start(_) => {
                panic!("Unexpected start event, only inline events are allowed")
            }
            Event::End(_) => {
                panic!("Unexpected end event, only inline events are allowed")
            }
        }
    }

    fn into_static(self) -> NodeKind<'static> {
        match self {
            NodeKind::Document => NodeKind::Document,
            NodeKind::Paragraph => NodeKind::Paragraph,
            NodeKind::Heading {
                level,
                id,
                classes,
                attrs,
            } => NodeKind::Heading {
                level,
                id: id.map(|s| s.into_static()),
                classes: classes.into_iter().map(|s| s.into_static()).collect(),
                attrs: attrs
                    .into_iter()
                    .map(|(k, v)| (k.into_static(), v.map(|s| s.into_static())))
                    .collect(),
            },
            NodeKind::BlockQuote(k) => NodeKind::BlockQuote(k),
            NodeKind::CodeBlock(kb) => NodeKind::CodeBlock(kb.into_static()),
            NodeKind::HtmlBlock => NodeKind::HtmlBlock,
            NodeKind::List(v) => NodeKind::List(v),
            NodeKind::Item => NodeKind::Item,
            NodeKind::FootnoteDefinition(a) => {
                NodeKind::FootnoteDefinition(a.into_static())
            }
            NodeKind::Table(v) => NodeKind::Table(v),
            NodeKind::TableHead => NodeKind::TableHead,
            NodeKind::TableRow => NodeKind::TableRow,
            NodeKind::TableCell => NodeKind::TableCell,
            NodeKind::Emphasis => NodeKind::Emphasis,
            NodeKind::Strong => NodeKind::Strong,
            NodeKind::Strikethrough => NodeKind::Strikethrough,
            NodeKind::Superscript => NodeKind::Superscript,
            NodeKind::Subscript => NodeKind::Subscript,
            NodeKind::Link {
                link_type,
                dest_url,
                title,
                id,
            } => NodeKind::Link {
                link_type,
                dest_url: dest_url.into_static(),
                title: title.into_static(),
                id: id.into_static(),
            },
            NodeKind::Image {
                link_type,
                dest_url,
                title,
                id,
            } => NodeKind::Image {
                link_type,
                dest_url: dest_url.into_static(),
                title: title.into_static(),
                id: id.into_static(),
            },
            NodeKind::MetadataBlock(v) => NodeKind::MetadataBlock(v),
            NodeKind::DefinitionList => NodeKind::DefinitionList,
            NodeKind::DefinitionListTitle => NodeKind::DefinitionListTitle,
            NodeKind::DefinitionListDefinition => {
                NodeKind::DefinitionListDefinition
            }
            NodeKind::Text(s) => NodeKind::Text(s.into_static()),
            NodeKind::Code(s) => NodeKind::Code(s.into_static()),
            NodeKind::InlineMath(s) => NodeKind::InlineMath(s.into_static()),
            NodeKind::DisplayMath(s) => NodeKind::DisplayMath(s.into_static()),
            NodeKind::Html(s) => NodeKind::Html(s.into_static()),
            NodeKind::InlineHtml(s) => NodeKind::InlineHtml(s.into_static()),
            NodeKind::FootnoteReference(s) => {
                NodeKind::FootnoteReference(s.into_static())
            }
            NodeKind::SoftBreak => NodeKind::SoftBreak,
            NodeKind::HardBreak => NodeKind::HardBreak,
            NodeKind::Rule => NodeKind::Rule,
            NodeKind::TaskListMarker(b) => NodeKind::TaskListMarker(b),
        }
    }
}

pub enum InvalidNode {
    InvalidNode,
}

impl validate::Validate for Node<'_> {
    type ValidationError = InvalidNode;

    fn validate(
        &self,
    ) -> Result<(), nonempty::NonEmpty<Self::ValidationError>> {
        todo!()
    }
}

impl Node<'_> {
    fn child(&self, i: usize) -> Option<&Node<'_>> {
        todo!()
    }

    fn child_count(&self) -> usize {
        todo!()
    }
}

pub fn build_ast<'a>(
    events_with_offset: Vec<(Event<'a>, Range<usize>)>,
    opts: Options,
) -> Tree<'a> {
    // Stack to keep track of the things we are working on (excluding the root)
    // Each item in the stack is a tuple containing the current node and its previous
    // siblings.
    // When we detect a deeper nesting level, we push a new node and its existing siblings
    let mut stack: Vec<(Node<'a>, Vec<Node<'a>>)> = Vec::new();
    let mut curr_children: Vec<Node<'a>> = Vec::new();

    let doc_start = events_with_offset
        .first()
        .map(|(_, r)| r.start)
        .expect("No events found");
    let doc_end = events_with_offset
        .last()
        .map(|(_, r)| r.end)
        .expect("No events found");

    for (event, offset) in events_with_offset {
        match event {
            Event::Start(tag) => {
                let node = Node {
                    children: Vec::new(),
                    range: offset.clone(),
                    kind: NodeKind::from_tag(tag),
                };
                stack.push((node, curr_children));
                curr_children = Vec::new();
            }
            Event::End(_tag) => {
                // Wrap up the current node
                let (mut completed_node, siblings) =
                    stack.pop().expect("Unbalanced tags");
                completed_node.children = curr_children;

                curr_children = siblings;
                curr_children.push(completed_node);
            }
            inline_event => {
                let leaf_node = Node {
                    children: Vec::new(),
                    range: offset.clone(),
                    kind: NodeKind::from_event(inline_event),
                };
                curr_children.push(leaf_node);
            }
        }
    }

    let root_node = Node {
        children: curr_children,
        range: doc_start..doc_end,
        kind: NodeKind::Document,
    };

    Tree {
        root_node: root_node,
        opts: opts,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use insta::{assert_debug_snapshot, assert_snapshot};
    use pulldown_cmark::{Event, Options, Parser};
    use std::fs;

    fn data() -> String {
        let md = fs::read_to_string("src/basic.md").unwrap();
        md
    }

    #[test]
    fn test_build_ast() {
        let md = data();
        let opts = parse::default_opts();
        let parser = Parser::new_ext(&md, opts);
        let events_with_offsets = parser.into_offset_iter().collect::<Vec<_>>();

        let ast = build_ast(events_with_offsets, opts);
        assert_debug_snapshot!(ast, @r#"
        Tree {
            root_node: Node {
                children: [
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 4..49,
                                kind: Text(
                                    Borrowed(
                                        "title: \"Test Document\"\nauthor: \"Test Author\"\n",
                                    ),
                                ),
                            },
                        ],
                        range: 0..52,
                        kind: MetadataBlock(
                            YamlStyle,
                        ),
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 56..65,
                                kind: Text(
                                    Borrowed(
                                        "Heading 1",
                                    ),
                                ),
                            },
                        ],
                        range: 54..66,
                        kind: Heading {
                            level: H1,
                            id: None,
                            classes: [],
                            attrs: [],
                        },
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 67..83,
                                kind: Text(
                                    Borrowed(
                                        "Basic text with ",
                                    ),
                                ),
                            },
                            Node {
                                children: [
                                    Node {
                                        children: [],
                                        range: 85..89,
                                        kind: Text(
                                            Borrowed(
                                                "bold",
                                            ),
                                        ),
                                    },
                                ],
                                range: 83..91,
                                kind: Strong,
                            },
                            Node {
                                children: [],
                                range: 91..96,
                                kind: Text(
                                    Borrowed(
                                        " and ",
                                    ),
                                ),
                            },
                            Node {
                                children: [
                                    Node {
                                        children: [],
                                        range: 97..103,
                                        kind: Text(
                                            Borrowed(
                                                "italic",
                                            ),
                                        ),
                                    },
                                ],
                                range: 96..104,
                                kind: Emphasis,
                            },
                            Node {
                                children: [],
                                range: 104..116,
                                kind: Text(
                                    Borrowed(
                                        " formatting.",
                                    ),
                                ),
                            },
                        ],
                        range: 67..117,
                        kind: Paragraph,
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 121..127,
                                kind: Text(
                                    Borrowed(
                                        "A List",
                                    ),
                                ),
                            },
                        ],
                        range: 118..128,
                        kind: Heading {
                            level: H2,
                            id: None,
                            classes: [],
                            attrs: [],
                        },
                    },
                    Node {
                        children: [
                            Node {
                                children: [
                                    Node {
                                        children: [],
                                        range: 131..137,
                                        kind: Text(
                                            Borrowed(
                                                "item 1",
                                            ),
                                        ),
                                    },
                                    Node {
                                        children: [
                                            Node {
                                                children: [
                                                    Node {
                                                        children: [],
                                                        range: 142..150,
                                                        kind: Text(
                                                            Borrowed(
                                                                "item 1.1",
                                                            ),
                                                        ),
                                                    },
                                                ],
                                                range: 140..151,
                                                kind: Item,
                                            },
                                            Node {
                                                children: [
                                                    Node {
                                                        children: [],
                                                        range: 155..163,
                                                        kind: Text(
                                                            Borrowed(
                                                                "item 1.2",
                                                            ),
                                                        ),
                                                    },
                                                ],
                                                range: 153..164,
                                                kind: Item,
                                            },
                                        ],
                                        range: 140..164,
                                        kind: List(
                                            None,
                                        ),
                                    },
                                ],
                                range: 129..164,
                                kind: Item,
                            },
                            Node {
                                children: [
                                    Node {
                                        children: [],
                                        range: 166..172,
                                        kind: Text(
                                            Borrowed(
                                                "item 2",
                                            ),
                                        ),
                                    },
                                    Node {
                                        children: [
                                            Node {
                                                children: [
                                                    Node {
                                                        children: [],
                                                        range: 177..185,
                                                        kind: Text(
                                                            Borrowed(
                                                                "item 2.1",
                                                            ),
                                                        ),
                                                    },
                                                ],
                                                range: 175..187,
                                                kind: Item,
                                            },
                                        ],
                                        range: 175..187,
                                        kind: List(
                                            None,
                                        ),
                                    },
                                ],
                                range: 164..187,
                                kind: Item,
                            },
                        ],
                        range: 129..187,
                        kind: List(
                            None,
                        ),
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 190..200,
                                kind: Text(
                                    Borrowed(
                                        "Task Lists",
                                    ),
                                ),
                            },
                        ],
                        range: 187..206,
                        kind: Heading {
                            level: H2,
                            id: None,
                            classes: [],
                            attrs: [],
                        },
                    },
                    Node {
                        children: [
                            Node {
                                children: [
                                    Node {
                                        children: [],
                                        range: 209..212,
                                        kind: TaskListMarker(
                                            true,
                                        ),
                                    },
                                    Node {
                                        children: [],
                                        range: 213..227,
                                        kind: Text(
                                            Borrowed(
                                                "Completed task",
                                            ),
                                        ),
                                    },
                                ],
                                range: 207..228,
                                kind: Item,
                            },
                            Node {
                                children: [
                                    Node {
                                        children: [],
                                        range: 230..233,
                                        kind: TaskListMarker(
                                            false,
                                        ),
                                    },
                                    Node {
                                        children: [],
                                        range: 234..249,
                                        kind: Text(
                                            Borrowed(
                                                "Incomplete task",
                                            ),
                                        ),
                                    },
                                    Node {
                                        children: [
                                            Node {
                                                children: [
                                                    Node {
                                                        children: [],
                                                        range: 254..257,
                                                        kind: TaskListMarker(
                                                            true,
                                                        ),
                                                    },
                                                    Node {
                                                        children: [],
                                                        range: 258..274,
                                                        kind: Text(
                                                            Borrowed(
                                                                "Nested completed",
                                                            ),
                                                        ),
                                                    },
                                                ],
                                                range: 252..275,
                                                kind: Item,
                                            },
                                            Node {
                                                children: [
                                                    Node {
                                                        children: [],
                                                        range: 279..282,
                                                        kind: TaskListMarker(
                                                            false,
                                                        ),
                                                    },
                                                    Node {
                                                        children: [],
                                                        range: 283..300,
                                                        kind: Text(
                                                            Borrowed(
                                                                "Nested incomplete",
                                                            ),
                                                        ),
                                                    },
                                                ],
                                                range: 277..302,
                                                kind: Item,
                                            },
                                        ],
                                        range: 252..302,
                                        kind: List(
                                            None,
                                        ),
                                    },
                                ],
                                range: 228..302,
                                kind: Item,
                            },
                        ],
                        range: 207..302,
                        kind: List(
                            None,
                        ),
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 305..318,
                                kind: Text(
                                    Borrowed(
                                        "Strikethrough",
                                    ),
                                ),
                            },
                        ],
                        range: 302..319,
                        kind: Heading {
                            level: H2,
                            id: None,
                            classes: [],
                            attrs: [],
                        },
                    },
                    Node {
                        children: [
                            Node {
                                children: [
                                    Node {
                                        children: [],
                                        range: 322..346,
                                        kind: Text(
                                            Borrowed(
                                                "This text is crossed out",
                                            ),
                                        ),
                                    },
                                ],
                                range: 320..348,
                                kind: Strikethrough,
                            },
                        ],
                        range: 320..349,
                        kind: Paragraph,
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 353..363,
                                kind: Text(
                                    Borrowed(
                                        "Code Block",
                                    ),
                                ),
                            },
                        ],
                        range: 350..364,
                        kind: Heading {
                            level: H2,
                            id: None,
                            classes: [],
                            attrs: [],
                        },
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 375..399,
                                kind: Text(
                                    Borrowed(
                                        "def foo():\n    return 1\n",
                                    ),
                                ),
                            },
                        ],
                        range: 365..402,
                        kind: CodeBlock(
                            Fenced(
                                Borrowed(
                                    "python",
                                ),
                            ),
                        ),
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 407..411,
                                kind: Text(
                                    Borrowed(
                                        "Math",
                                    ),
                                ),
                            },
                        ],
                        range: 404..412,
                        kind: Heading {
                            level: H2,
                            id: None,
                            classes: [],
                            attrs: [],
                        },
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 413..426,
                                kind: Text(
                                    Borrowed(
                                        "Inline math: ",
                                    ),
                                ),
                            },
                            Node {
                                children: [],
                                range: 426..436,
                                kind: InlineMath(
                                    Borrowed(
                                        "E = mc^2",
                                    ),
                                ),
                            },
                        ],
                        range: 413..437,
                        kind: Paragraph,
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 438..449,
                                kind: Text(
                                    Borrowed(
                                        "Block math:",
                                    ),
                                ),
                            },
                            Node {
                                children: [],
                                range: 449..450,
                                kind: SoftBreak,
                            },
                            Node {
                                children: [],
                                range: 450..504,
                                kind: DisplayMath(
                                    Boxed(
                                        "\n\\int_{-\\infty}^{\\infty} e^{-x^2} dx = \\sqrt{\\pi}\n",
                                    ),
                                ),
                            },
                        ],
                        range: 438..505,
                        kind: Paragraph,
                    },
                    Node {
                        children: [
                            Node {
                                children: [],
                                range: 510..521,
                                kind: Text(
                                    Borrowed(
                                        "Heading 3.1",
                                    ),
                                ),
                            },
                        ],
                        range: 506..522,
                        kind: Heading {
                            level: H3,
                            id: None,
                            classes: [],
                            attrs: [],
                        },
                    },
                ],
                range: 0..522,
                kind: Document,
            },
            opts: Options(
                ENABLE_TABLES | ENABLE_FOOTNOTES | ENABLE_STRIKETHROUGH | ENABLE_TASKLISTS | ENABLE_SMART_PUNCTUATION | ENABLE_HEADING_ATTRIBUTES | ENABLE_YAML_STYLE_METADATA_BLOCKS | ENABLE_PLUSES_DELIMITED_METADATA_BLOCKS | ENABLE_MATH | ENABLE_GFM | ENABLE_DEFINITION_LIST | ENABLE_SUPERSCRIPT | ENABLE_SUBSCRIPT | ENABLE_WIKILINKS,
            ),
        }
        "#);
    }
}
