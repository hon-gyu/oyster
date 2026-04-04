# Struct Specification

Struct is a syntax extension that restructures the document tree based on colon-suffixed labels. A colon at the end of a list item or paragraph declares that the following contiguous content is its children, causing the `Cmarkit.Doc` tree to be rewritten with new parent-child relationships.

## Syntax

A **keyed node** is a list item or paragraph whose text content ends with `:` (a single colon, excluding trailing whitespace).

```
- foo:          ← keyed list item
foo:            ← keyed paragraph
- foo: bar:     ← "foo:" is parent, "bar:" is a nested keyed node
- foo\: bar:    ← "foo: bar" is the label (escaped colon), keyed
- foo           ← not keyed (no trailing colon)
```

### Escaped Colons

A colon preceded by a backslash (`\:`) is not a key delimiter. It is treated as literal text. Only an unescaped trailing colon makes a node keyed.

```
- foo\: bar:    ← label is "foo: bar", keyed
- foo\: bar     ← label is "foo: bar", NOT keyed
- foo: bar:     ← "foo:" is parent of "bar:"
```

### Inline Keying (Colon Chains)

When a list item or paragraph contains multiple unescaped colons, each colon-terminated segment introduces a nesting level. The first segment is the outermost parent, and the last segment is the innermost keyed node.

`````markdown
- foo: bar:
  - baz
`````

```
List
└── KeyedListItem "foo"
    └── KeyedListItem "bar"
        └── List
            └── ListItem "baz"
```

`foo:` is the parent of `bar:`, and `bar:` is the parent of `baz`.

## Tree Restructuring Rules

### Rule 1: Keyed list item with indented content

Already parsed as children by CommonMark. No transform needed.

`````markdown
- foo:
  - bar
  - baz
`````

```
List
└── KeyedListItem "foo"
    └── List
        ├── ListItem "bar"
        └── ListItem "baz"
```

### Rule 2: Keyed list item with empty continuation

If the next element after a colon-suffixed list item is blank (empty), the colon is not structural. The item remains a regular `ListItem` with the colon preserved as literal text.

`````markdown
- foo:

bar
`````

```
List
└── ListItem "foo:"
Paragraph "bar"
```

`bar` is not a child of `foo:`. The blank line means the colon has no effect — it's just punctuation.

### Rule 3: Keyed list item with contiguous block content

Content that is contiguous (no blank line) after a keyed list item becomes its children, even if CommonMark would not normally nest it. The content does **not** need to be indented.

`````markdown
- foo:
```
bar
```
`````

```
List
└── KeyedListItem "foo"
    └── CodeBlock "bar"
```

In standard CommonMark, the unindented code block would not be a child of the list item. Struct overrides this: the colon claims the following contiguous content regardless of indentation.

### Rule 4: Keyed paragraph with following list

A bare keyed paragraph reparents the immediately following contiguous content as its children.

`````markdown
foo:
- bar
- baz

bee
`````

Before transform (CommonMark parse):
```
Paragraph "foo:"
List
├── ListItem "bar"
└── ListItem "baz"
Paragraph "bee"
```

After transform:
```
KeyedBlock "foo"
└── List
    ├── ListItem "bar"
    └── ListItem "baz"
Paragraph "bee"
```

`bee` is not a child of `foo:` — the blank line breaks the scope.

### Rule 5: Keyed paragraph with contiguous blocks

A keyed paragraph claims all immediately following contiguous blocks (no blank line separation) as children.

`````markdown
foo:
- bar
- baz
some text
`````

After transform:
```
KeyedBlock "foo"
├── List
│   ├── ListItem "bar"
│   └── ListItem "baz"
└── Paragraph "some text"
```

### Rule 6: Nesting

Keyed nodes can nest, forming deeper trees.

`````markdown
foo:
- bar:
  - baz
- qux
`````

```
KeyedBlock "foo"
└── List
    ├── KeyedListItem "bar"
    │   └── List
    │       └── ListItem "baz"
    └── ListItem "qux"
```

Here `qux` is both a child of `foo:` and a sibling of `bar:`.

## Scope Termination

A keyed node's scope (the content it claims as children) ends when:

1. **Blank line** — a blank line after the keyed node terminates its scope
2. **End of parent block** — the enclosing block's boundary terminates scope
3. **Empty next element** — if the immediately next element is empty/blank, no children are claimed (Rule 2)

## Constraints

- A keyed node's label **cannot contain a hard break**. The colon must be on the same inline span as the rest of the label.

## Interaction with Other Extensions

### Wikilinks and Block IDs (separate layer)

Struct operates at the tree structure level only. Cross-referencing nodes via wikilinks (`[[foo]]`) or block identifiers (`^id`) is a separate layer that operates after struct transformation. These mechanisms can be used to express graph relationships (identity, equivalence) on top of the tree that struct produces.

```markdown
- foo: ^node-foo
  - bar

- elsewhere:
  - references [[foo]]
```

Struct builds the tree; links and IDs build the graph.

## Implementation

Struct is implemented as a post-parse `Cmarkit.Doc` transformation:

1. Parse the document normally with cmarkit (+ existing oystermark extensions)
2. Walk the AST, identify colon-suffixed paragraphs and list items
3. Rewrite the tree: reparent following contiguous blocks as children of keyed nodes
4. The result is a valid `Cmarkit.Doc` with restructured parent-child relationships
