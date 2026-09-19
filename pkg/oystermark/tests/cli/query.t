  $ cat > n.md <<'EOF'
  > # Top
  > 
  > ## Setup
  > 
  > owner: alice
  > 
  > ```sh
  > echo one
  > ```
  > 
  > ```python
  > print("a")
  > ```
  > 
  > ## Other
  > 
  > Some prose.
  > EOF

  $ oyster query n.md '[Section([top, setup]), Child(where=[Is(code_block)])]'
  ```sh
  echo one
  ```
  
  ```python
  print("a")
  ```
  $ oyster query n.md '[Section([top, setup]), Child(where=[Is(code_block)]), Self(nth=1)]'
  ```python
  print("a")
  ```

exact=false allows sub-path matching

  $ oyster query n.md '[Section([setup], exact=false), Child(where=[Is(code_block)], nth=-1)]'
  ```python
  print("a")
  ```
[Field] reads the keyed syntax.

  $ oyster query n.md '[Section([top, setup]), Field(owner)]'
  alice

  $ cat > n2.md << 'EOF'
  > - foo: bar
  > - baz: 
  >   - apple:
  >     - pear
  > - cider:
  > ```md
  > hi
  > ```
  > EOF

  $ oyster query n2.md '[Field(foo)]'
  n2.md: step 0 (Field(foo)): nothing to move to from root
  [1]

  $ oyster query n2.md '[Child(nth=0), Field(foo)]'
  bar

  $ oyster query n2.md '[Child(nth=0), Field(baz), Field(apple)]' -print kind -print source
  list	- pear

  $ oyster query n2.md '[Child(nth=0), Field(baz), Field(apple), Child(nth=0)]' -print kind -print source
  list_item	- pear

  $ oyster query n2.md '[Child(nth=0), Field(baz), Field(apple), Child(nth=0), Child(nth=0)]' -print kind -print source
  paragraph	pear

  $ oyster query n2.md '[Child(nth=0), Field(cider)]' -print source
  ```md
  hi
  ```
  $ oyster query n2.md '[Child(nth=0), Field(cider)]' -print prop:text -print prop:lang
  hi	md

[-print] chooses what to print of each match, one field per flag, separated by
tabs.

  $ oyster query n.md '[Descendant(where=[Is(code_block)])]' -print kind -print line -print path
  code_block	8	0.0.1
  code_block	12	0.0.2

  $ oyster query n.md '[Descendant(where=[Is(code_block)])]' -print prop:lang
  sh
  python

[-count] prints how many matched, [-quiet] nothing at all.

  $ oyster query n.md '[Descendant(where=[Is(code_block)])]' -count
  2
  $ oyster query n.md '[Descendant(where=[Is(code_block)])]' -quiet
  $ oyster query n.md '[Descendant]' -count
  8

Nothing matched: exit 1, and stderr names the step that emptied the sequence.

  $ oyster query n.md '[Section([missing])]'
  n.md: step 0 (Section([missing])): nothing to move to from root
  [1]

  $ oyster query n.md '[Section([missing])]' -quiet
  [1]

A query that cannot be read, a note that cannot be read, and an unknown
[-print] field are all exit 2.

  $ oyster query n.md '[Child('
  unexpected the end
  [2]

  $ oyster query nosuch.md '[Child]'
  cannot read nosuch.md
  [2]

  $ oyster query n.md '[Child]' -print lang
  unknown -print field lang
  [2]

A djot attribute is part of the node it is written on: its identifier, its
classes and its key/value pairs are all properties, so a query can test them
and [-print] can print them. This is the shape [oyster] writes for an expanded
embed.

  $ cat > n3.md <<'EOF'
  > {source="notes/a.md" fragment=Intro depth=1}
  > ::: embed
  > Transcluded body.
  > :::
  > EOF

  $ oyster query n3.md '[Descendant(where=[Is(div)])]' -print prop:class -print prop:source -print prop:fragment -print prop:depth
  embed	notes/a.md	Intro	1

An attribute value is written without a type, so it is offered as the text and
as the number it spells; both comparisons hold.

  $ oyster query n3.md '[Descendant(where=[Prop(depth, =, 1)])]' -print kind
  div
  $ oyster query n3.md '[Descendant(where=[Prop(depth, =, "1")])]' -print kind
  div
  $ oyster query n3.md '[Descendant(where=[Has(source)])]' -print kind
  div

The div is a node of its own, so the transcluded blocks are its children.

  $ oyster query n3.md '[Descendant(where=[Is(div)]), Child]' -print kind -print path
  paragraph	0.0
