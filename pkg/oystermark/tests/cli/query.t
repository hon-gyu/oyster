The CLI evaluates XPath 1.0 over the parsed Djot XML view.

  $ cat > n.md <<'NOTE'
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
  > > [!note] A callout
  > > Body.
  > NOTE

XPath can navigate sections, blocks, and attributes.

  $ oyster query n.md 'count(//section)'
  2
  $ oyster query n.md '//code_block/@lang'
  sh
  python
  $ oyster query n.md 'string(//section[heading="Setup"]/keyed[@key="owner"]/paragraph)'
  alice
  $ oyster query n.md 'string(//callout[@type="note"]/title)'
  A callout
  $ oyster query n.md '/doc/references'
  <references/>

[-source] uses the selected element's Djot byte span.

  $ oyster query n.md '//code_block[@lang="sh"]' -source
  ```sh
  echo one
  ```

An empty node set gives exit status 1. A scalar prints its XPath string value.

  $ oyster query n.md '//code_block[@lang="ruby"]'
  [1]
  $ oyster query n.md 'boolean(//code_block[@lang="ruby"])'
  false
  [1]
  $ oyster query n.md '//code_block[@lang="python"]' -quiet

Parts of a block that are not nodes have byte spans too.

  $ cat > parts.md <<'NOTE'
  > ---
  > tags: [bread, sourdough]
  > serves: 4
  > ---
  > - [ ] buy flour
  > - [x] feed starter
  >   - twice a day
  > 
  > | a | b |
  > |---|---|
  > | 1 | 2 |
  > NOTE
  $ oyster query parts.md '//item[@task="unchecked"]' -source
  - [ ] buy flour
  $ oyster query parts.md '//row[2]' -source
  | 1 | 2 |
  $ oyster query n.md '//callout/title' -source
  A callout

The frontmatter is the first child of [doc].

  $ oyster query parts.md '/doc/frontmatter/field[@name="tags"]/entry'
  <entry type="string">bread</entry>
  <entry type="string">sourdough</entry>
  $ oyster query parts.md 'string(/doc/frontmatter/field[@name="serves"])'
  4
  $ oyster query parts.md 'string(//item[/doc/frontmatter/field[@name="tags"]/entry="bread"][@task="checked"]/paragraph)'
  feed starter

Several notes are queried in turn, the XPath last. Each line names its note.
The exit status is 0 when any note gives a truthy result.

  $ oyster query n.md parts.md 'count(//item)'
  n.md:0
  parts.md:3
  $ oyster query n.md parts.md '//item[@task="unchecked"]' -source
  parts.md:- [ ] buy flour
  $ oyster query n.md parts.md '//code_block[@lang="ruby"]'
  [1]
  $ oyster query '//paragraph' > /dev/null 2> /dev/null; echo $?
  2

An unreadable note is an error.

  $ oyster query n.md '[' > /dev/null 2> /dev/null; echo $?
  2
  $ oyster query nosuch.md '//paragraph'
  cannot read nosuch.md
  [2]
