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

An unreadable note is an error.

  $ oyster query n.md '[' > /dev/null 2> /dev/null; echo $?
  2
  $ oyster query nosuch.md '//paragraph'
  cannot read nosuch.md
  [2]
