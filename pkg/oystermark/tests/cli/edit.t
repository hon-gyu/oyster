[oyster edit] rewrites the source of the elements an XPath selects.

  $ cat > n.md <<'NOTE'
  > # Shopping
  > 
  > owner: alice
  > 
  > - [x] flour
  >   - strong white
  > - [ ] salt
  > - [x] yeast
  > 
  > | a | b |
  > |---|---|
  > | 1 | 2 |
  > NOTE

A dry run lists the edits, as byte ranges with an exclusive end, and writes
nothing.

  $ oyster edit n.md '//keyed[@key="owner"]/paragraph' -replace bob
  n.md:19-24 -> "bob"
  dry run; pass --apply to write
  $ grep owner n.md
  owner: alice

A deleted match that is alone on its lines takes the lines with it. A match
inside another match is covered by the outer one.

  $ oyster edit n.md '//item[@task="checked"] | //item[@task="checked"]//item' -delete --apply
  n.md:26-55 -> ""
  n.md:66-78 -> ""
  $ oyster edit n.md '//keyed[@key="owner"]/paragraph' -replace bob --apply
  n.md:19-24 -> "bob"
  $ cat n.md
  # Shopping
  
  owner: bob
  
  - [ ] salt
  
  | a | b |
  |---|---|
  | 1 | 2 |

Nothing matched gives exit status 1.

  $ oyster edit n.md '//code_block' -delete
  [1]

A match must be an element with a source span, and matches must not overlap:
neighbouring cells share a pipe, their contents do not.

  $ oyster edit n.md '//heading/@level' -replace 2
  match is not an element
  [2]
  $ oyster edit n.md '/doc/footnotes' -delete
  match has no source span: <footnotes/>
  [2]
  $ oyster edit n.md 'count(//item)' -delete
  the query does not select nodes
  [2]
  $ oyster edit n.md '//row[2]/cell' -replace x
  matches overlap at byte 60
  [2]
  $ oyster edit n.md '//row[2]/cell/text' -replace x --apply
  n.md:58-59 -> "x"
  n.md:62-63 -> "x"
  $ tail -n 1 n.md
  | x | x |

Exactly one of [-replace] and [-delete] is required.

  $ oyster edit n.md '//item'
  pass exactly one of -replace and -delete
  [2]
