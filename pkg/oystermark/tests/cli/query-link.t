A query link names a note by its frontmatter. See the query-link page.

  $ mkdir -p vault/d
  $ printf -- '---\nid: foo\ntags: [essay, draft]\n---\n# Foo\n\n## Intro\n' > vault/d/foo.md
  $ printf -- '---\nid: bar\ndraft: false\nyear: 2026\n---\n# Bar\n' > vault/bar.md
  $ printf -- '---\nid: baz\ndraft: true\n---\n# Baz\n' > vault/baz.md
  $ printf '[[?id==foo]] [[?id==foo#Intro]] [[?year=ge=2026]] [[?tags==essay;draft!=true]]\n' > vault/ok.md

Links that resolve are not reported.

  $ oyster check vault

A query link that matches no note, several notes, or does not parse is an
error, and the command exits 1. An ordinary unresolved link is a warning. A
markdown link to [?...] is a URL query, not a vault link.

  $ printf '[[?id==nope]]\n[[?id=in=(foo,bar)]]\n[[?title==\x27a#b\x27]]\n[[?id=]]\n[[missing]] [x](?id==foo)\n' > vault/bad.md
  $ oyster check vault
  bad.md:1:1: error: unresolved link: ?id==nope: no note matches the query
  bad.md:2:1: error: unresolved link: ?id=in=(foo,bar): the query matches 2 notes: bar.md, d/foo.md
  bad.md:3:1: error: unresolved link: ?title=='a#b': invalid query: unterminated quoted value (a query value cannot contain `#` or `|`, even quoted)
  bad.md:4:1: error: unresolved link: ?id=: invalid query: expected a comparison operator after `id`
  bad.md:5:1: warning: unresolved link: missing: no such note or file
  [1]

Only warnings: exit 0.

  $ printf '[[missing]]\n' > vault/bad.md
  $ oyster check vault
  bad.md:1:1: warning: unresolved link: missing: no such note or file

Moving the note leaves its query links as written.

  $ oyster move vault d/foo.md elsewhere/foo.md --apply
  move d/foo.md -> elsewhere/foo.md
  $ cat vault/ok.md
  [[?id==foo]] [[?id==foo#Intro]] [[?year=ge=2026]] [[?tags==essay;draft!=true]]

Renaming a heading still rewrites the fragment of a query link to it.

  $ oyster rename-heading vault elsewhere/foo.md Intro Opening --apply
  elsewhere/foo.md:47-52 -> "Opening"
  ok.md:24-29 -> "Opening"
  $ cat vault/ok.md
  [[?id==foo]] [[?id==foo#Opening]] [[?year=ge=2026]] [[?tags==essay;draft!=true]]
  $ oyster check vault
  bad.md:1:1: warning: unresolved link: missing: no such note or file
