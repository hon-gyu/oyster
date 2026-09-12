Moving a directory rewrites the links it would break and moves the files.

  $ mkdir -p vault/d
  $ printf '[[a]] [[d/a#A]] [x](d/a.md) ![[d/img.png]]\n' > vault/top.md
  $ printf '# A\n\n[[b]] [up](../top.md) [[d/b]]\n' > vault/d/a.md
  $ printf 'B\n' > vault/d/b.md
  $ touch vault/d/img.png

A dry run lists the edits and the move, and writes nothing.

  $ oyster move vault d/ archive/e
  d/a.md:16-25 -> "../../top.md"
  d/a.md:29-32 -> "e/b"
  top.md:8-11 -> "e/a"
  top.md:20-26 -> "e/a.md"
  top.md:31-40 -> "e/img.png"
  move d -> archive/e
  dry run; pass --apply to write
  $ ls vault
  d
  top.md

  $ oyster move vault d archive/e --apply
  d/a.md:16-25 -> "../../top.md"
  d/a.md:29-32 -> "e/b"
  top.md:8-11 -> "e/a"
  top.md:20-26 -> "e/a.md"
  top.md:31-40 -> "e/img.png"
  move d -> archive/e
  $ find vault -type f | sort
  vault/archive/e/a.md
  vault/archive/e/b.md
  vault/archive/e/img.png
  vault/top.md
  $ cat vault/top.md vault/archive/e/a.md
  [[a]] [[e/a#A]] [x](e/a.md) ![[e/img.png]]
  # A
  
  [[b]] [up](../../top.md) [[e/b]]

Moving onto an existing file is refused.

  $ oyster move vault top.md archive/e/a.md
  destination already exists: archive/e/a.md
  [1]
