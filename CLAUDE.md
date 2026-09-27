# General
- We consider specification a part of the program.
  - odoc is used to tie spec <- implementation <- test

# Tips

## odoc escaping
In most contexts, the characters { [ ] } @ all need to be escaped with a backslash. In inline source code style, only square brackets need to be escaped. However, as a convenience, matched square brackets need not be escaped to aid in typesetting code. For example, the following would be acceptable in a documentation comment:

```odoc
The list [ [1;2;3] ] needs no escaping
```

# Oystermark doc

The following content is excerpted from pkg/oystermark/doc/index.mld

{0 oystermark index}

{1 Library oystermark}

The entry point module: {!Oystermark}.

A note is djot with a YAML frontmatter. The parser is [Djot], extracted from
djot.v (vendored at [vendor/djot]), whose design goals are proved in Rocq. It
parses {!Oystermark.Parse.profile}: djot with Markdown spellings, wikilinks,
keyed blocks and callouts. Every syntax extension is a constructor of djot's
tree; oyster adds none of its own.

{2 Parse}

Pre-resolution file-level parsing: the frontmatter, then djot.

See {!Oystermark.Parse}.

{2 Note}

A single note: what in it can be addressed and linked to, and the operations
that read and update it.

See {!Oystermark.Note}.

{2 Vault}

Vault-level operations: directory indexing, link resolution, and embed expansion.

See {!Oystermark.Vault}.

Shared link queries, graph statistics, and rename plans are services of this
module, consumed by both the protocol and command-line clients.

{2 Context}

A vault snapshot as JSON, for a Jinja template to compute a document from —
generated index pages, tables of contents, tag listings.

See {!Oystermark.Context} and {!page-"template-context"}.

{1 Other pages}

{2 Reference Specifications of Common CommonMark Extensions}
{ul
  {- {!page-"pandoc-attribute"}}
}

{2 Notes}
{ul
  {- {!page-"cmarkit-label-resolution"}}
  {- {!page-"cmarkit-mapper-api"}}
}
