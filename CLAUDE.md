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

The goal of this library is to extend CommonMark with extensions.

[Cmarkit] is the core dependency. Syntax extensions are implemented as one of the following:
{ul
    {- [Cmarkit.Inline.t]}
    {- [Cmarkit.Block.t]}
    {- [Cmarkit.Meta.t] attached to [Cmarkit.Inline.t] and [Cmarkit.Block.t]}
}

{2 Parse}

Pre-resolution file-level parsing: frontmatter, wikilinks, block IDs, and callouts.

See {!Oystermark.Parse}.

{2 Extract}

Read-only queries over parsed blocks: the blocks a link target denotes, and a
walk over every addressable block of a note.

See {!Oystermark.Extract}.

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
