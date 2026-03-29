# Cmarkit Label Resolution

Reference: `cmarkit/src/cmarkit.mli`, around line 425-522.

## Overview

Cmarkit's label resolution mechanism controls how **link reference definitions** and **reference links/images** are processed during parsing. It provides a hook (`Label.resolver`) that acts as a gatekeeper — deciding which definitions are stored and how references are resolved.

The key types are:

```ocaml
type context =
[ `Def of t option * t
| `Ref of [ `Link | `Image ] * t * t option ]

type resolver = context -> t option
```

The resolver is called in two situations:

1. **`Def` — when a definition is encountered** (e.g. `[foo]: /url "title"` or a footnote definition). The resolver decides whether to accept or discard it.

2. **`Ref` — when a reference link/image is encountered** (e.g. `[text][foo]`, `[foo][]`, `[foo]`). The resolver decides whether the reference resolves to a link or falls back to plain text.

### Two-phase parsing

CommonMark parsing happens in two phases:

1. **Block parsing** — scans the entire document, collecting all link reference definitions into the defs map.
2. **Inline parsing** — parses inline content where reference links are encountered and resolved.

Since all definitions are gathered before any reference resolution happens, document order doesn't matter. A reference can appear before its definition.

### What the resolver does NOT apply to

Inline links like `[text](/url "title")` bypass the resolver entirely. They carry their own destination directly and become `Inline.Link` with reference `` `Inline of Link_definition.t node ``. The resolver only deals with reference-style constructs.

## Q&A

### What are `Def` and `Ref`?

**`` `Def (prev, current) ``** is called when the parser encounters a link reference definition or footnote definition.

- `current` — the label being defined
- `prev` — `Some label` if a definition for the same key already exists, `None` if this is the first

**`` `Ref (kind, ref, def) ``** is called when the parser encounters a reference link or image.

- `kind` — `` `Link `` or `` `Image ``
- `ref` — the referencing label as found in the source
- `def` — `Some label` if a definition for that key exists in the defs map, `None` if undefined

### What does the resolver return? (`resolver = context -> t option`)

The resolver always returns `Label.t option`, but its meaning differs depending on the context:

**When handling `` `Def `` (definitions):**

- `Some label` — accept the definition. The parser stores it in the defs map under `label`'s key. You can return a different label than `current` to remap the key.
- `None` — reject the definition. The parser discards it. If a previous definition (`prev`) existed, it remains.

**When handling `` `Ref `` (references):**

- `Some label` — the reference resolves successfully. It becomes a `Link` or `Image` node in the AST. The returned label is used to look up the definition (destination, title, etc.) from the defs map.
- `None` — the reference is undefined. The parser treats it as plain text (e.g. `[foo]` renders literally as `[foo]`).

Note that `t` here is `Label.t`, not the definition content itself. The label is just a **key** — the parser uses it to look up the actual link destination/title from the defs map. This is why the wikilink example works: it returns a synthetic label even though no definition exists for it, and the actual URL can be constructed later in a mapper or renderer.

### How does the default resolver work?

```ocaml
let default_resolver = function
| `Def (None, l) -> Some l        (* First definition wins *)
| `Def (Some _, _) -> None        (* Duplicate ignored *)
| `Ref (_, _, def) -> def          (* Pass through as-is *)
```

- For definitions: first one wins. If there's already a definition (`prev = Some _`), the new one is discarded.
- For references: passes through unchanged. If the label was defined, it resolves; if not, the reference becomes plain text.

### Does the resolver store definitions into the database?

No. The resolver acts as a **gatekeeper**, not a store. The parser calls the resolver and based on the return value:

- `Some label` — the parser stores the definition under that label's key
- `None` — the parser discards the definition

The final result of all stored definitions is accessible via `Doc.defs`.

### Do inline links go through resolution?

No. Inline links like `[text](/url "title")` do not involve the resolver. They become an `Inline.Link` with reference `` `Inline of Link_definition.t node `` — the URL and title are embedded directly. Only reference-style links/images go through resolution.

### How does the wikilink resolver example work?

From `cmarkit.mli` (line 510-521):

```ocaml
let wikilink = Cmarkit.Meta.key () (* A meta key to recognize them *)

let make_wikilink label =
  let meta = Cmarkit.Meta.tag wikilink (Cmarkit.Label.meta label) in
  Cmarkit.Label.with_meta meta label

let with_wikilinks = function
| `Def _ as ctx -> Cmarkit.Label.default_resolver ctx
| `Ref (_, _, (Some _ as def)) -> def
| `Ref (_, ref, None) -> Some (make_wikilink ref)
```

This resolver intercepts **undefined references** and turns them into wikilinks. Line by line:

1. **`` `Def _ as ctx ``** — For definitions, delegate to the default resolver (first definition wins, duplicates ignored). The wikilink resolver doesn't change definition behavior.

2. **`` `Ref (_, _, (Some _ as def)) -> def ``** — If a reference has an existing definition (e.g. `[foo]` and `[foo]: /url` exists), use it as-is. Normal CommonMark behavior.

3. **`` `Ref (_, ref, None) -> Some (make_wikilink ref) ``** — **The key part.** If a reference has **no** definition (`None`), instead of letting it fall back to plain text (which is what the default resolver does), create a synthetic label tagged with the `wikilink` meta key. This means `[My Page]` in the source won't become literal text — it stays as a `Link` node in the AST.

The `make_wikilink` helper takes the referencing label, tags its metadata with the `wikilink` key (so you can identify it later in a mapper or renderer), and returns it. The tag carries no data (`unit key`) — it's just a marker.

**Downstream usage:** After parsing, you can check for the `wikilink` meta key on link nodes to identify them and handle them specially — e.g. rewriting their destination to `/wiki/My_Page` in a renderer or mapper.
