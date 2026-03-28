---
date: 2026-03-18
---
# Code Execution Cache — Design Notes

This note records the design space explored when adding execution caching to
`py_executor`, and explains the tradeoffs behind the chosen approach.

---

## Background

`py_executor` runs Python code cells via `uv` + `jupyter nbconvert` at the
parse stage of the pipeline.  Re-executing unchanged cells on every build is
expensive.  The goal is to skip execution when the inputs have not changed.

**Cache key**: hash of (filtered python cell contents, uv version, sorted
dependencies).  File path is deliberately excluded — same code produces the
same output regardless of where it lives in the vault.

**Cache file**: `_exec_cache.json` written into the output directory.  Storing
it alongside the site output makes CI caching trivial (cache `_site/`) and
means the cache is naturally invalidated when the output is deleted.

---

## What was built

- `Code_executor.cache` — a mutable `String.Map.t ref` keyed by vault-relative
  path, holding `{ hash; outputs }` per document.
- `Code_executor.run` — the single entry point for execution, encapsulating the
  hash → lookup → execute → store flow.  `py_executor` in `pipeline.ml` is a
  thin wrapper around it.
- `Code_executor.load_cache` / `save_cache` — load from `_exec_cache.json` in a
  directory (empty cache if missing or malformed), write back after the build.
- `Pipeline.default` became a function `?cache -> unit -> t` so the cache can be
  threaded into `py_executor` at construction time.
- `vault_cmd` in `main.ml` owns the lifecycle: load cache → build pipeline →
  `render_vault` → save cache.

---

## Alternatives considered

### Option A (chosen): profile factory owns the decision, caller owns the lifecycle

```
(* main.ml *)
let cache = Code_executor.load_cache ~dir:output_dir in
let pipeline = pipeline_of_profile ~cache config.pipeline_profile in
render_vault ~pipeline ...;
Code_executor.save_cache cache ~dir:output_dir
```

```
(* main.ml — pipeline_of_profile *)
| Default      -> Pipeline.default ~cache ()
| Basic        -> Pipeline.basic          (* value, ignores cache *)
| None_profile -> Pipeline.id             (* value, ignores cache *)
```

`vault_cmd` always creates and saves the cache regardless of profile.
`pipeline_of_profile` pattern-matches and wires it only where needed.  `basic`
and `id` remain plain values.

**Pros**
- Lifecycle is explicit and local to `vault_cmd`, the one place that knows the
  output directory.
- `basic` / `id` stay simple values; no fake arguments accepted and ignored.
- Adding a new cache-aware profile is a one-line change in `pipeline_of_profile`.
- No changes to `Pipeline.t`, `render_vault`, or any hook signature.

**Cons**
- `vault_cmd` always writes `_exec_cache.json` even for profiles that never
  populate it (empty file, harmless but nonzero surprise).
- Cache lifecycle is the caller's responsibility — library consumers composing
  their own pipelines must manage load/save themselves.

---

### Option B (rejected): uniform profile constructor signature

All profile constructors made to share signature `?cache -> unit -> t`:

```
let basic ?(_ : Code_executor.cache option) () : t = id >> backlinks
let none  ?(_ : Code_executor.cache option) () : t = id
```

`pipeline_of_profile` would call `Pipeline.basic ~cache ()` etc. uniformly,
with no profile-specific branching.

**Pros**
- `pipeline_of_profile` becomes a uniform call site with no per-profile `match`
  logic for cache wiring.

**Cons**
- Functions that accept and silently ignore a labeled argument are a code smell —
  they create a false promise of configurability.
- The "uniformity" is cosmetic.  The match still exists; it is just moved into
  each profile function as a no-op.
- Every future profile must accept the cache argument even if irrelevant.

---

### Option C (rejected): add cache to `Pipeline.t`

Add a `cache` field (or a general shared `context`) to the pipeline record:

```
type t =
  { on_discover : string -> string list -> bool
  ; on_parse    : string -> Cmarkit.Doc.t -> (string * Cmarkit.Doc.t) list
  ; on_vault    : Vault.t -> Vault.t
  ; cache       : Code_executor.cache      (* or: context : Context.t *)
  }
```

`render_vault` would save `pipeline.cache` after processing.

**Pros**
- The pipeline is self-contained; callers do not manage the lifecycle.
- Generalises naturally if multiple hooks need shared pre-computation in the
  future.

**Cons**
- Does not actually solve the lifecycle problem: `render_vault` must still call
  `save_cache`, which means importing `Code_executor` at the render layer —
  coupling execution infrastructure to the rendering stage.
- Alternatively, a new `on_finish : unit -> unit` hook would be needed, adding
  permanent complexity to the pipeline type for one use case.
- Every `Pipeline.make` call (including `basic`, `id`, all tests) must supply a
  cache, or the field must be optional — reintroducing the same awkwardness.
- "Future shared pre-computation" is a real concern, but the right abstraction
  is not obvious from a single example.  Generalising prematurely risks
  designing the wrong interface.

---

## Summary

The lifecycle of the cache (load → use during pipeline → save) naturally belongs
at the call site that already knows the output directory: `vault_cmd`.  Making
it explicit there is simpler than hiding it inside the pipeline type or adding
lifecycle hooks.  Profile-specific wiring lives in `pipeline_of_profile`, which
is the appropriate factory for this decision.
