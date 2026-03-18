# Execution Caching Plan

## Goal

Skip re-executing Python code cells when inputs have not changed, making incremental vault builds fast.

## Cache Key

Hash over exactly what affects execution output:
- Filtered python cell **contents** (after `attr_filter` is applied — so `.no-exec` changes which cells are hashed)
- `uv_config` (Python version + sorted dependencies)

```
hash = SHA256(sexp_of(python_cells_content_list, uv_version, sorted_deps))
```

File path is **not** part of the key — same content in two files produces the same outputs.

## Cache Storage

A single `_exec_cache.json` file in the output directory:

```json
{
  "notes/my-analysis.md": {
    "hash": "a3f9...",
    "outputs": "(((id 0)(res(Markdown \"Hello world\\n\"))))"
  }
}
```

Outputs are stored as sexp strings (reusing the existing `sexp_of` derivations).
This makes CI caching trivial: just cache your `_site/` directory.

## CLI

- **Default**: load `_exec_cache.json` from `output_dir` if it exists (incremental rebuild).
- **`--cache-dir PATH`**: point to a *different* previous build — useful when building to a fresh directory but wanting to reuse an old cache.

## Implementation Plan

### 1. `code_executor.ml` — cache type + helpers

```ocaml
type cache_entry = { hash : string; outputs : output list }
type cache  (* mutable: string Map.t wrapping cache_entry *)

val empty_cache : unit -> cache
val load_cache : dir:string -> cache        (* reads _exec_cache.json; empty if missing *)
val save_cache : cache -> dir:string -> unit

val compute_hash : cell list -> uv_config -> string
val cache_lookup : cache -> path:string -> hash:string -> output list option
val cache_set    : cache -> path:string -> hash:string -> outputs:output list -> unit
```

Hash computation uses `Digest.string` (MD5 — sufficient for cache invalidation) over the sexp serialization of `(cell contents list, uv_version, sorted deps)`.

Serialise/deserialise `output list` via `sexp_of_output` / `output_of_sexp`.

### 2. `pipeline.ml` — `py_executor` gains `?cache` param

```ocaml
let py_executor ?cache () =
  make ~on_parse:(fun path doc ->
    ...
    let hash = Code_executor.compute_hash python_cells uv_config in
    let outputs =
      match Option.bind cache ~f:(Code_executor.cache_lookup ~path ~hash) with
      | Some cached -> cached                   (* cache hit — skip nbconvert *)
      | None ->
        let outs = Code_executor.uv_executor ctx in
        Option.iter cache ~f:(Code_executor.cache_set ~path ~hash ~outputs:outs);
        outs
    in
    [ path, Code_executor.merge_outputs outputs doc ])
  ()
```

### 3. `main.ml` — load/save cache around `render_vault`

Add `--cache-dir` flag to `vault_cmd`. Before rendering:

```ocaml
let cache_dir = Option.value cache_dir_flag ~default:output_dir in
let cache = Code_executor.load_cache ~dir:cache_dir in
(* pipeline composed with: py_executor ~cache () >> ... *)
```

After writing HTML output:

```ocaml
Code_executor.save_cache cache ~dir:output_dir
```

### 4. `output` type — add `of_sexp`

Currently `output` only derives `sexp_of`. Add `of_sexp` so outputs can be round-tripped through the cache file.

## Files Changed

| File | Change |
|------|--------|
| `pkg/oystermark/lib/code_executor.ml` | Cache type, load/save/lookup/set, compute_hash; add `of_sexp` to `output` |
| `pkg/oystermark/lib/pipeline.ml` | `py_executor` gains `?cache` param |
| `pkg/oystermark/bin/main.ml` | `--cache-dir` flag, load/save cache around render |
| `pkg/oystermark/lib/dune` | Add `ppx_sexp_conv` to `output` if not already present |
