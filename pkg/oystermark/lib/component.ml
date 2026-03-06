(** UI components.

  Usage:
  - Used by {Pipeline} to add components as HTML blocks using [backend_block].
  - Used in {Vault.render_vault}'s last step to render non-body content (WIP).
*)

type html = string
type doc_component = string * Parse.doc -> html
type vault_component = Vault.t -> html

let todo = failwith "TODO"

let toc (paths : string list) : html = todo

let backlinks (rel_path : string) : vault_component = todo

let file_explorer : vault_component = todo
