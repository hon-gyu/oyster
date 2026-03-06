(** Processing pipeline for vault rendering.

    The pipeline is a record of hooks that run at successive stages of vault
    processing.  Each hook receives exactly the data available at its stage
    and returns [Some _] to keep the note or [None] to drop it.

    Stages (in order):
    1. {b discover} — path only, before reading.  Return [false] to skip.
    2. {b frontmatter} — after frontmatter extraction, before full parse.
    3. {b parse} — after full parse, before index construction.
    4. {b index} — after the vault index is built; vault context available. *)

open Core

(** Vault context available after indexing. *)
type vault_ctx =
  { vault_root : string
  ; index : Vault.Index.t
  ; docs : (string * Parse.doc) list
  ; vault_meta : Cmarkit.Meta.t
  }

(** Pipeline: a record of hooks, one per stage. *)
type t =
  { on_discover : string -> bool
  ; on_frontmatter : string -> Yaml.value option -> Yaml.value option option
  ; on_parse : string -> Parse.doc -> Parse.doc option
  ; on_index : vault_ctx -> string -> Parse.doc -> Parse.doc option
  }

(** The identity pipeline — passes everything through unchanged. *)
let default : t =
  { on_discover = (fun _path -> true)
  ; on_frontmatter = (fun _path fm -> Some fm)
  ; on_parse = (fun _path doc -> Some doc)
  ; on_index = (fun _ctx _path doc -> Some doc)
  }
;;

(** Compose two pipelines: run [a] then [b] at each stage.
    Short-circuits on [None]/[false]. *)
let compose (a : t) (b : t) : t =
  { on_discover = (fun p -> a.on_discover p && b.on_discover p)
  ; on_frontmatter =
      (fun path fm ->
        match a.on_frontmatter path fm with
        | None -> None
        | Some fm' -> b.on_frontmatter path fm')
  ; on_parse =
      (fun path doc ->
        match a.on_parse path doc with
        | None -> None
        | Some doc' -> b.on_parse path doc')
  ; on_index =
      (fun ctx path doc ->
        match a.on_index ctx path doc with
        | None -> None
        | Some doc' -> b.on_index ctx path doc')
  }
;;

(* {1 Frontmatter helpers} *)

(** Test whether frontmatter has [draft: true]. *)
let is_draft_frontmatter (fm : Yaml.value option) : bool =
  match fm with
  | Some (`O fields) ->
    (match List.Assoc.find fields ~equal:String.equal "draft" with
     | Some (`Bool true) -> true
     | _ -> false)
  | _ -> false
;;

(* {1 Built-in pipelines} *)

(** Exclude drafts at the frontmatter stage (before full parse & indexing). *)
let exclude_drafts : t =
  { default with
    on_frontmatter =
      (fun _path fm -> if is_draft_frontmatter fm then None else Some fm)
  }
;;
