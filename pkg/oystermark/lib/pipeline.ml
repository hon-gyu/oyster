(** Processing pipeline for vault rendering.

    The pipeline is a record of hooks that run at successive stages of vault
    processing.  Each hook receives exactly the data available at its stage
    and returns [Some _] to keep the note or [None] to drop it.

    Stages (in order):
    1. {b discover} — path only, before reading.  Return [false] to skip.
    2. {b frontmatter} — after frontmatter extraction, before full parse.
    3. {b parse} — after full parse, before index construction.
    4. {b vault} — after indexing and link resolution; full vault context available. *)

open Core

(** Pipeline: a record of hooks, one per stage. *)
type t =
  { on_discover : string -> bool
  ; on_frontmatter : string -> Yaml.value option -> Yaml.value option option
  ; on_parse : string -> Parse.doc -> Parse.doc option
  ; on_vault : Vault.t -> string -> Parse.doc -> Parse.doc option
  }

(** The identity pipeline — passes everything through unchanged. *)
let default : t =
  { on_discover = (fun _path -> true)
  ; on_frontmatter = (fun _path fm -> Some fm)
  ; on_parse = (fun _path doc -> Some doc)
  ; on_vault = (fun _ctx _path doc -> Some doc)
  }
;;

(** Make a pipeline from individual hooks. *)
let make
      ?(on_discover = default.on_discover)
      ?(on_frontmatter = default.on_frontmatter)
      ?(on_parse = default.on_parse)
      ?(on_vault = default.on_vault)
      ()
  =
  { on_discover; on_frontmatter; on_parse; on_vault }
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
  ; on_vault =
      (fun ctx path doc ->
        match a.on_vault ctx path doc with
        | None -> None
        | Some doc' -> b.on_vault ctx path doc')
  }
;;

(* {1 Frontmatter helpers} *)

(* {1 Built-in pipelines} *)

(** Exclude files with [draft: true] frontmatter. Apply on frontmatter stage. *)
let exclude_drafts : t =
  make
    ~on_frontmatter:(fun _path fm ->
      match fm with
      | Some (`O fields) ->
        (match List.Assoc.find fields ~equal:String.equal "draft" with
         | Some (`Bool true) -> None
         | _ -> Some fm)
      | _ -> invalid_arg "frontmatter is not an object")
    ()
;;

(** Exclude files without [publish: true] frontmatter. Apply on frontmatter stage. *)
let exclude_unpublish : t =
  make
    ~on_frontmatter:(fun _path fm ->
      match fm with
      | Some (`O fields) ->
        (match List.Assoc.find fields ~equal:String.equal "publish" with
         | Some (`Bool true) -> Some fm
         | _ -> None)
      | _ -> invalid_arg "frontmatter is not an object")
    ()
;;

(** Exclude notes that has `.draft` in stem. Apply on discover stage. *)
let exclude_draft_from_note_name : t =
  make ~on_discover:(fun path -> not (String.is_suffix ~suffix:".draft.md" path)) ()
;;
