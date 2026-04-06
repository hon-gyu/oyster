open Core
(** Pipeline: a record of hooks, one per stage. *)
type t =
  { on_discover : string -> string list -> bool
  ; on_parse : string -> Cmarkit.Doc.t -> (string * Cmarkit.Doc.t) list
  ; on_vault : Vault.t -> Vault.t
  }

(** The identity pipeline — passes everything through unchanged. *)
let id : t =
  { on_discover = (fun _path _paths -> true)
  ; on_parse = (fun path doc -> [ path, doc ])
  ; on_vault = (fun ctx -> ctx)
  }
;;

(** Make a pipeline from individual hooks. *)
let make
      ?(on_discover = id.on_discover)
      ?(on_parse = id.on_parse)
      ?(on_vault = id.on_vault)
      ()
  =
  { on_discover; on_parse; on_vault }
;;

(** Compose two pipelines: run [a] then [b] at each stage.
    Short-circuits on [false]/empty. *)
let compose (a : t) (b : t) : t =
  { on_discover = (fun p ps -> a.on_discover p ps && b.on_discover p ps)
  ; on_parse =
      (fun path doc ->
        List.concat_map (a.on_parse path doc) ~f:(fun (p', d') -> b.on_parse p' d'))
  ; on_vault = (fun ctx -> b.on_vault (a.on_vault ctx))
  }
;;

let ( >> ) a b = compose a b

(** Lift a per-doc concat_map into an [on_vault] hook.
    @param f A function that takes the vault context, path, and document, and
           returns a list of (path, doc) pairs to replace the original doc with.
    @return An [on_vault] hook that applies [f] to each document in the vault.
*)
let map_each_doc (f : Vault.t -> string -> Cmarkit.Doc.t -> (string * Cmarkit.Doc.t) list)
  : Vault.t -> Vault.t
  =
  fun (ctx : Vault.t) ->
  let docs' : (string * Cmarkit.Doc.t) list =
    List.concat_map ctx.docs ~f:(fun (path, doc) -> f ctx path doc)
  in
  { ctx with docs = docs' }
;;

let of_block_mapper (block_mapper : Cmarkit.Block.t Cmarkit.Mapper.mapper) : t =
  let open Cmarkit in
  let mapper : Mapper.t =
    Mapper.make ~inline_ext_default:(fun _m i -> Some i) ~block:block_mapper ()
  in
  make ~on_parse:(fun path doc -> [ path, Mapper.map_doc mapper doc ]) ()
;;
