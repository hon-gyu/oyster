type target =
  | Note of { path : string }
  | File of { path : string }
  | Heading of
      { path : string
      ; heading : string
      ; level : int
      }
  | Block of
      { path : string
      ; block_id : string
      }
  | Curr_file
  | Curr_heading of
      { heading : string
      ; level : int
      }
  | Curr_block of { block_id : string }
  | Unresolved

(** Meta key for storing resolved targets in the target *)
val resolved_key : target Cmarkit.Meta.key

(** Make a wikilink from an already resolved target. *)
val make_wikilink
  :  target:string option
  -> fragment:Parse.Wikilink.fragment option
  -> display:string option
  -> embed:bool
  -> resolved_target:target
  -> Cmarkit.Inline.t

(** Resolve a link reference against the vault index. *)
val resolve : Link_ref.t -> string -> Index.t -> target

(** Build a [Cmarkit.Mapper.t] that resolves links against the vault index. *)
val resolution_cmarkit_mapper : index:Index.t -> curr_file:string -> Cmarkit.Mapper.t

val resolve_docs
  :  (string * Cmarkit.Doc.t) list
  -> Index.t
  -> (string * Cmarkit.Doc.t) list
