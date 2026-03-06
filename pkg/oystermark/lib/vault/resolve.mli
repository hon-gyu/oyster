type target =
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

(** Resolve a link reference against the vault index. *)
val resolve : Link_ref.t -> string -> Index.t -> target

(** Build a [Cmarkit.Mapper.t] that resolves links against the vault index. *)
val resolution_cmarkit_mapper
  :  index:Index.t
  -> curr_file:string
  -> Cmarkit.Mapper.t

val resolve_docs : (string * Parse.doc) list ->  Index.t  -> (string * Parse.doc) list
