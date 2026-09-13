(** {1 Vault-wide rename planning}

  This describes byte edits and file
  moves; clients decide how to apply or encode them.

  {@meta[
  ai-disclosure: autonomous
  ]}
*)

open Core

(** {2 Rename target} *)

type target =
  { path : string
  ; address : Note.Anchor.Address.t option (** [None] is the note itself. *)
  }
[@@deriving sexp, equal]

type edit =
  { rel_path : string
  ; first_byte : int
  ; last_byte : int (** Exclusive. *)
  ; new_text : string
  }
[@@deriving sexp, equal, compare]

type change =
  { edits : edit list (** Addressed by pre-move paths. *)
  ; moves : (string * string) list
    (** Files or directories to move, source to destination, after the edits. *)
  }
[@@deriving sexp, equal]

val valid_note_name : string -> bool
val renamed_note_path : path:string -> new_name:string -> string

(** {2 Move edits} *)

val moved_path : (string * string) list -> string -> string

(** {2 Change planning} *)

(** Move notes, assets, or directories of them, and rewrite every resolved link
    that would otherwise stop resolving to what it resolves to now: links into
    moved files, relative links out of them, and links a move would capture.

    Links that still resolve are left alone. *)
val plan_moves
  :  index:Index.t
  -> read_file:(string -> string option)
  -> (string * string) list
  -> (change, string) Result.t

val plan
  :  index:Index.t
  -> docs:(string * Cmarkit.Doc.t) list
  -> read_file:(string -> string option)
  -> target
  -> new_name:string
  -> (change, string) Result.t

module For_test : sig
  type vault =
    { files : (string * string) list
    ; docs : (string * Cmarkit.Doc.t) list
    ; index : Index.t
    }

  val stat : string -> Index.file_stat
  val vault : (string * string) list -> vault
  val read_file : vault -> string -> string option

  (** Print the files [change] rewrites or moves, as they are afterwards. *)
  val show_change : vault -> (change, string) Result.t -> unit
end
