(** Converting a compiled odoc file into notes. *)

type note =
  { path : string (** vault-relative, without the [.md] extension *)
  ; body : string
  }

(** [of_odocl file] asks the [odoc] executable on [PATH] to generate embeddable
    JSON for [file], then converts every generated page. The active [odoc]
    therefore owns its internal [.odocl] representation; this library crosses
    only the textual JSON and HTML boundary. One [.odocl] yields one note per
    generated odoc page. *)
val of_odocl : Fpath.t -> (note list, string) result

(** [write ~dir note] writes [note] beneath [dir], creating the directories its
    path names. *)
val write : dir:string -> note -> unit
