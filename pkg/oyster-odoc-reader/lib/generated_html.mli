(** The textual boundary between odoc and the reader. *)

type note =
  { path : string
  ; body : string
  }

(** [of_directory dir] reads the embeddable JSON pages produced beneath [dir]
    by [odoc html-generate --as-json]. *)
val of_directory : string -> (note list, string) result

(** [generate file] asks the [odoc] on [PATH] to render [file] as embeddable
    JSON, then converts every generated page. The subprocess, rather than this
    library, owns the internal [.odocl] representation. *)
val generate : Fpath.t -> (note list, string) result
