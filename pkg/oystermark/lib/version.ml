(** The version of the [oystermark] package a binary was built from. *)

(** [to_string ()] is the package version as declared by [(version)] in
    [dune-project], or ["dev"] when the binary carries no version information. *)
let to_string () : string =
  match Build_info.V1.version () with
  | Some version -> Build_info.V1.Version.to_string version
  | None -> "dev"
;;
