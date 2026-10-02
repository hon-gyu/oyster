(** Command-line client for vault queries and renames. *)
(* TODO(refa): split out each subcommand as modules *)
(* TODO(feat): add transclusion as subcommand *)
(* TODO(refa): unify / group rename related commands *)

open Core
open Common_

let command =
  Command.group
    ~summary:"Inspect and rename notes in an OysterMark vault"
    [ "query", Query.query_command
    ; "edit", Query.edit_command
    ; "move", List.Assoc.find_exn Upd.commands ~equal:String.equal "move"
    ; ( "rename-heading"
      , List.Assoc.find_exn Upd.commands ~equal:String.equal "rename-heading" )
    ; "stats", Stat.command
    ]
;;

let () =
  let (version : string) = Oystermark.Version.to_string () in
  (* [build_info] defaults to a placeholder sexp that [oyster version] prints
     verbatim; give it something readable instead. *)
  Command_unix.run ~version ~build_info:("oystermark " ^ version) command
;;
