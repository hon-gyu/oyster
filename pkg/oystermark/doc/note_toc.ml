(* Walk note/ dir, find all .md files, and prints a list *)

open Core
module List' = List
module String' = String

let () =
  let open Shexp_process in
  let open Shexp_process.Infix in
  eval
    (run "find" [ "note"; "-name"; "*.md"; "-type"; "f" ]
     |> capture_unit [ Stdout ]
     >>= fun output ->
     let files =
       String'.split output ~on:'\n'
       |> List'.filter ~f:(fun s -> not (String'.is_empty s))
       |> List'.sort ~compare:String'.compare
     in
     List'.fold_left files ~init:(return ()) ~f:(fun acc file ->
       acc
       >> printf "- {!page-\"%s\"}\n" (Filename.chop_extension (Filename.basename file)))
    )
;;
