open Core

(** Build a vault index from a list of [(rel_path, content)] pairs.
    Shared between integration and trace tests. *)
let make_index (files : (string * string) list) : Oystermark.Vault.Index.t =
  let md_docs =
    List.filter_map files ~f:(fun (rel_path, content) ->
      if String.is_suffix rel_path ~suffix:".md"
      then (
        let doc = Oystermark.Parse.of_string ~locs:true content in
        Some (rel_path, doc))
      else None)
  in
  let other_files =
    List.filter_map files ~f:(fun (p, _) ->
      if not (String.is_suffix p ~suffix:".md") then Some p else None)
  in
  Oystermark.Vault.build_index ~md_docs ~other_files ~dirs:[]
;;
