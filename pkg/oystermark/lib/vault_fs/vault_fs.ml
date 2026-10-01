(** Loading a vault from the filesystem.

    Separate from {!Vault} so that the pure, in-memory vault API carries no
    Unix dependency: {!Oystermark} links into a js_of_ocaml bundle, and
    [core_unix]'s native stubs have no JavaScript implementation. *)

module Fs_utils = Fs_utils
open Core

(** Read, parse, resolve, and by default expand the files beneath [vault_root]. *)
let of_root_path
      ?(skip_expand = false)
      ?(exclude : string -> bool = fun _ -> false)
      (vault_root : string)
  : Vault.t
  =
  (* [exclude] goes to the walk rather than filtering its result: an excluded
     directory is then never descended into. *)
  let entries = Fs_utils.walk ~root:vault_root ~exclude () in
  let files = List.filter entries ~f:(fun p -> not (String.is_suffix p ~suffix:"/")) in
  let parsed_docs =
    List.filter_map files ~f:(fun path ->
      if String.is_suffix path ~suffix:".md"
      then
        Some
          ( path
          , Parse.of_string
              ~locs:true
              (In_channel.read_all (Filename.concat vault_root path)) )
      else None)
  in
  let other_files =
    List.filter files ~f:(fun p -> not (String.is_suffix p ~suffix:".md"))
  in
  let stat_of_path rel_path : Vault.Index.file_stat =
    { rel_path
    ; birthtime = None
    ; mtime = Fs_utils.mtime_date (Filename.concat vault_root rel_path)
    }
  in
  let index = Vault.build_index ~stat_of_path ~md_docs:parsed_docs ~other_files () in
  let vault : Vault.t =
    { vault_root; index; documents = String.Map.of_alist_exn parsed_docs }
  in
  if skip_expand then vault else Vault.expand vault
;;
