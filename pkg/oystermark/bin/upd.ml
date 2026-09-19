open Core
open Common_

let resolve_note (vault : Vault.t) note =
  match
    Vault.Index.resolve vault.index "__cli__.md" { target = Some note; fragment = None }
  with
  | Ok (Vault.Index.Note path) -> path
  | _ -> failwithf "note not found: %s" note ()
;;

let apply_edits (content : string) (edits : Vault.Rename.edit list) : string =
  List.sort edits ~compare:(fun (a : Vault.Rename.edit) b ->
    Int.descending a.first_byte b.first_byte)
  |> List.fold ~init:content ~f:(fun content edit ->
    String.prefix content edit.first_byte
    ^ edit.new_text
    ^ String.drop_prefix content edit.last_byte)
;;

let apply_change (root : string) (change : Vault.Rename.change) : unit =
  List.iter change.moves ~f:(fun (_, dst) ->
    let dst = Filename.concat root dst in
    match Sys_unix.file_exists dst with
    | `No -> ()
    | `Yes | `Unknown -> failwithf "refusing to overwrite %s" dst ());
  List.group change.edits ~break:(fun a b -> not (String.equal a.rel_path b.rel_path))
  |> List.iter ~f:(fun edits ->
    let rel_path = (List.hd_exn edits).rel_path in
    let path = Filename.concat root rel_path in
    let content = In_channel.read_all path in
    Out_channel.write_all path ~data:(apply_edits content edits));
  List.iter change.moves ~f:(fun (src, dst) ->
    let dst = Filename.concat root dst in
    Core_unix.mkdir_p (Filename.dirname dst);
    Core_unix.rename ~src:(Filename.concat root src) ~dst)
;;

let print_change (change : Vault.Rename.change) =
  List.iter change.edits ~f:(fun edit ->
    printf "%s:%d-%d -> %S\n" edit.rel_path edit.first_byte edit.last_byte edit.new_text);
  List.iter change.moves ~f:(fun (src, dst) -> printf "move %s -> %s\n" src dst)
;;

let apply_flag : bool Command.Param.t =
  Command.Param.flag
    "--apply"
    Command.Param.no_arg
    ~doc:" Apply the displayed change to disk"
;;

(* CR: ? *)
let read_file root rel_path =
  try Some (In_channel.read_all (Filename.concat root rel_path)) with
  | _ -> None
;;

let finish_change root ~apply = function
  | Error message ->
    eprintf "%s\n" message;
    exit 1
  | Ok change ->
    print_change change;
    if apply then apply_change root change else printf "dry run; pass --apply to write\n"
;;

let rename_command
      (target_name : string)
      (summary : string)
      (make_target : Vault.t -> string -> string -> Vault.Rename.target)
  : Command.t
  =
  Command.basic
    ~summary
    (let%map_open.Command root = vault_param
     and note = anon ("note" %: string)
     and target = anon (target_name %: string)
     and new_name = anon ("new-name" %: string)
     and apply = apply_flag in
     fun () ->
       let vault = load_vault root in
       let path = resolve_note vault note in
       Vault.Rename.plan
         ~index:vault.index
         ~docs:(Vault.docs vault)
         ~read_file:(read_file root)
         (make_target vault path target)
         ~new_name
       |> finish_change root ~apply)
;;

let heading_target (vault : Vault.t) path heading =
  let entry =
    Vault.Index.find_note vault.index path
    |> Option.value_exn ~message:(sprintf "note not found: %s" path)
  in
  let heading =
    Vault.Index.Entry.headings entry
    |> List.map ~f:fst
    |> List.find ~f:(fun h -> String.equal h.text heading || String.equal h.slug heading)
    |> Option.value_exn ~message:(sprintf "heading not found in %s: %s" path heading)
  in
  ({ path; address = Some (Heading heading.slug) } : Vault.Rename.target)
;;

let rename_heading_command =
  rename_command "heading" "Rename a heading and its incoming references" heading_target
;;

let move_command =
  Command.basic
    ~summary:"Move a note, asset, or directory while preserving links"
    (let%map_open.Command root = vault_param
     and src = anon ("path" %: string)
     and dst = anon ("new-path" %: string)
     and apply = apply_flag in
     fun () ->
       let vault = load_vault root in
       let vault_path p = String.rstrip p ~drop:(Char.equal '/') in
       Vault.Rename.plan_moves
         ~index:vault.index
         ~read_file:(read_file root)
         [ vault_path src, vault_path dst ]
       |> finish_change root ~apply)
;;

let (commands : (string * Command.t) list) =
  [ "move", move_command; "rename-heading", rename_heading_command ]
;;
