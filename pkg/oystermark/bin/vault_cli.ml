(** Command-line client for vault queries and renames. *)

open Core
module Query = Oystermark.Note.Query
module Parse = Oystermark.Parse
module Vault = Oystermark.Vault

let load root = Vault_fs.of_root_path ~skip_expand:true root

let is_image_target target =
  let target = String.lowercase target in
  List.exists [ ".png"; ".jpg"; ".jpeg"; ".gif"; ".svg"; ".webp" ] ~f:(fun ext ->
    String.is_suffix target ~suffix:ext)
;;

let links index =
  Vault.Index.notes index
  |> List.concat_map ~f:(fun note ->
    let source = Vault.Index.Entry.path note in
    Vault.Index.Entry.links note
    |> List.map ~f:(fun link ->
      source, link, Vault.Index.resolve index source link.reference))
;;

let kind_name (_, (link : Vault.Index.Link.t), resolution) =
  match link.kind, resolution with
  | Vault.Index.Link.Link, _ -> "link"
  | Embed, Ok (Vault.Index.Note _ | Anchor _) -> "embed"
  | Embed, Ok (Vault.Index.Asset _) -> "image"
  | Embed, Error _ ->
    (match link.reference.target with
     | Some target when is_image_target target -> "image"
     | _ -> "embed")
;;

let destination_name (_, _, resolution) =
  match resolution with
  | Ok target -> Vault.Index.target_path target
  | Error _ -> "<unresolved>"
;;

let line_number (_, (link : Vault.Index.Link.t), _) =
  fst (Cmarkit.Textloc.first_line link.loc)
;;

let print_link ((source, _, _) as occurrence) =
  printf
    "%s:%d\t%s\t%s\n"
    source
    (line_number occurrence)
    (kind_name occurrence)
    (destination_name occurrence)
;;

let (vault_param : string Command.Param.t) = Command.Param.(anon ("vault" %: string))

let unresolved_command =
  Command.basic
    ~summary:"List unresolved links, embeds, and images"
    (let%map_open.Command root = vault_param in
     fun () ->
       let vault = load root in
       links vault.index
       |> List.filter ~f:(fun (_, _, resolution) -> Result.is_error resolution)
       |> List.iter ~f:print_link)
;;

type stats =
  { nodes : int
  ; edges : int
  ; self_links : int
  ; unresolved_links : int
  }

let stats (vault : Vault.t) =
  let links = links vault.index in
  let edges, self_links, unresolved_links =
    List.fold
      links
      ~init:(0, 0, 0)
      ~f:(fun (edges, self_links, unresolved) (source, _, resolution) ->
        match resolution with
        | Error _ -> edges, self_links, unresolved + 1
        | Ok target ->
          let destination = Vault.Index.target_path target in
          ( edges + 1
          , self_links + Bool.to_int (String.equal source destination)
          , unresolved ))
  in
  { nodes = List.length (Vault.Index.notes vault.index)
  ; edges
  ; self_links
  ; unresolved_links
  }
;;

let stats_command =
  Command.basic
    ~summary:"Show vault graph statistics"
    (let%map_open.Command root = vault_param in
     fun () ->
       let stats = stats (load root) in
       printf "nodes\t%d\n" stats.nodes;
       printf "edges\t%d\n" stats.edges;
       printf "self links\t%d\n" stats.self_links;
       printf "unresolved links\t%d\n" stats.unresolved_links)
;;

let resolve_note (vault : Vault.t) note =
  match
    Vault.Index.resolve vault.index "__cli__.md" { target = Some note; fragment = None }
  with
  | Ok (Vault.Index.Note path) -> path
  | _ -> failwithf "note not found: %s" note ()
;;

let apply_edits content edits =
  List.sort edits ~compare:(fun (a : Vault.Rename.edit) b ->
    Int.descending a.first_byte b.first_byte)
  |> List.fold ~init:content ~f:(fun content edit ->
    String.prefix content edit.first_byte
    ^ edit.new_text
    ^ String.drop_prefix content edit.last_byte)
;;

let apply_change root (change : Vault.Rename.change) =
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

let apply_flag =
  Command.Param.flag
    "--apply"
    Command.Param.no_arg
    ~doc:" Apply the displayed change to disk"
;;

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

let rename_command target_name summary make_target =
  Command.basic
    ~summary
    (let%map_open.Command root = vault_param
     and note = anon ("note" %: string)
     and target = anon (target_name %: string)
     and new_name = anon ("new-name" %: string)
     and apply = apply_flag in
     fun () ->
       let vault = load root in
       let path = resolve_note vault note in
       Vault.Rename.plan
         ~index:vault.index
         ~docs:(Vault.docs vault)
         ~read_file:(read_file root)
         (make_target vault path target)
         ~new_name
       |> finish_change root ~apply)
;;

let rename_note_command =
  Command.basic
    ~summary:"Rename a note and its incoming references"
    (let%map_open.Command root = vault_param
     and note = anon ("note" %: string)
     and new_name = anon ("new-name" %: string)
     and apply = apply_flag in
     fun () ->
       let vault = load root in
       let path = resolve_note vault note in
       Vault.Rename.plan
         ~index:vault.index
         ~docs:(Vault.docs vault)
         ~read_file:(read_file root)
         ({ path; address = None } : Vault.Rename.target)
         ~new_name
       |> finish_change root ~apply)
;;

let move_command =
  Command.basic
    ~summary:"Move a note, asset, or directory and rewrite the links it would break"
    (let%map_open.Command root = vault_param
     and src = anon ("path" %: string)
     and dst = anon ("new-path" %: string)
     and apply = apply_flag in
     fun () ->
       let vault = load root in
       let vault_path p = String.rstrip p ~drop:(Char.equal '/') in
       Vault.Rename.plan_moves
         ~index:vault.index
         ~read_file:(read_file root)
         [ vault_path src, vault_path dst ]
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

(** Emit the vault as a Jinja template context on stdout.

    Rendering is left to a Jinja engine invoked by the build system, so that
    this executable stays a pure query over a snapshot and gains no runtime
    dependency on a template binary. See {!page-"template-context"}. *)
let context_command =
  Command.basic
    ~summary:"Print the vault as a JSON template context"
    (let%map_open.Command root = vault_param
     and compact =
       flag "-compact" no_arg ~doc:" emit one line instead of indented JSON"
     in
     fun () ->
       let json = Oystermark.Context.of_vault (load root) in
       print_endline
         (if compact
          then Yojson.Safe.to_string json
          else Yojson.Safe.pretty_to_string json))
;;

(** Query the blocks of a note.

    Each flag adds steps to one {!Oystermark.Note.Query.t}; see
    {!Oystermark.Note.Query.of_flags}. [-json] prints each match with its path,
    names and enclosing headings, which can be used to write the next query.

    Default output is the block's source, verbatim. *)
let block_command =
  Command.basic
    ~summary:"Print blocks of a note, filtered by heading, kind, id or position"
    (let%map_open.Command note = anon ("NOTE" %: string)
     and under =
       flag
         "-under"
         (optional string)
         ~doc:"SLUG only blocks in the section of the heading with this id or text"
     and direct = flag "-direct" no_arg ~doc:" with -under, stop at the first subheading"
     and kind = flag "-kind" (optional string) ~doc:"KIND only blocks of this kind"
     and lang =
       flag
         "-lang"
         (optional string)
         ~doc:"INFO only code blocks whose info string is exactly INFO"
     and id =
       flag "-id" (optional string) ~doc:"ID only the block a link to #ID names ({#ID})"
     and caret_id =
       flag
         "-caret-id"
         (optional string)
         ~doc:"ID only the block a link to #^ID names (^ID)"
     and nth = flag "-nth" (optional int) ~doc:"N keep only the Nth match, 1-based"
     and content =
       flag "-content" no_arg ~doc:" print what the container holds, not its source"
     and json = flag "-json" no_arg ~doc:" print the matching blocks as JSON" in
     fun () ->
       let die fmt =
         ksprintf
           (fun s ->
              prerr_endline s;
              exit 1)
           fmt
       in
       let source = In_channel.read_all note in
       let doc = Parse.of_string ~locs:true source in
       let result =
         Query.run
           (Query.of_flags { under; direct; kind; lang; attr_id = id; caret_id; nth })
           (Query.top doc)
       in
       Option.iter (Query.why_empty result) ~f:(fun why -> die "%s: %s" note why);
       let content_of cursor =
         match Query.content_string cursor with
         | Ok content -> content
         | Error kind -> die "%s: a %s has no contents to print" note kind
       in
       let source_of cursor =
         let textloc = Cmarkit.Meta.textloc (Query.meta cursor) in
         if Cmarkit.Textloc.is_none textloc
         then
           die
             "%s: the %s at %s has no location"
             note
             (Query.kind cursor)
             (String.concat ~sep:"." (List.map (Query.path cursor) ~f:Int.to_string))
         else (
           let first = Cmarkit.Textloc.first_byte textloc in
           let last = Cmarkit.Textloc.last_byte textloc in
           String.sub source ~pos:first ~len:(last - first + 1))
       in
       if json
       then (
         let json_of cursor =
           let textloc = Cmarkit.Meta.textloc (Query.meta cursor) in
           let name (address : Oystermark.Note.Anchor.Address.t) =
             let kind, id =
               match address with
               | Heading id -> "heading", id
               | Caret id -> "caret", id
               | Attr id -> "attr", id
             in
             `Assoc [ "kind", `String kind; "id", `String id ]
           in
           let heading (h : Oystermark.Note.Anchor.heading) =
             `Assoc
               [ "id", `String h.slug; "text", `String h.text; "level", `Int h.level ]
           in
           `Assoc
             [ "path", `List (List.map (Query.path cursor) ~f:(fun i -> `Int i))
             ; "kind", `String (Query.kind cursor)
             ; ( "info"
               , match Query.info_string cursor with
                 | Some info -> `String info
                 | None -> `Null )
             ; "names", `List (List.map (Query.names cursor) ~f:name)
             ; "headings", `List (List.map (Query.headings cursor) ~f:heading)
             ; ( "loc"
               , `Assoc
                   [ "first_line", `Int (fst (Cmarkit.Textloc.first_line textloc))
                   ; "last_line", `Int (fst (Cmarkit.Textloc.last_line textloc))
                   ; "first_byte", `Int (Cmarkit.Textloc.first_byte textloc)
                   ; "last_byte", `Int (Cmarkit.Textloc.last_byte textloc)
                   ] )
             ; "text", `String (source_of cursor)
             ; ( "content"
               , match Query.content_string cursor with
                 | Ok content -> `String content
                 | Error _ -> `Null )
             ]
         in
         print_endline
           (Yojson.Safe.pretty_to_string (`List (List.map result.matches ~f:json_of))))
       else
         List.map result.matches ~f:(fun cursor ->
           if content then content_of cursor else source_of cursor)
         |> String.concat ~sep:"\n\n"
         |> print_endline)
;;

let command =
  Command.group
    ~summary:"Inspect and rename notes in an OysterMark vault"
    [ "unresolved", unresolved_command
    ; "stats", stats_command
    ; "context", context_command
    ; "block", block_command
    ; "rename-note", rename_note_command
    ; "move", move_command
    ; ( "rename-heading"
      , rename_command
          "heading"
          "Rename a heading and its incoming references"
          heading_target )
    ]
;;

let () =
  let version = Oystermark.Version.to_string () in
  (* [build_info] defaults to a placeholder sexp that [oyster version] prints
     verbatim; give it something readable instead. *)
  Command_unix.run ~version ~build_info:("oystermark " ^ version) command
;;
