(** Command-line client for vault queries and renames. *)

open Core
module Node = Oystermark.Note.Node
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

(** Query the nodes of a note.

    QUERY is the syntax {!Oystermark.Note.Query.of_string} reads: a step is an
    axis and what modifies it, and [|] starts the next step.

    The exit status answers on its own: [0] when something matched, [1] when
    nothing did, [2] when the query or the note could not be read. A query that
    matches nothing says on stderr which step stopped it. *)
let block_command =
  Command.basic
    ~summary:"Print the nodes of a note that a query selects"
    (let%map_open.Command note = anon ("NOTE" %: string)
     and query_text = anon ("QUERY" %: string)
     and print =
       flag
         "-print"
         (listed string)
         ~doc:
           "FIELD what to print of each match: markdown (the default), source, path, \
            kind, line, file or prop:NAME; repeat for several, separated by tabs"
     and count = flag "-count" no_arg ~doc:" print how many nodes matched"
     and quiet =
       flag "-quiet" no_arg ~doc:" print nothing; the exit status is the answer"
     in
     fun () ->
       let die code fmt =
         ksprintf
           (fun message ->
              prerr_endline message;
              exit code)
           fmt
       in
       let query =
         match Query.of_string query_text with
         | Ok query -> query
         | Error message -> die 2 "%s" message
       in
       let source =
         try In_channel.read_all note with
         | _ -> die 2 "cannot read %s" note
       in
       let result = Query.run query (Parse.of_string ~locs:true source) in
       match result.matches with
       | [] ->
         if not quiet
         then
           Option.iter result.why_empty ~f:(fun why ->
             eprintf "%s: %s\n" note (Query.no_match_to_string why));
         exit 1
       | matches ->
         if quiet then exit 0;
         if count
         then printf "%d\n" (List.length matches)
         else (
           let text (value : Node.value) =
             match value with
             | String s -> s
             | Int n -> Int.to_string n
             | Bool b -> Bool.to_string b
           in
           let field (found : Node.found_t) name =
             match name with
             | "markdown" -> String.strip found.markdown
             | "source" ->
               (match found.span with
                | Some { first_byte; last_byte; _ } ->
                  String.sub source ~pos:first_byte ~len:(last_byte - first_byte + 1)
                | None -> String.strip found.markdown)
             | "path" -> String.concat ~sep:"." (List.map found.path ~f:Int.to_string)
             | "kind" -> Node.kind found.node
             | "line" ->
               Option.value_map found.span ~default:"" ~f:(fun span ->
                 Int.to_string (span.first_line + 1))
             | "file" -> note
             | name when String.is_prefix name ~prefix:"prop:" ->
               Option.value_map
                 (Node.prop (String.drop_prefix name 5) found.node)
                 ~default:""
                 ~f:text
             | other -> die 2 "unknown -print field %s" other
           in
           let fields = if List.is_empty print then [ "markdown" ] else print in
           let whole =
             List.exists fields ~f:(fun field ->
               String.equal field "markdown" || String.equal field "source")
           in
           List.map matches ~f:(fun found ->
             String.concat ~sep:"\t" (List.map fields ~f:(field found)))
           |> String.concat ~sep:(if whole then "\n\n" else "\n")
           |> print_endline))
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
