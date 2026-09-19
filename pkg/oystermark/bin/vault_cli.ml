(** Command-line client for vault queries and renames. *)
(* TODO(refa): split out each subcommand as modules *)
(* TODO(feat): add transclusion as subcommand *)
(* TODO(refa): unify / group rename related commands *)

open Core
open Common_

(** Query the nodes of a note.

    QUERY is the syntax {!Oystermark.Note.Query.of_string} reads: a list of
    steps, each written as its constructor, as
    [ [Section([setup]), Descendant(where=[Is(code_block)])] ].

    exit status:
    - [0] when something matched
    - [1] when nothing matched
    - [2] when the query or the note could not be read. Check stderr *)
let query_command =
  Command.basic
    ~summary:"Print the nodes of a note that a query selects"
    (let%map_open.Command (note : string) = anon ("NOTE" %: string)
     and (query_text : string) = anon ("QUERY" %: string)
     and (print : string list) =
       flag
         "-print"
         (listed string)
         ~doc:
           "FIELD what to print of each match: markdown (the default), source, path, \
            kind, line, file or prop:NAME; repeat for several, separated by tabs"
     and (count : bool) = flag "-count" no_arg ~doc:" print how many nodes matched"
     and (quiet : bool) =
       flag "-quiet" no_arg ~doc:" print nothing; check exit status for matches"
     in
     fun () ->
       let die (code : int) fmt =
         ksprintf
           (fun message ->
              prerr_endline message;
              exit code)
           fmt
       in
       let (query : Query.steps) =
         match Query.of_string query_text with
         | Ok query -> query
         | Error message -> die 2 "%s" message
       in
       let (source : string) =
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
                 (List.Assoc.find
                    found.props
                    (String.drop_prefix name 5)
                    ~equal:String.equal)
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
    [ "query", query_command
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
