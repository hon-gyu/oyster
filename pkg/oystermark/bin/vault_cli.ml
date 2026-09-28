(** Command-line client for vault queries and renames. *)
(* TODO(refa): split out each subcommand as modules *)
(* TODO(feat): add transclusion as subcommand *)
(* TODO(refa): unify / group rename related commands *)

open Core
open Common_

(** Evaluate XPath 1.0 over {!Oystermark.Note.Xml.of_doc}.

    A node set prints one XML element or value per line. A scalar prints its
    XPath string value. [-source] prints the original source for matched
    elements with byte spans. Exit status is [0] for a truthy result, [1] for a false
    result, and [2] for an invalid query or an unreadable note. *)
let query_command =
  Command.basic
    ~summary:"Run XPath over the XML view of a note"
    (let%map_open.Command (note : string) = anon ("NOTE" %: string)
     and (query_text : string) = anon ("XPATH" %: string)
     and (print_source : bool) =
       flag "-source" no_arg ~doc:" print original source for matched elements"
     and (quiet : bool) = flag "-quiet" no_arg ~doc:" print nothing; check exit status" in
     fun () ->
       let die (code : int) fmt =
         ksprintf
           (fun message ->
              prerr_endline message;
              exit code)
           fmt
       in
       let (source : string) =
         try In_channel.read_all note with
         | _ -> die 2 "cannot read %s" note
       in
       let query =
         try Xpath.parse_utf8_exn query_text with
         | exn -> die 2 "invalid XPath: %s" (Exn.to_string exn)
       in
       let parsed : Parse.t = Parse.of_string ~locs:true source in
       let result =
         try
           Xpath.run_exn (Xpath.Context.create_exn ()) (Note.Xml.of_doc parsed.doc) query
         with
         | exn -> die 2 "XPath evaluation failed: %s" (Exn.to_string exn)
       in
       if not quiet
       then (
         let source_of_element (element : Simple_xml.element) =
           let value key =
             List.find_map element.attributes ~f:(fun (attr : Simple_xml.Attribute.t) ->
               if String.equal attr.key key then Int.of_string_opt attr.value else None)
           in
           match value "start-byte", value "end-byte" with
           | Some first, Some last
             when first >= 0 && last >= first && last < String.length source ->
             Some (String.sub source ~pos:first ~len:(last - first + 1))
           | _ -> None
         in
         match Xpath.Value.Cast.to_node_set result with
         | None -> print_endline (Xpath.Value.Cast.to_string result)
         | Some nodes ->
           Map.iter nodes ~f:(function
             | Xpath.Node.Root element | Element element ->
               let output = if print_source then source_of_element element else None in
               print_endline
                 (match output with
                  | Some source -> source
                  | None -> Simple_xml.to_string ~decl:false (Element element))
             | Attribute attribute -> print_endline attribute.value
             | Namespace { value; _ } | Text value -> print_endline value));
       if not (Xpath.Value.Cast.to_boolean result) then exit 1)
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
