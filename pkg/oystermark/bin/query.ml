(** XPath over {!Oystermark.Note.Xml.of_doc}: [query] reads a note, [edit]
    rewrites the source of what a query selects. *)

open Core
open Common_

let die (code : int) fmt =
  ksprintf
    (fun message ->
       prerr_endline message;
       exit code)
    fmt
;;

let read (note : string) : string =
  try In_channel.read_all note with
  | _ -> die 2 "cannot read %s" note
;;

let parse_query (text : string) =
  try Xpath.parse_utf8_exn text with
  | exn -> die 2 "invalid XPath: %s" (Exn.to_string exn)
;;

let eval query (source : string) : Xpath.Value.t =
  let parsed : Parse.t = Parse.of_string ~locs:true source in
  try
    Xpath.run_exn
      (Xpath.Context.create_exn ())
      (Note.Xml.of_doc ?frontmatter:parsed.frontmatter parsed.doc)
      query
  with
  | exn -> die 2 "XPath evaluation failed: %s" (Exn.to_string exn)
;;

let slice (source : string) ((first, last) : int * int) : string option =
  if last < String.length source
  then Some (String.sub source ~pos:first ~len:(last - first + 1))
  else None
;;

(** Evaluate XPath 1.0 over each note.

    A node set prints one XML element or value per line. A scalar prints its
    XPath string value. [-source] prints the original source for matched
    elements with byte spans. With several notes, each output line is prefixed
    by its note's path and a colon. The XPath comes last, so a shell glob can
    supply the notes. Exit status is [0] when the result is truthy for some
    note, [1] when it is for none, and [2] for an invalid query or an
    unreadable note. *)
let query_command =
  Command.basic
    ~summary:"Run XPath over the XML view of notes"
    ~readme:(fun () -> "oyster query NOTE... XPATH")
    (let%map_open.Command (args : string list) =
       anon (non_empty_sequence_as_list ("NOTE... XPATH" %: string))
     and (print_source : bool) =
       flag "-source" no_arg ~doc:" print original source for matched elements"
     and (quiet : bool) = flag "-quiet" no_arg ~doc:" print nothing; check exit status" in
     fun () ->
       let notes, query_text =
         match List.rev args with
         | query_text :: (_ :: _ as notes) -> List.rev notes, query_text
         | _ -> die 2 "usage: oyster query NOTE... XPATH"
       in
       let query = parse_query query_text in
       let prefix_lines =
         match notes with
         | [ _ ] -> fun _ output -> output
         | _ ->
           fun note output ->
             String.split_lines output
             |> List.map ~f:(fun line -> note ^ ":" ^ line)
             |> String.concat ~sep:"\n"
       in
       let truthy =
         List.map notes ~f:(fun note ->
           let source = read note in
           let result = eval query source in
           let print output =
             if not quiet then print_endline (prefix_lines note output)
           in
           (match Xpath.Value.Cast.to_node_set result with
            | None -> print (Xpath.Value.Cast.to_string result)
            | Some nodes ->
              Map.iter nodes ~f:(function
                | Xpath.Node.Root element | Element element ->
                  let output =
                    if print_source
                    then Option.bind (Note.Xml.span element) ~f:(slice source)
                    else None
                  in
                  print
                    (match output with
                     | Some source -> source
                     | None -> Simple_xml.to_string ~decl:false (Element element))
                | Attribute attribute -> print attribute.value
                | Namespace { value; _ } | Text value -> print value));
           Xpath.Value.Cast.to_boolean result)
       in
       if not (List.exists truthy ~f:Fn.id) then exit 1)
;;

(* Deleting a match that is all there is on its lines takes the lines with it,
   so a deleted list item or block leaves no blank line behind. *)
let whole_lines (source : string) ((first, stop) : int * int) : int * int =
  let rec line_start i =
    if i = 0 || Char.equal source.[i - 1] '\n'
    then Some i
    else if Char.is_whitespace source.[i - 1]
    then line_start (i - 1)
    else None
  in
  match line_start first with
  | Some first when stop = String.length source -> first, stop
  | Some first when Char.equal source.[stop] '\n' -> first, stop + 1
  | _ -> first, stop
;;

(** Replace or delete the source of the elements an XPath selects.

    Every match must be an element with a byte span. A match inside another is
    dropped, since the outer one covers it; matches that overlap otherwise, as
    neighbouring table cells do, are an error. [-delete] of a match that is alone
    on its lines removes the lines. Prints each edit as
    [NOTE:FIRST-LAST -> "TEXT"], [LAST] exclusive, and writes the note only
    with [--apply]. Exit status is [1] when nothing matches and [2] for an
    invalid query, an unreadable note, or matches that cannot be edited. *)
let edit_command =
  Command.basic
    ~summary:"Replace or delete the source of the elements an XPath selects"
    (let%map_open.Command (note : string) = anon ("NOTE" %: string)
     and (query_text : string) = anon ("XPATH" %: string)
     and (replacement : string option) =
       flag "-replace" (optional string) ~doc:"TEXT replace each match with TEXT"
     and (delete : bool) = flag "-delete" no_arg ~doc:" delete each match"
     and (apply : bool) = Upd.apply_flag in
     fun () ->
       let new_text =
         match replacement, delete with
         | Some text, false -> text
         | None, true -> ""
         | _ -> die 2 "pass exactly one of -replace and -delete"
       in
       let source = read note in
       let nodes =
         match Xpath.Value.Cast.to_node_set (eval (parse_query query_text) source) with
         | Some nodes -> Map.data nodes
         | None -> die 2 "the query does not select nodes"
       in
       let spans =
         List.map nodes ~f:(function
           | Xpath.Node.Root element | Element element ->
             (match Note.Xml.span element with
              | Some (first, last) when last < String.length source ->
                if delete then whole_lines source (first, last + 1) else first, last + 1
              | _ ->
                die
                  2
                  "match has no source span: %s"
                  (Simple_xml.to_string ~decl:false (Element element)))
           | Attribute _ | Namespace _ | Text _ -> die 2 "match is not an element")
         |> List.sort ~compare:(fun (a, a') (b, b') ->
           match Int.compare a b with
           | 0 -> Int.descending a' b'
           | c -> c)
         |> List.fold ~init:[] ~f:(fun kept (first, stop) ->
           match kept with
           | (_, outer_stop) :: _ when stop <= outer_stop -> kept
           | (_, outer_stop) :: _ when first < outer_stop ->
             die 2 "matches overlap at byte %d" first
           | _ -> (first, stop) :: kept)
       in
       if List.is_empty spans then exit 1;
       List.iter (List.rev spans) ~f:(fun (first, stop) ->
         printf "%s:%d-%d -> %S\n" note first stop new_text);
       if apply
       then
         Out_channel.write_all
           note
           ~data:
             (List.fold spans ~init:source ~f:(fun content (first, stop) ->
                String.prefix content first ^ new_text ^ String.drop_prefix content stop))
       else printf "dry run; pass --apply to write\n")
;;
