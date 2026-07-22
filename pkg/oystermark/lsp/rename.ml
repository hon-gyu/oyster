(** Vault-aware rename refactoring.

    Spec: {!page-"feature-rename"}. *)

open Core

type edit =
  { rel_path : string
  ; first_byte : int
  ; last_byte : int
  ; new_text : string
  }
[@@deriving sexp, equal, compare]

let valid_id s =
  (not (String.is_empty s))
  && String.for_all s ~f:(fun c ->
    Char.is_alphanum c || Char.equal c '-' || Char.equal c '_')
;;

let valid_note_name s =
  (not (String.is_empty s))
  && (not (String.exists s ~f:(fun c -> Char.equal c '/' || Char.equal c '\\')))
  && not (String.equal s "." || String.equal s "..")
;;

let renamed_note_path ~path ~new_name =
  let new_name =
    if String.is_suffix new_name ~suffix:".md" then new_name else new_name ^ ".md"
  in
  Filename.concat (Filename.dirname path) new_name
;;

(** Byte offset within [line] of the {e id text} (past the [#]) of an attribute
    specifier naming [id] — block-level ([ {#id} ] on its own line) or inline
    ([ [text]{#id} ]).

    Only genuine specifiers match, because the candidate is parsed with
    {!Oystermark.Parse.Cb_attribute.of_string} rather than string-searched.
    That rejects the two ways a bare ["#" ^ id] substring search goes wrong:
    a {e link fragment} ([ [[note#id]] ]) is not a definition, and an id that
    merely {e shares a prefix} ([ {#id-2} ]) is a different anchor.
    See {!page-"feature-attribute-anchors"}. *)
let attr_id_offset ~(id : string) (line : string) : int option =
  let n = String.length line in
  let is_id_char c = Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' in
  (* Offset of ["#" ^ id] within a specifier body, requiring the id to end at a
     boundary so [#id] does not match inside [#id-2]. *)
  let offset_in_body body =
    let pattern = "#" ^ id in
    let rec seek from =
      match String.substr_index body ~pos:from ~pattern with
      | None -> None
      | Some p ->
        let after = p + String.length pattern in
        if after >= String.length body || not (is_id_char body.[after])
        then Some p
        else seek (p + 1)
    in
    seek 0
  in
  let rec scan i =
    if i >= n
    then None
    else if Char.equal line.[i] '{'
    then (
      match String.index_from line i '}' with
      | None -> None
      | Some close ->
        let body = String.sub line ~pos:(i + 1) ~len:(close - i - 1) in
        let matched =
          match Oystermark.Parse.Cb_attribute.of_string body with
          | Some { id = Some found; _ } when String.equal found id -> offset_in_body body
          | _ -> None
        in
        (match matched with
         | Some p -> Some (i + 1 + p + 1)
         | None -> scan (close + 1)))
    else scan (i + 1)
  in
  scan 0
;;

let line_bounds content line =
  let rec loop pos current =
    if current = line
    then (
      let stop =
        Option.value (String.index_from content pos '\n') ~default:(String.length content)
      in
      Some (pos, stop))
    else (
      match String.index_from content pos '\n' with
      | None -> None
      | Some newline -> loop (newline + 1) (current + 1))
  in
  loop 0 0
;;

let definition_edit ~rel_path ~content ~line target ~new_name =
  match line_bounds content line with
  | None -> None
  | Some (start, stop) ->
    let text = String.sub content ~pos:start ~len:(stop - start) in
    (match target with
     | Find_references.Path_heading _ ->
       let hashes =
         String.length text - String.length (String.lstrip text ~drop:(Char.equal '#'))
       in
       let text_start =
         let rec skip i =
           if i < String.length text && Char.equal text.[i] ' ' then skip (i + 1) else i
         in
         skip hashes
       in
       let text_stop =
         String.substr_index text ~pos:text_start ~pattern:" {"
         |> Option.value ~default:(String.length text)
       in
       Some
         { rel_path
         ; first_byte = start + text_start
         ; last_byte = start + text_stop
         ; new_text = new_name
         }
     | Path_block { block_id; _ } ->
       String.substr_index text ~pattern:("^" ^ block_id)
       |> Option.map ~f:(fun pos ->
         { rel_path
         ; first_byte = start + pos + 1
         ; last_byte = start + pos + 1 + String.length block_id
         ; new_text = new_name
         })
     | Path_attr { id; _ } ->
       attr_id_offset ~id text
       |> Option.map ~f:(fun pos ->
         { rel_path
         ; first_byte = start + pos
         ; last_byte = start + pos + String.length id
         ; new_text = new_name
         })
     | Path_only _ -> None)
;;

let target_path = function
  | Find_references.Path_only { path }
  | Path_heading { path; _ }
  | Path_block { path; _ }
  | Path_attr { path; _ } -> path
;;

let find_definition_line content target =
  String.split_lines content
  |> List.find_mapi ~f:(fun line text ->
    let found =
      match target with
      | Find_references.Path_heading { slug; _ } ->
        Hover.heading_level_of_line text
        |> Option.exists ~f:(fun _ ->
          let heading =
            String.lstrip text ~drop:(Char.equal '#')
            |> String.lstrip ~drop:(Char.equal ' ')
          in
          String.equal (Oystermark.Parse.Heading_slug.slugify heading) slug)
      | Path_block { block_id; _ } ->
        Option.equal String.equal (Find_references.block_id_of_line text) (Some block_id)
      | Path_attr { id; _ } -> Option.is_some (attr_id_offset ~id text)
      | Path_only _ -> false
    in
    Option.some_if found line)
;;

(** Bounds of a link's destination within [slice] (the link's full source text):
    [(style, dest_start, dest_stop)], both {e slice-relative}, with [dest_stop]
    exclusive.  The destination excludes a wikilink's [|alias] and a markdown
    link's title, so it is exactly the part a rename may rewrite. *)
let destination_bounds slice =
  match String.substr_index slice ~pattern:"[[" with
  | Some open_pos ->
    let start = open_pos + 2 in
    let finish =
      String.substr_index ~pos:start slice ~pattern:"]]"
      |> Option.value_map ~default:(String.length slice) ~f:Fn.id
    in
    let finish =
      String.index_from slice start '|'
      |> Option.filter ~f:(fun p -> p < finish)
      |> Option.value ~default:finish
    in
    Some (`Wikilink, start, finish)
  | None ->
    String.substr_index slice ~pattern:"]("
    |> Option.bind ~f:(fun open_pos ->
      let start = open_pos + 2 in
      let rec finish i =
        if i >= String.length slice
        then i
        else if Char.equal slice.[i] ')' || Char.is_whitespace slice.[i]
        then i
        else finish (i + 1)
      in
      Some (`Markdown, start, finish start))
;;

let reference_edit ~content (r : Find_references.reference) target ~new_name =
  let len = r.last_byte - r.first_byte + 1 in
  if r.first_byte < 0 || len <= 0 || r.first_byte + len > String.length content
  then None
  else (
    let slice = String.sub content ~pos:r.first_byte ~len in
    destination_bounds slice
    |> Option.bind ~f:(fun (style, dest_start, dest_stop) ->
      let destination = String.sub slice ~pos:dest_start ~len:(dest_stop - dest_start) in
      (* [dest_start] and [dest_stop] are slice-relative; offsets derived from
         [destination] below are destination-relative and so need [dest_start]
         added back. Keeping the two frames straight matters: conflating them
         once made fragment renames overrun the link's closing delimiter. *)
      match target with
      | Find_references.Path_only _ ->
        let target_stop =
          Option.value (String.index destination '#') ~default:(String.length destination)
        in
        let old_target = String.prefix destination target_stop in
        let basename =
          if String.is_suffix destination ~suffix:".md"
          then
            if String.is_suffix new_name ~suffix:".md" then new_name else new_name ^ ".md"
          else Option.value (String.chop_suffix new_name ~suffix:".md") ~default:new_name
        in
        let replacement =
          match Filename.dirname old_target with
          | "." -> basename
          | dir -> Filename.concat dir basename
        in
        let replacement =
          match style with
          | `Wikilink -> replacement
          | `Markdown -> String.substr_replace_all replacement ~pattern:" " ~with_:"%20"
        in
        Some
          { rel_path = r.rel_path
          ; first_byte = r.first_byte + dest_start
          ; last_byte = r.first_byte + dest_start + target_stop
          ; new_text = replacement
          }
      | Path_heading _ | Path_block _ | Path_attr _ ->
        String.index destination '#'
        |> Option.map ~f:(fun hash ->
          let marker =
            match target with
            | Path_block _ -> "^"
            | Path_heading _ | Path_attr _ | Path_only _ -> ""
          in
          let new_text =
            match style with
            | `Wikilink -> new_name
            | `Markdown -> String.substr_replace_all new_name ~pattern:" " ~with_:"%20"
          in
          { rel_path = r.rel_path
          ; first_byte = r.first_byte + dest_start + hash + 1 + String.length marker
          ; last_byte = r.first_byte + dest_stop
          ; new_text
          })))
;;

let rename ~index ~docs ~read_file ~rel_path ~content ~line ~character ~new_name () =
  match Find_references.detect_target ~index ~rel_path ~content ~line ~character with
  | None -> []
  | Some target ->
    let ids_are_valid =
      match target with
      | Path_block _ | Path_attr _ -> valid_id new_name
      | Path_heading _ -> not (String.is_empty (String.strip new_name))
      | Path_only _ -> valid_note_name new_name
    in
    if not ids_are_valid
    then []
    else (
      let references = Find_references.scan_vault ~docs target in
      let reference_edits =
        List.filter_map references ~f:(fun r ->
          read_file r.rel_path
          |> Option.bind ~f:(fun content -> reference_edit ~content r target ~new_name))
      in
      let definition_path = target_path target in
      let definition =
        read_file definition_path
        |> Option.bind ~f:(fun definition_content ->
          find_definition_line definition_content target
          |> Option.bind ~f:(fun definition_line ->
            definition_edit
              ~rel_path:definition_path
              ~content:definition_content
              ~line:definition_line
              target
              ~new_name))
      in
      List.sort (Option.to_list definition @ reference_edits) ~compare:compare_edit)
;;

(** Test helpers are kept explicit so external tests need not depend on the
    text-edit implementation details. *)
module For_test = struct
  let rename = rename
end
