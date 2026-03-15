(** Oystermark LSP server — go-to-definition for resolved links. *)

open Linol_eio
module Lsp = Linol_lsp.Lsp

(** {1 Position utilities} *)

(** Convert an LSP position (0-based line, UTF-16 character offset) to a byte
    offset in the UTF-8 encoded [content]. Treats each code unit as one byte
    (correct for ASCII/BMP). *)
let byte_offset_of_position (content : string) (pos : Position.t) : int =
  let len = String.length content in
  let line = ref 0 in
  let i = ref 0 in
  while !line < pos.line && !i < len do
    if content.[!i] = '\n' then incr line;
    incr i
  done;
  min (! i + pos.character) len
;;

(** {1 Link detection in raw text} *)

(** Find a substring [needle] in [haystack] starting at position [from].
    Returns the index of the first character of the match, or [None]. *)
let find_substring haystack ~needle ~from =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if from + nlen > hlen
  then None
  else (
    let rec loop i =
      if i + nlen > hlen
      then None
      else if String.sub haystack i nlen = needle
      then Some i
      else loop (i + 1)
    in
    loop from)
;;

(** Find the wikilink ([[…]]) enclosing [offset] in [text], if any.
    Returns the parsed {!Oystermark.Parse.Wikilink.t}. *)
let find_wikilink_at_offset (text : string) (offset : int)
  : Oystermark.Parse.Wikilink.t option
  =
  let len = String.length text in
  let rec scan i =
    if i > len - 4
    then None
    else (
      match find_substring text ~needle:"[[" ~from:i with
      | None -> None
      | Some open_pos ->
        let embed =
          open_pos > 0 && text.[open_pos - 1] = '!' in
        let span_start = if embed then open_pos - 1 else open_pos in
        let content_start = open_pos + 2 in
        (match find_substring text ~needle:"]]" ~from:content_start with
         | None -> None
         | Some close_pos ->
           let span_end = close_pos + 1 in
           if span_start <= offset && offset <= span_end
           then (
             let content =
               String.sub text content_start (close_pos - content_start)
             in
             Some (Oystermark.Parse.Wikilink.make ~embed content))
           else scan (close_pos + 2)))
  in
  scan 0
;;

(** Find a markdown link destination [text](url) where [offset] falls inside
    the link span. Returns the raw URL string. *)
let find_mdlink_dest_at_offset (text : string) (offset : int) : string option =
  let len = String.length text in
  (* Strategy: scan for all ]( pairs, find the matching ), check if offset is
     within the whole [text](url) span. *)
  let rec scan i =
    if i >= len - 2
    then None
    else (
      match find_substring text ~needle:"](" ~from:i with
      | None -> None
      | Some bracket_pos ->
        let url_start = bracket_pos + 2 in
        (* Find matching ) — handle nesting *)
        let rec find_close j depth =
          if j >= len
          then None
          else if text.[j] = '(' then find_close (j + 1) (depth + 1)
          else if text.[j] = ')'
          then if depth = 0 then Some j else find_close (j + 1) (depth - 1)
          else find_close (j + 1) depth
        in
        (match find_close url_start 0 with
         | None -> scan (bracket_pos + 2)
         | Some close_paren ->
           (* Find the matching [ before ] *)
           let rec find_open j depth =
             if j < 0
             then None
             else if text.[j] = ']' then find_open (j - 1) (depth + 1)
             else if text.[j] = '['
             then if depth = 0 then Some j else find_open (j - 1) (depth - 1)
             else find_open (j - 1) depth
           in
           (match find_open (bracket_pos - 1) 0 with
            | None -> scan (bracket_pos + 2)
            | Some open_bracket ->
              (* Also include a preceding ! for images *)
              let span_start =
                if open_bracket > 0 && text.[open_bracket - 1] = '!'
                then open_bracket - 1
                else open_bracket
              in
              if span_start <= offset && offset <= close_paren
              then (
                let url =
                  String.sub text url_start (close_paren - url_start)
                in
                Some url)
              else scan (bracket_pos + 2))))
  in
  scan 0
;;

(** Try to extract a {!Oystermark.Vault.Link_ref.t} from the raw text at
    [offset]. Checks wikilinks first, then markdown links. *)
let find_link_ref_at_offset (text : string) (offset : int)
  : Oystermark.Vault.Link_ref.t option
  =
  match find_wikilink_at_offset text offset with
  | Some wl -> Some (Oystermark.Vault.Link_ref.of_wikilink wl)
  | None ->
    (match find_mdlink_dest_at_offset text offset with
     | Some url -> Oystermark.Vault.Link_ref.of_cmark_dest url
     | None -> None)
;;

(** {1 Target → LSP Location} *)

(** Find the 0-based line number of a heading in file content by matching
    heading text. Returns 0 if not found. *)
let find_heading_line (content : string) (heading_text : string) : int =
  let lines = String.split_on_char '\n' content in
  let rec loop i = function
    | [] -> 0
    | line :: rest ->
      let trimmed = String.trim line in
      if String.length trimmed > 0 && trimmed.[0] = '#'
      then (
        (* Strip leading #s and spaces *)
        let j = ref 0 in
        while !j < String.length trimmed && trimmed.[!j] = '#' do
          incr j
        done;
        while !j < String.length trimmed && trimmed.[!j] = ' ' do
          incr j
        done;
        let text =
          String.sub trimmed !j (String.length trimmed - !j)
        in
        if text = heading_text then i else loop (i + 1) rest)
      else loop (i + 1) rest
  in
  loop 0 lines
;;

(** Find the 0-based line number of a block ID (^id) in file content.
    Returns 0 if not found. *)
let find_block_id_line (content : string) (block_id : string) : int =
  let pattern = "^" ^ block_id in
  let lines = String.split_on_char '\n' content in
  let rec loop i = function
    | [] -> 0
    | line :: rest ->
      (match find_substring line ~needle:pattern ~from:0 with
       | Some _ -> i
       | None -> loop (i + 1) rest)
  in
  loop 0 lines
;;

let pos_zero = Position.create ~line:0 ~character:0

(** Map a resolved target to an LSP Location. *)
let target_to_location (vault_root : string) (curr_file_path : string)
    (target : Oystermark.Vault.Resolve.target)
  : Location.t option
  =
  let make_loc path line =
    let full_path = Filename.concat vault_root path in
    let uri = DocumentUri.of_path full_path in
    let pos = Position.create ~line ~character:0 in
    let range = Range.create ~start:pos ~end_:pos in
    Some (Location.create ~uri ~range)
  in
  let read_file path =
    let full_path = Filename.concat vault_root path in
    try Some (In_channel.with_open_text full_path In_channel.input_all)
    with _ -> None
  in
  match target with
  | Note { path } | File { path } -> make_loc path 0
  | Heading { path; heading; _ } ->
    (match read_file path with
     | Some content -> make_loc path (find_heading_line content heading)
     | None -> make_loc path 0)
  | Block { path; block_id } ->
    (match read_file path with
     | Some content -> make_loc path (find_block_id_line content block_id)
     | None -> make_loc path 0)
  | Curr_file ->
    let uri = DocumentUri.of_path curr_file_path in
    let range = Range.create ~start:pos_zero ~end_:pos_zero in
    Some (Location.create ~uri ~range)
  | Curr_heading { heading; _ } ->
    (match read_file curr_file_path with
     | Some content ->
       let uri = DocumentUri.of_path curr_file_path in
       let line = find_heading_line content heading in
       let pos = Position.create ~line ~character:0 in
       let range = Range.create ~start:pos ~end_:pos in
       Some (Location.create ~uri ~range)
     | None ->
       let uri = DocumentUri.of_path curr_file_path in
       let range = Range.create ~start:pos_zero ~end_:pos_zero in
       Some (Location.create ~uri ~range))
  | Curr_block { block_id } ->
    (match read_file curr_file_path with
     | Some content ->
       let uri = DocumentUri.of_path curr_file_path in
       let line = find_block_id_line content block_id in
       let pos = Position.create ~line ~character:0 in
       let range = Range.create ~start:pos ~end_:pos in
       Some (Location.create ~uri ~range)
     | None ->
       let uri = DocumentUri.of_path curr_file_path in
       let range = Range.create ~start:pos_zero ~end_:pos_zero in
       Some (Location.create ~uri ~range))
  | Unresolved -> None
;;

(** {1 Vault index building} *)

(** Build a vault index by scanning the vault root directory, reading and
    parsing all markdown files. *)
let build_vault_index (vault_root : string) : Oystermark.Vault.Index.t =
  let all_entries = Oystermark.Vault.list_entries vault_root in
  let is_dir p = String.length p > 0 && p.[String.length p - 1] = '/' in
  let dirs = List.filter is_dir all_entries in
  let files = List.filter (fun p -> not (is_dir p)) all_entries in
  let is_md p =
    String.length p > 3 && String.sub p (String.length p - 3) 3 = ".md"
  in
  let md_files = List.filter is_md files in
  let other_files = List.filter (fun p -> not (is_md p)) files in
  let md_docs =
    List.filter_map
      (fun rel_path ->
        let full_path = Filename.concat vault_root rel_path in
        try
          let content =
            In_channel.with_open_text full_path In_channel.input_all
          in
          let doc = Oystermark.Parse.of_string content in
          Some (rel_path, doc)
        with
        | _ -> None)
      md_files
  in
  Oystermark.Vault.build_index ~md_docs ~other_files ~dirs
;;

(** {1 LSP server} *)

class oystermark_server =
  object (self)
    inherit Linol_eio.Jsonrpc2.server as super

    val mutable vault_root : string option = None

    val mutable index : Oystermark.Vault.Index.t =
      { Oystermark.Vault.Index.files = []; dirs = [] }

    method spawn_query_handler f = Linol_eio.spawn f

    (* Capabilities *)
    method! config_definition = Some (`Bool true)

    method! config_sync_opts =
      TextDocumentSyncOptions.create
        ~change:TextDocumentSyncKind.Full
        ~openClose:true
        ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
        ()

    (* Only process markdown files *)
    method! filter_text_document uri =
      let path = DocumentUri.to_path uri in
      Filename.check_suffix path ".md"

    (* Initialize: capture workspace root, build index *)
    method! on_req_initialize ~notify_back (params : InitializeParams.t) =
      let root =
        match params.rootUri with
        | Some uri -> Some (DocumentUri.to_path uri)
        | None -> Option.join params.rootPath
      in
      vault_root <- root;
      self#rebuild_index;
      super#on_req_initialize ~notify_back params

    method private rebuild_index =
      match vault_root with
      | None -> ()
      | Some root -> index <- build_vault_index root

    (* Document lifecycle *)
    method on_notif_doc_did_open ~notify_back:_ _doc ~content:_ =
      self#rebuild_index

    method on_notif_doc_did_close ~notify_back:_ _doc = ()

    method on_notif_doc_did_change ~notify_back:_ _doc _changes ~old_content:_
        ~new_content:_ =
      ()

    method! on_notif_doc_did_save ~notify_back:_ _params = self#rebuild_index

    (* Go to definition *)
    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ (doc_st : doc_state) =
      match vault_root with
      | None -> None
      | Some root ->
        let file_path = DocumentUri.to_path uri in
        let rel_path =
          let prefix = root ^ "/" in
          let plen = String.length prefix in
          if String.length file_path >= plen
             && String.sub file_path 0 plen = prefix
          then String.sub file_path plen (String.length file_path - plen)
          else file_path
        in
        let offset = byte_offset_of_position doc_st.content pos in
        (match find_link_ref_at_offset doc_st.content offset with
         | None -> None
         | Some link_ref ->
           let target =
             Oystermark.Vault.Resolve.resolve link_ref rel_path index
           in
           (match target_to_location root file_path target with
            | None -> None
            | Some loc -> Some (`Location [ loc ])))
  end

let () =
  Eio_main.run @@ fun env ->
  let s = new oystermark_server in
  let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
  Linol_eio.Jsonrpc2.run server
;;
