(** Completion: suggest note names, headings, block ids, and attribute ids as
    the user types inside wikilink brackets.

    Spec: {!page-"feature-completion"}; attribute ids per
    {!page-"feature-attribute-anchors"}. *)

open Core

(** {1:implementation Implementation} *)

(** The subset of [CompletionItemKind] this feature emits. *)
type kind =
  | File
  | Reference
[@@deriving sexp, equal, compare]

(** A completion suggestion.  Fields mirror the LSP [CompletionItem] subset used
    here; [main.ml] converts to the wire type.
    See {!page-"feature-completion".completion_item_shape}. *)
type item =
  { label : string
  ; detail : string option
  ; filter_text : string option
  ; insert_text : string option
  ; kind : kind
  }
[@@deriving sexp, equal, compare]

(** {2 Trigger detection} *)

(** The wikilink prefix under the cursor: the text after the innermost pair of
    opening square brackets (including an embed) and before the cursor. [None]
    if the cursor is not inside an open wikilink, or a
    [\]] closes it first.  See {!page-"feature-completion".trigger_context}. *)
let wikilink_prefix ~(content : string) ~(line : int) ~(character : int) : string option =
  let offset = Lsp_util.byte_offset_of_position content ~line ~character in
  let head = String.prefix content offset in
  let line_start =
    match String.rfindi head ~f:(fun _ c -> Char.equal c '\n') with
    | Some i -> i + 1
    | None -> 0
  in
  let before = String.subo head ~pos:line_start in
  match List.last (String.substr_index_all before ~may_overlap:false ~pattern:"[[") with
  | None -> None
  | Some i ->
    let prefix = String.subo before ~pos:(i + 2) in
    (* A [\]] between the [[[] and the cursor means the link is already closed. *)
    if String.contains prefix ']' then None else Some prefix
;;

(** {2 Note-name mode} *)

(** The note name a file is suggested under: its basename without [.md] when
    that basename is unique in the vault, else the full relative path without
    [.md] (so the suggestion stays unambiguous).
    See {!page-"feature-completion".note_name_completion}. *)
let note_name_items (index : Oystermark.Vault.Index.t) : item list =
  let md_files =
    List.filter_map index.files ~f:(fun (f : Oystermark.Vault.Index.file_entry) ->
      if String.is_suffix f.rel_path ~suffix:".md" then Some f.rel_path else None)
  in
  let basename p = String.chop_suffix_if_exists (Filename.basename p) ~suffix:".md" in
  let counts =
    List.fold
      md_files
      ~init:(Map.empty (module String))
      ~f:(fun m p ->
        Map.update m (basename p) ~f:(function
          | None -> 1
          | Some n -> n + 1))
  in
  List.map md_files ~f:(fun p ->
    let base = basename p in
    let label =
      if Map.find_exn counts base = 1
      then base
      else String.chop_suffix_if_exists p ~suffix:".md"
    in
    { label
    ; detail = Some p
    ; filter_text = Some label
    ; insert_text = Some label
    ; kind = File
    })
  |> List.sort ~compare:(fun a b -> String.compare a.label b.label)
;;

(** {2 Fragment mode} *)

(** The indexed file entry the fragment's note part refers to: the current file
    (parsed live for freshness) when [note_part] is empty, otherwise the note
    resolved against the vault (first candidate), or [None] if unresolved. *)
let target_entry
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      (note_part : string)
  : Oystermark.Vault.Index.file_entry option
  =
  let find path = List.find index.files ~f:(fun f -> String.equal f.rel_path path) in
  if String.is_empty note_part
  then (
    let doc = Lsp_util.parse_doc content in
    Some
      { rel_path
      ; headings = Oystermark.Vault.Index.extract_headings doc
      ; blocks = Oystermark.Vault.Index.extract_block_ids doc
      ; attrs = Oystermark.Vault.Index.extract_attr_ids doc
      })
  else (
    let link_ref =
      { Oystermark.Vault.Link_ref.target = Some note_part; fragment = None }
    in
    match Oystermark.Vault.Resolve.resolve link_ref rel_path index with
    | Note { path } | File { path } -> find path
    | Curr_file -> find rel_path
    | _ -> None)
;;

(** Heading, block-id, and attribute-id suggestions for a file entry.  All three
    kinds share one fragment namespace (see {!page-"feature-attribute-anchors"}).
    See {!page-"feature-completion".fragment_completion}. *)
let fragment_items (entry : Oystermark.Vault.Index.file_entry) : item list =
  let heading_items =
    List.map entry.headings ~f:(fun (h : Oystermark.Vault.Index.heading_entry) ->
      { label = h.text
      ; detail = None
      ; filter_text = Some h.slug
      ; insert_text = Some h.slug
      ; kind = Reference
      })
  in
  let block_items =
    List.map entry.blocks ~f:(fun (b : Oystermark.Vault.Index.block_entry) ->
      { label = "^" ^ b.id
      ; detail = None
      ; filter_text = Some b.id
      ; insert_text = Some ("^" ^ b.id)
      ; kind = Reference
      })
  in
  let attr_items =
    List.map entry.attrs ~f:(fun (a : Oystermark.Vault.Index.attr_entry) ->
      { label = "#" ^ a.id
      ; detail = Some "attribute"
      ; filter_text = Some a.id
      ; insert_text = Some a.id
      ; kind = Reference
      })
  in
  heading_items @ block_items @ attr_items
;;

(** {2 End-to-end} *)

(** Completion items for the cursor at [(line, character)] in [content] at
    [rel_path] within [index].  Empty when the cursor is not inside a wikilink
    or the fragment's note is unresolved.  See {!page-"feature-completion"}. *)
let complete
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ~(line : int)
      ~(character : int)
      ()
  : item list
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "completion.complete"
  @@ fun _sp ->
  match wikilink_prefix ~content ~line ~character with
  | None -> []
  | Some prefix ->
    (match String.lsplit2 prefix ~on:'#' with
     | None -> note_name_items index
     | Some (note_part, _fragment_prefix) ->
       (match target_entry ~index ~rel_path ~content note_part with
        | None -> []
        | Some entry -> fragment_items entry))
;;

(** {1:test Test} *)

let%test_module "completion" =
  (module struct
    let make_index (files : (string * string) list) : Oystermark.Vault.Index.t =
      let md_docs =
        List.filter_map files ~f:(fun (rel_path, content) ->
          if String.is_suffix rel_path ~suffix:".md"
          then Some (rel_path, Oystermark.Parse.of_string content)
          else None)
      in
      let other_files =
        List.filter_map files ~f:(fun (p, _) ->
          if not (String.is_suffix p ~suffix:".md") then Some p else None)
      in
      Oystermark.Vault.build_index ~md_docs ~other_files ~dirs:[]
    ;;

    let files =
      [ ( "note-a.md"
        , "# Alpha\n\n## Section One\n\nBody text ^block1\n\nThe [key]{#kt} span.\n" )
      ; "note-b.md", "# Beta\n\nText.\n"
      ; "sub/note-a.md", "# Sub Alpha\n\nText.\n"
      ]
    ;;

    let index = make_index files

    let show ~rel_path ~content ~line ~character =
      let items = complete ~index ~rel_path ~content ~line ~character () in
      List.iter items ~f:(fun i -> print_s [%sexp (i : item)])
    ;;

    let%expect_test "note-name mode: ambiguous basename disambiguated by path" =
      (* note-a appears twice, so both use their full path; note-b is unique. *)
      show ~rel_path:"note-b.md" ~content:"See [[" ~line:0 ~character:6;
      [%expect
        {|
        ((label note-a) (detail (note-a.md)) (filter_text (note-a))
         (insert_text (note-a)) (kind File))
        ((label note-b) (detail (note-b.md)) (filter_text (note-b))
         (insert_text (note-b)) (kind File))
        ((label sub/note-a) (detail (sub/note-a.md)) (filter_text (sub/note-a))
         (insert_text (sub/note-a)) (kind File))
        |}]
    ;;

    let%expect_test "fragment mode: headings, block ids, attribute ids" =
      show ~rel_path:"note-b.md" ~content:"See [[note-a#" ~line:0 ~character:13;
      [%expect
        {|
        ((label Alpha) (detail ()) (filter_text (alpha)) (insert_text (alpha))
         (kind Reference))
        ((label "Section One") (detail ()) (filter_text (section-one))
         (insert_text (section-one)) (kind Reference))
        ((label ^block1) (detail ()) (filter_text (block1)) (insert_text (^block1))
         (kind Reference))
        ((label #kt) (detail (attribute)) (filter_text (kt)) (insert_text (kt))
         (kind Reference))
        |}]
    ;;

    let%expect_test "fragment mode: current file (empty note part)" =
      let content = "# Self\n\n## Sec\n\n[[#" in
      show ~rel_path:"note-a.md" ~content ~line:4 ~character:3;
      [%expect
        {|
        ((label Self) (detail ()) (filter_text (self)) (insert_text (self))
         (kind Reference))
        ((label Sec) (detail ()) (filter_text (sec)) (insert_text (sec))
         (kind Reference))
        |}]
    ;;

    let%expect_test "embed wikilink triggers the same way" =
      show ~rel_path:"note-b.md" ~content:"![[note-a#" ~line:0 ~character:10;
      [%expect
        {|
        ((label Alpha) (detail ()) (filter_text (alpha)) (insert_text (alpha))
         (kind Reference))
        ((label "Section One") (detail ()) (filter_text (section-one))
         (insert_text (section-one)) (kind Reference))
        ((label ^block1) (detail ()) (filter_text (block1)) (insert_text (^block1))
         (kind Reference))
        ((label #kt) (detail (attribute)) (filter_text (kt)) (insert_text (kt))
         (kind Reference))
        |}]
    ;;

    let%expect_test "unresolved note in fragment mode: no items" =
      show ~rel_path:"note-b.md" ~content:"See [[missing#" ~line:0 ~character:14;
      [%expect {| |}]
    ;;

    let%expect_test "cursor not in a wikilink: no items" =
      show ~rel_path:"note-b.md" ~content:"just text here" ~line:0 ~character:5;
      [%expect {| |}]
    ;;

    let%expect_test "closed wikilink before cursor: no items" =
      show ~rel_path:"note-b.md" ~content:"[[note-a]] " ~line:0 ~character:11;
      [%expect {| |}]
    ;;
  end)
;;
