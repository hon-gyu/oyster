(** Completion: suggest note names, headings, block ids, and attribute ids as
    the user types inside wikilink brackets, and vault paths and fragments
    inside the destination of a Markdown link or image.

    Spec: {!page-"feature-completion"} and
    {!page-"feature-completion-markdown-links"}; attribute ids per
    {!page-"feature-attribute-anchors"}. *)

open Core

(** {1:implementation Implementation} *)

(** The subset of [CompletionItemKind] this feature emits. *)
type kind =
  | File
  | Reference
[@@deriving sexp, equal, compare]

(** A completion suggestion.  Fields mirror the LSP [CompletionItem] subset used
    here; {!Server.completion} converts to the wire type, where [insert_text]
    becomes the [textEdit]'s [newText].
    See {!page-"feature-completion".completion_item_shape}. *)
type item =
  { label : string
  ; detail : string option
  ; filter_text : string option
  ; insert_text : string option
  ; kind : kind
  }
[@@deriving sexp, equal, compare]

(** A completion response: the suggestions, the span they replace, and whether
    the list was cut short.

    [replace_from] is a byte offset into the document; the replaced span runs
    from there to the cursor.  Every item replaces the same span — the prefix
    the user has typed — so the range is a property of the request rather than
    of an item.  Handing the client an explicit range is what keeps a path with
    a [/] in it from being pasted on top of itself; see
    {!page-"feature-completion-markdown-links".replace_range}. *)
type completions =
  { items : item list
  ; replace_from : int
  ; incomplete : bool
  }
[@@deriving sexp, equal, compare]

(** {2 Trigger detection} *)

(** The line containing byte [offset], as [(text_before_offset, line_start)]. *)
let line_before ~(content : string) ~(offset : int) : string * int =
  let head = String.prefix content offset in
  let line_start =
    match String.rfindi head ~f:(fun _ c -> Char.equal c '\n') with
    | Some i -> i + 1
    | None -> 0
  in
  String.subo head ~pos:line_start, line_start
;;

(** The wikilink prefix under the cursor and the byte offset it starts at: the
    text after the innermost pair of opening square brackets (including an
    embed) and before the cursor.  [None] if the cursor is not inside an open
    wikilink, or a [\]] closes it first.
    See {!page-"feature-completion".trigger_context}. *)
let wikilink_prefix ~(content : string) ~(line : int) ~(character : int)
  : (string * int) option
  =
  let offset = Lsp_util.byte_offset_of_position content ~line ~character in
  let before, line_start = line_before ~content ~offset in
  match List.last (String.substr_index_all before ~may_overlap:false ~pattern:"[[") with
  | None -> None
  | Some i ->
    let prefix = String.subo before ~pos:(i + 2) in
    (* A [\]] between the [[[] and the cursor means the link is already closed. *)
    if String.contains prefix ']' then None else Some (prefix, line_start + i + 2)
;;

(** Where in a Markdown destination the cursor sits, and what the answer must
    replace.  Byte offsets are absolute in the document.
    See {!page-"feature-completion-markdown-links".trigger_context}. *)
type md_dest =
  | Path of
      { dest_start : int
        (** Just after the [(] — so the replacement covers an opening [<] and
          an angle-bracketed suggestion can supply its own. *)
      ; image : bool (** The label was introduced by [!]. *)
      }
  | Fragment of
      { note_part : string (** The destination the fragment belongs to. *)
      ; frag_start : int (** Just after the [#]. *)
      }

(** Detection is textual and single-line, matching {!wikilink_prefix}.
    [None] when the cursor is not inside an open destination.

    The angle-bracket form is where the two shapes differ: whitespace is legal
    inside the brackets, and the [>] ends the {e path} without ending the
    destination — [\[a\](<my note.md>#|)] is a fragment position, and has to be
    one, or the feature would abandon a spaced path the moment it wrote one.
    See {!page-"feature-completion-markdown-links".trigger_context}. *)
let markdown_dest ~(content : string) ~(line : int) ~(character : int) : md_dest option =
  let offset = Lsp_util.byte_offset_of_position content ~line ~character in
  let before, line_start = line_before ~content ~offset in
  match String.rindex before '(' with
  | None -> None
  (* An ordinary parenthesis: only [\]\(] opens a destination. *)
  | Some i when i = 0 || not (Char.equal before.[i - 1] ']') -> None
  | Some i ->
    let dest_start = line_start + i + 1 in
    let raw = String.subo before ~pos:(i + 1) in
    (* The label's own brackets need not be well-formed; only the [!]
       immediately before its [\[] is consulted. *)
    let image =
      match String.rindex (String.prefix before i) '[' with
      | Some j -> j > 0 && Char.equal before.[j - 1] '!'
      | None -> false
    in
    (* [path_start] is where the path text begins, past any [<]; [extra] is
       what separates the path from a [#] (the [>], when there is one). *)
    let split ~path_start ~extra text =
      match String.lsplit2 text ~on:'#' with
      | None -> Some (Path { dest_start; image })
      | Some (note_part, fragment) ->
        if String.exists fragment ~f:Char.is_whitespace
        then None
        else
          Some
            (Fragment
               { note_part
               ; frag_start = path_start + String.length note_part + extra + 1
               })
    in
    if String.contains raw ')'
    then None
    else if not (String.is_prefix raw ~prefix:"<")
    then
      if String.exists raw ~f:Char.is_whitespace
      then None
      else split ~path_start:dest_start ~extra:0 raw
    else (
      let inside = String.subo raw ~pos:1 in
      match String.lsplit2 inside ~on:'>' with
      (* Still between the brackets: whitespace is fine here. *)
      | None -> split ~path_start:(dest_start + 1) ~extra:0 inside
      (* Past the closing [>]: only a fragment may follow. *)
      | Some (path, after) ->
        if not (String.is_prefix after ~prefix:"#")
        then None
        else split ~path_start:(dest_start + 1) ~extra:1 (path ^ after))
;;

(** {2 Note-name mode} *)

(** The note name a file is suggested under: its basename without [.md] when
    that basename is unique in the vault, else the full relative path without
    [.md] (so the suggestion stays unambiguous).
    See {!page-"feature-completion".note_name_completion}. *)
let note_name_items (index : Oystermark.Vault.Index.t) : item list =
  let md_files =
    Oystermark.Vault.Index.notes index |> List.map ~f:Oystermark.Vault.Index.Note.path
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

(** {2 Path mode}

    Markdown destinations only.  See
    {!page-"feature-completion-markdown-links".path_completion}. *)

(** Whether [path] can be written as a bare CommonMark destination. *)
let needs_angles (path : string) : bool =
  String.exists path ~f:(fun c -> Char.is_whitespace c || Char.is_print c |> not)
;;

let parens_balanced (path : string) : bool =
  let rec loop i depth =
    if depth < 0
    then false
    else if i >= String.length path
    then depth = 0
    else (
      match path.[i] with
      | '(' -> loop (i + 1) (depth + 1)
      | ')' -> loop (i + 1) (depth - 1)
      | _ -> loop (i + 1) depth)
  in
  loop 0 0
;;

(** [path] as a Markdown destination.

    A path containing whitespace goes in angle brackets, the form CommonMark
    defines and the parser strips before the resolver ever sees it.  Otherwise
    only what would end the destination early is backslash-escaped: [<] and
    [>], and parentheses when they do not balance.  Balanced parentheses are
    legal unescaped and are left legible.

    Every escape here is undone by the parser — angle brackets by
    [Match.link_destination], backslashes by its unescaping — so the
    destination reaching {!Oystermark.Vault.Index.resolve} is [path] itself.
    See {!page-"feature-completion-markdown-links".destination_escaping}. *)
let escape_destination (path : string) : string =
  let escape chars s =
    String.concat_map s ~f:(fun c ->
      if List.mem chars c ~equal:Char.equal then sprintf "\\%c" c else String.of_char c)
  in
  if needs_angles path
  then "<" ^ escape [ '\\'; '<'; '>' ] path ^ ">"
  else
    escape
      ('\\' :: '<' :: '>' :: (if parens_balanced path then [] else [ '('; ')' ]))
      path
;;

(** Most paths a single [(] may offer before the response is cut short and
    marked incomplete.  A constant rather than a setting: it exists to keep a
    keystroke cheap, and a user has no way to know what value would.
    See {!page-"feature-completion-markdown-links".volume}. *)
let max_path_items = 500

(** Every file in the index as a destination: the vault-relative path, with its
    extension, escaped so that inserting it yields a link that resolves back to
    the same file.

    In image context images sort first and notes after — an image-syntax embed
    of a note is a transclusion, and a legitimate thing to write — and outside
    it the two groups swap.  Returns the items and whether the list was cut
    short at {!max_path_items}. *)
let path_items ~(image : bool) (index : Oystermark.Vault.Index.t) : item list * bool =
  let module Index = Oystermark.Vault.Index in
  let title (note : Index.Note.t) =
    Index.Note.headings note
    |> List.find_map ~f:(fun (h, _) -> if h.level = 1 then Some h.text else None)
  in
  let item rel_path detail =
    let group =
      (* [0] sorts first. *)
      match Link_collect.is_image_target rel_path, image with
      | true, true | false, false -> 0
      | _ -> 1
    in
    ( group
    , { label = rel_path
      ; detail
      ; filter_text = Some rel_path
      ; insert_text = Some (escape_destination rel_path)
      ; kind = File
      } )
  in
  let items =
    List.map (Index.notes index) ~f:(fun note -> item (Index.Note.path note) (title note))
    @ List.map (Index.assets index) ~f:(fun asset -> item (Index.Asset.path asset) None)
    |> List.sort ~compare:(fun (ga, a) (gb, b) ->
      match Int.compare ga gb with
      | 0 -> String.compare a.label b.label
      | n -> n)
    |> List.map ~f:snd
  in
  match List.split_n items max_path_items with
  | kept, [] -> kept, false
  | kept, _ -> kept, true
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
  : Oystermark.Vault.Index.Note.t option
  =
  let module Index = Oystermark.Vault.Index in
  let find = Index.find_note index in
  if String.is_empty note_part
  then (
    let doc = Lsp_util.parse_doc content in
    let file_stat : Index.file_stat = { rel_path; birthtime = None; mtime = None } in
    Some (Index.Note.of_doc_exn file_stat doc))
  else (
    let link_ref =
      { Oystermark.Vault.Link_ref.target = Some note_part; fragment = None }
    in
    match Oystermark.Vault.Index.resolve index rel_path link_ref with
    | Ok (Note path) -> find path
    | Ok (Asset _ | Anchor _) | Error _ -> None)
;;

(** Heading, block-id, and attribute-id suggestions for a file entry.  All three
    kinds share one fragment namespace (see {!page-"feature-attribute-anchors"}).
    See {!page-"feature-completion".fragment_completion}. *)
let fragment_items (entry : Oystermark.Vault.Index.Note.t) : item list =
  let module Index = Oystermark.Vault.Index in
  Index.Note.anchors entry
  |> List.map ~f:(fun anchor ->
    match anchor.value with
    | Index.Heading h ->
      { label = h.text
      ; detail = None
      ; filter_text = Some h.slug
      ; insert_text = Some h.slug
      ; kind = Reference
      }
    | Index.Caret id ->
      { label = "^" ^ id
      ; detail = None
      ; filter_text = Some id
      ; insert_text = Some ("^" ^ id)
      ; kind = Reference
      }
    | Index.Attr { id; _ } ->
      { label = "#" ^ id
      ; detail = Some "attribute"
      ; filter_text = Some id
      ; insert_text = Some id
      ; kind = Reference
      })
;;

(** {2 End-to-end} *)

(** Completion for the cursor at [(line, character)] in [content] at [rel_path]
    within [index].  No items when the cursor is inside neither a wikilink nor
    a Markdown destination, or when the fragment's note is unresolved.

    Wikilink detection runs first: inside [\[\[note (draft)#] the cursor is
    within wikilink brackets and the [(] is part of a note name, so the [(]
    before it does not open a destination.  See
    {!page-"feature-completion"} and
    {!page-"feature-completion-markdown-links"}. *)
let complete
      ~(index : Oystermark.Vault.Index.t)
      ~(rel_path : string)
      ~(content : string)
      ~(line : int)
      ~(character : int)
      ()
  : completions
  =
  Trace_core.with_span ~__FILE__ ~__LINE__ "completion.complete"
  @@ fun _sp ->
  let cursor = Lsp_util.byte_offset_of_position content ~line ~character in
  let nothing = { items = []; replace_from = cursor; incomplete = false } in
  let only items ~replace_from = { items; replace_from; incomplete = false } in
  (* The fragment prefix starts one byte past the [#] that ends [note_part]. *)
  let fragment_start ~prefix_start ~note_part =
    prefix_start + String.length note_part + 1
  in
  let fragments ~note_part ~replace_from =
    match target_entry ~index ~rel_path ~content note_part with
    | None -> nothing
    | Some entry -> only (fragment_items entry) ~replace_from
  in
  let result =
    match wikilink_prefix ~content ~line ~character with
    | Some (prefix, prefix_start) ->
      (match String.lsplit2 prefix ~on:'#' with
       | None -> only (note_name_items index) ~replace_from:prefix_start
       | Some (note_part, _fragment_prefix) ->
         fragments ~note_part ~replace_from:(fragment_start ~prefix_start ~note_part))
    | None ->
      (match markdown_dest ~content ~line ~character with
       | None -> nothing
       | Some (Path { dest_start; image }) ->
         let items, incomplete = path_items ~image index in
         { items; replace_from = dest_start; incomplete }
       | Some (Fragment { note_part; frag_start }) ->
         fragments ~note_part ~replace_from:frag_start)
  in
  Trace_core.add_data_to_span
    _sp
    [ "num_items", `Int (List.length result.items)
    ; "incomplete", `Bool result.incomplete
    ];
  result
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
      Oystermark.Vault.build_index ~md_docs ~other_files ()
    ;;

    let files =
      [ ( "note-a.md"
        , "# Alpha\n\n## Section One\n\nBody text ^block1\n\nThe [key]{#kt} span.\n" )
      ; "note-b.md", "# Beta\n\nText.\n"
      ; "sub/note-a.md", "# Sub Alpha\n\nText.\n"
      ]
    ;;

    let index = make_index files

    (** Items, then the span they replace — the prefix already typed — since
        that range is as much a part of the answer as the items are.  Silent
        when there is nothing to offer.  See
        {!page-"feature-completion-markdown-links".replace_range}. *)
    let show ~rel_path ~content ~line ~character =
      let { items; replace_from; incomplete } =
        complete ~index ~rel_path ~content ~line ~character ()
      in
      List.iter items ~f:(fun i -> print_s [%sexp (i : item)]);
      if not (List.is_empty items)
      then (
        let cursor = Lsp_util.byte_offset_of_position content ~line ~character in
        printf
          "replaces %S%s\n"
          (String.sub content ~pos:replace_from ~len:(cursor - replace_from))
          (if incomplete then " (incomplete)" else ""))
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
        replaces ""
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
        replaces ""
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
        replaces ""
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
        replaces ""
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

let%test_module "markdown links" =
  (module struct
    (** Spec: {!page-"feature-completion-markdown-links"}. *)

    let files =
      [ "note-a.md", "# Alpha\n\n## Section One\n\nBody ^block1\n"
      ; "sub/note-b.md", "# Beta\n\nText.\n"
      ; "untitled.md", "No heading here.\n"
      ; "assets/diagram.png", ""
      ; "assets/paper.pdf", ""
      ; "my note.md", "# Spaced\n"
      ]
    ;;

    let index =
      let md_docs =
        List.filter_map files ~f:(fun (rel_path, content) ->
          if String.is_suffix rel_path ~suffix:".md"
          then Some (rel_path, Oystermark.Parse.of_string content)
          else None)
      in
      let other_files =
        List.filter_map files ~f:(fun (p, _) ->
          if String.is_suffix p ~suffix:".md" then None else Some p)
      in
      Oystermark.Vault.build_index ~md_docs ~other_files ()
    ;;

    (** Labels and what would be inserted, then the replaced span. *)
    let show ?(rel_path = "note-a.md") content =
      let line, character =
        let lines = String.split_lines content in
        List.length lines - 1, String.length (List.last_exn lines)
      in
      let { items; replace_from; incomplete } =
        complete ~index ~rel_path ~content ~line ~character ()
      in
      List.iter items ~f:(fun i ->
        let insert = Option.value i.insert_text ~default:i.label in
        printf
          "%s -> %s%s\n"
          i.label
          insert
          (match i.detail with
           | Some d -> sprintf "   (%s)" d
           | None -> ""));
      if not (List.is_empty items)
      then (
        let cursor = Lsp_util.byte_offset_of_position content ~line ~character in
        printf
          "replaces %S%s\n"
          (String.sub content ~pos:replace_from ~len:(cursor - replace_from))
          (if incomplete then " (incomplete)" else ""))
    ;;

    (** {2 Path mode} *)

    (* Notes carry their title; assets do not.  Outside image context notes and
       non-image files sort ahead of images. *)
    let%expect_test "every file is offered as a path, with its extension" =
      show "See [label](";
      [%expect
        {|
        assets/paper.pdf -> assets/paper.pdf
        my note.md -> <my note.md>   (Spaced)
        note-a.md -> note-a.md   (Alpha)
        sub/note-b.md -> sub/note-b.md   (Beta)
        untitled.md -> untitled.md
        assets/diagram.png -> assets/diagram.png
        replaces ""
        |}]
    ;;

    (* The ordering flips; nothing is filtered out, because an image-syntax
       embed of a note is a transclusion and a legitimate thing to write. *)
    let%expect_test "image context sorts images first" =
      show "See ![alt](";
      [%expect
        {|
        assets/diagram.png -> assets/diagram.png
        assets/paper.pdf -> assets/paper.pdf
        my note.md -> <my note.md>   (Spaced)
        note-a.md -> note-a.md   (Alpha)
        sub/note-b.md -> sub/note-b.md   (Beta)
        untitled.md -> untitled.md
        replaces ""
        |}]
    ;;

    (* The whole typed prefix is replaced, [/] and all: this is the range a
       client left to its own word rules would get wrong.  See
       {!page-"feature-completion-markdown-links".replace_range}. *)
    let%expect_test "the replaced span covers a typed path prefix" =
      show "See [label](sub/no";
      [%expect
        {|
        assets/paper.pdf -> assets/paper.pdf
        my note.md -> <my note.md>   (Spaced)
        note-a.md -> note-a.md   (Alpha)
        sub/note-b.md -> sub/note-b.md   (Beta)
        untitled.md -> untitled.md
        assets/diagram.png -> assets/diagram.png
        replaces "sub/no"
        |}]
    ;;

    (* An empty label is enough; the label text is never consulted. *)
    let%expect_test "the label need not be well-formed" =
      show "[](note";
      [%expect
        {|
        assets/paper.pdf -> assets/paper.pdf
        my note.md -> <my note.md>   (Spaced)
        note-a.md -> note-a.md   (Alpha)
        sub/note-b.md -> sub/note-b.md   (Beta)
        untitled.md -> untitled.md
        assets/diagram.png -> assets/diagram.png
        replaces "note"
        |}]
    ;;

    (** {2 Angle brackets} *)

    (* The replacement covers the [<] the user typed, so the suggestion's own
       brackets do not double it. *)
    let%expect_test "an opened angle bracket is part of the replaced span" =
      show "See [label](<my no";
      [%expect
        {|
        assets/paper.pdf -> assets/paper.pdf
        my note.md -> <my note.md>   (Spaced)
        note-a.md -> note-a.md   (Alpha)
        sub/note-b.md -> sub/note-b.md   (Beta)
        untitled.md -> untitled.md
        assets/diagram.png -> assets/diagram.png
        replaces "<my no"
        |}]
    ;;

    (* Whitespace ends a bare destination but not an angle-bracketed one —
       without which a spaced path could never be completed again, fragment
       included. *)
    let%expect_test "whitespace: fatal bare, harmless inside brackets" =
      show "See [label](my no";
      [%expect {| |}];
      show "See [label](<my note.md>#";
      [%expect
        {|
        Spaced -> spaced
        replaces ""
        |}]
    ;;

    let%expect_test "a closed destination offers nothing" =
      show "See [label](note-a.md) ";
      [%expect {| |}];
      show "See [label](<my note.md> ";
      [%expect {| |}]
    ;;

    (** {2 Fragment mode} *)

    let%expect_test "fragments of the destination note" =
      show "See [label](note-a.md#";
      [%expect
        {|
        Alpha -> alpha
        Section One -> section-one
        ^block1 -> ^block1
        replaces ""
        |}]
    ;;

    let%expect_test "an empty destination means the current file" =
      show ~rel_path:"note-a.md" "# Alpha\n\n## Section One\n\nSee [label](#sec";
      [%expect
        {|
        Alpha -> alpha
        Section One -> section-one
        replaces "sec"
        |}]
    ;;

    let%expect_test "an unresolved destination offers no fragments" =
      show "See [label](missing.md#";
      [%expect {| |}]
    ;;

    (** {2 Not a destination} *)

    let%expect_test "a parenthesis not preceded by a bracket" =
      show "ordinary prose (as it happens";
      [%expect {| |}]
    ;;

    (* Wikilink detection runs first: the [(] here is part of a note name. *)
    let%expect_test "wikilink precedence" =
      show "See [[note (draft)#";
      [%expect {| |}]
    ;;

    (** {2 Escaping}

        See {!page-"feature-completion-markdown-links".destination_escaping}. *)

    let%expect_test "what each kind of path is written as" =
      List.iter
        [ "plain.md"
        ; "my note.md"
        ; "a(b)c.md"
        ; "a(b.md"
        ; "less<than>.md"
        ; "with space (and parens).md"
        ]
        ~f:(fun p -> printf "%s -> %s\n" p (escape_destination p));
      [%expect
        {|
        plain.md -> plain.md
        my note.md -> <my note.md>
        a(b)c.md -> a(b)c.md
        a(b.md -> a\(b.md
        less<than>.md -> less\<than\>.md
        with space (and parens).md -> <with space (and parens).md>
        |}]
    ;;

    (* The obligation the escaping exists to meet: what is offered, once
       inserted, is a link that resolves to the file it came from.  Driven
       through the real parser and resolver rather than a restatement of the
       escaping rules.  See {!page-"feature-completion-markdown-links".testing}. *)
    let%expect_test "round trip: every offered path resolves back to its file" =
      let items, _ = path_items ~image:false index in
      List.iter items ~f:(fun i ->
        let dest = Option.value_exn i.insert_text in
        let doc = Lsp_util.parse_doc (sprintf "[label](%s)" dest) in
        let resolved =
          match Link_collect.collect_links ~index ~rel_path:"note-a.md" doc with
          | [ ll ] ->
            (match ll.destination with
             | Ok (Note path | Asset path) -> path
             | Ok other -> Sexp.to_string [%sexp (other : Oystermark.Vault.Index.target)]
             | Error _ -> "<unresolved>")
          | links -> sprintf "<%d links>" (List.length links)
        in
        if String.equal resolved i.label
        then printf "%s ok\n" i.label
        else printf "%s -> %s MISMATCH\n" i.label resolved);
      [%expect
        {|
        assets/paper.pdf ok
        my note.md ok
        note-a.md ok
        sub/note-b.md ok
        untitled.md ok
        assets/diagram.png ok
        |}]
    ;;
  end)
;;
