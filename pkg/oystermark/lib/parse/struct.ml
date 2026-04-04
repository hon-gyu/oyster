(** Struct: colon-keyed tree restructuring.

    A keyed node is a list item or paragraph whose text content ends with
    [:] (unescaped).  The following contiguous content is reparented as its
    children, producing a new tree with {!Ext_keyed_block} and
    {!Ext_keyed_list_item} nodes.

    {1 Parsing}

    Parsing is a single-pass rewrite on the already-parsed Cmarkit AST:
    {ol
    {- Walk sibling block lists left-to-right.}
    {- When a keyed paragraph is found, collect contiguous following blocks
       and wrap as {!Ext_keyed_block}.}
    {- When a list's last item is keyed and followed by contiguous blocks,
       reparent those blocks under the item as {!Ext_keyed_list_item}.}
    {- Recurse into container blocks.}}
*)

open Core

type t = { label : Cmarkit.Inline.t }

type Cmarkit.Block.t +=
  | Ext_keyed_list_item of t * Cmarkit.Block.t
  | Ext_keyed_block of t * Cmarkit.Block.t

(* Inline text extraction
   =======================

   Minimal inline-to-plain-text for colon chain splitting.  Does not need
   wikilink support since wikilinks in key labels would be unusual. *)

let rec inline_to_text (inline : Cmarkit.Inline.t) : string =
  match inline with
  | Cmarkit.Inline.Text (s, _) -> s
  | Cmarkit.Inline.Inlines (is, _) -> String.concat (List.map is ~f:inline_to_text)
  | Cmarkit.Inline.Emphasis (e, _) | Cmarkit.Inline.Strong_emphasis (e, _) ->
    inline_to_text (Cmarkit.Inline.Emphasis.inline e)
  | Cmarkit.Inline.Break (_, _) -> " "
  | _ -> ""
;;

(* Colon detection
   ================

   Walk the inline tree to find the rightmost [Text] node and check whether
   it ends with an unescaped [:].  Returns [Some inline'] where [inline'] has
   the trailing colon (and any surrounding whitespace) stripped, or [None].

   Escaped-colon detection: when [~source] is provided and the text node
   carries location information, we look one byte before the colon in the
   original source to see if it is a backslash. *)

(** Check whether a trailing colon at source byte [colon_byte] is escaped. *)
let is_escaped_in_source ~(source : string option) (colon_byte : int) : bool =
  match source with
  | None -> false
  | Some src ->
    colon_byte > 0
    && colon_byte < String.length src
    && Char.equal src.[colon_byte - 1] '\\'
;;

(** Try to strip a trailing colon from a single [Text] node's string.
    Returns [Some stripped_text] or [None]. *)
let strip_colon_from_text ~(source : string option) (s : string) (meta : Cmarkit.Meta.t)
  : string option
  =
  let s' = String.rstrip s in
  if String.is_empty s' || not (Char.equal s'.[String.length s' - 1] ':')
  then None
  else (
    let loc = Cmarkit.Meta.textloc meta in
    let colon_source_byte =
      if Cmarkit.Textloc.is_none loc
      then -1
      else (
        let first = Cmarkit.Textloc.first_byte loc in
        let text_colon_pos = String.length s' - 1 in
        first + text_colon_pos)
    in
    if is_escaped_in_source ~source colon_source_byte
    then None
    else Some (String.rstrip (String.chop_suffix_exn s' ~suffix:":")))
;;

(** Strip the trailing colon from the rightmost [Text] leaf of an inline tree.
    Returns [Some inline'] with the colon removed, or [None]. *)
let rec strip_trailing_colon ~(source : string option) (inline : Cmarkit.Inline.t)
  : Cmarkit.Inline.t option
  =
  match inline with
  | Cmarkit.Inline.Text (s, meta) ->
    (match strip_colon_from_text ~source s meta with
     | None -> None
     | Some stripped -> Some (Cmarkit.Inline.Text (stripped, meta)))
  | Cmarkit.Inline.Inlines (inlines, meta) ->
    let rec go_last rev_prefix = function
      | [] -> None
      | [ last ] ->
        (match strip_trailing_colon ~source last with
         | Some last' ->
           Some (Cmarkit.Inline.Inlines (List.rev_append rev_prefix [ last' ], meta))
         | None -> None)
      | x :: rest -> go_last (x :: rev_prefix) rest
    in
    go_last [] inlines
  | _ -> None
;;

(* Colon chains
   =============

   When a text contains multiple colon-terminated segments separated by
   spaces, e.g. ["foo: bar"], split into [["foo"; "bar"]].  Each segment
   becomes a nesting level.  The input has already had its final trailing
   colon stripped. *)

(** Split a label string at [: ] (colon-space) boundaries.
    Returns label strings outermost-first. *)
let split_colon_chain (s : string) : string list =
  let parts = String.split_on_chars s ~on:[ ':' ] in
  match parts with
  | [] -> []
  | [ single ] -> [ String.strip single ]
  | _ ->
    let rec merge acc current = function
      | [] -> List.rev (String.strip current :: acc)
      | next :: rest ->
        if String.is_prefix next ~prefix:" "
        then merge (String.strip current :: acc) (String.lstrip next) rest
        else merge acc (current ^ ":" ^ next) rest
    in
    (match parts with
     | first :: rest ->
       let labels = merge [] first rest in
       List.filter labels ~f:(fun s -> not (String.is_empty s))
     | [] -> [])
;;

(** Extract label strings from an inline whose trailing colon was stripped.
    Returns a singleton list for simple labels, or multiple for colon chains. *)
let labels_of_inline (inline : Cmarkit.Inline.t) : string list =
  let text = inline_to_text inline in
  let labels = split_colon_chain text in
  if List.is_empty labels
  then (
    let t = String.strip text in
    if String.is_empty t then [] else [ t ])
  else labels
;;

(** Build nested keyed nodes from a list of labels (outermost-first) and
    a body block. *)
let build_nested_keyed
      ~(make_node : t -> Cmarkit.Block.t -> Cmarkit.Block.t)
      (labels : string list)
      (body : Cmarkit.Block.t)
  : Cmarkit.Block.t
  =
  let mk s b =
    let label = Cmarkit.Inline.Text (s, Cmarkit.Meta.none) in
    make_node { label } b
  in
  match List.rev labels with
  | [] -> body
  | innermost :: outers ->
    List.fold outers ~init:(mk innermost body) ~f:(fun acc s -> mk s acc)
;;

(* Tree rewrite
   =============

   Single-pass left-to-right traversal of sibling block lists. *)

let is_blank_line : Cmarkit.Block.t -> bool = function
  | Cmarkit.Block.Blank_line _ -> true
  | _ -> false
;;

(** Collect contiguous (no blank line) blocks starting at [start]. *)
let collect_contiguous (arr : Cmarkit.Block.t array) (start : int) (len : int)
  : Cmarkit.Block.t list * int
  =
  let collected = ref [] in
  let i = ref start in
  while !i < len && not (is_blank_line arr.(!i)) do
    collected := arr.(!i) :: !collected;
    incr i
  done;
  List.rev !collected, !i
;;

let wrap_blocks : Cmarkit.Block.t list -> Cmarkit.Block.t = function
  | [] -> Cmarkit.Block.Blocks ([], Cmarkit.Meta.none)
  | [ single ] -> single
  | multiple -> Cmarkit.Block.Blocks (multiple, Cmarkit.Meta.none)
;;

(** Get the paragraph inline from a list item's block content.
    Returns [(paragraph, para_meta, sub_blocks)] where [sub_blocks] are
    any additional blocks already nested under the item (indented content). *)
let list_item_paragraph (item : Cmarkit.Block.List_item.t)
  : (Cmarkit.Block.Paragraph.t * Cmarkit.Meta.t * Cmarkit.Block.t list) option
  =
  match Cmarkit.Block.List_item.block item with
  | Cmarkit.Block.Paragraph (p, meta) -> Some (p, meta, [])
  | Cmarkit.Block.Blocks (Cmarkit.Block.Paragraph (p, meta) :: rest, _) ->
    Some (p, meta, rest)
  | _ -> None
;;

(** Rebuild a [List_item] preserving layout, but with new block content. *)
let rebuild_item (item : Cmarkit.Block.List_item.t) (block : Cmarkit.Block.t)
  : Cmarkit.Block.List_item.t
  =
  Cmarkit.Block.List_item.make
    ~before_marker:(Cmarkit.Block.List_item.before_marker item)
    ~marker:(Cmarkit.Block.List_item.marker item)
    ~after_marker:(Cmarkit.Block.List_item.after_marker item)
    block
;;

(** Process list items: tag keyed items (Rule 1).  Returns the rebuilt items
    and, if the last item is keyed with no sub-blocks, its label info for
    Rule 3 handling. *)
let process_list_items
      ~(source : string option)
      (items : Cmarkit.Block.List_item.t Cmarkit.node list)
  : Cmarkit.Block.List_item.t Cmarkit.node list * (Cmarkit.Inline.t * string list) option
  =
  let len = List.length items in
  let rebuilt_items = ref [] in
  let last_keyed = ref None in
  List.iteri items ~f:(fun i (item, item_meta) ->
    let is_last = i = len - 1 in
    match list_item_paragraph item with
    | Some (p, _para_meta, sub_blocks) ->
      let inline = Cmarkit.Block.Paragraph.inline p in
      (match strip_trailing_colon ~source inline with
       | None -> rebuilt_items := (item, item_meta) :: !rebuilt_items
       | Some label_inline ->
         let labels = labels_of_inline label_inline in
         if not (List.is_empty sub_blocks)
         then (
           (* Rule 1: already has indented children — tag as keyed *)
           let body = wrap_blocks sub_blocks in
           let keyed =
             build_nested_keyed
               ~make_node:(fun t b -> Ext_keyed_list_item (t, b))
               labels
               body
           in
           rebuilt_items := (rebuild_item item keyed, item_meta) :: !rebuilt_items)
         else if is_last
         then (
           (* Last item, keyed, no sub-blocks — candidate for Rule 3 *)
           last_keyed := Some (label_inline, labels);
           rebuilt_items := (item, item_meta) :: !rebuilt_items)
         else rebuilt_items := (item, item_meta) :: !rebuilt_items)
    | None -> rebuilt_items := (item, item_meta) :: !rebuilt_items);
  List.rev !rebuilt_items, !last_keyed
;;

(** Main sibling-list rewrite. *)
let rec rewrite_block_list ~(source : string option) (blocks : Cmarkit.Block.t list)
  : Cmarkit.Block.t list
  =
  let arr = Array.of_list blocks in
  let len = Array.length arr in
  let result = ref [] in
  let i = ref 0 in
  while !i < len do
    let block = arr.(!i) in
    match block with
    (* Keyed paragraph (Rule 4/5) *)
    | Cmarkit.Block.Paragraph (p, _meta) ->
      let inline = Cmarkit.Block.Paragraph.inline p in
      (match strip_trailing_colon ~source inline with
       | None ->
         result := rewrite_within_block ~source block :: !result;
         incr i
       | Some label_inline ->
         incr i;
         let children, new_i = collect_contiguous arr !i len in
         i := new_i;
         if List.is_empty children
         then result := block :: !result
         else (
           let children = rewrite_block_list ~source children in
           let body = wrap_blocks children in
           let labels = labels_of_inline label_inline in
           let keyed =
             build_nested_keyed
               ~make_node:(fun t b -> Ext_keyed_block (t, b))
               labels
               body
           in
           result := keyed :: !result))
    (* List — process items, handle Rule 2/3 for last item *)
    | Cmarkit.Block.List (l, list_meta) ->
      let items = Cmarkit.Block.List'.items l in
      let rebuilt_items, last_keyed = process_list_items ~source items in
      (* Recurse into each item's block content *)
      let recurse_items items =
        List.map items ~f:(fun (item, item_meta) ->
          let block = Cmarkit.Block.List_item.block item in
          let block' = rewrite_within_block ~source block in
          if phys_equal block block'
          then item, item_meta
          else rebuild_item item block', item_meta)
      in
      let make_list items =
        Cmarkit.Block.List
          ( Cmarkit.Block.List'.make
              ~tight:(Cmarkit.Block.List'.tight l)
              (Cmarkit.Block.List'.type' l)
              items
          , list_meta )
      in
      (match last_keyed with
       | Some (_label_inline, labels) ->
         incr i;
         if !i >= len || is_blank_line arr.(!i)
         then
           (* Rule 2: blank or end follows — no reparenting *)
           result := make_list (recurse_items rebuilt_items) :: !result
         else (
           (* Rule 3: contiguous blocks follow — reparent under last item *)
           let following, new_i = collect_contiguous arr !i len in
           i := new_i;
           let following = rewrite_block_list ~source following in
           let body = wrap_blocks following in
           let keyed =
             build_nested_keyed
               ~make_node:(fun t b -> Ext_keyed_list_item (t, b))
               labels
               body
           in
           let prev_items = List.take rebuilt_items (List.length rebuilt_items - 1) in
           let last_item, last_item_meta = List.last_exn rebuilt_items in
           let new_last = rebuild_item last_item keyed in
           let new_items = prev_items @ [ new_last, last_item_meta ] in
           result := make_list (recurse_items new_items) :: !result)
       | None ->
         result := make_list (recurse_items rebuilt_items) :: !result;
         incr i)
    | _ ->
      result := rewrite_within_block ~source block :: !result;
      incr i
  done;
  List.rev !result

(** Recurse into block containers. *)
and rewrite_within_block ~(source : string option) (block : Cmarkit.Block.t)
  : Cmarkit.Block.t
  =
  match block with
  | Cmarkit.Block.Blocks (blocks, meta) ->
    Cmarkit.Block.Blocks (rewrite_block_list ~source blocks, meta)
  | Cmarkit.Block.Block_quote (bq, meta) ->
    let inner = Cmarkit.Block.Block_quote.block bq in
    let inner' = rewrite_within_block ~source inner in
    Cmarkit.Block.Block_quote (Cmarkit.Block.Block_quote.make inner', meta)
  | Cmarkit.Block.List _ ->
    (* Delegate to rewrite_block_list so that process_list_items handles
       keyed list items (Rule 1) before we recurse into item blocks. *)
    (match rewrite_block_list ~source [ block ] with
     | [ single ] -> single
     | multiple -> Cmarkit.Block.Blocks (multiple, Cmarkit.Meta.none))
  | Div.Ext_div (div, body) ->
    let body' = rewrite_within_block ~source body in
    Div.Ext_div (div, body')
  | Ext_keyed_list_item (t, body) ->
    let body' = rewrite_within_block ~source body in
    Ext_keyed_list_item (t, body')
  | Ext_keyed_block (t, body) ->
    let body' = rewrite_within_block ~source body in
    Ext_keyed_block (t, body')
  | _ -> block
;;

(** Entry point: rewrite a document. *)
let rewrite_doc ~(source : string option) (doc : Cmarkit.Doc.t) : Cmarkit.Doc.t =
  let block = Cmarkit.Doc.block doc in
  let block' = rewrite_within_block ~source block in
  if phys_equal block block' then doc else Cmarkit.Doc.make block'
;;

module For_test = struct
  let example_rule1_indented =
    {|- foo:
  - bar
  - baz|}
  ;;

  let example_rule2_blank_after =
    {|- foo:

bar|}
  ;;

  let example_rule3_contiguous_after_list =
    {|- foo:
```
bar
```|}
  ;;

  let example_rule4_keyed_paragraph =
    {|foo:
- bar
- baz

bee|}
  ;;

  let example_rule5_multiple_children =
    {|foo:
- bar
- baz
some text|}
  ;;

  let example_rule6_nesting =
    {|foo:
- bar:
  - baz
- qux|}
  ;;

  let example_colon_chain =
    {|- foo: bar:
  - baz|}
  ;;

  let non_example_no_colon =
    {|- foo
- bar|}
  ;;

  let non_example_colon_in_code =
    {|text with `code:`
following paragraph|}
  ;;

  let all_examples =
    [ example_rule1_indented
    ; example_rule2_blank_after
    ; example_rule3_contiguous_after_list
    ; example_rule4_keyed_paragraph
    ; example_rule5_multiple_children
    ; example_rule6_nesting
    ; example_colon_chain
    ; non_example_no_colon
    ; non_example_colon_in_code
    ]
  ;;
end
