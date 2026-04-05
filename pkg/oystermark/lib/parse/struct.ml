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

(* Colon detection
   ================

   Walk the inline tree to find the rightmost [Text] node and check whether
   it ends with an unescaped [:].  Escaped-colon detection uses source byte
   positions when [~source] is provided. *)

include (
struct
  (** Flatten an inline tree to plain text.  Simplified version that skips
      wikilinks (returns [""] for unknown extensions). *)
  let rec inline_to_text (inline : Cmarkit.Inline.t) : string =
    match inline with
    | Cmarkit.Inline.Text (s, _) -> s
    | Cmarkit.Inline.Inlines (is, _) -> String.concat (List.map is ~f:inline_to_text)
    | Cmarkit.Inline.Emphasis (e, _) | Cmarkit.Inline.Strong_emphasis (e, _) ->
      inline_to_text (Cmarkit.Inline.Emphasis.inline e)
    | Cmarkit.Inline.Break (_, _) -> " "
    | _ -> ""
  ;;

  (** [true] when the byte at [colon_byte] in [source] is preceded by ['\\'].
      Returns [false] when [source] is [None] or the position is out of range. *)
  let is_escaped_in_source ~(source : string option) (colon_byte : int) : bool =
    match source with
    | None -> false
    | Some src ->
      colon_byte > 0
      && colon_byte < String.length src
      && Char.equal src.[colon_byte - 1] '\\'
  ;;

  (** Strip a trailing [:] from a raw text string, consulting [source] byte
      positions for escape detection.  Returns [Some stripped] or [None]. *)
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
          first + String.length s' - 1)
      in
      if is_escaped_in_source ~source colon_source_byte
      then None
      else Some (String.rstrip (String.chop_suffix_exn s' ~suffix:":")))
  ;;

  (** Walk the inline tree rightward; if the rightmost [Text] leaf ends with
      an unescaped [:], return the tree with that colon removed. *)
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
     -------------

     Split [: ] (colon-space) boundaries into label segments.
     ["foo: bar"] → [["foo"; "bar"]]. *)

  (** Split at [: ] (colon followed by space) boundaries.  Colons not
      followed by a space are kept literal (e.g. URLs). *)
  let split_colon_chain (s : string) : string list =
    let parts = String.split_on_chars s ~on:[ ':' ] in
    match parts with
    | [] | [ _ ] -> [ String.strip s ]
    | first :: rest ->
      let rec merge acc current = function
        | [] -> List.rev (String.strip current :: acc)
        | next :: rest ->
          if String.is_prefix next ~prefix:" "
          then merge (String.strip current :: acc) (String.lstrip next) rest
          else merge acc (current ^ ":" ^ next) rest
      in
      let labels = merge [] first rest in
      List.filter labels ~f:(fun s -> not (String.is_empty s))
  ;;

  (** Extract label strings from a colon-stripped inline.  Returns a
      singleton for simple labels, or multiple segments for colon chains. *)
  let labels_of_inline (inline : Cmarkit.Inline.t) : string list =
    let text = inline_to_text inline in
    let labels = split_colon_chain text in
    if List.is_empty labels
    then (
      let t = String.strip text in
      if String.is_empty t then [] else [ t ])
    else labels
  ;;

  let%test_module "split_colon_chain" =
    (module struct
      let%test_unit _ = [%test_eq: string list] (split_colon_chain "foo") [ "foo" ]

      let%test_unit _ =
        [%test_eq: string list] (split_colon_chain "foo: bar") [ "foo"; "bar" ]
      ;;

      let%test_unit _ =
        [%test_eq: string list] (split_colon_chain "a: b: c") [ "a"; "b"; "c" ]
      ;;

      let%test_unit "no split without space" =
        [%test_eq: string list] (split_colon_chain "foo:bar") [ "foo:bar" ]
      ;;

      let%test_unit "url-like" =
        [%test_eq: string list] (split_colon_chain "http://x.com") [ "http://x.com" ]
      ;;

      let%test_unit _ = [%test_eq: string list] (split_colon_chain "") [ "" ]
    end)
  ;;

  let%test_module "strip_trailing_colon" =
    (module struct
      open Cmarkit

      let text s = Inline.Text (s, Meta.none)

      let check s =
        strip_trailing_colon ~source:None (text s)
        |> Option.map ~f:(fun i ->
          match i with
          | Inline.Text (s, _) -> s
          | _ -> "<non-text>")
      ;;

      let%test_unit _ = [%test_eq: string option] (check "foo:") (Some "foo")
      let%test_unit _ = [%test_eq: string option] (check "foo") None
      let%test_unit _ = [%test_eq: string option] (check "foo: ") (Some "foo")
      let%test_unit _ = [%test_eq: string option] (check ":") (Some "")
      let%test_unit _ = [%test_eq: string option] (check "") None

      let%test "inlines" =
        let inline = Inline.Inlines ([ text "hello "; text "world:" ], Meta.none) in
        Option.is_some (strip_trailing_colon ~source:None inline)
      ;;

      let%test "code span is not text" =
        let cs = Inline.Code_span (Inline.Code_span.of_string "foo:", Meta.none) in
        Option.is_none (strip_trailing_colon ~source:None cs)
      ;;
    end)
  ;;
end :
sig
  val strip_trailing_colon
    :  source:string option
    -> Cmarkit.Inline.t
    -> Cmarkit.Inline.t option

  val labels_of_inline : Cmarkit.Inline.t -> string list
end)

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

(** Split a non-empty list into [(all_but_last, last)].
    Single-pass, O(n). *)
let split_last (l : 'a list) : ('a list * 'a) option =
  match List.rev l with
  | [] -> None
  | last :: rev_prefix -> Some (List.rev rev_prefix, last)
;;

include (
struct
  (** Decompose a list item into its leading paragraph and any indented
      sub-blocks.  Returns [None] for items without a leading paragraph
      (e.g. a bare code block inside a list item). *)
  let list_item_paragraph (item : Cmarkit.Block.List_item.t)
    : (Cmarkit.Block.Paragraph.t * Cmarkit.Meta.t * Cmarkit.Block.t list) option
    =
    match Cmarkit.Block.List_item.block item with
    | Cmarkit.Block.Paragraph (p, meta) -> Some (p, meta, [])
    | Cmarkit.Block.Blocks (Cmarkit.Block.Paragraph (p, meta) :: rest, _) ->
      Some (p, meta, rest)
    | _ -> None
  ;;

  (** Reconstruct a [List_item] with new block content, preserving the
      original marker layout (indentation, bullet character). *)
  let rebuild_item (item : Cmarkit.Block.List_item.t) (block : Cmarkit.Block.t)
    : Cmarkit.Block.List_item.t
    =
    Cmarkit.Block.List_item.make
      ~before_marker:(Cmarkit.Block.List_item.before_marker item)
      ~marker:(Cmarkit.Block.List_item.marker item)
      ~after_marker:(Cmarkit.Block.List_item.after_marker item)
      block
  ;;

  (** Walk list items and tag keyed ones (Rule 1).  Returns rebuilt items
      and, if the last item is keyed with no sub-blocks, its label info
      for Rule 3 handling by the caller. *)
  let process_list_items
        ~(source : string option)
        (items : Cmarkit.Block.List_item.t Cmarkit.node list)
    : Cmarkit.Block.List_item.t Cmarkit.node list
      * (Cmarkit.Inline.t * string list) option
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
             (* Rule 1: already has indented children *)
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
             last_keyed := Some (label_inline, labels);
             rebuilt_items := (item, item_meta) :: !rebuilt_items)
           else rebuilt_items := (item, item_meta) :: !rebuilt_items)
      | None -> rebuilt_items := (item, item_meta) :: !rebuilt_items);
    List.rev !rebuilt_items, !last_keyed
  ;;

  (** Rebuild a [List] block preserving tightness and list type. *)
  let make_list
        (l : Cmarkit.Block.List'.t)
        (list_meta : Cmarkit.Meta.t)
        (items : Cmarkit.Block.List_item.t Cmarkit.node list)
    : Cmarkit.Block.t
    =
    Cmarkit.Block.List
      ( Cmarkit.Block.List'.make
          ~tight:(Cmarkit.Block.List'.tight l)
          (Cmarkit.Block.List'.type' l)
          items
      , list_meta )
  ;;

  (** Recursively rewrite the block content of each list item. *)
  let rec recurse_items ~source items =
    List.map items ~f:(fun (item, item_meta) ->
      let block = Cmarkit.Block.List_item.block item in
      let block' = rewrite_within_block ~source block in
      if phys_equal block block'
      then item, item_meta
      else rebuild_item item block', item_meta)

  (** Rewrite a flat list of sibling blocks left-to-right, consuming
      contiguous followers when a keyed paragraph or list is found. *)
  and rewrite_block_list ~(source : string option) (blocks : Cmarkit.Block.t list)
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
        let rebuilt_items, last_keyed =
          process_list_items ~source (Cmarkit.Block.List'.items l)
        in
        (match last_keyed with
         | Some (_label_inline, labels) ->
           incr i;
           if !i >= len || is_blank_line arr.(!i)
           then
             (* Rule 2: blank or end follows — no reparenting *)
             result
             := make_list l list_meta (recurse_items ~source rebuilt_items) :: !result
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
             match split_last rebuilt_items with
             | Some (prev_items, (last_item, last_item_meta)) ->
               let new_items =
                 prev_items @ [ rebuild_item last_item keyed, last_item_meta ]
               in
               result
               := make_list l list_meta (recurse_items ~source new_items) :: !result
             | None ->
               (* Should not happen: last_keyed implies non-empty list *)
               result
               := make_list l list_meta (recurse_items ~source rebuilt_items) :: !result)
         | None ->
           result
           := make_list l list_meta (recurse_items ~source rebuilt_items) :: !result;
           incr i)
      | _ ->
        result := rewrite_within_block ~source block :: !result;
        incr i
    done;
    List.rev !result

  (** Recurse into container blocks ([Blocks], [Block_quote], [List],
      [Ext_div], and our own keyed nodes).  Lists are delegated to
      {!rewrite_block_list} so that [process_list_items] runs first. *)
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
end :
sig
  val rewrite_block_list
    :  source:string option
    -> Cmarkit.Block.t list
    -> Cmarkit.Block.t list

  val rewrite_within_block : source:string option -> Cmarkit.Block.t -> Cmarkit.Block.t
end)

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
