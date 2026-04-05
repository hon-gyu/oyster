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

(* Specification
   ==============

   [Spec] codifies the struct rules as:
   {ul
   {- boolean {e universal predicates} that hold for every [Cmarkit.Doc.t]
      produced by {!rewrite_doc};}
   {- named {e example} markdown strings, one per rule in
      [specification/oyster/struct.md];}
   {- a line-based [gen_markdown] generator for property-based testing.}}

   The predicates are driven by two witness streams in [parse.ml]:
   the named examples (hand-picked, act as regression tests) and
   generator-driven strings via [Core.Quickcheck].  The expected
   rewritten tree for each example is pinned in the expect-tests in
   [parse.ml] via [pp_doc], so we don't encode expected output twice. *)

module Spec = struct
  (** Visit every block reachable through container blocks, including
      keyed nodes and [Div.Ext_div]. *)
  let rec iter_blocks (b : Cmarkit.Block.t) ~(f : Cmarkit.Block.t -> unit) : unit =
    f b;
    match b with
    | Ext_keyed_block (_, body) | Ext_keyed_list_item (_, body) -> iter_blocks body ~f
    | Cmarkit.Block.Blocks (bs, _) -> List.iter bs ~f:(iter_blocks ~f)
    | Cmarkit.Block.List (l, _) ->
      List.iter (Cmarkit.Block.List'.items l) ~f:(fun (item, _) ->
        iter_blocks (Cmarkit.Block.List_item.block item) ~f)
    | Cmarkit.Block.Block_quote (bq, _) ->
      iter_blocks (Cmarkit.Block.Block_quote.block bq) ~f
    | Div.Ext_div (_, body) -> iter_blocks body ~f
    | _ -> ()
  ;;

  let iter_doc (d : Cmarkit.Doc.t) ~f = iter_blocks (Cmarkit.Doc.block d) ~f

  (* Universal predicates
     -------------------- *)

  (** Every keyed node's body is non-empty.  Rule 2: if the next
      element is blank, no keying happens — so an empty body would
      indicate a bug. *)
  let keyed_bodies_non_empty (doc : Cmarkit.Doc.t) : bool =
    let ok = ref true in
    iter_doc doc ~f:(fun b ->
      match b with
      | Ext_keyed_block (_, Cmarkit.Block.Blocks ([], _))
      | Ext_keyed_list_item (_, Cmarkit.Block.Blocks ([], _)) -> ok := false
      | _ -> ());
    !ok
  ;;

  (** Every keyed label is a plain [Inline.Text].  The rewriter
      reconstructs labels via {!build_nested_keyed} as [Text], and the
      spec forbids hard breaks in labels. *)
  let labels_are_plain_text (doc : Cmarkit.Doc.t) : bool =
    let ok = ref true in
    iter_doc doc ~f:(fun b ->
      let check (label : Cmarkit.Inline.t) =
        match label with
        | Cmarkit.Inline.Text _ -> ()
        | _ -> ok := false
      in
      match b with
      | Ext_keyed_block ({ label }, _) | Ext_keyed_list_item ({ label }, _) -> check label
      | _ -> ());
    !ok
  ;;

  (** Does a list item's leading paragraph end with an unescaped
      trailing colon, with no indented sub-blocks?  Such an item could
      still be keyed under Rule 3 if not followed by a blank line. *)
  let list_last_item_is_bare_keyed ~(source : string option) (l : Cmarkit.Block.List'.t)
    : bool
    =
    match List.last (Cmarkit.Block.List'.items l) with
    | None -> false
    | Some (item, _) ->
      let block = Cmarkit.Block.List_item.block item in
      let leading_para =
        match block with
        | Cmarkit.Block.Paragraph (p, _) -> Some p
        | _ -> None
        (* With sub-blocks the item would be [Blocks (Paragraph :: rest)];
           that path is Rule 1 and would have become [Ext_keyed_list_item]. *)
      in
      (match leading_para with
       | None -> false
       | Some p ->
         let inline = Cmarkit.Block.Paragraph.inline p in
         Option.is_some (strip_trailing_colon ~source inline))
  ;;

  (** The rewriter's maximality guarantee: no sibling-level keyed
      paragraph or keyed-last-item list is immediately followed by a
      non-blank block.  If this fires, the rewriter missed an
      absorption (Rule 3, 4, or 5).  Needs [~source] for escaped-colon
      detection. *)
  let keying_is_maximal ~(source : string option) (doc : Cmarkit.Doc.t) : bool =
    let ok = ref true in
    let check_siblings (bs : Cmarkit.Block.t list) =
      let arr = Array.of_list bs in
      let len = Array.length arr in
      for i = 0 to len - 1 do
        let absorbable =
          match arr.(i) with
          | Cmarkit.Block.Paragraph (p, _) ->
            Option.is_some
              (strip_trailing_colon ~source (Cmarkit.Block.Paragraph.inline p))
          | Cmarkit.Block.List (l, _) -> list_last_item_is_bare_keyed ~source l
          | _ -> false
        in
        if absorbable && i + 1 < len && not (is_blank_line arr.(i + 1))
        then ok := false
      done
    in
    let rec walk (b : Cmarkit.Block.t) =
      match b with
      | Cmarkit.Block.Blocks (bs, _) ->
        check_siblings bs;
        List.iter bs ~f:walk
      | Cmarkit.Block.List (l, _) ->
        List.iter (Cmarkit.Block.List'.items l) ~f:(fun (item, _) ->
          walk (Cmarkit.Block.List_item.block item))
      | Cmarkit.Block.Block_quote (bq, _) -> walk (Cmarkit.Block.Block_quote.block bq)
      | Ext_keyed_block (_, body) | Ext_keyed_list_item (_, body) -> walk body
      | Div.Ext_div (_, body) -> walk body
      | _ -> ()
    in
    walk (Cmarkit.Doc.block doc);
    !ok
  ;;

  (* Examples
     --------

     One named string per example in [specification/oyster/struct.md].
     The variable name encodes the rule number and a short description;
     expected rewritten trees are pinned in [parse.ml] expect-tests. *)

  let rule1_keyed_list_item_with_indented_content =
    {|- foo:
  - bar
  - baz|}
  ;;

  let rule2_keyed_list_item_followed_by_blank_line =
    {|- foo:

bar|}
  ;;

  let rule3_keyed_list_item_with_contiguous_blocks =
    {|- foo:
```
bar
```|}
  ;;

  let rule4_keyed_paragraph =
    {|foo:
- bar
- baz

bee|}
  ;;

  let rule5_keyed_paragraph_multiple_children =
    {|foo:
- bar
- baz
some text|}
  ;;

  let rule6_nesting =
    {|foo:
- bar:
  - baz
- qux|}
  ;;

  let colon_chain_inline_keying =
    {|- foo: bar:
  - baz|}
  ;;

  let non_example_no_colon =
    {|- foo
- bar|}
  ;;

  let non_example_colon_in_code_span =
    {|text with `code:`
following paragraph|}
  ;;

  (** All examples, used as [~examples:] seed for
      [Core.Quickcheck.test] and for commonmark-roundtrip checking in
      [parse.ml]. *)
  let all_examples =
    [ rule1_keyed_list_item_with_indented_content
    ; rule2_keyed_list_item_followed_by_blank_line
    ; rule3_keyed_list_item_with_contiguous_blocks
    ; rule4_keyed_paragraph
    ; rule5_keyed_paragraph_multiple_children
    ; rule6_nesting
    ; colon_chain_inline_keying
    ; non_example_no_colon
    ; non_example_colon_in_code_span
    ]
  ;;

  (* Generator for PBT
     -----------------

     A tiny line-based generator that samples from a vocabulary of
     markdown lines likely to exercise keying, nesting, blank lines,
     and escape handling.  The output is not always "valid" structurally
     — that is the point: the rewriter must preserve invariants on
     arbitrary inputs, not just well-formed ones. *)

  let line_vocabulary =
    [| "- foo:"
     ; "- foo\\: bar:"
     ; "- foo: bar:"
     ; "- bar"
     ; "  - nested"
     ; "foo:"
     ; "plain paragraph"
     ; ""
     ; "```"
     ; "code line"
     ; "> quoted"
    |]
  ;;

  let gen_markdown : string Core.Quickcheck.Generator.t =
    let open Core.Quickcheck.Generator in
    let open Core.Quickcheck.Generator.Let_syntax in
    let%bind n = Core.Int.gen_incl 1 8 in
    let%map lines =
      list_with_length
        n
        (Core.Int.gen_incl 0 (Array.length line_vocabulary - 1)
         >>| fun i -> line_vocabulary.(i))
    in
    String.concat ~sep:"\n" lines
  ;;
end
