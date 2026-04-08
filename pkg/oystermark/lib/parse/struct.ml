(** Struct: colon-keyed tree restructuring.

    See [struct.mli] for the rule specification.  Rules below are
    numbered per [specification/oyster/struct.md].

    {1 Parsing}

    Parsing is a single-pass rewrite on the already-parsed Cmarkit AST:
    {ol
    {- Walk sibling block lists left-to-right.}
    {- When a keyed paragraph is found, collect contiguous following
       blocks and wrap as {!Ext_keyed_block}.}
    {- When a list's last item is keyed and followed by contiguous
       blocks, reparent those blocks under the item as
       {!Ext_keyed_list_item}.}
    {- Recurse into container blocks.}}
*)

open Core

type t = { label : Cmarkit.Inline.t }

type Cmarkit.Block.t +=
  | Ext_keyed_list_item of t * Cmarkit.Block.t
  | Ext_keyed_block of t * Cmarkit.Block.t

(* Colon detection
   =============== *)

module Colon : sig
  val strip_trailing_colon
    :  source:string option
    -> Cmarkit.Inline.t
    -> Cmarkit.Inline.t option

  val labels_of_inline : Cmarkit.Inline.t -> string list
end = struct
  (** Flatten an inline tree to plain text.  Returns [""] for unknown
      extensions (e.g. wikilinks). *)
  let rec inline_to_text (inline : Cmarkit.Inline.t) : string =
    match inline with
    | Cmarkit.Inline.Text (s, _) -> s
    | Cmarkit.Inline.Inlines (is, _) -> String.concat (List.map is ~f:inline_to_text)
    | Cmarkit.Inline.Emphasis (e, _) | Cmarkit.Inline.Strong_emphasis (e, _) ->
      inline_to_text (Cmarkit.Inline.Emphasis.inline e)
    | Cmarkit.Inline.Break (_, _) -> " "
    | _ -> ""
  ;;

  (** [true] when the byte at [colon_byte] in [source] is preceded by
      ['\\'].  Returns [false] when [source] is [None] or the position
      is out of range. *)
  let is_escaped_in_source ~(source : string option) (colon_byte : int) : bool =
    match source with
    | None -> false
    | Some src ->
      colon_byte > 0
      && colon_byte < String.length src
      && Char.equal src.[colon_byte - 1] '\\'
  ;;

  (** Strip a trailing [:] from a raw text string, consulting [source]
      byte positions for escape detection. *)
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

  (** Walk the inline tree rightward; if the rightmost [Text] leaf ends
      with an unescaped [:], return the tree with that colon removed. *)
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
     ------------

     Split [: ] (colon-space) boundaries into label segments.
     ["foo: bar"] -> [["foo"; "bar"]]. *)

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
      merge [] first rest |> List.filter ~f:(fun s -> not (String.is_empty s))
  ;;

  let labels_of_inline (inline : Cmarkit.Inline.t) : string list =
    split_colon_chain (inline_to_text inline)
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
end

(* Shared helpers
   ============== *)

let is_blank_line : Cmarkit.Block.t -> bool = function
  | Cmarkit.Block.Blank_line _ -> true
  | _ -> false
;;

(** Split [bs] at the first blank line.  Returns [(prefix, rest)] where
    [prefix] is the maximal contiguous non-blank head and [rest] is
    everything from the first blank line onward (or [[]] if none). *)
let span_non_blank (bs : Cmarkit.Block.t list)
  : Cmarkit.Block.t list * Cmarkit.Block.t list
  =
  let rec go acc = function
    | [] -> List.rev acc, []
    | b :: _ as rest when is_blank_line b -> List.rev acc, rest
    | b :: rest -> go (b :: acc) rest
  in
  go [] bs
;;

(** Wrap a list of blocks into a single block. *)
let wrap_blocks : Cmarkit.Block.t list -> Cmarkit.Block.t = function
  | [] -> Cmarkit.Block.Blocks ([], Cmarkit.Meta.none)
  | [ single ] -> single
  | multiple -> Cmarkit.Block.Blocks (multiple, Cmarkit.Meta.none)
;;

(** Build nested keyed nodes from a list of labels (outermost-first) and
    a body block.  Returns [body] unchanged when [labels] is empty. *)
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

let mk_keyed_block t b = Ext_keyed_block (t, b)
let mk_keyed_item t b = Ext_keyed_list_item (t, b)

(* Tree rewrite
   ============

   Single-pass left-to-right traversal of sibling block lists. *)

module Rewrite : sig
  val rewrite_within_block : source:string option -> Cmarkit.Block.t -> Cmarkit.Block.t
end = struct
  (** Decompose a list item into its leading paragraph and any indented
      sub-blocks.  Returns [None] for items without a leading paragraph
      (e.g. a bare code block inside a list item). *)
  let list_item_paragraph (item : Cmarkit.Block.List_item.t)
    : (Cmarkit.Block.Paragraph.t * Cmarkit.Block.t list) option
    =
    match Cmarkit.Block.List_item.block item with
    | Cmarkit.Block.Paragraph (p, _) -> Some (p, [])
    | Cmarkit.Block.Blocks (Cmarkit.Block.Paragraph (p, _) :: rest, _) -> Some (p, rest)
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

  (** Replace the last element of a non-empty list. *)
  let replace_last items new_last =
    match List.rev items with
    | [] -> assert false
    | _ :: rev_prefix -> List.rev_append rev_prefix [ new_last ]
  ;;

  (* List-item tagging (Rule 1)
     --------------------------

     [tag_keyed_items] walks list items, applying Rule 1 (keyed item
     with indented children) to every item.  For the {b last} item,
     if it is a bare-keyed paragraph (trailing colon, no sub-blocks),
     the label segments are returned so that the caller can decide
     whether Rule 3 applies. *)

  let tag_middle_item ~source item =
    match list_item_paragraph item with
    | Some (p, (_ :: _ as sub_blocks)) ->
      (match Colon.strip_trailing_colon ~source (Cmarkit.Block.Paragraph.inline p) with
       | None -> item
       | Some label_inline ->
         let labels = Colon.labels_of_inline label_inline in
         let body = wrap_blocks sub_blocks in
         let keyed = build_nested_keyed ~make_node:mk_keyed_item labels body in
         rebuild_item item keyed)
    | _ -> item
  ;;

  let tag_last_item ~source item : Cmarkit.Block.List_item.t * string list option =
    match list_item_paragraph item with
    | None -> item, None
    | Some (p, sub_blocks) ->
      (match Colon.strip_trailing_colon ~source (Cmarkit.Block.Paragraph.inline p) with
       | None -> item, None
       | Some label_inline ->
         let labels = Colon.labels_of_inline label_inline in
         if not (List.is_empty sub_blocks)
         then (
           (* Rule 1 *)
           let body = wrap_blocks sub_blocks in
           let keyed = build_nested_keyed ~make_node:mk_keyed_item labels body in
           rebuild_item item keyed, None)
         else (* Defer to caller: Rule 2 or Rule 3 *)
           item, Some labels)
  ;;

  let rec tag_keyed_items
            ~(source : string option)
            (items : Cmarkit.Block.List_item.t Cmarkit.node list)
    : Cmarkit.Block.List_item.t Cmarkit.node list * string list option
    =
    match items with
    | [] -> [], None
    | [ (item, meta) ] ->
      let item', bare = tag_last_item ~source item in
      [ item', meta ], bare
    | (item, meta) :: rest ->
      let item' = tag_middle_item ~source item in
      let rest', bare = tag_keyed_items ~source rest in
      (item', meta) :: rest', bare
  ;;

  (* Sibling-block rewrite
     ---------------------

     Recursive descent on a flat list of sibling blocks.  When a keyed
     paragraph or keyed-last-item list is encountered, contiguous
     non-blank followers are consumed and reparented. *)

  let rec rewrite_block_list ~(source : string option) (blocks : Cmarkit.Block.t list)
    : Cmarkit.Block.t list
    =
    match blocks with
    | [] -> []
    | (Cmarkit.Block.Paragraph (p, _) as block) :: rest ->
      (match Colon.strip_trailing_colon ~source (Cmarkit.Block.Paragraph.inline p) with
       | None -> rewrite_within_block ~source block :: rewrite_block_list ~source rest
       | Some label_inline -> absorb_paragraph ~source ~original:block ~label_inline rest)
    | Cmarkit.Block.List (l, list_meta) :: rest -> handle_list ~source l list_meta rest
    | block :: rest ->
      rewrite_within_block ~source block :: rewrite_block_list ~source rest

  (** Rule 4/5: a keyed paragraph absorbs contiguous following blocks. *)
  and absorb_paragraph ~source ~original ~label_inline rest =
    let children, after = span_non_blank rest in
    if List.is_empty children
    then original :: rewrite_block_list ~source rest
    else (
      let children = rewrite_block_list ~source children in
      let body = wrap_blocks children in
      let labels = Colon.labels_of_inline label_inline in
      let keyed = build_nested_keyed ~make_node:mk_keyed_block labels body in
      keyed :: rewrite_block_list ~source after)

  (** Rules 1/2/3 in one place: tag items for Rule 1, then decide Rule 3
      absorption from the sibling context. *)
  and handle_list ~source l list_meta rest =
    let tagged, bare_last = tag_keyed_items ~source (Cmarkit.Block.List'.items l) in
    let items, rest =
      match bare_last, rest with
      | Some labels, (next :: _ as rest) when not (is_blank_line next) ->
        (* Rule 3: reparent contiguous following blocks under the last
           item.  [span_non_blank] is guaranteed non-empty here since
           [next] is non-blank. *)
        let following, after = span_non_blank rest in
        let body = wrap_blocks (rewrite_block_list ~source following) in
        let keyed = build_nested_keyed ~make_node:mk_keyed_item labels body in
        let last_item, last_meta =
          match List.last tagged with
          | Some x -> x
          | None -> assert false
        in
        let new_last = rebuild_item last_item keyed, last_meta in
        replace_last tagged new_last, after
      | _ ->
        (* Rule 2 (blank/end follows) or no bare-keyed last item. *)
        tagged, rest
    in
    let items = recurse_items ~source items in
    make_list l list_meta items :: rewrite_block_list ~source rest

  and recurse_items ~source items =
    List.map items ~f:(fun (item, item_meta) ->
      let block = Cmarkit.Block.List_item.block item in
      let block' = rewrite_within_block ~source block in
      if phys_equal block block'
      then item, item_meta
      else rebuild_item item block', item_meta)

  (** Recurse into container blocks ([Blocks], [Block_quote], [List],
      [Ext_div], and keyed nodes).  Lists are delegated to
      {!handle_list} so that {!tag_keyed_items} runs first. *)
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
    | Cmarkit.Block.List (l, list_meta) ->
      (* With no siblings to absorb, [handle_list] returns a single
         block; take it unwrapped. *)
      (match handle_list ~source l list_meta [] with
       | [ single ] -> single
       | multiple -> Cmarkit.Block.Blocks (multiple, Cmarkit.Meta.none))
    | Div.Ext_div (div, body) ->
      let body' = rewrite_within_block ~source body in
      Div.Ext_div (div, body')
    | Ext_keyed_list_item (t, body) ->
      Ext_keyed_list_item (t, rewrite_within_block ~source body)
    | Ext_keyed_block (t, body) -> Ext_keyed_block (t, rewrite_within_block ~source body)
    | _ -> block
  ;;
end

let rewrite_doc ~(source : string option) (doc : Cmarkit.Doc.t) : Cmarkit.Doc.t =
  let block = Cmarkit.Doc.block doc in
  let block' = Rewrite.rewrite_within_block ~source block in
  if phys_equal block block' then doc else Cmarkit.Doc.make block'
;;

(* Specification
   =============

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
  (* Traversal helpers
     ----------------- *)

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

  (** Visit every sibling-block list reachable through container blocks.
      Used by {!keying_is_maximal} to inspect neighbouring blocks, which
      {!iter_blocks} flattens away. *)
  let rec iter_sibling_lists (b : Cmarkit.Block.t) ~(f : Cmarkit.Block.t list -> unit)
    : unit
    =
    match b with
    | Cmarkit.Block.Blocks (bs, _) ->
      f bs;
      List.iter bs ~f:(iter_sibling_lists ~f)
    | Cmarkit.Block.List (l, _) ->
      List.iter (Cmarkit.Block.List'.items l) ~f:(fun (item, _) ->
        iter_sibling_lists (Cmarkit.Block.List_item.block item) ~f)
    | Cmarkit.Block.Block_quote (bq, _) ->
      iter_sibling_lists (Cmarkit.Block.Block_quote.block bq) ~f
    | Ext_keyed_block (_, body) | Ext_keyed_list_item (_, body) ->
      iter_sibling_lists body ~f
    | Div.Ext_div (_, body) -> iter_sibling_lists body ~f
    | _ -> ()
  ;;

  (* Universal predicates
     -------------------- *)

  (** Every keyed node's body is non-empty.  Rule 2: if the next element
      is blank, no keying happens — so an empty body would indicate a
      bug. *)
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
      (* With sub-blocks the item would be [Blocks (Paragraph :: rest)];
         that path is Rule 1 and would have become [Ext_keyed_list_item]. *)
      (match Cmarkit.Block.List_item.block item with
       | Cmarkit.Block.Paragraph (p, _) ->
         Option.is_some
           (Colon.strip_trailing_colon ~source (Cmarkit.Block.Paragraph.inline p))
       | _ -> false)
  ;;

  (** The rewriter's maximality guarantee: no sibling-level keyed
      paragraph or keyed-last-item list is immediately followed by a
      non-blank block.  If this fires, the rewriter missed an
      absorption (Rule 3, 4, or 5).  Needs [~source] for escaped-colon
      detection. *)
  let keying_is_maximal ~(source : string option) (doc : Cmarkit.Doc.t) : bool =
    let ok = ref true in
    iter_sibling_lists (Cmarkit.Doc.block doc) ~f:(fun bs ->
      let arr = Array.of_list bs in
      let len = Array.length arr in
      for i = 0 to len - 1 do
        let absorbable =
          match arr.(i) with
          | Cmarkit.Block.Paragraph (p, _) ->
            Option.is_some
              (Colon.strip_trailing_colon ~source (Cmarkit.Block.Paragraph.inline p))
          | Cmarkit.Block.List (l, _) -> list_last_item_is_bare_keyed ~source l
          | _ -> false
        in
        if absorbable && i + 1 < len && not (is_blank_line arr.(i + 1)) then ok := false
      done);
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

  (** All examples, used as [~examples:] seed for [Core.Quickcheck.test]
      and for commonmark-roundtrip checking in [parse.ml]. *)
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
