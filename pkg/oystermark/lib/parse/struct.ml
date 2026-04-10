(** {0 Struct: colon-keyed tree restructuring}

    {1 Specification}

    A {b keyed node} is a list item or paragraph whose inline content
    ends with an unescaped [:].

    {2 Label detection}

    A trailing colon is detected by walking the inline tree rightward:
    follow the last child of each [Inlines] container until a [Text]
    leaf is reached.  If that leaf's content (after stripping
    whitespace) ends with [:], the node is keyed.

    Only [Text] and [Inlines] nodes are traversed.  Emphasis, code
    spans, links, images, raw HTML, breaks, and extension inlines are
    {b opaque} — a colon inside them does not make the node keyed.

    After the trailing colon is stripped, the resulting inline is
    decomposed into label segments (see {b Colon chains} below).
    Each segment must be a {b single inline unit}: pure text,
    emphasis, strong emphasis, or code span.  Mixed content — e.g.
    emphasis followed by text in the same segment — is rejected, and
    the node is not keyed.

    {ul
    {- [foo bar:] → keyed, label [Text "foo bar"].}
    {- [*foo*:] → keyed, label [Emphasis "foo"].
       The colon follows the emphasis in a trailing [Text ":"] node.}
    {- [**foo**:] → keyed, label [Strong_emphasis "foo"].}
    {- [*foo* bar:] → {b not keyed}.  After stripping the colon the
       segment is [Emphasis "foo"; Text " bar"] — mixed content.}
    {- [*foo:*] → {b not keyed}.  Emphasis is opaque; the colon
       inside is not examined.}
    {- [`code:`] → {b not keyed}.  Code spans are opaque.}}

    {2 Escaped colons}

    A backslash immediately before the trailing colon in the {e parsed}
    inline text (i.e. [Text] node content) suppresses keying.

    Because CommonMark already consumes one level of backslash
    escaping ([\\:] in source becomes [\:] in the AST), write [\\\\:]
    in source to get [\\:] in the AST — which struct treats as a
    literal colon.  In practice, most renderers (including Cmarkit)
    will display the remaining backslash, so this is a deliberate
    opt-out that is visible in the output.

    More precisely, the number of consecutive backslashes immediately
    before the colon is counted.  An odd count means the colon is
    escaped; an even count means the backslashes pair up and the colon
    is structural.

    {2 Colon chains}

    When the inline content (after trailing colon removal) contains
    [: ] (colon followed by space) boundaries inside [Text] nodes,
    it is split into segments.  Each segment must be a single inline
    unit (see {b Label detection}).

    {ul
    {- [- foo: bar:] with body [baz] →
       [Keyed_list_item "foo" (Keyed_list_item "bar" (... baz ...))].
       Two text segments, two nesting levels.}
    {- [- a: b: c:] with body [x] →
       three nesting levels.}
    {- [- *foo*: bar:] with body [baz] →
       [Keyed_list_item (Emphasis "foo") (Keyed_list_item "bar" (...))].
       Chain splitting works across inline types.}
    {- [- http://example.com:] → single label ["http://example.com"].
       The [:] after [http] has no trailing space, so no split.}
    {- [- *foo* bar:] → {b not keyed}.  The single segment contains
       emphasis + text — mixed content is rejected.}}

    {2 Tree restructuring rules}

    {ol
    {- {b Keyed list item with indented content.}  The indented sub-blocks
       are already children in the CommonMark AST; they become the body of
       an {!Ext_keyed_list_item}.}
    {- {b Keyed list item followed by a blank line.}  No transformation —
       the trailing colon is treated as literal punctuation.}
    {- {b Keyed list item followed by contiguous blocks.}  Unindented
       blocks immediately after the list are reparented under the last
       item as an {!Ext_keyed_list_item}.}
    {- {b Keyed paragraph.}  A paragraph ending with [:] claims all
       immediately following contiguous blocks (no blank-line separation)
       as children, producing an {!Ext_keyed_block}.}
    {- Same as rule 4 but with multiple child block types.}
    {- {b Nesting.}  Keyed nodes nest: a keyed paragraph can contain a
       list whose items are themselves keyed.}}

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
open Cmarkit

type t = { label : Inline.t }
type Block.t += Ext_keyed_list_item of t * Block.t | Ext_keyed_block of t * Block.t

let block_commonmark_renderer : Cmarkit_renderer.block =
  let open Cmarkit_renderer in
  fun (c : context) (b : Block.t) ->
    match b with
    | Ext_keyed_block ({ label }, body) ->
      Context.inline c label;
      Context.string c ":\n";
      Context.block c body;
      true
    | Ext_keyed_list_item ({ label }, body) ->
      Context.string c "- ";
      Context.inline c label;
      Context.string c ":\n";
      Context.block c body;
      true
    | _ -> false
;;

let sexp_of_block : Common.block_sexp =
  fun ~recurse_inline ~recurse_block ~with_meta:_ b ->
  match b with
  | Ext_keyed_list_item ({ label }, body) ->
    Some (Sexp.List [ Atom "Keyed_list_item"; recurse_inline label; recurse_block body ])
  | Ext_keyed_block ({ label }, body) ->
    Some (Sexp.List [ Atom "Keyed_block"; recurse_inline label; recurse_block body ])
  | _ -> None
;;

(* Colon detection
   =============== *)

module Colon : sig
  val strip_trailing_colon : Inline.t -> Inline.t option
  val labels_of_inline : Inline.t -> Inline.t list
end = struct
  (** Count consecutive backslashes immediately before position [pos]
      in [s]. *)
  let count_preceding_backslashes (s : string) (pos : int) : int =
    let rec go i n = if i >= 0 && Char.equal s.[i] '\\' then go (i - 1) (n + 1) else n in
    go (pos - 1) 0
  ;;

  (** Strip a trailing [:] from a raw text string.  A colon preceded by
      an odd number of backslashes is escaped and not stripped. *)
  let strip_colon_from_text (s : string) : string option =
    let s' = String.rstrip s in
    let len = String.length s' in
    if len = 0 || not (Char.equal s'.[len - 1] ':')
    then None
    else if count_preceding_backslashes s' (len - 1) mod 2 = 1
    then None
    else Some (String.rstrip (String.chop_suffix_exn s' ~suffix:":"))
  ;;

  (** Walk the inline tree rightward; if the rightmost [Text] leaf ends
      with an unescaped [:], return the tree with that colon removed. *)
  let rec strip_trailing_colon (inline : Inline.t) : Inline.t option =
    match inline with
    | Inline.Text (s, meta) ->
      (match strip_colon_from_text s with
       | None -> None
       | Some stripped -> Some (Inline.Text (stripped, meta)))
    | Inline.Inlines (inlines, meta) ->
      let rec go_last rev_prefix = function
        | [] -> None
        | [ last ] ->
          (match strip_trailing_colon last with
           | Some last' ->
             Some (Inline.Inlines (List.rev_append rev_prefix [ last' ], meta))
           | None -> None)
        | x :: rest -> go_last (x :: rev_prefix) rest
      in
      go_last [] inlines
    | _ -> None
  ;;

  (* Colon chains
     ------------ *)

  (** Unwrap [Inlines] to a flat child list; other nodes become a
      singleton. *)
  let unwrap_inline (inline : Inline.t) : Inline.t list =
    match inline with
    | Inline.Inlines (is, _) -> is
    | other -> [ other ]
  ;;

  (** A segment is a valid label if it consists of a single simple
      inline unit after filtering empty text nodes. *)
  let as_simple_label (segment : Inline.t list) : Inline.t option =
    let segment =
      List.filter segment ~f:(fun i ->
        match i with
        | Inline.Text (s, _) -> not (String.is_empty (String.strip s))
        | _ -> true)
    in
    match segment with
    | [] -> None
    | [ Inline.Text (s, meta) ] -> Some (Inline.Text (String.strip s, meta))
    | [ (Inline.Emphasis _ as e) ] -> Some e
    | [ (Inline.Strong_emphasis _ as e) ] -> Some e
    | [ (Inline.Code_span _ as e) ] -> Some e
    | _ -> None
  ;;

  (** Split a flat list of inlines into segments at [: ] (colon-space)
      boundaries found inside [Text] nodes.  Non-text inlines are never
      split. *)
  let split_at_colon_space (children : Inline.t list) : Inline.t list list =
    (* [current_rev]: inlines accumulated for the segment being built (reversed).
       [segments_rev]: completed segments (reversed). *)
    let flush current_rev segments_rev = List.rev current_rev :: segments_rev in
    let rec go current_rev segments_rev = function
      | [] -> List.rev (flush current_rev segments_rev)
      | Inline.Text (s, meta) :: rest -> split_text current_rev segments_rev s meta rest
      | other :: rest -> go (other :: current_rev) segments_rev rest
    and split_text current_rev segments_rev s meta rest =
      match String.substr_index s ~pattern:": " with
      | None -> go (Inline.Text (s, meta) :: current_rev) segments_rev rest
      | Some i ->
        let before = String.prefix s i in
        let after = String.lstrip (String.drop_prefix s (i + 1)) in
        let current_rev =
          if String.is_empty (String.strip before)
          then current_rev
          else Inline.Text (String.rstrip before, meta) :: current_rev
        in
        let segments_rev = flush current_rev segments_rev in
        if String.is_empty (String.strip after)
        then go [] segments_rev rest
        else split_text [] segments_rev after meta rest
    in
    go [] [] children
  ;;

  (** Decompose a colon-stripped inline into label segments.

      Each label must be a single inline unit: [Text], [Emphasis],
      [Strong_emphasis], or [Code_span].  Mixed content (e.g.
      emphasis followed by text in the same segment) is rejected.

      Chain splitting at [: ] boundaries works across inline types:
      [*foo*: bar] becomes two labels, [Emphasis "foo"] and
      [Text "bar"].

      Returns [[]] when the inline cannot be decomposed into valid
      labels (mixed content, empty result, etc.). *)
  let labels_of_inline (inline : Inline.t) : Inline.t list =
    let children = unwrap_inline inline in
    let segments = split_at_colon_space children in
    let labels = List.filter_map segments ~f:as_simple_label in
    if List.length labels = List.length segments then labels else []
  ;;

  let%test_module "strip_trailing_colon" =
    (module struct
      let text s = Inline.Text (s, Meta.none)

      let check s =
        strip_trailing_colon (text s)
        |> Option.map ~f:(fun i ->
          match i with
          | Inline.Text (s, _) -> s
          | _ -> "<non-text>")
      ;;

      let%test_unit "basic" = [%test_eq: string option] (check "foo:") (Some "foo")
      let%test_unit "no colon" = [%test_eq: string option] (check "foo") None

      let%test_unit "trailing space" =
        [%test_eq: string option] (check "foo: ") (Some "foo")
      ;;

      let%test_unit "bare colon" = [%test_eq: string option] (check ":") (Some "")
      let%test_unit "empty" = [%test_eq: string option] (check "") None

      let%test_unit "escaped (odd backslash)" =
        [%test_eq: string option] (check "foo\\:") None
      ;;

      let%test_unit "double backslash (even) is not escaped" =
        [%test_eq: string option] (check "foo\\\\:") (Some "foo\\\\")
      ;;

      let%test_unit "triple backslash (odd) is escaped" =
        [%test_eq: string option] (check "foo\\\\\\:") None
      ;;

      let%test "inlines" =
        let inline = Inline.Inlines ([ text "hello "; text "world:" ], Meta.none) in
        Option.is_some (strip_trailing_colon inline)
      ;;

      let%test "code span is not text" =
        let cs = Inline.Code_span (Inline.Code_span.of_string "foo:", Meta.none) in
        Option.is_none (strip_trailing_colon cs)
      ;;

      let%test "emphasis is opaque" =
        let em = Inline.Emphasis (Inline.Emphasis.make (text "foo:"), Meta.none) in
        Option.is_none (strip_trailing_colon em)
      ;;

      let%test "emphasis before trailing colon" =
        let inline =
          Inline.Inlines
            ( [ Inline.Emphasis (Inline.Emphasis.make (text "foo"), Meta.none)
              ; text " bar:"
              ]
            , Meta.none )
        in
        Option.is_some (strip_trailing_colon inline)
      ;;
    end)
  ;;

  let%test_module "labels_of_inline" =
    (module struct
      let text s = Inline.Text (s, Meta.none)
      let emph s = Inline.Emphasis (Inline.Emphasis.make (text s), Meta.none)
      let strong s = Inline.Strong_emphasis (Inline.Emphasis.make (text s), Meta.none)
      let label_count inline = List.length (labels_of_inline inline)

      (* Simple labels *)
      let%test_unit "pure text" = [%test_eq: int] (label_count (text "foo")) 1

      let%test_unit "single emphasis" =
        let inline = Inline.Inlines ([ emph "foo"; text "" ], Meta.none) in
        [%test_eq: int] (label_count inline) 1
      ;;

      let%test_unit "single strong emphasis" =
        let inline = Inline.Inlines ([ strong "foo"; text "" ], Meta.none) in
        [%test_eq: int] (label_count inline) 1
      ;;

      (* Mixed content rejected *)
      let%test_unit "emphasis + text is mixed" =
        let inline = Inline.Inlines ([ emph "foo"; text " bar" ], Meta.none) in
        [%test_eq: int] (label_count inline) 0
      ;;

      (* Chain splitting *)
      let%test_unit "pure text chain" = [%test_eq: int] (label_count (text "foo: bar")) 2

      let%test_unit "three-way text chain" =
        [%test_eq: int] (label_count (text "a: b: c")) 3
      ;;

      let%test_unit "emphasis chain" =
        let inline = Inline.Inlines ([ emph "foo"; text ": bar" ], Meta.none) in
        [%test_eq: int] (label_count inline) 2
      ;;

      let%test_unit "text then emphasis chain" =
        let inline = Inline.Inlines ([ text "foo: "; emph "bar"; text "" ], Meta.none) in
        [%test_eq: int] (label_count inline) 2
      ;;

      (* No split without space *)
      let%test_unit "url-like" = [%test_eq: int] (label_count (text "http://x.com")) 1

      let%test_unit "colon without space" =
        [%test_eq: int] (label_count (text "foo:bar")) 1
      ;;

      (* Empty / bare *)
      let%test_unit "empty text" = [%test_eq: int] (label_count (text "")) 0
    end)
  ;;
end

(* Shared helpers
   ============== *)

let is_blank_line : Block.t -> bool = function
  | Block.Blank_line _ -> true
  | _ -> false
;;

(** Split [bs] at the first blank line. *)
let span_non_blank (bs : Block.t list) : Block.t list * Block.t list =
  let rec go acc = function
    | [] -> List.rev acc, []
    | b :: _ as rest when is_blank_line b -> List.rev acc, rest
    | b :: rest -> go (b :: acc) rest
  in
  go [] bs
;;

let wrap_blocks : Block.t list -> Block.t = function
  | [] -> Block.Blocks ([], Meta.none)
  | [ single ] -> single
  | multiple -> Block.Blocks (multiple, Meta.none)
;;

(** Build nested keyed nodes from a list of label inlines (outermost
    first) and a body block. *)
let build_nested_keyed
      ~(make_node : t -> Block.t -> Block.t)
      (labels : Inline.t list)
      (body : Block.t)
  : Block.t
  =
  let mk label b = make_node { label } b in
  match List.rev labels with
  | [] -> failwith "build_nested_keyed: empty labels"
  | innermost :: outers ->
    List.fold outers ~init:(mk innermost body) ~f:(fun acc lbl -> mk lbl acc)
;;

let mk_keyed_block t b = Ext_keyed_block (t, b)
let mk_keyed_item t b = Ext_keyed_list_item (t, b)

(* Tree rewrite
   ============ *)

module Rewrite : sig
  val rewrite_within_block : Block.t -> Block.t
end = struct
  let list_item_paragraph (item : Block.List_item.t)
    : (Block.Paragraph.t * Block.t list) option
    =
    match Block.List_item.block item with
    | Block.Paragraph (p, _) -> Some (p, [])
    | Block.Blocks (Block.Paragraph (p, _) :: rest, _) -> Some (p, rest)
    | _ -> None
  ;;

  let rebuild_item (item : Block.List_item.t) (block : Block.t) : Block.List_item.t =
    Block.List_item.make
      ~before_marker:(Block.List_item.before_marker item)
      ~marker:(Block.List_item.marker item)
      ~after_marker:(Block.List_item.after_marker item)
      block
  ;;

  let make_list
        (l : Block.List'.t)
        (list_meta : Meta.t)
        (items : Block.List_item.t node list)
    : Block.t
    =
    Block.List
      ( Block.List'.make ~tight:(Block.List'.tight l) (Block.List'.type' l) items
      , list_meta )
  ;;

  let replace_last items new_last =
    match List.rev items with
    | [] -> assert false
    | _ :: rev_prefix -> List.rev_append rev_prefix [ new_last ]
  ;;

  (* List-item tagging (Rule 1)
     -------------------------- *)

  let tag_middle_item item =
    match list_item_paragraph item with
    | Some (p, (_ :: _ as sub_blocks)) ->
      (match Colon.strip_trailing_colon (Block.Paragraph.inline p) with
       | None -> item
       | Some label_inline ->
         (match Colon.labels_of_inline label_inline with
          | [] -> item
          | labels ->
            let body = wrap_blocks sub_blocks in
            rebuild_item item (build_nested_keyed ~make_node:mk_keyed_item labels body)))
    | _ -> item
  ;;

  let tag_last_item item : Block.List_item.t * Inline.t list option =
    match list_item_paragraph item with
    | None -> item, None
    | Some (p, sub_blocks) ->
      (match Colon.strip_trailing_colon (Block.Paragraph.inline p) with
       | None -> item, None
       | Some label_inline ->
         (match Colon.labels_of_inline label_inline with
          | [] -> item, None
          | labels ->
            if not (List.is_empty sub_blocks)
            then (
              let body = wrap_blocks sub_blocks in
              ( rebuild_item item (build_nested_keyed ~make_node:mk_keyed_item labels body)
              , None ))
            else item, Some labels))
  ;;

  let rec tag_keyed_items (items : Block.List_item.t node list)
    : Block.List_item.t node list * Inline.t list option
    =
    match items with
    | [] -> [], None
    | [ (item, meta) ] ->
      let item', bare = tag_last_item item in
      [ item', meta ], bare
    | (item, meta) :: rest ->
      let item' = tag_middle_item item in
      let rest', bare = tag_keyed_items rest in
      (item', meta) :: rest', bare
  ;;

  (* Sibling-block rewrite
     --------------------- *)

  let rec rewrite_block_list (blocks : Block.t list) : Block.t list =
    match blocks with
    | [] -> []
    | (Block.Paragraph (p, _) as block) :: rest ->
      (match Colon.strip_trailing_colon (Block.Paragraph.inline p) with
       | None -> rewrite_within_block block :: rewrite_block_list rest
       | Some label_inline -> absorb_paragraph ~original:block ~label_inline rest)
    | Block.List (l, list_meta) :: rest -> handle_list l list_meta rest
    | block :: rest -> rewrite_within_block block :: rewrite_block_list rest

  and absorb_paragraph ~original ~label_inline rest =
    let children, after = span_non_blank rest in
    match children, Colon.labels_of_inline label_inline with
    | [], _ | _, [] -> original :: rewrite_block_list rest
    | _ :: _, (_ :: _ as labels) ->
      let children = rewrite_block_list children in
      let body = wrap_blocks children in
      let keyed = build_nested_keyed ~make_node:mk_keyed_block labels body in
      keyed :: rewrite_block_list after

  and handle_list l list_meta rest =
    let tagged, bare_last = tag_keyed_items (Block.List'.items l) in
    let items, rest =
      match bare_last, rest with
      | Some labels, (next :: _ as rest) when not (is_blank_line next) ->
        let following, after = span_non_blank rest in
        let body = wrap_blocks (rewrite_block_list following) in
        let keyed = build_nested_keyed ~make_node:mk_keyed_item labels body in
        let last_item, last_meta =
          match List.last tagged with
          | Some x -> x
          | None -> assert false
        in
        replace_last tagged (rebuild_item last_item keyed, last_meta), after
      | _ -> tagged, rest
    in
    let items = recurse_items items in
    make_list l list_meta items :: rewrite_block_list rest

  and recurse_items items =
    List.map items ~f:(fun (item, item_meta) ->
      let block = Block.List_item.block item in
      let block' = rewrite_within_block block in
      if phys_equal block block'
      then item, item_meta
      else rebuild_item item block', item_meta)

  and rewrite_within_block (block : Block.t) : Block.t =
    match block with
    | Block.Blocks (blocks, meta) -> Block.Blocks (rewrite_block_list blocks, meta)
    | Block.Block_quote (bq, meta) ->
      let inner = Block.Block_quote.block bq in
      Block.Block_quote (Block.Block_quote.make (rewrite_within_block inner), meta)
    | Block.List (l, list_meta) ->
      (match handle_list l list_meta [] with
       | [ single ] -> single
       | multiple -> Block.Blocks (multiple, Meta.none))
    | Div.Ext_div (div, body) -> Div.Ext_div (div, rewrite_within_block body)
    | Ext_keyed_list_item (t, body) -> Ext_keyed_list_item (t, rewrite_within_block body)
    | Ext_keyed_block (t, body) -> Ext_keyed_block (t, rewrite_within_block body)
    | _ -> block
  ;;
end

let rewrite_doc (doc : Doc.t) : Doc.t =
  let block = Doc.block doc in
  let block' = Rewrite.rewrite_within_block block in
  if phys_equal block block' then doc else Doc.make block'
;;

(** {1 For test} *)
module For_test = struct
  open Common.For_test

  let doc_of_string s =
    let doc = Doc.of_string s in
    rewrite_doc doc
  ;;

  let pp_doc doc = mk_pp_doc ~blocks:[ sexp_of_block ] () doc

  let block_ext_fold : (Block.t, 'a) Folder.fold =
    fun f acc b ->
    match b with
    | Ext_keyed_block (_, body) | Ext_keyed_list_item (_, body) ->
      Folder.fold_block f acc body
    | _ -> acc
  ;;

  (** {2 Predicates} *)

  (** Every keyed node's body is non-empty. *)
  let keyed_bodies_non_empty (doc : Doc.t) : bool =
    let folder =
      Folder.make
        ~block_ext_default:block_ext_fold
        ~block:(fun _f acc b ->
          match b with
          | Ext_keyed_block (_, Block.Blocks ([], _))
          | Ext_keyed_list_item (_, Block.Blocks ([], _)) -> Folder.ret false
          | _ -> if acc then Folder.default else Folder.ret false)
        ()
    in
    Folder.fold_doc folder true doc
  ;;

  (** Would [strip_trailing_colon] + [labels_of_inline] produce valid
      labels for this inline? *)
  let has_valid_labels (inline : Inline.t) : bool =
    match Colon.strip_trailing_colon inline with
    | None -> false
    | Some label_inline -> not (List.is_empty (Colon.labels_of_inline label_inline))
  ;;

  (** Does the last item of a list have a bare trailing colon with
      valid labels? *)
  let list_last_item_is_bare_keyed (l : Block.List'.t) : bool =
    match List.last (Block.List'.items l) with
    | None -> false
    | Some (item, _) ->
      (match Block.List_item.block item with
       | Block.Paragraph (p, _) -> has_valid_labels (Block.Paragraph.inline p)
       | _ -> false)
  ;;

  (** No keyable paragraph or keyable-last-item list is immediately
      followed by a non-blank block.  Violation means the rewriter
      missed an absorption. *)
  let keying_is_maximal (doc : Doc.t) : bool =
    let ok = ref true in
    let check_siblings bs =
      let arr = Array.of_list bs in
      let len = Array.length arr in
      for i = 0 to len - 1 do
        let absorbable =
          match arr.(i) with
          | Block.Paragraph (p, _) -> has_valid_labels (Block.Paragraph.inline p)
          | Block.List (l, _) -> list_last_item_is_bare_keyed l
          | _ -> false
        in
        if absorbable && i + 1 < len && not (is_blank_line arr.(i + 1)) then ok := false
      done
    in
    let folder =
      Folder.make
        ~block_ext_default:block_ext_fold
        ~block:(fun _f acc b ->
          (match b with
           | Block.Blocks (bs, _) -> check_siblings bs
           | _ -> ());
          if acc then Folder.default else Folder.ret false)
        ()
    in
    ignore (Folder.fold_doc folder true doc : bool);
    !ok
  ;;

  (** {2 Examples} *)

  let keyed_list_item_with_indented_content =
    {|- foo:
  - bar
  - baz|}
  ;;

  let keyed_list_item_with_contiguous_blocks =
    {|- foo:
```
bar
```|}
  ;;

  let keyed_paragraph =
    {|foo:
- bar
- baz

bee|}
  ;;

  let keyed_paragraph_multiple_children =
    {|foo:
- bar
- baz
some text|}
  ;;

  let nesting =
    {|foo:
- bar:
  - baz
- qux|}
  ;;

  let colon_chain_inline_keying =
    {|- foo: bar:
  - baz|}
  ;;

  let emphasis_keyed_item =
    {|- *foo*:
  - bar
  - baz|}
  ;;

  let emphasis_chain =
    {|- *foo*: bar:
  - baz|}
  ;;

  let escaped_colon =
    {|- foo\\:
- bar|}
  ;;

  let non_example_no_colon =
    {|- foo
- bar|}
  ;;

  let non_example_colon_in_code_span =
    {|text with `code:`
following paragraph|}
  ;;

  let non_example_mixed_inline =
    {|*foo* bar:
following|}
  ;;

  let non_example_blank_line =
    {|- foo:

bar|}
  ;;

  let examples =
    [ keyed_list_item_with_indented_content
    ; non_example_blank_line
    ; keyed_list_item_with_contiguous_blocks
    ; keyed_paragraph
    ; keyed_paragraph_multiple_children
    ; nesting
    ; colon_chain_inline_keying
    ; emphasis_keyed_item
    ; emphasis_chain
    ; non_example_no_colon
    ; non_example_colon_in_code_span
    ; non_example_mixed_inline
    ; escaped_colon
    ]
  ;;

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

let%test_module "Struct" =
  (module struct
    open For_test

    let%expect_test _ =
      keyed_list_item_with_indented_content |> doc_of_string |> pp_doc;
      [%expect
        {|
        (List
          (Keyed_list_item (Text foo)
            (List (Paragraph (Text bar)) (Paragraph (Text baz)))))
        |}]
    ;;

    let%expect_test _ =
      keyed_list_item_with_contiguous_blocks |> doc_of_string |> pp_doc;
      [%expect
        {| (Blocks (List (Keyed_list_item (Text foo) (Code_block no-info bar)))) |}]
    ;;

    let%expect_test _ =
      keyed_paragraph |> doc_of_string |> pp_doc;
      [%expect
        {|
        (Blocks
          (Keyed_block (Text foo)
            (List (Paragraph (Text bar)) (Paragraph (Text baz))))
          Blank_line (Paragraph (Text bee)))
        |}]
    ;;

    let%expect_test _ =
      keyed_paragraph_multiple_children |> doc_of_string |> pp_doc;
      [%expect
        {|
        (Blocks
          (Keyed_block (Text foo)
            (List (Paragraph (Text bar))
              (Paragraph (Inlines (Text baz) (Break soft) (Text "some text"))))))
        |}]
    ;;

    let%expect_test _ =
      nesting |> doc_of_string |> pp_doc;
      [%expect
        {|
        (Blocks
          (Keyed_block (Text foo)
            (List (Keyed_list_item (Text bar) (List (Paragraph (Text baz))))
              (Paragraph (Text qux)))))
        |}]
    ;;

    let%expect_test _ =
      colon_chain_inline_keying |> doc_of_string |> pp_doc;
      [%expect
        {|
        (List
          (Keyed_list_item (Text foo)
            (Keyed_list_item (Text bar) (List (Paragraph (Text baz))))))
        |}]
    ;;

    let%expect_test _ =
      emphasis_keyed_item |> doc_of_string |> pp_doc;
      [%expect
        {|
        (List
          (Keyed_list_item (Emphasis (Text foo))
            (List (Paragraph (Text bar)) (Paragraph (Text baz)))))
        |}]
    ;;

    let%expect_test _ =
      emphasis_chain |> doc_of_string |> pp_doc;
      [%expect
        {|
        (List
          (Keyed_list_item (Emphasis (Text foo))
            (Keyed_list_item (Text bar) (List (Paragraph (Text baz))))))
        |}]
    ;;

    let%expect_test _ =
      non_example_no_colon |> doc_of_string |> pp_doc;
      [%expect {| (List (Paragraph (Text foo)) (Paragraph (Text bar))) |}]
    ;;

    let%expect_test _ =
      non_example_colon_in_code_span |> doc_of_string |> pp_doc;
      [%expect
        {|
        (Paragraph
          (Inlines (Text "text with ") (Code_span code:) (Break soft)
            (Text "following paragraph")))
        |}]
    ;;

    let%expect_test _ =
      non_example_mixed_inline |> doc_of_string |> pp_doc;
      [%expect
        {|
        (Paragraph
          (Inlines (Emphasis (Text foo)) (Text " bar:") (Break soft)
            (Text following)))
        |}]
    ;;

    let%expect_test _ =
      non_example_blank_line |> doc_of_string |> pp_doc;
      [%expect
        {| (Blocks (List (Paragraph (Text foo:))) Blank_line (Paragraph (Text bar))) |}]
    ;;
  end)
;;
