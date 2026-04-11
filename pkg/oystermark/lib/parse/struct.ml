(** {0 Struct: colon-keyed tree restructuring}

    {1 Specification}

    A {b keyed node} is a list item or paragraph whose inline content
    carries a colon-delimited label / value relationship.  There are
    two mutually exclusive forms:

    {ul
    {- {b Trailing-colon form.}  The inline ends with an unescaped
       [:] (no space after).  The labels are all the segments
       separated by [: ] (colon-space).  The body comes from the
       node's sub-blocks, or — if there are none — from content that
       is {e absorbed} from the surrounding context (see
       {{!section:restructuring}Tree restructuring}).}
    {- {b Inline-value form.}  The inline does {e not} end with a
       colon, but contains at least one [: ] split.  The last
       segment is the {b value} (unrestricted free-form inline); all
       preceding segments are labels.  Unlike the trailing-colon
       form, this node does {e not} absorb following content.}}

    In both forms, each {b label} segment must be a single inline
    unit: pure text, emphasis, strong emphasis, or code span.  Mixed
    content — e.g. emphasis followed by text in the same segment —
    disqualifies the entire decomposition.  The {b value} segment in
    the inline-value form has no such restriction.

    For cross-line absorption (trailing-colon form), there must be no
    whitespace between the colon and the line break.  For inline
    splits (both forms), there must be exactly [": "] — a colon
    immediately followed by a space.  [- foo:bar] (no space) and
    [- foo: ] (trailing space) are both non-keying.

    {2 Label detection}

    A trailing colon is detected by walking the inline tree rightward:
    follow the last child of each [Inlines] container until a [Text]
    leaf is reached.  If that leaf's raw content ends with [:]
    (no trailing whitespace), the node is keyed in trailing-colon
    form.

    Only [Text] and [Inlines] nodes are traversed.  Emphasis, code
    spans, links, images, raw HTML, breaks, and extension inlines are
    {b opaque} — a colon inside them does not participate in
    keying.

    {ul
    {- [foo bar:] → trailing-colon, label [Text "foo bar"].}
    {- [*foo*:] → trailing-colon, label [Emphasis "foo"].}
    {- [**foo**:] → trailing-colon, label [Strong_emphasis "foo"].}
    {- [*foo* bar:] → {b not keyed}.  Mixed content.}
    {- [*foo:*] → {b not keyed}.  Emphasis is opaque.}
    {- [`code:`] → {b not keyed}.  Code spans are opaque.}
    {- [foo: bar] → inline-value, label [Text "foo"], value
       [Text "bar"].}
    {- [foo: `code: thing`] → inline-value, label [Text "foo"],
       value [Code_span "code: thing"] (code spans inside values are
       free-form).}}

    {2 Escaped colons}

    A backslash immediately before the trailing colon in the {e parsed}
    inline text (i.e. [Text] node content) suppresses keying.

    Because CommonMark already consumes one level of backslash
    escaping ([\\:] in source becomes [\:] in the AST), write [\\\\:]
    in source to get [\\:] in the AST — which struct treats as a
    literal colon.

    More precisely, the number of consecutive backslashes immediately
    before the colon is counted.  An odd count means the colon is
    escaped; an even count means the backslashes pair up and the colon
    is structural.

    {2 Colon chains}

    Chains combine with both forms.  [a: b: c:] yields three labels
    in trailing-colon form; [a: b: c] yields two labels and a
    value [c] in inline-value form.

    {ul
    {- [- foo: bar:] with body [baz] →
       [Keyed_list_item "foo" (Keyed_list_item "bar" (... baz ...))].}
    {- [- a: b: c] (no trailing colon, no sub-blocks) →
       [Keyed_list_item "a" (Keyed_list_item "b" (Paragraph "c"))].}
    {- [- *foo*: bar:] with body [baz] →
       [Keyed_list_item (Emphasis "foo") (Keyed_list_item "bar" (...))].}
    {- [- http://example.com:] → single label ["http://example.com"].
       The [:] after [http] has no trailing space, so no split.}}

    {2:restructuring Tree restructuring rules}

    {ol
    {- {b Keyed list item with indented content.}  The indented
       sub-blocks become the body of an {!Ext_keyed_list_item}.
       Applies to both forms; in the inline-value form the value
       paragraph is prepended to the body.}
    {- {b Keyed list item followed by a blank line.}  No
       transformation — the trailing colon is treated as literal
       punctuation.}
    {- {b Keyed list item followed by contiguous blocks.}  For
       trailing-colon form only, unindented blocks immediately after
       the list are reparented under the last item as an
       {!Ext_keyed_list_item}.}
    {- {b Middle-item absorption.}  A non-last list item whose
       paragraph has a bare trailing colon (no inline value, no
       sub-blocks) absorbs all remaining sibling items of the same
       list as a nested list under its label.}
    {- {b Keyed paragraph.}  In trailing-colon form, a paragraph
       ending with [:] claims all immediately following contiguous
       blocks (no blank-line separation) as children, producing an
       {!Ext_keyed_block}.  In inline-value form, the paragraph is
       rewritten to an {!Ext_keyed_block} whose body is the value
       paragraph; no following content is absorbed.  The
       inline-value rewrite on paragraphs is gated by the
       [paragraph_inline_value] parameter of {!rewrite_doc}.}
    {- {b Nesting.}  Keyed nodes nest: a keyed paragraph can contain
       a list whose items are themselves keyed.}}

    {1 Parsing}

    Parsing is a single-pass rewrite on the already-parsed Cmarkit
    AST.  [decompose] classifies each candidate inline into one of
    the two forms, and the sibling-block walker in [Rewrite] applies
    the restructuring rules above. *)

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
  type decomposition =
    | Chain_trailing_colon of Inline.t list
    | Chain_with_value of Inline.t list * Inline.t

  val decompose : Inline.t -> decomposition option
end = struct
  (** Count consecutive backslashes immediately before position [pos]
      in [s]. *)
  let count_preceding_backslashes (s : string) (pos : int) : int =
    let rec go i n = if i >= 0 && Char.equal s.[i] '\\' then go (i - 1) (n + 1) else n in
    go (pos - 1) 0
  ;;

  (** Strip a trailing [:] from a raw text string.  The original must
      end with [:] directly — any trailing whitespace before the colon
      means the colon was followed by a space, not by end-of-line,
      and is not a structural trailing colon.  A colon preceded by an
      odd number of backslashes is escaped. *)
  let strip_colon_from_text (s : string) : string option =
    let len = String.length s in
    if len = 0 || not (Char.equal s.[len - 1] ':')
    then None
    else if count_preceding_backslashes s (len - 1) mod 2 = 1
    then None
    else Some (String.rstrip (String.chop_suffix_exn s ~suffix:":"))
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

  (** Reassemble a segment (inline list) into a single [Inline.t]. *)
  let rebuild_value_inline (segment : Inline.t list) : Inline.t =
    match segment with
    | [] -> Inline.Text ("", Meta.none)
    | [ single ] -> single
    | multiple -> Inline.Inlines (multiple, Meta.none)
  ;;

  (** A value segment is valid iff — after removing empty text nodes —
      it is non-empty and does not begin with a soft/hard break.  A
      leading break would indicate the value is on the next source
      line; the colon must be followed by its value on the same line. *)
  let is_valid_value_segment (segment : Inline.t list) : bool =
    let stripped =
      List.filter segment ~f:(fun i ->
        match i with
        | Inline.Text (s, _) -> not (String.is_empty (String.strip s))
        | _ -> true)
    in
    match stripped with
    | [] -> false
    | Inline.Break _ :: _ -> false
    | _ -> true
  ;;

  let validate_labels (segments : Inline.t list list) : Inline.t list option =
    let labels = List.filter_map segments ~f:as_simple_label in
    if List.length labels = List.length segments && not (List.is_empty labels)
    then Some labels
    else None
  ;;

  type decomposition =
    | Chain_trailing_colon of Inline.t list
    | Chain_with_value of Inline.t list * Inline.t

  (** Decompose an inline into a keying decomposition.

      {ul
      {- Trailing [:] (no space after) → all [: ]-separated segments
         are chain labels; body must come from sub-blocks or absorbed
         following content.}
      {- No trailing [:] but at least one [: ] split → the last
         segment is the inline value (unrestricted), preceding
         segments are chain labels.  Each label must be a single
         simple inline unit.}
      {- Otherwise → no decomposition.}} *)
  let decompose (inline : Inline.t) : decomposition option =
    match strip_trailing_colon inline with
    | Some stripped ->
      let segments = split_at_colon_space (unwrap_inline stripped) in
      Option.map (validate_labels segments) ~f:(fun labels ->
        Chain_trailing_colon labels)
    | None ->
      let segments = split_at_colon_space (unwrap_inline inline) in
      (match List.rev segments with
       | [] | [ _ ] -> None
       | value_seg :: rev_label_segs ->
         let label_segs = List.rev rev_label_segs in
         if not (is_valid_value_segment value_seg)
         then None
         else (
           match validate_labels label_segs with
           | None -> None
           | Some labels ->
             let value = rebuild_value_inline value_seg in
             Some (Chain_with_value (labels, value))))
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

      let%test_unit "trailing space prevents stripping" =
        [%test_eq: string option] (check "foo: ") None
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

(** Mutable config.  Set by [rewrite_doc] before each run. *)
module Config = struct
  let paragraph_inline_value = ref true
end

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

  let value_paragraph (value : Inline.t) : Block.t =
    Block.Paragraph (Block.Paragraph.make value, Meta.none)
  ;;

  (* Sibling-block rewrite
     --------------------- *)

  let rec rewrite_block_list (blocks : Block.t list) : Block.t list =
    match blocks with
    | [] -> []
    | (Block.Paragraph (p, _) as block) :: rest ->
      (match Colon.decompose (Block.Paragraph.inline p) with
       | None -> rewrite_within_block block :: rewrite_block_list rest
       | Some (Colon.Chain_trailing_colon labels) ->
         absorb_paragraph_trailing ~original:block ~labels rest
       | Some (Colon.Chain_with_value (labels, value)) ->
         if !Config.paragraph_inline_value
         then (
           let body = value_paragraph value in
           let keyed = build_nested_keyed ~make_node:mk_keyed_block labels body in
           keyed :: rewrite_block_list rest)
         else rewrite_within_block block :: rewrite_block_list rest)
    | Block.List (l, list_meta) :: rest -> handle_list l list_meta rest
    | block :: rest -> rewrite_within_block block :: rewrite_block_list rest

  and absorb_paragraph_trailing ~original ~labels rest =
    let children, after = span_non_blank rest in
    match children with
    | [] -> original :: rewrite_block_list rest
    | _ :: _ ->
      let children = rewrite_block_list children in
      let body = wrap_blocks children in
      let keyed = build_nested_keyed ~make_node:mk_keyed_block labels body in
      keyed :: rewrite_block_list after

  and handle_list l list_meta rest =
    let items = Block.List'.items l in
    let items, rest = rewrite_list_items l items rest in
    make_list l list_meta items :: rewrite_block_list rest

  and rewrite_list_items
    (l : Block.List'.t)
    (items : Block.List_item.t node list)
    (following : Block.t list)
    : Block.List_item.t node list * Block.t list
    =
    match items with
    | [] -> [], following
    | [ (item, meta) ] ->
      let item', following = rewrite_last_item item following in
      [ item', meta ], following
    | (item, meta) :: rest_items ->
      (match try_tag_non_last_item l item rest_items with
       | `Absorbed_rest new_block ->
         [ rebuild_item item new_block, meta ], following
       | `Tagged new_block ->
         let rest_items, following = rewrite_list_items l rest_items following in
         (rebuild_item item new_block, meta) :: rest_items, following
       | `Untouched ->
         let block = Block.List_item.block item in
         let block' = rewrite_within_block block in
         let item =
           if phys_equal block block' then item else rebuild_item item block'
         in
         let rest_items, following = rewrite_list_items l rest_items following in
         (item, meta) :: rest_items, following)

  and try_tag_non_last_item
    (l : Block.List'.t)
    (item : Block.List_item.t)
    (rest_items : Block.List_item.t node list)
    =
    match list_item_paragraph item with
    | None -> `Untouched
    | Some (p, sub_blocks) ->
      (match Colon.decompose (Block.Paragraph.inline p) with
       | None -> `Untouched
       | Some (Colon.Chain_with_value (labels, value)) ->
         let sub_blocks = rewrite_block_list sub_blocks in
         let body = wrap_blocks (value_paragraph value :: sub_blocks) in
         `Tagged (build_nested_keyed ~make_node:mk_keyed_item labels body)
       | Some (Colon.Chain_trailing_colon labels) ->
         if not (List.is_empty sub_blocks)
         then (
           let sub_blocks = rewrite_block_list sub_blocks in
           let body = wrap_blocks sub_blocks in
           `Tagged (build_nested_keyed ~make_node:mk_keyed_item labels body))
         else (
           (* Bare trailing-colon middle item absorbs remaining siblings
              as a nested list of the same type. *)
           let absorbed_items, _ = rewrite_list_items l rest_items [] in
           let nested_list = make_list l Meta.none absorbed_items in
           `Absorbed_rest
             (build_nested_keyed ~make_node:mk_keyed_item labels nested_list)))

  and rewrite_last_item (item : Block.List_item.t) (following : Block.t list)
    : Block.List_item.t * Block.t list
    =
    let recurse_item () =
      let block = Block.List_item.block item in
      let block' = rewrite_within_block block in
      if phys_equal block block' then item else rebuild_item item block'
    in
    match list_item_paragraph item with
    | None -> recurse_item (), following
    | Some (p, sub_blocks) ->
      (match Colon.decompose (Block.Paragraph.inline p) with
       | None -> recurse_item (), following
       | Some (Colon.Chain_with_value (labels, value)) ->
         let sub_blocks = rewrite_block_list sub_blocks in
         let body = wrap_blocks (value_paragraph value :: sub_blocks) in
         let new_block = build_nested_keyed ~make_node:mk_keyed_item labels body in
         rebuild_item item new_block, following
       | Some (Colon.Chain_trailing_colon labels) ->
         if not (List.is_empty sub_blocks)
         then (
           let sub_blocks = rewrite_block_list sub_blocks in
           let body = wrap_blocks sub_blocks in
           let new_block = build_nested_keyed ~make_node:mk_keyed_item labels body in
           rebuild_item item new_block, following)
         else (
           let absorbed, remaining = span_non_blank following in
           if List.is_empty absorbed
           then item, following
           else (
             let absorbed = rewrite_block_list absorbed in
             let body = wrap_blocks absorbed in
             let new_block =
               build_nested_keyed ~make_node:mk_keyed_item labels body
             in
             rebuild_item item new_block, remaining)))

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

let rewrite_doc ?(paragraph_inline_value = true) (doc : Doc.t) : Doc.t =
  Config.paragraph_inline_value := paragraph_inline_value;
  let block = Doc.block doc in
  let block' = Rewrite.rewrite_within_block block in
  if phys_equal block block' then doc else Doc.make block'
;;

(** {1 For test} *)
module For_test = struct
  open Common.For_test

  let doc_of_string ?paragraph_inline_value s =
    let doc = Doc.of_string s in
    rewrite_doc ?paragraph_inline_value doc
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

  (** Does this inline have a trailing colon that would absorb
      following content?  Inline-value decompositions don't trigger
      absorption and return [false]. *)
  let is_trailing_colon_absorbable (inline : Inline.t) : bool =
    match Colon.decompose inline with
    | Some (Colon.Chain_trailing_colon _) -> true
    | _ -> false
  ;;

  (** Does the last item of a list have a bare trailing colon with
      valid labels? *)
  let list_last_item_is_bare_keyed (l : Block.List'.t) : bool =
    match List.last (Block.List'.items l) with
    | None -> false
    | Some (item, _) ->
      (match Block.List_item.block item with
       | Block.Paragraph (p, _) ->
         is_trailing_colon_absorbable (Block.Paragraph.inline p)
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
          | Block.Paragraph (p, _) ->
            is_trailing_colon_absorbable (Block.Paragraph.inline p)
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

  (** {2 Generator} *)

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

    let%expect_test _ =
      (* Two levels: A is keyed around the list; each item is keyed
         with an inline value. *)
      let eg = {|A:
- B: b
- C: c|} in
      eg |> doc_of_string |> pp_doc;
      [%expect
        {|
        (Blocks
          (Keyed_block (Text A)
            (List (Keyed_list_item (Text B) (Paragraph (Text b)))
              (Keyed_list_item (Text C) (Paragraph (Text c))))))
        |}]
    ;;

    let%expect_test _ =
      (* Four levels: A -> B -> b -> C.  The first item's trailing
         colon ([b:]) makes [b] a label, and [b] absorbs the
         following [C: c] sibling as its nested body. *)
      let eg = {|A:
- B: b:
- C: c|} in
      eg |> doc_of_string |> pp_doc;
      [%expect
        {|
        (Blocks
          (Keyed_block (Text A)
            (List
              (Keyed_list_item (Text B)
                (Keyed_list_item (Text b)
                  (List (Keyed_list_item (Text C) (Paragraph (Text c)))))))))
        |}]
    ;;
  end)
;;
