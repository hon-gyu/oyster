open Core
open Cmarkit

type t = { label : Inline.t }
type Block.t += Ext_keyed_list_item of t * Block.t | Ext_keyed_block of t * Block.t

let debug_block_renderer : Cmarkit_renderer.block =
  let open Cmarkit_renderer in
  fun (c : context) (b : Block.t) ->
    match b with
    | Ext_keyed_block ({ label }, body) ->
      Context.string c "K(";
      Context.inline c label;
      Context.string c ", ";
      Context.block c body;
      Context.string c ")";
      true
    | Ext_keyed_list_item ({ label }, body) ->
      Context.string c "K(";
      Context.inline c label;
      Context.string c ", ";
      Context.block c body;
      Context.string c ")";
      true
    | Block.List (l, _) ->
      Context.string c "List[";
      let items = Block.List'.items l in
      List.iteri items ~f:(fun i (item, _) ->
        if i > 0 then Context.string c ", ";
        Context.block c (Block.List_item.block item));
      Context.string c "]";
      true
    | _ -> false
;;

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
      Option.map (validate_labels segments) ~f:(fun labels -> Chain_trailing_colon labels)
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
       | `Absorbed_rest new_block -> [ rebuild_item item new_block, meta ], following
       | `Tagged new_block ->
         let rest_items, following = rewrite_list_items l rest_items following in
         (rebuild_item item new_block, meta) :: rest_items, following
       | `Untouched ->
         let block = Block.List_item.block item in
         let block' = rewrite_within_block block in
         let item = if phys_equal block block' then item else rebuild_item item block' in
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
           `Absorbed_rest (build_nested_keyed ~make_node:mk_keyed_item labels nested_list)))

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
             let new_block = build_nested_keyed ~make_node:mk_keyed_item labels body in
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
