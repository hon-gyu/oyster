(** {0 Block attributes}

    Implements the Djot block attribute syntax. A line of the form
    [{...}] immediately preceding a block-level element attaches the
    attribute spec to that block's metadata. Repeated specifiers stack
    via {!Oy_attribute.merge}.

    {1 Strategy}

    A doc-level rewrite that walks every list of sibling blocks and
    looks for runs of "attribute paragraphs" — paragraphs whose textual
    content is one or more [{...}] groups, separated by whitespace.
    Each run becomes an {!Ext_attribute_lines} block (preserving the
    source structure for round-trip and folding). When the run is
    immediately followed (no [Blank_line] in between) by a non-attribute
    block, the merged attribute is also stamped onto that block's
    metadata via {!meta_key}.

    The rewrite recurses into block containers ([Blocks], [Block_quote],
    list items, and {!Cmarkit.Block.Ext_div}) so attributes work inside them.

    {1 AST}

    The resolved attribute lives in two places after the rewrite:
    - {!Ext_attribute_lines} carries the source-level [{...}] specs so
      the commonmark renderer can faithfully re-emit them and folders
      can see the literal source structure.
    - The target block's [Meta.t] carries the {e merged} attribute under
      {!meta_key} so renderers can read it without scanning siblings.
*)

open Core
open Cmarkit
open Common

(** Attached to the target block (Heading/Paragraph/Code_block/...) when
    one or more preceding {!Ext_attribute_lines} blocks merge into it. *)
let meta_key : Oy_attribute.t Cmarkit.Meta.key = Cmarkit.Meta.key ()

(** A literal run of [{...}] source lines. Produced by the rewrite for
    every attribute paragraph it encounters, whether or not the run
    successfully attaches to a following block. *)
type Cmarkit.Block.t += Ext_attribute_lines of Oy_attribute.t list node

let sexp_of_meta : Common.meta_sexp =
  fun meta ->
  Cmarkit.Meta.find meta_key meta
  |> Option.map ~f:(fun a -> Sexp.List [ Atom "block_attribute"; Oy_attribute.sexp_of_t a ])
;;

let sexp_of_block : block_sexp =
  fun ~recurse_inline:_ ~recurse_block:_ ~with_meta:_ b ->
  match b with
  | Ext_attribute_lines (specs, _) ->
    Some (Sexp.List (Atom "Attribute_lines" :: List.map specs ~f:Oy_attribute.sexp_of_t))
  | _ -> None
;;

let block_commonmark_renderer : Cmarkit_renderer.block =
  let open Cmarkit_renderer in
  fun (c : context) (b : Block.t) ->
    match b with
    | Ext_attribute_lines (specs, _) ->
      List.iter specs ~f:(fun spec ->
        Context.string c "{";
        Context.string c (Oy_attribute.to_string spec);
        Context.string c "}\n");
      true
    | _ -> false
;;

(* Detecting attribute paragraphs
==================== *)

(** Flatten a paragraph's inline to a string. Soft and hard breaks
    become ['\n']; any non-textual constructor (emphasis, code span,
    link, ...) aborts with [None] — attribute paragraphs must be pure
    text. *)
let rec inline_text (i : Inline.t) : string option =
  match i with
  | Inline.Text (s, _) -> Some s
  | Inline.Break _ -> Some "\n"
  | Inline.Inlines (is, _) ->
    let parts = List.map is ~f:inline_text in
    if List.for_all parts ~f:Option.is_some
    then Some (String.concat (List.filter_map parts ~f:Fn.id))
    else None
  | _ -> None
;;

(** Parse a flattened paragraph into a list of attribute specs. Returns
    [None] if the string is not exactly a (whitespace-separated) sequence
    of [{...}] groups, or if any group is malformed. *)
let parse_attr_paragraph (text : string) : Oy_attribute.t list option =
  let s = String.strip text in
  if String.is_empty s
  then None
  else (
    let len = String.length s in
    let rec skip_ws i =
      if i < len && Char.is_whitespace s.[i] then skip_ws (i + 1) else i
    in
    let rec scan acc i =
      let i = skip_ws i in
      if i >= len
      then Some (List.rev acc)
      else if not (Char.equal s.[i] '{')
      then None
      else (
        match String.index_from s i '}' with
        | None -> None
        | Some j ->
          let inner = String.sub s ~pos:(i + 1) ~len:(j - i - 1) in
          (match Oy_attribute.of_string_or_error inner with
           | Ok a -> scan (a :: acc) (j + 1)
           | Error _ -> None))
    in
    scan [] 0)
;;

(** If [block] is a paragraph that consists entirely of attribute
    specifiers, return the merged spec. *)
let attr_paragraph_spec (block : Block.t) : Oy_attribute.t option =
  match block with
  | Block.Paragraph (p, _) ->
    (match inline_text (Block.Paragraph.inline p) with
     | None -> None
     | Some s ->
       (match parse_attr_paragraph s with
        | None | Some [] -> None
        | Some specs -> Some (List.reduce_exn specs ~f:Oy_attribute.merge)))
  | _ -> None
;;

(*
   Splitting paragraphs that begin with attribute lines
   ====================

   Cmarkit fuses non-blank consecutive lines into one paragraph. So a
   source like

       {#water}
       Don't forget!

   becomes one [Paragraph] with a [Break `Soft] between the two text
   nodes. We split such paragraphs on soft breaks, peel off the
   leading lines that are attribute specifiers, and emit them as
   standalone attribute paragraphs followed by a paragraph for the
   remaining content.
   ============================================================ *)

(** Split an inline at [Break `Soft] boundaries. Returns a non-empty
    list of "line" inlines (each with all its non-break children
    concatenated back into [Inlines] when needed). *)
let split_on_soft_breaks (i : Inline.t) : Inline.t list =
  let parts =
    match i with
    | Inline.Inlines (is, _) -> is
    | other -> [ other ]
  in
  let is_soft = function
    | Inline.Break (b, _) ->
      (match Inline.Break.type' b with
       | `Soft -> true
       | `Hard -> false)
    | _ -> false
  in
  let rec go cur acc = function
    | [] -> List.rev (List.rev cur :: acc)
    | x :: rest when is_soft x -> go [] (List.rev cur :: acc) rest
    | x :: rest -> go (x :: cur) acc rest
  in
  let groups = go [] [] parts in
  List.map groups ~f:(function
    | [ single ] -> single
    | many -> Inline.Inlines (many, Meta.none))
;;

(** Rebuild a single [Inline.t] by joining lines with [Break `Soft]. *)
let join_with_soft_breaks (lines : Inline.t list) : Inline.t =
  match lines with
  | [] -> Inline.Inlines ([], Meta.none)
  | [ single ] -> single
  | many ->
    let soft = Inline.Break (Inline.Break.make `Soft, Meta.none) in
    let rec interleave = function
      | [] -> []
      | [ x ] -> [ x ]
      | x :: rest -> x :: soft :: interleave rest
    in
    Inline.Inlines (interleave many, Meta.none)
;;

(** Try to peel off leading attribute-only lines from a paragraph. If
    any are found, returns a list of standalone attribute paragraphs
    plus a paragraph holding the rest (if any). Otherwise returns
    [[block]] unchanged. *)
let split_attr_prefix (block : Block.t) : Block.t list =
  match block with
  | Block.Paragraph (p, meta) ->
    let inline = Block.Paragraph.inline p in
    let lines = split_on_soft_breaks inline in
    let line_is_attr line =
      match inline_text line with
      | None -> None
      | Some s ->
        (match parse_attr_paragraph s with
         | Some (_ :: _) -> Some line
         | _ -> None)
    in
    let attr_lines, rest_lines =
      List.split_while lines ~f:(fun l -> Option.is_some (line_is_attr l))
    in
    if List.is_empty attr_lines
    then [ block ]
    else (
      let attr_blocks =
        List.map attr_lines ~f:(fun line ->
          Block.Paragraph (Block.Paragraph.make line, Meta.none))
      in
      let rest_block =
        match rest_lines with
        | [] -> []
        | _ ->
          [ Block.Paragraph (Block.Paragraph.make (join_with_soft_breaks rest_lines), meta)
          ]
      in
      attr_blocks @ rest_block)
  | _ -> [ block ]
;;

(* Rewriting
   ========= *)

(** Stamp [attr] into [block]'s metadata. Handles all standard Cmarkit
    block constructors and the oystermark extension blocks. *)
let attach_attr (attr : Oy_attribute.t) (block : Block.t) : Block.t =
  let add meta = Meta.add meta_key attr meta in
  match block with
  | Block.Paragraph (p, m) -> Block.Paragraph (p, add m)
  | Block.Heading (h, m) -> Block.Heading (h, add m)
  | Block.Code_block (cb, m) -> Block.Code_block (cb, add m)
  | Block.Block_quote (bq, m) -> Block.Block_quote (bq, add m)
  | Block.List (l, m) -> Block.List (l, add m)
  | Block.Blocks (bs, m) -> Block.Blocks (bs, add m)
  | Block.Thematic_break (tb, m) -> Block.Thematic_break (tb, add m)
  | Block.Html_block (lines, m) -> Block.Html_block (lines, add m)
  | Block.Ext_div (d, m) -> Block.Ext_div (d, add m)
  | Struct.Ext_keyed_block (x, m) -> Struct.Ext_keyed_block (x, add m)
  | Struct.Ext_keyed_list_item (x, m) -> Struct.Ext_keyed_list_item (x, add m)
  | other -> other
;;

let is_blank_line : Block.t -> bool = function
  | Block.Blank_line _ -> true
  | _ -> false
;;

(** Rewrite a sibling block list: collapse consecutive attribute
    paragraphs into {!Ext_attribute_lines} runs and stamp the merged
    attribute onto the next non-attribute block when one immediately
    follows (no blank line in between). *)
let rec rewrite_block_list (blocks : Block.t list) : Block.t list =
  let blocks = List.concat_map blocks ~f:split_attr_prefix in
  let flush_orphan pending =
    (* Orphan: emit the run as Ext_attribute_lines, no attaching. *)
    match List.rev pending with
    | [] -> []
    | specs -> [ Ext_attribute_lines (specs, Meta.none) ]
  in
  let rec go pending = function
    | [] -> flush_orphan pending
    | block :: rest ->
      (match attr_paragraph_spec block with
       | Some spec -> go (spec :: pending) rest
       | None ->
         if is_blank_line block
         then flush_orphan pending @ (block :: go [] rest)
         else (
           let block' = rewrite_within_block block in
           match List.rev pending with
           | [] -> block' :: go [] rest
           | specs ->
             let merged = List.reduce_exn specs ~f:Oy_attribute.merge in
             Ext_attribute_lines (specs, Meta.none)
             :: attach_attr merged block'
             :: go [] rest))
  in
  go [] blocks

and rewrite_within_block (block : Block.t) : Block.t =
  match block with
  | Block.Blocks (bs, meta) -> Block.Blocks (rewrite_block_list bs, meta)
  | Block.Block_quote (bq, meta) ->
    let inner = Block.Block_quote.block bq in
    let inner' = rewrite_within_block inner in
    Block.Block_quote (Block.Block_quote.make inner', meta)
  | Block.List (l, meta) ->
    let items =
      List.map (Block.List'.items l) ~f:(fun (item, item_meta) ->
        let inner = Block.List_item.block item in
        let inner' = rewrite_within_block inner in
        let item' =
          Block.List_item.make ?marker:(Some (Block.List_item.marker item)) inner'
        in
        item', item_meta)
    in
    let l' = Block.List'.make ~tight:(Block.List'.tight l) (Block.List'.type' l) items in
    Block.List (l', meta)
  | Block.Ext_div (d, meta) ->
    Block.Ext_div (div_with_body d (rewrite_within_block (Block.Div.block d)), meta)
  | _ -> block
;;

let rewrite_doc (doc : Doc.t) : Doc.t =
  let block = Doc.block doc in
  let block' =
    match rewrite_block_list [ block ] with
    | [ single ] -> single
    | many -> Block.Blocks (many, Meta.none)
  in
  if phys_equal block block' then doc else Doc.make block'
;;

(** Default folder/mapper continuation for [Ext_attribute_lines]. The
    constructor is a leaf (no inner block to recurse into), so the
    accumulator passes through unchanged. Other extensions in this
    library expose similar [block_ext_*] helpers; downstream folders
    that traverse the post-rewrite AST should compose them. *)
let block_ext_fold : (Block.t, 'a) Folder.fold =
  fun _f acc b ->
  match b with
  | Ext_attribute_lines _ -> acc
  | _ -> acc
;;

module For_test = struct
  let count_with_attr (doc : Doc.t) : int =
    let folder =
      Folder.make
        ~block_ext_default:block_ext_fold
        ~block:(fun _f acc b ->
          let m =
            match b with
            | Block.Paragraph (_, m)
            | Block.Heading (_, m)
            | Block.Code_block (_, m)
            | Block.Block_quote (_, m)
            | Block.List (_, m)
            | Block.Blocks (_, m)
            | Block.Thematic_break (_, m)
            | Block.Html_block (_, m) -> m
            | _ -> Meta.none
          in
          if Option.is_some (Meta.find meta_key m)
          then Folder.ret (acc + 1)
          else Folder.default)
        ()
    in
    Folder.fold_doc folder 0 doc
  ;;
end

let%test_module "Djot block attributes" =
  (module struct
    open Common.For_test

    let doc_of_string (s : string) : Doc.t =
      Doc.of_string ~strict:false ~layout:false ~locs:true s |> rewrite_doc
    ;;

    let pp_doc (doc : Doc.t) : unit =
      mk_pp_doc ~blocks:[ sexp_of_block ] ~metas:[ sexp_of_meta ] () doc
    ;;

    let%expect_test "single attribute on paragraph" =
      let doc = doc_of_string "{#water}\nDon't forget to turn off the water!" in
      [%test_result: int] (For_test.count_with_attr doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Attribute_lines ((id (#water)) (classes ()) (kvs ())))
          ((Paragraph (Text "Don't forget to turn off the water!"))
            (meta (block_attribute ((id (#water)) (classes ()) (kvs ()))))))
        |}]
    ;;

    let%expect_test "stacked attributes merge" =
      let doc = doc_of_string "{#water}\n{.important .large}\nDon't forget!" in
      [%test_result: int] (For_test.count_with_attr doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks
          (Attribute_lines ((id (#water)) (classes ()) (kvs ()))
            ((id ()) (classes (.important .large)) (kvs ())))
          ((Paragraph (Text "Don't forget!"))
            (meta
              (block_attribute
                ((id (#water)) (classes (.important .large)) (kvs ()))))))
        |}]
    ;;

    let%expect_test "attaches to blockquote" =
      let doc =
        doc_of_string "{source=\"Iliad\"}\n> Sing, muse, of the wrath of Achilles"
      in
      [%test_result: int] (For_test.count_with_attr doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Attribute_lines ((id ()) (classes ()) (kvs ((source Iliad)))))
          ((Block_quote (Paragraph (Text "Sing, muse, of the wrath of Achilles")))
            (meta (block_attribute ((id ()) (classes ()) (kvs ((source Iliad))))))))
        |}]
    ;;

    let%expect_test "blank line breaks association" =
      let doc = doc_of_string "{#water}\n\nDon't forget!" in
      [%test_result: int] (For_test.count_with_attr doc) ~expect:0;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Attribute_lines ((id (#water)) (classes ()) (kvs ()))) Blank_line
          (Paragraph (Text "Don't forget!")))
        |}]
    ;;

    let%expect_test "orphan attribute at end stays as paragraph" =
      let doc = doc_of_string "Some text\n\n{#trailing}" in
      [%test_result: int] (For_test.count_with_attr doc) ~expect:0;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Paragraph (Text "Some text")) Blank_line
          (Attribute_lines ((id (#trailing)) (classes ()) (kvs ()))))
        |}]
    ;;

    let%test_unit "commonmark roundtrip is idempotent" =
      let renderer =
        Cmarkit_renderer.compose
          (Cmarkit_commonmark.renderer ())
          (Cmarkit_renderer.make ~block:block_commonmark_renderer ())
      in
      let commonmark_of_doc = Cmarkit_renderer.doc_to_string renderer in
      List.iter
        [ "{#water}\nDon't forget!"
        ; "{#water}\n{.important .large}\nDon't forget!"
        ; "{source=\"Iliad\"}\n> Sing, muse, of the wrath of Achilles"
        ; "{key=\"my value\"}\nThe paragraph."
        ; "{#orphan}\n\nSome text"
        ]
        ~f:(commonmark_of_doc_idempotent ~doc_of_string ~commonmark_of_doc)
    ;;
  end)
;;
