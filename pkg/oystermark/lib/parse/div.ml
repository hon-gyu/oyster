(** Djot div block extension.

    A div begins with a line of three or more consecutive colons, optionally
    followed by white space and a class name (but nothing else). It ends with
    a line of consecutive colons at least as long as the opening fence, or
    with the end of the document or containing block.

    The contents of a div are interpreted as block-level content.

    {1 Syntax}

{v
::: warning
Here is a paragraph.

And here is another.
:::
v}

    {1 Parsing}

    Parsing is a two-pass process on the already-parsed Cmarkit AST:
    {ol
    {- {b Extract fences} ({!extract_fences}): walk paragraphs, detect
       fence text in inlines, and split them out as standalone fence-only
       paragraphs.}
    {- {b Group into divs} ({!rewrite_doc}): match opening/closing fence
       paragraphs and wrap the blocks between them in {!Ext_div} nodes.}}

    A fence paragraph with a class name always opens a new div. A fence
    paragraph without a class name closes the nearest matching open div
    (i.e. one whose opening colon count {m \le} this fence's colon count).
*)

open Core
open Cmarkit
open Common

type t =
  { class_name : string option (** Optional class from the opening fence. *)
  ; colons : int (** Number of colons in the opening fence. *)
  }
[@@deriving sexp]

type Cmarkit.Block.t += Ext_div of t * Cmarkit.Block.t

let block_commonmark_renderer : Cmarkit_renderer.block =
  let open Cmarkit_renderer in
  fun (c : context) (b : Block.t) ->
    match b with
    | Ext_div (div, body) ->
      let fence = String.make div.colons ':' in
      let class_suffix =
        match div.class_name with
        | Some cls -> " " ^ cls
        | None -> ""
      in
      let buf = Context.buffer c in
      let len = Buffer.length buf in
      let needs_nl = len > 0 && not (Char.equal (Buffer.nth buf (len - 1)) '\n') in
      if needs_nl then Context.byte c '\n';
      Context.string c (fence ^ class_suffix ^ "\n\n");
      Context.block c body;
      Context.string c ("\n" ^ fence ^ "\n");
      true
    | _ -> false
;;

let sexp_of_block : block_sexp =
  fun ~recurse_inline:_ ~recurse_block ~with_meta:_ b ->
  match b with
  | Ext_div (div, body) ->
    Some (Sexp.List [ Atom "Div"; sexp_of_t div; recurse_block body ])
  | _ -> None
;;

(** Parse a div fence line.  Returns [(colons, class_name option)] or [None].
    A valid fence is 3+ consecutive colons, optionally followed by whitespace
    and a single non-whitespace token (the class name). *)
let parse_fence (s : string) : (int * string option) option =
  let s = String.strip s in
  let len = String.length s in
  if len < 3
  then None
  else (
    let n = ref 0 in
    while !n < len && Char.equal s.[!n] ':' do
      incr n
    done;
    if !n < 3
    then None
    else (
      let rest = String.strip (String.drop_prefix s !n) in
      if String.is_empty rest
      then Some (!n, None)
      else if String.exists rest ~f:Char.is_whitespace
      then None (* "but nothing else" -- must be a single word *)
      else Some (!n, Some rest)))
;;

(** Extract fence info from a paragraph whose inline is a single [Text] node. *)
let paragraph_fence (block : Cmarkit.Block.t) : (int * string option) option =
  match block with
  | Cmarkit.Block.Paragraph (p, _) ->
    (match Cmarkit.Block.Paragraph.inline p with
     | Cmarkit.Inline.Text (s, _) -> parse_fence s
     | _ -> None)
  | _ -> None
;;

(** Pass 1: extract fence lines from paragraph inlines *)
include (
struct
  (** Flatten top-level [Inlines] wrappers. *)
  let rec flatten_inlines (i : Cmarkit.Inline.t) : Cmarkit.Inline.t list =
    match i with
    | Cmarkit.Inline.Inlines (is, _) -> List.concat_map is ~f:flatten_inlines
    | other -> [ other ]
  ;;

  (** Split a flat inline list at [Break `Soft] boundaries into per-line groups. *)
  let split_at_soft_breaks (nodes : Cmarkit.Inline.t list) : Cmarkit.Inline.t list list =
    let rec go acc cur = function
      | [] ->
        let groups = List.rev (List.rev cur :: acc) in
        List.filter groups ~f:(fun g -> not (List.is_empty g))
      | Cmarkit.Inline.Break (b, _) :: rest
        when [%equal: [ `Hard | `Soft ]] (Cmarkit.Inline.Break.type' b) `Soft ->
        go (List.rev cur :: acc) [] rest
      | node :: rest -> go acc (node :: cur) rest
    in
    go [] [] nodes
  ;;

  (** Turn a group of inline nodes back into a single inline. *)
  let inline_of_group (group : Cmarkit.Inline.t list) : Cmarkit.Inline.t =
    match group with
    | [ single ] -> single
    | multiple -> Cmarkit.Inline.Inlines (multiple, Cmarkit.Meta.none)
  ;;

  (** Check whether a single-line inline group is a fence. *)
  let group_is_fence (group : Cmarkit.Inline.t list) : bool =
    match group with
    | [ Cmarkit.Inline.Text (s, _) ] -> Option.is_some (parse_fence s)
    | _ -> false
  ;;

  (** Split a paragraph into multiple blocks when fence lines are mixed in
    with regular content.  Returns [None] if no splitting is needed. *)
  let split_paragraph_fences (p : Cmarkit.Block.Paragraph.t) (meta : Cmarkit.Meta.t)
    : Cmarkit.Block.t list option
    =
    let inline = Cmarkit.Block.Paragraph.inline p in
    let flat = flatten_inlines inline in
    let lines = split_at_soft_breaks flat in
    if List.length lines <= 1
    then None (* single-line paragraph — nothing to split *)
    else (
      let has_fence = List.exists lines ~f:group_is_fence in
      if not has_fence
      then None
      else
        Some
          (List.mapi lines ~f:(fun i group ->
             let para_inline = inline_of_group group in
             let para = Cmarkit.Block.Paragraph.make para_inline in
             (* Preserve original meta on the first sub-paragraph *)
             let m = if i = 0 then meta else Cmarkit.Meta.none in
             Cmarkit.Block.Paragraph (para, m))))
  ;;
end :
sig
  val split_paragraph_fences
    :  Cmarkit.Block.Paragraph.t
    -> Cmarkit.Meta.t
    -> Cmarkit.Block.t list option
end)

(** Pass 1 of 2: walk the block tree and split paragraphs that contain fence lines
    mixed with other content. After this pass every fence is a standalone
    single-[Text] paragraph. *)
let rec extract_fences (block : Cmarkit.Block.t) : Cmarkit.Block.t =
  match block with
  | Cmarkit.Block.Blocks (blocks, meta) ->
    let blocks' = List.concat_map blocks ~f:extract_fences_in_list in
    Cmarkit.Block.Blocks (blocks', meta)
  | Cmarkit.Block.Block_quote (bq, meta) ->
    let inner = Cmarkit.Block.Block_quote.block bq in
    let inner' = extract_fences inner in
    Cmarkit.Block.Block_quote (Cmarkit.Block.Block_quote.make inner', meta)
  | Cmarkit.Block.Paragraph (p, meta) ->
    (match split_paragraph_fences p meta with
     | None -> block
     | Some blocks -> Cmarkit.Block.Blocks (blocks, Cmarkit.Meta.none))
  | _ -> block

and extract_fences_in_list (block : Cmarkit.Block.t) : Cmarkit.Block.t list =
  match block with
  | Cmarkit.Block.Paragraph (p, meta) ->
    (match split_paragraph_fences p meta with
     | None -> [ block ]
     | Some blocks -> blocks)
  | other -> [ extract_fences other ]
;;

let is_blank_line : Cmarkit.Block.t -> bool = function
  | Cmarkit.Block.Blank_line _ -> true
  | _ -> false
;;

(** Strip leading and trailing [Blank_line] nodes from a block list.
    The commonmark renderer emits blank lines around div body content for
    paragraph separation; re-parsing those produces [Blank_line] nodes that
    would accumulate on each roundtrip without this normalization. *)
let strip_surrounding_blanks (blocks : Cmarkit.Block.t list) : Cmarkit.Block.t list =
  let blocks = List.drop_while blocks ~f:is_blank_line in
  List.rev blocks |> List.drop_while ~f:is_blank_line |> List.rev
;;

(** Rewrite a list of sibling blocks, collecting div fences into [Ext_div] nodes.
    Pass 2 of 2: match fences and group children into Ext_div
*)
let rec rewrite_block_list (blocks : Cmarkit.Block.t list) : Cmarkit.Block.t list =
  let arr = Array.of_list blocks in
  let len = Array.length arr in
  let result = ref [] in
  let i = ref 0 in
  while !i < len do
    match paragraph_fence arr.(!i) with
    | Some (colons, class_name) ->
      incr i;
      let body_blocks, new_i = collect_body colons arr !i len in
      i := new_i;
      let body_blocks = rewrite_block_list body_blocks in
      let body_blocks = strip_surrounding_blanks body_blocks in
      let body =
        match body_blocks with
        | [] -> Cmarkit.Block.Blocks ([], Cmarkit.Meta.none)
        | [ single ] -> single
        | multiple -> Cmarkit.Block.Blocks (multiple, Cmarkit.Meta.none)
      in
      result := Ext_div ({ class_name; colons }, body) :: !result
    | None ->
      result := rewrite_within_block arr.(!i) :: !result;
      incr i
  done;
  List.rev !result

(** Collect blocks until a closing fence matching [open_colons] is found.
    Tracks nested named opening fences so their matching closing fences are
    not mistaken for ours. *)
and collect_body
      (open_colons : int)
      (arr : Cmarkit.Block.t array)
      (start : int)
      (len : int)
  : Cmarkit.Block.t list * int
  =
  let collected = ref [] in
  let i = ref start in
  let nesting : int Stack.t = Stack.create () in
  let found_close = ref false in
  while !i < len && not !found_close do
    match paragraph_fence arr.(!i) with
    | Some (colons, Some _) ->
      (* Named opening fence -- track for nesting *)
      Stack.push nesting colons;
      collected := arr.(!i) :: !collected;
      incr i
    | Some (colons, None) ->
      if (not (Stack.is_empty nesting)) && colons >= Stack.top_exn nesting
      then (
        (* Closes the innermost nested div *)
        ignore (Stack.pop_exn nesting : int);
        collected := arr.(!i) :: !collected;
        incr i)
      else if colons >= open_colons
      then (
        (* Closes our div *)
        found_close := true;
        incr i)
      else (
        (* Doesn't match anything -- treat as content *)
        collected := arr.(!i) :: !collected;
        incr i)
    | None ->
      collected := arr.(!i) :: !collected;
      incr i
  done;
  List.rev !collected, !i

(** Recurse into block containers to rewrite div fences in their children. *)
and rewrite_within_block (block : Cmarkit.Block.t) : Cmarkit.Block.t =
  match block with
  | Cmarkit.Block.Blocks (blocks, meta) ->
    Cmarkit.Block.Blocks (rewrite_block_list blocks, meta)
  | Cmarkit.Block.Block_quote (bq, meta) ->
    let inner = Cmarkit.Block.Block_quote.block bq in
    let inner' = rewrite_within_block inner in
    Cmarkit.Block.Block_quote (Cmarkit.Block.Block_quote.make inner', meta)
  | _ -> block
;;

(** Process a document: extract fences (pass 1) then group into divs (pass 2). *)
let rewrite_doc (doc : Cmarkit.Doc.t) : Cmarkit.Doc.t =
  let block = Cmarkit.Doc.block doc in
  let block' = extract_fences block in
  let block' = rewrite_within_block block' in
  if phys_equal block block' then doc else Cmarkit.Doc.make block'
;;

module For_test = struct
  (** Count the number of div blocks  *)
  let count_div (doc : Cmarkit.Doc.t) : int =
    let folder =
      Cmarkit.Folder.make
        ~block:(fun f acc -> function
           | Ext_div (_div, body) ->
             Cmarkit.Folder.ret (1 + Cmarkit.Folder.fold_block f acc body)
           | _ -> Cmarkit.Folder.default)
        ()
    in
    Cmarkit.Folder.fold_doc folder 0 doc
  ;;

  (** Examples  *)
  let example_basic =
    {|::: warning
Here is a paragraph.

And here is another.
:::|}
  ;;

  let example_no_class =
    {|:::
content
:::
|}
  ;;

  let example_nested_divs =
    {|:::: outer
::: inner
content
:::
::::
|}
  ;;

  let example_nested_divs_same_length =
    {|::: warning
content
:::
:::|}
  ;;

  let example_EOF_closes =
    {|::: warning
unclosed content|}
  ;;

  let example_extra_closing_fence =
    {|::: warning
content
:::
:::|}
  ;;

  let non_example_less_than_3_colons =
    {|:: not-a-div
content
::|}
  ;;

  let non_example_extra_words_after_class =
    {|::: warning extra
content
:::|}
  ;;

  let non_example_div_does_not_interfere_with_code_blocks =
    {|```
::: not-a-div
```|}
  ;;

  let example_closing_fence_must_be_at_least_as_long =
    {|:::: warning
content
:::
::::|}
  ;;

  let all_examples =
    [ example_basic
    ; example_no_class
    ; example_nested_divs
    ; example_nested_divs_same_length
    ; example_EOF_closes
    ; example_extra_closing_fence
    ; non_example_less_than_3_colons
    ; non_example_div_does_not_interfere_with_code_blocks
    ; example_closing_fence_must_be_at_least_as_long
    ]
  ;;
end

let%test_module "Div" =
  (module struct
    open Common.For_test
    open For_test

    let doc_of_string  s =
      let doc = Doc.of_string s in
      rewrite_doc doc
    ;;

    let pp_doc doc = mk_pp_doc ~blocks:[sexp_of_block] () doc

    let%expect_test _ =
      let doc = doc_of_string example_basic in
      [%test_result: int] (count_div doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks
          (Div ((class_name (warning)) (colons 3))
            (Blocks (Paragraph (Text "Here is a paragraph.")) Blank_line
              (Paragraph (Text "And here is another.")))))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string example_no_class in
      [%test_result: int] (count_div doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Div ((class_name ()) (colons 3)) (Paragraph (Text content)))
          Blank_line)
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string example_nested_divs in
      [%test_result: int] (count_div doc) ~expect:2;
      pp_doc doc;
      [%expect
        {|
        (Blocks
          (Div ((class_name (outer)) (colons 4))
            (Div ((class_name (inner)) (colons 3)) (Paragraph (Text content))))
          Blank_line)
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string example_nested_divs_same_length in
      [%test_result: int] (count_div doc) ~expect:2;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content)))
          (Div ((class_name ()) (colons 3)) (Blocks)))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string example_EOF_closes in
      [%test_result: int] (count_div doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks
          (Div ((class_name (warning)) (colons 3))
            (Paragraph (Text "unclosed content"))))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string example_extra_closing_fence in
      [%test_result: int] (count_div doc) ~expect:2;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content)))
          (Div ((class_name ()) (colons 3)) (Blocks)))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string non_example_less_than_3_colons in
      [%test_result: int] (count_div doc) ~expect:0;
      pp_doc doc;
      [%expect
        {|
        (Paragraph
          (Inlines (Text ":: not-a-div") (Break soft) (Text content) (Break soft)
            (Text ::)))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string non_example_extra_words_after_class in
      [%test_result: int] (count_div doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Paragraph (Text "::: warning extra")) (Paragraph (Text content))
          (Div ((class_name ()) (colons 3)) (Blocks)))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string non_example_div_does_not_interfere_with_code_blocks in
      [%test_result: int] (count_div doc) ~expect:0;
      pp_doc doc;
      [%expect {| (Code_block no-info "::: not-a-div") |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string example_closing_fence_must_be_at_least_as_long in
      pp_doc doc;
      [%expect
        {|
        (Blocks
          (Div ((class_name (warning)) (colons 4))
            (Blocks (Paragraph (Text content))
              (Div ((class_name ()) (colons 3)) (Blocks)))))
        |}]
    ;;

    let%test_unit "roundtrip: commonmark output is idempotent" =
      let commonmark_of_doc =
        Cmarkit_renderer.doc_to_string (Cmarkit_commonmark.renderer ())
      in
      List.iter all_examples ~f:(commonmark_of_doc_idempotent ~doc_of_string ~commonmark_of_doc)
    ;;
  end)
;;
