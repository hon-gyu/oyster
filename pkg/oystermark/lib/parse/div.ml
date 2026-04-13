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
  | Cmarkit.Block.List (l, list_meta) -> extract_fences_from_list l list_meta
  | other -> [ extract_fences other ]

(** When cmarkit absorbs a [:::] fence as a lazy continuation line
    inside a list item paragraph, we need to split it out so the fence
    ends up at the parent level where div grouping can see it. *)
and extract_fences_from_list (l : Cmarkit.Block.List'.t) (list_meta : Cmarkit.Meta.t)
  : Cmarkit.Block.t list
  =
  let items = Cmarkit.Block.List'.items l in
  let list_block = Cmarkit.Block.List (l, list_meta) in
  let rebuild_item item block =
    Cmarkit.Block.List_item.make
      ~before_marker:(Cmarkit.Block.List_item.before_marker item)
      ~marker:(Cmarkit.Block.List_item.marker item)
      ~after_marker:(Cmarkit.Block.List_item.after_marker item)
      block
  in
  let rebuild_list new_items =
    Cmarkit.Block.List
      ( Cmarkit.Block.List'.make
          ~tight:(Cmarkit.Block.List'.tight l)
          (Cmarkit.Block.List'.type' l)
          new_items
      , list_meta )
  in
  match List.rev items with
  | [] -> [ list_block ]
  | (last_item, last_item_meta) :: rev_rest ->
    let try_split_paragraph p pmeta ~wrap_item =
      match split_paragraph_fences p pmeta with
      | None -> None
      | Some [] -> None
      | Some (first :: extracted) ->
        let new_item = rebuild_item last_item (wrap_item first) in
        let new_items = List.rev ((new_item, last_item_meta) :: rev_rest) in
        Some (rebuild_list new_items :: extracted)
    in
    (match Cmarkit.Block.List_item.block last_item with
     | Cmarkit.Block.Paragraph (p, pmeta) ->
       (match try_split_paragraph p pmeta ~wrap_item:Fun.id with
        | Some result -> result
        | None -> [ list_block ])
     | Cmarkit.Block.Blocks (Cmarkit.Block.Paragraph (p, pmeta) :: rest, bmeta) ->
       (* Strip trailing blank lines that cmarkit adds for loose lists;
          they would otherwise become spurious sub-blocks in the item. *)
       let rest =
         List.filter rest ~f:(fun b ->
           match b with
           | Cmarkit.Block.Blank_line _ -> false
           | _ -> true)
       in
       let wrap_item first =
         match rest with
         | [] -> first
         | _ -> Cmarkit.Block.Blocks (first :: rest, bmeta)
       in
       (match try_split_paragraph p pmeta ~wrap_item with
        | Some result -> result
        | None -> [ list_block ])
     | _ -> [ list_block ])
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

  let commonmark_of_doc =
    let r = Cmarkit_renderer.make ~block:block_commonmark_renderer () in
    let r' = Cmarkit_renderer.compose (Cmarkit_commonmark.renderer ()) r in
    Cmarkit_renderer.doc_to_string r'
  ;;

  (** Examples : name, content, expected number of divs *)
  let example_basic =
    ( "basic"
    , {|::: warning
Here is a paragraph.

And here is another.
:::|}
    , 1 )
  ;;

  let example_no_class =
    ( "no_class"
    , {|:::
content
:::
|}
    , 1 )
  ;;

  let example_nested_divs =
    ( "nested_divs"
    , {|:::: outer
::: inner
content
:::
::::
|}
    , 2 )
  ;;

  let example_nested_divs_same_length =
    ( "nested_divs_same_length"
    , {|::: warning
content
:::
:::|}
    , 2 )
  ;;

  let example_EOF_closes =
    ( "EOF_closes"
    , {|::: warning
unclosed content|}
    , 1 )
  ;;

  let example_extra_closing_fence =
    ( "extra_closing_fence"
    , {|::: warning
content
:::
:::|}
    , 2 )
  ;;

  let non_example_less_than_3_colons =
    ( "less_than_3_colons"
    , {|:: not-a-div
content
::|}
    , 0 )
  ;;

  let non_example_extra_words_after_class =
    ( "extra_words_after_class"
    , {|::: warning extra
content
:::|}
    , 0 )
  ;;

  let non_example_div_does_not_interfere_with_code_blocks =
    ( "div_does_not_interfere_with_code_blocks"
    , {|```
::: not-a-div
```|}
    , 0 )
  ;;

  let example_closing_fence_must_be_at_least_as_long =
    ( "closing_fence_must_be_at_least_as_long"
    , {|:::: warning
content
:::
::::|}
    , 2 )
  ;;

  let example_lazy_continuation_1 =
    ( "lazy_continuation_1"
    , {|- foo
- bar:
::: two-example
```py
code1
```
:::|}
    , 1 )
  ;;

  let example_lazy_continuation_2 =
    ( "lazy_continuation_2"
    , {|::: two-example
- foo
- bar:
:::|}
    , 1 )
  ;;

  (** Loose list (blank line between items) with lazy continuation *)
  let example_lazy_continuation_loose =
    ( "lazy_continuation_loose"
    , {|- foo

- bar:
::: two-example
```py
code1
```
:::|}
    , 1 )
  ;;

  (** Fence absorbed into a middle item (not the last) *)
  let example_lazy_continuation_middle =
    ( "lazy_continuation_middle"
    , {|- foo:
::: warning
content
:::
- bar|}
    , 1 )
  ;;

  (** Multi-item prefix before the keyed last item *)
  let example_lazy_continuation_multi_prefix =
    ( "lazy_continuation_multi_prefix"
    , {|- aaa
- bbb
- ccc:
::: note
body
:::|}
    , 1 )
  ;;

  let examples =
    [ example_basic
    ; example_no_class
    ; example_nested_divs
    ; example_nested_divs_same_length
    ; example_EOF_closes
    ; example_extra_closing_fence
    ; non_example_less_than_3_colons
    ; non_example_div_does_not_interfere_with_code_blocks
    ; example_closing_fence_must_be_at_least_as_long
    ; example_lazy_continuation_1
    ; example_lazy_continuation_2
    ; example_lazy_continuation_loose
    ; example_lazy_continuation_middle
    ; example_lazy_continuation_multi_prefix
    ]
  ;;
end

let%test_module "Div" =
  (module struct
    open Common.For_test
    open For_test

    let doc_of_string s =
      let doc = Doc.of_string s in
      rewrite_doc doc
    ;;

    let pp_doc doc = mk_pp_doc ~blocks:[ sexp_of_block ] () doc

    let test (name, content, expected_n_div) =
      let doc = doc_of_string content in
      print_endline name;
      print_endline (String.make 10 '-');
      print_endline "```md {#original}";
      print_endline content;
      print_endline "```";
      print_endline "```sexp";
      pp_doc doc;
      print_endline "```";
      [%test_result: int] (count_div doc) ~expect:expected_n_div
    ;;

    let%expect_test _ =
      List.iter examples ~f:(fun x -> test x; print_endline "");
      [%expect.unreachable]
    [@@expect.uncaught_exn {|
      (* CR expect_test_collector: This test expectation appears to contain a backtrace.
         This is strongly discouraged as backtraces are fragile.
         Please change this test to not include a backtrace. *)
      (runtime-lib/runtime.ml.E "got unexpected result"
        ((expected 1) (got 0) (Loc pkg/oystermark/lib/parse/div.ml:581:21)))
      Raised at Ppx_assert_lib__Runtime.test_result in file "runtime-lib/runtime.ml", line 115, characters 27-83
      Called from Parse__Div.(fun).M.(fun) in file "pkg/oystermark/lib/parse/div.ml", line 585, characters 38-44
      Called from Base__List0.iter in file "src/list0.ml", line 66, characters 4-7
      Called from Parse__Div.(fun).M.(fun) in file "pkg/oystermark/lib/parse/div.ml", line 585, characters 6-63
      Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28

      Trailing output
      ---------------
      basic
      ----------
      ```md {#original}
      ::: warning
      Here is a paragraph.

      And here is another.
      :::
      ```
      ```sexp
      (Blocks
        (Div ((class_name (warning)) (colons 3))
          (Blocks (Paragraph (Text "Here is a paragraph.")) Blank_line
            (Paragraph (Text "And here is another.")))))
      ```

      no_class
      ----------
      ```md {#original}
      :::
      content
      :::

      ```
      ```sexp
      (Blocks (Div ((class_name ()) (colons 3)) (Paragraph (Text content)))
        Blank_line)
      ```

      nested_divs
      ----------
      ```md {#original}
      :::: outer
      ::: inner
      content
      :::
      ::::

      ```
      ```sexp
      (Blocks
        (Div ((class_name (outer)) (colons 4))
          (Div ((class_name (inner)) (colons 3)) (Paragraph (Text content))))
        Blank_line)
      ```

      nested_divs_same_length
      ----------
      ```md {#original}
      ::: warning
      content
      :::
      :::
      ```
      ```sexp
      (Blocks (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content)))
        (Div ((class_name ()) (colons 3)) (Blocks)))
      ```

      EOF_closes
      ----------
      ```md {#original}
      ::: warning
      unclosed content
      ```
      ```sexp
      (Blocks
        (Div ((class_name (warning)) (colons 3))
          (Paragraph (Text "unclosed content"))))
      ```

      extra_closing_fence
      ----------
      ```md {#original}
      ::: warning
      content
      :::
      :::
      ```
      ```sexp
      (Blocks (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content)))
        (Div ((class_name ()) (colons 3)) (Blocks)))
      ```

      less_than_3_colons
      ----------
      ```md {#original}
      :: not-a-div
      content
      ::
      ```
      ```sexp
      (Paragraph
        (Inlines (Text ":: not-a-div") (Break soft) (Text content) (Break soft)
          (Text ::)))
      ```

      div_does_not_interfere_with_code_blocks
      ----------
      ```md {#original}
      ```
      ::: not-a-div
      ```
      ```
      ```sexp
      (Code_block no-info "::: not-a-div")
      ```

      closing_fence_must_be_at_least_as_long
      ----------
      ```md {#original}
      :::: warning
      content
      :::
      ::::
      ```
      ```sexp
      (Blocks
        (Div ((class_name (warning)) (colons 4))
          (Blocks (Paragraph (Text content))
            (Div ((class_name ()) (colons 3)) (Blocks)))))
      ```

      lazy_continuation_1
      ----------
      ```md {#original}
      - foo
      - bar:
      ::: two-example
      ```py
      code1
      ```
      :::
      ```
      ```sexp
      (Blocks (List (Paragraph (Text foo)) (Paragraph (Text bar:)))
        (Div ((class_name (two-example)) (colons 3)) (Code_block py code1)))
      ```

      lazy_continuation_2
      ----------
      ```md {#original}
      ::: two-example
      - foo
      - bar:
      :::
      ```
      ```sexp
      (Blocks
        (Div ((class_name (two-example)) (colons 3))
          (List (Paragraph (Text foo)) (Paragraph (Text bar:)))))
      ```

      lazy_continuation_loose
      ----------
      ```md {#original}
      - foo

      - bar:
      ::: two-example
      ```py
      code1
      ```
      :::
      ```
      ```sexp
      (Blocks
        (List (Blocks (Paragraph (Text foo)) Blank_line) (Paragraph (Text bar:)))
        (Div ((class_name (two-example)) (colons 3)) (Code_block py code1)))
      ```

      lazy_continuation_middle
      ----------
      ```md {#original}
      - foo:
      ::: warning
      content
      :::
      - bar
      ```
      ```sexp
      (List
        (Paragraph
          (Inlines (Text foo:) (Break soft) (Text "::: warning") (Break soft)
            (Text content) (Break soft) (Text :::)))
        (Paragraph (Text bar)))
      ```
      |}]
    ;;

    let%test_unit "roundtrip: commonmark output is idempotent" =
      List.iter
        (List.map examples ~f:(fun (_, content, _) -> content))
        ~f:(commonmark_of_doc_idempotent ~doc_of_string ~commonmark_of_doc)
    ;;
  end)
;;
