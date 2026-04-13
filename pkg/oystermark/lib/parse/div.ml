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

module Split_paragraph : sig
  (** Split a paragraph into multiple blocks when fence lines are mixed in
      with regular content.  Returns [None] if no splitting is needed. *)
  val split_paragraph_fences
    :  Cmarkit.Block.Paragraph.t
    -> Cmarkit.Meta.t
    -> Cmarkit.Block.t list option
end = struct
  let rec flatten_inlines (i : Cmarkit.Inline.t) : Cmarkit.Inline.t list =
    match i with
    | Cmarkit.Inline.Inlines (is, _) -> List.concat_map is ~f:flatten_inlines
    | other -> [ other ]
  ;;

  let split_at_soft_breaks nodes =
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

  let group_is_fence = function
    | [ Cmarkit.Inline.Text (s, _) ] -> Option.is_some (parse_fence s)
    | _ -> false
  ;;

  let split_paragraph_fences (p : Cmarkit.Block.Paragraph.t) (meta : Cmarkit.Meta.t)
    : Cmarkit.Block.t list option
    =
    let inline = Cmarkit.Block.Paragraph.inline p in
    let lines = split_at_soft_breaks (flatten_inlines inline) in
    if List.length lines <= 1
    then None
    else if not (List.exists lines ~f:group_is_fence)
    then None
    else
      Some
        (List.mapi lines ~f:(fun i group ->
           let para_inline =
             match group with
             | [ single ] -> single
             | multiple -> Cmarkit.Inline.Inlines (multiple, Cmarkit.Meta.none)
           in
           let para = Cmarkit.Block.Paragraph.make para_inline in
           let m = if i = 0 then meta else Cmarkit.Meta.none in
           Cmarkit.Block.Paragraph (para, m)))
  ;;
end

(** Pass 1 of 2: walk the block tree and split paragraphs that contain fence lines
    mixed with other content. After this pass every fence is a standalone
    single-[Text] paragraph.

    Returns a list because splitting a paragraph or list may produce multiple
    sibling blocks. *)
let rec extract_fences (block : Cmarkit.Block.t) : Cmarkit.Block.t list =
  match block with
  | Cmarkit.Block.Blocks (blocks, meta) ->
    [ Cmarkit.Block.Blocks (List.concat_map blocks ~f:extract_fences, meta) ]
  | Cmarkit.Block.Block_quote (bq, meta) ->
    let inner = Cmarkit.Block.Block_quote.block bq in
    let inner' =
      match extract_fences inner with
      | [ single ] -> single
      | multiple -> Cmarkit.Block.Blocks (multiple, Cmarkit.Meta.none)
    in
    [ Cmarkit.Block.Block_quote (Cmarkit.Block.Block_quote.make inner', meta) ]
  | Cmarkit.Block.List (l, list_meta) -> extract_fences_from_list l list_meta
  | Cmarkit.Block.Paragraph (p, meta) ->
    (match Split_paragraph.split_paragraph_fences p meta with
     | Some blocks -> blocks
     | None -> [ block ])
  | _ -> [ block ]

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
  let try_split_item item =
    let try_split_paragraph p pmeta ~wrap_item =
      match Split_paragraph.split_paragraph_fences p pmeta with
      | None -> None
      | Some [] -> None
      | Some (first :: extracted) -> Some (wrap_item first, extracted)
    in
    match Cmarkit.Block.List_item.block item with
    | Cmarkit.Block.Paragraph (p, pmeta) ->
      try_split_paragraph p pmeta ~wrap_item:Fun.id
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
      try_split_paragraph p pmeta ~wrap_item
    | _ -> None
  in
  (* Find the first item whose paragraph contains fence lines. *)
  let rec find_split prefix = function
    | [] -> None
    | (item, item_meta) :: rest ->
      (match try_split_item item with
       | Some (new_block, extracted) ->
         let new_item = rebuild_item item new_block in
         Some (List.rev ((new_item, item_meta) :: prefix), extracted, rest)
       | None -> find_split ((item, item_meta) :: prefix) rest)
  in
  match find_split [] items with
  | None -> [ list_block ]
  | Some (before_items, extracted, after_items) ->
    let after =
      match after_items with
      | [] -> []
      | _ ->
        let after_l =
          Cmarkit.Block.List'.make
            ~tight:(Cmarkit.Block.List'.tight l)
            (Cmarkit.Block.List'.type' l)
            after_items
        in
        extract_fences_from_list after_l list_meta
    in
    rebuild_list before_items :: (extracted @ after)
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
    Pass 2 of 2: match fences and group children into Ext_div *)
let rec rewrite_block_list (blocks : Cmarkit.Block.t list) : Cmarkit.Block.t list =
  match blocks with
  | [] -> []
  | block :: rest ->
    (match paragraph_fence block with
     | Some (colons, class_name) ->
       let body_blocks, remaining = collect_body colons rest in
       let body_blocks = rewrite_block_list body_blocks in
       let body_blocks = strip_surrounding_blanks body_blocks in
       let body =
         match body_blocks with
         | [] -> Cmarkit.Block.Blocks ([], Cmarkit.Meta.none)
         | [ single ] -> single
         | multiple -> Cmarkit.Block.Blocks (multiple, Cmarkit.Meta.none)
       in
       Ext_div ({ class_name; colons }, body) :: rewrite_block_list remaining
     | None -> rewrite_within_block block :: rewrite_block_list rest)

(** Collect blocks until a closing fence matching [open_colons] is found.
    Tracks nested named opening fences (as a list used as a stack) so their
    matching closing fences are not mistaken for ours.  Skips one trailing
    blank line after the closing fence to prevent roundtrip accumulation. *)
and collect_body (open_colons : int) (blocks : Cmarkit.Block.t list)
  : Cmarkit.Block.t list * Cmarkit.Block.t list
  =
  let rec go nesting acc = function
    | [] -> List.rev acc, []
    | block :: rest ->
      (match paragraph_fence block with
       | Some (colons, Some _) ->
         go (colons :: nesting) (block :: acc) rest
       | Some (colons, None) ->
         (match nesting with
          | top :: nesting_rest when colons >= top ->
            go nesting_rest (block :: acc) rest
          | _ when colons >= open_colons ->
            let rest =
              match rest with
              | b :: rest' when is_blank_line b -> rest'
              | _ -> rest
            in
            List.rev acc, rest
          | _ -> go nesting (block :: acc) rest)
       | None -> go nesting (block :: acc) rest)
  in
  go [] [] blocks

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
  let block' =
    match extract_fences block with
    | [ single ] -> single
    | multiple -> Cmarkit.Block.Blocks (multiple, Cmarkit.Meta.none)
  in
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
::: foo
```py
code1
```
:::|}
    , 1 )
  ;;

  let example_lazy_continuation_2 =
    ( "lazy_continuation_2"
    , {|::: foo
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
::: foo
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
      [%expect {|
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
        (Blocks (Div ((class_name ()) (colons 3)) (Paragraph (Text content))))
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
            (Div ((class_name (inner)) (colons 3)) (Paragraph (Text content)))))
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
        ::: foo
        ```py
        code1
        ```
        :::
        ```
        ```sexp
        (Blocks (List (Paragraph (Text foo)) (Paragraph (Text bar:)))
          (Div ((class_name (foo)) (colons 3)) (Code_block py code1)))
        ```

        lazy_continuation_2
        ----------
        ```md {#original}
        ::: foo
        - foo
        - bar:
        :::
        ```
        ```sexp
        (Blocks
          (Div ((class_name (foo)) (colons 3))
            (List (Paragraph (Text foo)) (Paragraph (Text bar:)))))
        ```

        lazy_continuation_loose
        ----------
        ```md {#original}
        - foo

        - bar:
        ::: foo
        ```py
        code1
        ```
        :::
        ```
        ```sexp
        (Blocks
          (List (Blocks (Paragraph (Text foo)) Blank_line) (Paragraph (Text bar:)))
          (Div ((class_name (foo)) (colons 3)) (Code_block py code1)))
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
        (Blocks (List (Paragraph (Text foo:)))
          (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content)))
          (List (Paragraph (Text bar))))
        ```

        lazy_continuation_multi_prefix
        ----------
        ```md {#original}
        - aaa
        - bbb
        - ccc:
        ::: note
        body
        :::
        ```
        ```sexp
        (Blocks
          (List (Paragraph (Text aaa)) (Paragraph (Text bbb))
            (Paragraph (Text ccc:)))
          (Div ((class_name (note)) (colons 3)) (Paragraph (Text body))))
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
