(** {1 Struct.For_test} *)

open Core
open Cmarkit
open Common.For_test

let rewrite_doc = Cmarkit.Struct.rewrite_doc

let sexp_of_block : Common.block_sexp =
  fun ~recurse_inline ~recurse_block ~with_meta b ->
  match b with
  | Block.Ext_keyed ((label, body), meta) ->
    Some
      (with_meta
         meta
         (Sexp.List [ Atom "Keyed"; recurse_inline label; recurse_block body ]))
  | _ -> None
;;

let debug_block_renderer : Cmarkit_renderer.block =
  let open Cmarkit_renderer in
  fun (c : context) (b : Block.t) ->
    match b with
    | Block.Ext_keyed ((label, body), _) ->
      Context.string c "K(";
      Context.inline c label;
      Context.string c ", ";
      Context.block c body;
      Context.string c ")";
      true
    | Block.List (l, _) ->
      Context.string c "List[";
      List.iteri (Block.List'.items l) ~f:(fun i (item, _) ->
        if i > 0 then Context.string c ", ";
        Context.block c (Block.List_item.block item));
      Context.string c "]";
      true
    | _ -> false
;;

let count_keyed (doc : Cmarkit.Doc.t) : int =
  let folder =
    Cmarkit.Folder.make
      ~block:(fun f acc -> function
         | Block.Ext_keyed ((_, b), _) ->
           Cmarkit.Folder.ret (1 + Cmarkit.Folder.fold_block f acc b)
         | _ -> Cmarkit.Folder.default)
      ()
  in
  Cmarkit.Folder.fold_doc folder 0 doc
;;

let doc_of_string ?paragraph_inline_value s =
  (* The Struct pass keys on colon segments the inline parser attaches to a
     paragraph's meta, which only happens in extension mode ([strict:false]). *)
  let doc = Doc.of_string ~strict:false s in
  rewrite_doc ?paragraph_inline_value doc
;;

let pp_doc_sexp ppf doc = mk_pp_doc ~blocks:[ sexp_of_block ] () ppf doc

let pp_doc_debug ppf doc =
  let r =
    Cmarkit_renderer.compose
      (Cmarkit_commonmark.renderer ())
      (Cmarkit_renderer.make ~block:debug_block_renderer ())
  in
  Cmarkit_renderer.doc_to_string r doc |> Format.fprintf ppf "%s@\n"
;;

(** {1 Examples} *)
type example =
  { name : string
  ; content : string
  }

let mk_example name content : example = { name; content }

let expect_example ?paragraph_inline_value s =
  s |> printf "```md {#original}\n%s\n```\n";
  print_endline "```debug-view";
  s |> doc_of_string ?paragraph_inline_value |> Format.printf "%a%!" pp_doc_debug;
  print_endline "```";
  print_endline "```sexp";
  s |> doc_of_string |> Format.printf "%a%!" pp_doc_sexp;
  print_endline "```"
;;

(* Basics *)

(** [- foo: bar] → inline-value list item *)
let inline_value_list_item = mk_example "inline_value_list_item" {|- foo: bar|}

(** [- foo:] with indented body *)
let keyed_list_item_with_indented_content =
  mk_example
    "keyed_list_item_with_indented_content"
    {|- foo:
  - bar
  - baz|}
;;

(** [- foo:] alone (no body, no following) → not keyed *)
let no_body_no_following = mk_example "no_body_no_following" {|- foo:|}

(* Chains *)

(** [- a: b: c] — chain of 2 labels + inline value *)
let chain_with_value = mk_example "chain_with_value" {|- a: b: c|}

(** [- a: b:] with indented body — chain in trailing-colon form *)
let colon_chain_inline_keying =
  mk_example
    "colon_chain_inline_keying"
    {|- foo: bar:
  - baz|}
;;

(** [- a: b: c:] with indented body — chain of 3 labels *)
let three_label_chain =
  mk_example
    "three_label_chain"
    {|- a: b: c:
  - baz|}
;;

(** two independent inline-value siblings — no cross-item absorption *)
let two_independent_siblings =
  mk_example
    "two_independent_siblings"
    {|- a: b: c
- x: y|}
;;

(** Three levels: A is keyed around the list; each item is keyed
      with an inline value. *)
let three_levels =
  mk_example
    "three_levels"
    {|A:
- B: b
- C: c|}
;;

(** Four levels: A -> B -> b -> C.  The first item's trailing colon ([b:]) makes [b]
a label, and [b] absorbs the following [C: c] sibling as its nested body. *)
let four_levels =
  mk_example
    "four_levels"
    {|A:
- B: b:
- C: c|}
;;

(* Paragraphs *)

(** [foo: bar] standalone paragraph → inline-value keyed block *)
let paragraph_inline_value = mk_example "paragraph_inline_value" {|foo: bar|}

(** same, with [paragraph_inline_value = false] → not keyed *)
let paragraph_inline_value_disabled =
  mk_example "paragraph_inline_value_disabled" {|foo: bar|}
;;

(** trailing-colon paragraph with following list, then blank *)
let keyed_paragraph =
  mk_example
    "keyed_paragraph"
    {|foo:
- bar
- baz

bee|}
;;

(** trailing-colon paragraph absorbs multiple contiguous blocks *)
let keyed_paragraph_multiple_children =
  mk_example
    "keyed_paragraph_multiple_children"
    {|foo:
- bar
- baz
some text|}
;;

(** [foo: bar:] paragraph + indented body — chain on paragraph *)
let paragraph_chain =
  mk_example
    "paragraph_chain"
    {|foo: bar:
- baz|}
;;

(** keyed paragraph containing nested keyed items *)
let nesting =
  mk_example
    "nesting"
    {|foo:
- bar:
  - baz
- qux|}
;;

(* Value contents *)

(** value may contain mixed inline content *)
let mixed_inline_value = mk_example "mixed_inline_value" {|- foo: bar *baz* qux|}

(** code span in value — only [foo] is the label, code interior is opaque *)
let code_span_in_value = mk_example "code_span_in_value" {|- foo: `code: thing`|}

(** trailing space after colon — cross-line absorption does not fire *)
let non_example_trailing_space =
  mk_example "non_example_trailing_space" "- foo: \nfollowing"
;;

(** no space after colon → not keyed *)
let non_example_no_space_after_colon =
  mk_example "non_example_no_space_after_colon" {|- foo:bar|}
;;

(** URL label — [://] has no space, so only one split on [": "] *)
let url_as_label = mk_example "url_as_label" {|- http://x.com: click here|}

(* Escapes *)

(** [\\\\:] in source → [\\:] in AST → escaped, not keyed *)
let escaped_colon =
  mk_example
    "escaped_colon"
    {|- foo\\:
- bar|}
;;

(* Emphasis / labels *)

(** emphasis label with trailing-colon form *)
let emphasis_keyed_item =
  mk_example
    "emphasis_keyed_item"
    {|- *foo*:
  - bar
  - baz|}
;;

(** emphasis label with inline-value form *)
let emphasis_inline_value = mk_example "emphasis_inline_value" {|- *foo*: bar|}

(** emphasis label in chain (trailing-colon form) *)
let emphasis_chain =
  mk_example
    "emphasis_chain"
    {|- *foo*: bar:
  - baz|}
;;

(** mixed-inline paragraph — not keyed because label segment is mixed *)
let non_example_mixed_inline =
  mk_example
    "non_example_mixed_inline"
    {|*foo* bar:
following|}
;;

(** mixed-inline list item — not keyed *)
let non_example_mixed_inline_list_item =
  mk_example "non_example_mixed_inline_list_item" {|- *foo* x: bar|}
;;

(** value is unrestricted — mixed inline in value is fine *)
let free_form_value = mk_example "free_form_value" {|- foo: *bar* x|}

let autolink = mk_example "autolink" {|- <http://foo.bar.baz>: bat|}

(* Trailing colon on last item *)

(** blank line between trailing-colon item and following block → no absorption *)
let non_example_blank_line =
  mk_example
    "non_example_blank_line"
    {|- foo:

bar|}
;;

(** [b:] absorbs following [text] even when a non-keyed sibling precedes it *)
let last_item_absorbs_following_text =
  mk_example
    "last_item_absorbs_following_text"
    {|- a
- b:
text|}
;;

(** same as §21, but the preceding sibling has an inline value *)
let last_item_absorbs_following_text_2 =
  mk_example
    "last_item_absorbs_following_text_2"
    {|- a: x
- b:
text|}
;;

(** last item absorbs a contiguous code-fence block *)
let keyed_list_item_with_contiguous_blocks =
  mk_example
    "keyed_list_item_with_contiguous_blocks"
    {|- foo:
```
bar
```|}
;;

(** absorbed code fence does not interrupt the list — [baz] is a
    continuation sibling of [foo] and [bar:] *)
let list_continuation_across_absorbed_block =
  mk_example
    "list_continuation_across_absorbed_block"
    {|- foo
- bar:
```py
cb
```
- baz|}
;;

(** continuation cascades: the spliced-in last item itself absorbs
    a code fence, and the next same-type list continues again *)
let list_continuation_cascades =
  mk_example
    "list_continuation_cascades"
    {|- a
- b:
```
cb1
```
- c:
```
cb2
```
- d|}
;;

(** an ordered list does not continue an unordered one across
    absorbed content *)
let list_continuation_type_mismatch =
  mk_example
    "list_continuation_type_mismatch"
    {|- a:
```
cb
```
1. b|}
;;

(* Empty label *)
let empty_label_in_list_item =
  mk_example
    "empty_label_in_list_item"
    {|- : a
- B: b|}
;;

let empty_label_nested_under_paragraph_key =
  mk_example
    "empty_label_nested_under_paragraph_key"
    {|A:
- : a
- B: b|}
;;

let empty_label_with_trailing_colon_and_body =
  mk_example
    "empty_label_with_trailing_colon_and_body"
    {|- :
  - nested|}
;;

(* Non-examples *)

let non_example_no_colon =
  mk_example
    "non_example_no_colon"
    {|- foo
- bar|}
;;

(** code span is opaque — colon inside code does not trigger keying *)
let non_example_colon_in_code_span =
  mk_example
    "non_example_colon_in_code_span"
    {|text with `code:`
following paragraph|}
;;

let examples =
  [ (* Basics *)
    inline_value_list_item
  ; keyed_list_item_with_indented_content
  ; three_levels
  ; four_levels
  ; no_body_no_following (* Chains *)
  ; chain_with_value
  ; colon_chain_inline_keying
  ; three_label_chain
  ; two_independent_siblings (* Paragraphs *)
  ; paragraph_inline_value
  ; keyed_paragraph
  ; keyed_paragraph_multiple_children
  ; paragraph_chain
  ; nesting (* Value contents *)
  ; mixed_inline_value
  ; code_span_in_value
  ; non_example_trailing_space
  ; non_example_no_space_after_colon
  ; url_as_label (* Escapes *)
  ; escaped_colon (* Emphasis / labels *)
  ; emphasis_keyed_item
  ; emphasis_inline_value
  ; emphasis_chain
  ; non_example_mixed_inline
  ; non_example_mixed_inline_list_item
  ; autolink
  ; free_form_value (* Trailing colon on last item *)
  ; non_example_blank_line
  ; last_item_absorbs_following_text
  ; last_item_absorbs_following_text_2
  ; keyed_list_item_with_contiguous_blocks
  ; list_continuation_across_absorbed_block
  ; list_continuation_cascades
  ; list_continuation_type_mismatch
  ; empty_label_in_list_item
  ; empty_label_nested_under_paragraph_key
  ; empty_label_with_trailing_colon_and_body (* General non-examples *)
  ; non_example_no_colon
  ; non_example_colon_in_code_span
  ]
;;

let%test_module _ =
  (module struct
    let%expect_test _ =
      examples
      |> List.iteri ~f:(fun i ex ->
        print_endline {%string|Example %{i+1#Int}: %{ex.name}|};
        print_endline (String.make 10 '-');
        ex.content |> expect_example;
        print_endline "");
      [%expect
        {|
        Example 1: inline_value_list_item
        ----------
        ```md {#original}
        - foo: bar
        ```
        ```debug-view
        List[K(foo: , bar)]
        ```
        ```sexp
        (List (Keyed (Text "foo: ") (Paragraph (Text bar))))
        ```

        Example 2: keyed_list_item_with_indented_content
        ----------
        ```md {#original}
        - foo:
          - bar
          - baz
        ```
        ```debug-view
        List[K(foo:, List[bar,
        baz])]
        ```
        ```sexp
        (List
          (Keyed (Text foo:) (List (Paragraph (Text bar)) (Paragraph (Text baz)))))
        ```

        Example 3: three_levels
        ----------
        ```md {#original}
        A:
        - B: b
        - C: c
        ```
        ```debug-view
        K(A:, List[K(B: , b), K(C: ,
        c)])
        ```
        ```sexp
        (Blocks
          (Keyed (Text A:)
            (List (Keyed (Text "B: ") (Paragraph (Text b)))
              (Keyed (Text "C: ") (Paragraph (Text c))))))
        ```

        Example 4: four_levels
        ----------
        ```md {#original}
        A:
        - B: b:
        - C: c
        ```
        ```debug-view
        K(A:, List[K(B: , K(b:, List[K(C: , c)]))])
        ```
        ```sexp
        (Blocks
          (Keyed (Text A:)
            (List
              (Keyed (Text "B: ")
                (Keyed (Text b:) (List (Keyed (Text "C: ") (Paragraph (Text c)))))))))
        ```

        Example 5: no_body_no_following
        ----------
        ```md {#original}
        - foo:
        ```
        ```debug-view
        List[foo:]
        ```
        ```sexp
        (List (Paragraph (Text foo:)))
        ```

        Example 6: chain_with_value
        ----------
        ```md {#original}
        - a: b: c
        ```
        ```debug-view
        List[K(a: , K(b: , c))]
        ```
        ```sexp
        (List (Keyed (Text "a: ") (Keyed (Text "b: ") (Paragraph (Text c)))))
        ```

        Example 7: colon_chain_inline_keying
        ----------
        ```md {#original}
        - foo: bar:
          - baz
        ```
        ```debug-view
        List[K(foo: , K(bar:, List[baz]))]
        ```
        ```sexp
        (List
          (Keyed (Text "foo: ") (Keyed (Text bar:) (List (Paragraph (Text baz))))))
        ```

        Example 8: three_label_chain
        ----------
        ```md {#original}
        - a: b: c:
          - baz
        ```
        ```debug-view
        List[K(a: , K(b: , K(c:, List[baz])))]
        ```
        ```sexp
        (List
          (Keyed (Text "a: ")
            (Keyed (Text "b: ") (Keyed (Text c:) (List (Paragraph (Text baz)))))))
        ```

        Example 9: two_independent_siblings
        ----------
        ```md {#original}
        - a: b: c
        - x: y
        ```
        ```debug-view
        List[K(a: , K(b: , c)), K(x: ,
        y)]
        ```
        ```sexp
        (List (Keyed (Text "a: ") (Keyed (Text "b: ") (Paragraph (Text c))))
          (Keyed (Text "x: ") (Paragraph (Text y))))
        ```

        Example 10: paragraph_inline_value
        ----------
        ```md {#original}
        foo: bar
        ```
        ```debug-view
        K(foo: , bar)
        ```
        ```sexp
        (Keyed (Text "foo: ") (Paragraph (Text bar)))
        ```

        Example 11: keyed_paragraph
        ----------
        ```md {#original}
        foo:
        - bar
        - baz

        bee
        ```
        ```debug-view
        K(foo:, List[bar,
        baz])

        bee
        ```
        ```sexp
        (Blocks
          (Keyed (Text foo:) (List (Paragraph (Text bar)) (Paragraph (Text baz))))
          Blank_line (Paragraph (Text bee)))
        ```

        Example 12: keyed_paragraph_multiple_children
        ----------
        ```md {#original}
        foo:
        - bar
        - baz
        some text
        ```
        ```debug-view
        K(foo:, List[bar,
        baz
        some text])
        ```
        ```sexp
        (Blocks
          (Keyed (Text foo:)
            (List (Paragraph (Text bar))
              (Paragraph (Inlines (Text baz) (Break soft) (Text "some text"))))))
        ```

        Example 13: paragraph_chain
        ----------
        ```md {#original}
        foo: bar:
        - baz
        ```
        ```debug-view
        K(foo: , K(bar:, List[baz]))
        ```
        ```sexp
        (Blocks
          (Keyed (Text "foo: ") (Keyed (Text bar:) (List (Paragraph (Text baz))))))
        ```

        Example 14: nesting
        ----------
        ```md {#original}
        foo:
        - bar:
          - baz
        - qux
        ```
        ```debug-view
        K(foo:, List[K(bar:, List[baz]),
        qux])
        ```
        ```sexp
        (Blocks
          (Keyed (Text foo:)
            (List (Keyed (Text bar:) (List (Paragraph (Text baz))))
              (Paragraph (Text qux)))))
        ```

        Example 15: mixed_inline_value
        ----------
        ```md {#original}
        - foo: bar *baz* qux
        ```
        ```debug-view
        List[K(foo: , bar *baz* qux)]
        ```
        ```sexp
        (List
          (Keyed (Text "foo: ")
            (Paragraph (Inlines (Text "bar ") (Emphasis (Text baz)) (Text " qux")))))
        ```

        Example 16: code_span_in_value
        ----------
        ```md {#original}
        - foo: `code: thing`
        ```
        ```debug-view
        List[K(foo: , `code: thing`)]
        ```
        ```sexp
        (List (Keyed (Text "foo: ") (Paragraph (Code_span "code: thing"))))
        ```

        Example 17: non_example_trailing_space
        ----------
        ```md {#original}
        - foo:
        following
        ```
        ```debug-view
        List[foo:
        following]
        ```
        ```sexp
        (List (Paragraph (Inlines (Text foo:) (Break soft) (Text following))))
        ```

        Example 18: non_example_no_space_after_colon
        ----------
        ```md {#original}
        - foo:bar
        ```
        ```debug-view
        List[foo:bar]
        ```
        ```sexp
        (List (Paragraph (Text foo:bar)))
        ```

        Example 19: url_as_label
        ----------
        ```md {#original}
        - http://x.com: click here
        ```
        ```debug-view
        List[K(http://x.com: , click here)]
        ```
        ```sexp
        (List (Keyed (Text "http://x.com: ") (Paragraph (Text "click here"))))
        ```

        Example 20: escaped_colon
        ----------
        ```md {#original}
        - foo\\:
        - bar
        ```
        ```debug-view
        List[foo\\:,
        bar]
        ```
        ```sexp
        (List (Paragraph (Text "foo\\:")) (Paragraph (Text bar)))
        ```

        Example 21: emphasis_keyed_item
        ----------
        ```md {#original}
        - *foo*:
          - bar
          - baz
        ```
        ```debug-view
        List[K(*foo*:, List[bar,
        baz])]
        ```
        ```sexp
        (List
          (Keyed (Inlines (Emphasis (Text foo)) (Text :))
            (List (Paragraph (Text bar)) (Paragraph (Text baz)))))
        ```

        Example 22: emphasis_inline_value
        ----------
        ```md {#original}
        - *foo*: bar
        ```
        ```debug-view
        List[K(*foo*: , bar)]
        ```
        ```sexp
        (List
          (Keyed (Inlines (Emphasis (Text foo)) (Text ": ")) (Paragraph (Text bar))))
        ```

        Example 23: emphasis_chain
        ----------
        ```md {#original}
        - *foo*: bar:
          - baz
        ```
        ```debug-view
        List[K(*foo*: , K(bar:, List[baz]))]
        ```
        ```sexp
        (List
          (Keyed (Inlines (Emphasis (Text foo)) (Text ": "))
            (Keyed (Text bar:) (List (Paragraph (Text baz))))))
        ```

        Example 24: non_example_mixed_inline
        ----------
        ```md {#original}
        *foo* bar:
        following
        ```
        ```debug-view
        *foo* bar:
        following
        ```
        ```sexp
        (Paragraph
          (Inlines (Emphasis (Text foo)) (Text " bar:") (Break soft)
            (Text following)))
        ```

        Example 25: non_example_mixed_inline_list_item
        ----------
        ```md {#original}
        - *foo* x: bar
        ```
        ```debug-view
        List[*foo* x: bar]
        ```
        ```sexp
        (List (Paragraph (Inlines (Emphasis (Text foo)) (Text " x: bar"))))
        ```

        Example 26: autolink
        ----------
        ```md {#original}
        - <http://foo.bar.baz>: bat
        ```
        ```debug-view
        List[K(<http://foo.bar.baz>: , bat)]
        ```
        ```sexp
        (List
          (Keyed (Inlines (Autolink http://foo.bar.baz) (Text ": "))
            (Paragraph (Text bat))))
        ```

        Example 27: free_form_value
        ----------
        ```md {#original}
        - foo: *bar* x
        ```
        ```debug-view
        List[K(foo: , *bar* x)]
        ```
        ```sexp
        (List
          (Keyed (Text "foo: ")
            (Paragraph (Inlines (Emphasis (Text bar)) (Text " x")))))
        ```

        Example 28: non_example_blank_line
        ----------
        ```md {#original}
        - foo:

        bar
        ```
        ```debug-view
        List[foo:]

        bar
        ```
        ```sexp
        (Blocks (List (Paragraph (Text foo:))) Blank_line (Paragraph (Text bar)))
        ```

        Example 29: last_item_absorbs_following_text
        ----------
        ```md {#original}
        - a
        - b:
        text
        ```
        ```debug-view
        List[a,
        b:
        text]
        ```
        ```sexp
        (List (Paragraph (Text a))
          (Paragraph (Inlines (Text b:) (Break soft) (Text text))))
        ```

        Example 30: last_item_absorbs_following_text_2
        ----------
        ```md {#original}
        - a: x
        - b:
        text
        ```
        ```debug-view
        List[K(a: , x),
        b:
        text]
        ```
        ```sexp
        (List (Keyed (Text "a: ") (Paragraph (Text x)))
          (Paragraph (Inlines (Text b:) (Break soft) (Text text))))
        ```

        Example 31: keyed_list_item_with_contiguous_blocks
        ----------
        ```md {#original}
        - foo:
        ```
        bar
        ```
        ```
        ```debug-view
        List[K(foo:, ```
        bar
        ```)]
        ```
        ```sexp
        (Blocks (List (Keyed (Text foo:) (Code_block no-info bar))))
        ```

        Example 32: list_continuation_across_absorbed_block
        ----------
        ```md {#original}
        - foo
        - bar:
        ```py
        cb
        ```
        - baz
        ```
        ```debug-view
        List[foo, K(bar:,
        ```py
        cb
        ```List[
        baz])]
        ```
        ```sexp
        (Blocks
          (List (Paragraph (Text foo))
            (Keyed (Text bar:)
              (Blocks (Code_block py cb) (List (Paragraph (Text baz)))))))
        ```

        Example 33: list_continuation_cascades
        ----------
        ```md {#original}
        - a
        - b:
        ```
        cb1
        ```
        - c:
        ```
        cb2
        ```
        - d
        ```
        ```debug-view
        List[a, K(b:,
        ```
        cb1
        ```List[K(c:,
        ```
        cb2
        ```List[
        d])])]
        ```
        ```sexp
        (Blocks
          (List (Paragraph (Text a))
            (Keyed (Text b:)
              (Blocks (Code_block no-info cb1)
                (List
                  (Keyed (Text c:)
                    (Blocks (Code_block no-info cb2) (List (Paragraph (Text d))))))))))
        ```

        Example 34: list_continuation_type_mismatch
        ----------
        ```md {#original}
        - a:
        ```
        cb
        ```
        1. b
        ```
        ```debug-view
        List[K(a:, ```
        cb
        ```List[
        b])]
        ```
        ```sexp
        (Blocks
          (List
            (Keyed (Text a:)
              (Blocks (Code_block no-info cb) (List (Paragraph (Text b)))))))
        ```

        Example 35: empty_label_in_list_item
        ----------
        ```md {#original}
        - : a
        - B: b
        ```
        ```debug-view
        List[K(: , a), K(B: ,
        b)]
        ```
        ```sexp
        (List (Keyed (Text ": ") (Paragraph (Text a)))
          (Keyed (Text "B: ") (Paragraph (Text b))))
        ```

        Example 36: empty_label_nested_under_paragraph_key
        ----------
        ```md {#original}
        A:
        - : a
        - B: b
        ```
        ```debug-view
        K(A:, List[K(: , a), K(B: ,
        b)])
        ```
        ```sexp
        (Blocks
          (Keyed (Text A:)
            (List (Keyed (Text ": ") (Paragraph (Text a)))
              (Keyed (Text "B: ") (Paragraph (Text b))))))
        ```

        Example 37: empty_label_with_trailing_colon_and_body
        ----------
        ```md {#original}
        - :
          - nested
        ```
        ```debug-view
        List[K(:, List[nested])]
        ```
        ```sexp
        (List (Keyed (Text :) (List (Paragraph (Text nested)))))
        ```

        Example 38: non_example_no_colon
        ----------
        ```md {#original}
        - foo
        - bar
        ```
        ```debug-view
        List[foo,
        bar]
        ```
        ```sexp
        (List (Paragraph (Text foo)) (Paragraph (Text bar)))
        ```

        Example 39: non_example_colon_in_code_span
        ----------
        ```md {#original}
        text with `code:`
        following paragraph
        ```
        ```debug-view
        text with `code:`
        following paragraph
        ```
        ```sexp
        (Paragraph
          (Inlines (Text "text with ") (Code_span code:) (Break soft)
            (Text "following paragraph")))
        ```
        |}]
    ;;
  end)
;;

let%test_module "paragraph_inline_value" =
  (module struct
    let%expect_test "enabled (default)" =
      (* Standalone paragraph with inline value is keyed. *)
      {|foo: bar|} |> expect_example;
      [%expect
        {|
        ```md {#original}
        foo: bar
        ```
        ```debug-view
        K(foo: , bar)
        ```
        ```sexp
        (Keyed (Text "foo: ") (Paragraph (Text bar)))
        ```
        |}]
    ;;

    let%expect_test "with chain" =
      (* Paragraph chain without trailing-colon absorption. *)
      {|a: b: c|} |> doc_of_string |> Format.printf "%a%!" pp_doc_sexp;
      [%expect {| (Keyed (Text "a: ") (Keyed (Text "b: ") (Paragraph (Text c)))) |}]
    ;;

    let%expect_test "disabled" =
      (* When paragraph_inline_value is false, standalone paragraph
         is not keyed. *)
      {|foo: bar|} |> expect_example ~paragraph_inline_value:false;
      [%expect
        {|
        ```md {#original}
        foo: bar
        ```
        ```debug-view
        foo: bar
        ```
        ```sexp
        (Keyed (Text "foo: ") (Paragraph (Text bar)))
        ```
        |}]
    ;;

    let%expect_test "disabled does not affect trailing-colon" =
      (* Trailing-colon paragraph absorption still works even when
         paragraph_inline_value is false. *)
      {|foo:
- bar|}
      |> expect_example ~paragraph_inline_value:false;
      [%expect
        {|
        ```md {#original}
        foo:
        - bar
        ```
        ```debug-view
        K(foo:, List[bar])
        ```
        ```sexp
        (Blocks (Keyed (Text foo:) (List (Paragraph (Text bar)))))
        ```
        |}]
    ;;
  end)
;;
