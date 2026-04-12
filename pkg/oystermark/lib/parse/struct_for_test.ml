(** {0 Struct.For_test} *)

open Core
open Cmarkit
open Struct_common
open Common.For_test

let doc_of_string ?paragraph_inline_value s =
  let doc = Doc.of_string s in
  rewrite_doc ?paragraph_inline_value doc
;;

let pp_doc_sexp doc = mk_pp_doc ~blocks:[ sexp_of_block ] () doc

let pp_doc_debug doc =
  let r =
    Cmarkit_renderer.compose
      (Cmarkit_commonmark.renderer ())
      (Cmarkit_renderer.make ~block:debug_block_renderer ())
  in
  Cmarkit_renderer.doc_to_string r doc |> print_endline
;;

let block_ext_fold : (Block.t, 'a) Folder.fold =
  fun f acc b ->
  match b with
  | Ext_keyed_block (_, body) | Ext_keyed_list_item (_, body) ->
    Folder.fold_block f acc body
  | _ -> acc
;;

(** {1 Predicates} *)

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
     | Block.Paragraph (p, _) -> is_trailing_colon_absorbable (Block.Paragraph.inline p)
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

(** {1 Examples} *)
type example =
  { name : string
  ; content : string
  }

let mk_example name content : example = { name; content }

let expect_example ex : unit =
  ex.name |> printf "- name: %s\n";
  ex.content |> printf "- content: \n```md\n%s\n```\n";
  print_endline "```sexp";
  ex.content |> doc_of_string |> pp_doc_sexp;
  print_endline "```"
;;

(* Basics — spec §1-4 *)

(** spec §1: [- foo: bar] → inline-value list item *)
let inline_value_list_item = mk_example "inline_value_list_item" {|- foo: bar|}

(** spec §2: [- foo:] with indented body *)
let keyed_list_item_with_indented_content =
  mk_example
    "keyed_list_item_with_indented_content"
    {|- foo:
  - bar
  - baz|}
;;

(** spec §3: [- foo:] alone (no body, no following) → not keyed *)
let no_body_no_following = mk_example "no_body_no_following" {|- foo:|}

(* Chains — spec §5-8 *)

(** spec §5: [- a: b: c] — chain of 2 labels + inline value *)
let chain_with_value = mk_example "chain_with_value" {|- a: b: c|}

(** spec §6: [- a: b:] with indented body — chain in trailing-colon form *)
let colon_chain_inline_keying =
  mk_example
    "colon_chain_inline_keying"
    {|- foo: bar:
  - baz|}
;;

(** spec §7: [- a: b: c:] with indented body — chain of 3 labels *)
let three_label_chain =
  mk_example
    "three_label_chain"
    {|- a: b: c:
  - baz|}
;;

(** spec §8: two independent inline-value siblings — no cross-item absorption *)
let two_independent_siblings =
  mk_example
    "two_independent_siblings"
    {|- a: b: c
- x: y|}
;;

(* Paragraphs — spec §9-10 *)

(** spec §9: [foo: bar] standalone paragraph → inline-value keyed block *)
let paragraph_inline_value = mk_example "paragraph_inline_value" {|foo: bar|}

(** spec §9: same, with [paragraph_inline_value = false] → not keyed *)
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

(** spec §10: [foo: bar:] paragraph + indented body — chain on paragraph *)
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

(* Value contents — spec §11-15 *)

(** spec §11: value may contain mixed inline content *)
let mixed_inline_value = mk_example "mixed_inline_value" {|- foo: bar *baz* qux|}

(** spec §12: code span in value — only [foo] is the label, code interior is opaque *)
let code_span_in_value = mk_example "code_span_in_value" {|- foo: `code: thing`|}

(** spec §13: trailing space after colon — cross-line absorption does not fire *)
let non_example_trailing_space =
  mk_example "non_example_trailing_space" "- foo: \nfollowing"
;;

(** spec §14: no space after colon → not keyed *)
let non_example_no_space_after_colon =
  mk_example "non_example_no_space_after_colon" {|- foo:bar|}
;;

(** spec §15: URL label — [://] has no space, so only one split on [": "] *)
let url_as_label = mk_example "url_as_label" {|- http://x.com: click here|}

(* Escapes — spec §16 *)

(** spec §16: [\\\\:] in source → [\\:] in AST → escaped, not keyed *)
let escaped_colon =
  mk_example
    "escaped_colon"
    {|- foo\\:
- bar|}
;;

(* Emphasis / labels — spec §18-20 *)

(** spec §18: emphasis label with trailing-colon form *)
let emphasis_keyed_item =
  mk_example
    "emphasis_keyed_item"
    {|- *foo*:
  - bar
  - baz|}
;;

(** spec §18: emphasis label with inline-value form *)
let emphasis_inline_value = mk_example "emphasis_inline_value" {|- *foo*: bar|}

(** emphasis label in chain (trailing-colon form) *)
let emphasis_chain =
  mk_example
    "emphasis_chain"
    {|- *foo*: bar:
  - baz|}
;;

(** spec §19: mixed-inline paragraph — not keyed because label segment is mixed *)
let non_example_mixed_inline =
  mk_example
    "non_example_mixed_inline"
    {|*foo* bar:
following|}
;;

(** spec §19: mixed-inline list item — not keyed *)
let non_example_mixed_inline_list_item =
  mk_example "non_example_mixed_inline_list_item" {|- *foo* x: bar|}
;;

(** spec §20: value is unrestricted — mixed inline in value is fine *)
let free_form_value = mk_example "free_form_value" {|- foo: *bar* x|}

(* Trailing colon on last item — spec §21-22 *)

(** blank line between trailing-colon item and following block → no absorption *)
let non_example_blank_line =
  mk_example
    "non_example_blank_line"
    {|- foo:

bar|}
;;

(** spec §21: [b:] absorbs following [text] even when a non-keyed sibling precedes it *)
let last_item_absorbs_following_text =
  mk_example
    "last_item_absorbs_following_text"
    {|- a
- b:
text|}
;;

(** spec §22: same as §21, but the preceding sibling has an inline value *)
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

(* Non-examples — general *)

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
  (* Basics *)
  [ inline_value_list_item
  ; keyed_list_item_with_indented_content
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
  ; free_form_value (* Trailing colon on last item *)
  ; non_example_blank_line
  ; last_item_absorbs_following_text
  ; last_item_absorbs_following_text_2
  ; keyed_list_item_with_contiguous_blocks (* General non-examples *)
  ; non_example_no_colon
  ; non_example_colon_in_code_span
  ]
;;

(** {2 Generator} *)

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

let%test_module _ =
  (module struct
    let%expect_test _ =
      examples
      |> List.iteri ~f:(fun i ex ->
        print_endline {%string|Example %{i+1#Int}: %{ex.name}|};
        print_endline (String.make 10 '-');
        ex.content |> printf "```md {#original}\n%s\n```\n";
        print_endline "```debug-view";
        ex.content |> doc_of_string |> pp_doc_debug;
        print_endline "```";
        print_endline "```sexp";
        ex.content |> doc_of_string |> pp_doc_sexp;
        print_endline "```";
        print_endline "");
      [%expect
        {|
        Example 1: inline_value_list_item
        ----------
        ```md {#original}
        - foo: bar
        ```
        ```debug-view
        List[K(foo, bar)]
        ```
        ```sexp
        (List (Keyed_list_item (Text foo) (Paragraph (Text bar))))
        ```

        Example 2: keyed_list_item_with_indented_content
        ----------
        ```md {#original}
        - foo:
          - bar
          - baz
        ```
        ```debug-view
        List[K(foo, List[bar,
        baz])]
        ```
        ```sexp
        (List
          (Keyed_list_item (Text foo)
            (List (Paragraph (Text bar)) (Paragraph (Text baz)))))
        ```

        Example 3: no_body_no_following
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

        Example 4: chain_with_value
        ----------
        ```md {#original}
        - a: b: c
        ```
        ```debug-view
        List[K(a, K(b, c))]
        ```
        ```sexp
        (List
          (Keyed_list_item (Text a) (Keyed_list_item (Text b) (Paragraph (Text c)))))
        ```

        Example 5: colon_chain_inline_keying
        ----------
        ```md {#original}
        - foo: bar:
          - baz
        ```
        ```debug-view
        List[K(foo, K(bar, List[baz]))]
        ```
        ```sexp
        (List
          (Keyed_list_item (Text foo)
            (Keyed_list_item (Text bar) (List (Paragraph (Text baz))))))
        ```

        Example 6: three_label_chain
        ----------
        ```md {#original}
        - a: b: c:
          - baz
        ```
        ```debug-view
        List[K(a, K(b, K(c, List[baz])))]
        ```
        ```sexp
        (List
          (Keyed_list_item (Text a)
            (Keyed_list_item (Text b)
              (Keyed_list_item (Text c) (List (Paragraph (Text baz)))))))
        ```

        Example 7: two_independent_siblings
        ----------
        ```md {#original}
        - a: b: c
        - x: y
        ```
        ```debug-view
        List[K(a, K(b, c)), K(x,
        y)]
        ```
        ```sexp
        (List
          (Keyed_list_item (Text a) (Keyed_list_item (Text b) (Paragraph (Text c))))
          (Keyed_list_item (Text x) (Paragraph (Text y))))
        ```

        Example 8: paragraph_inline_value
        ----------
        ```md {#original}
        foo: bar
        ```
        ```debug-view
        K(foo, bar)
        ```
        ```sexp
        (Keyed_block (Text foo) (Paragraph (Text bar)))
        ```

        Example 9: keyed_paragraph
        ----------
        ```md {#original}
        foo:
        - bar
        - baz

        bee
        ```
        ```debug-view
        K(foo, List[bar,
        baz])

        bee
        ```
        ```sexp
        (Blocks
          (Keyed_block (Text foo)
            (List (Paragraph (Text bar)) (Paragraph (Text baz))))
          Blank_line (Paragraph (Text bee)))
        ```

        Example 10: keyed_paragraph_multiple_children
        ----------
        ```md {#original}
        foo:
        - bar
        - baz
        some text
        ```
        ```debug-view
        K(foo, List[bar,
        baz
        some text])
        ```
        ```sexp
        (Blocks
          (Keyed_block (Text foo)
            (List (Paragraph (Text bar))
              (Paragraph (Inlines (Text baz) (Break soft) (Text "some text"))))))
        ```

        Example 11: paragraph_chain
        ----------
        ```md {#original}
        foo: bar:
        - baz
        ```
        ```debug-view
        K(foo, K(bar, List[baz]))
        ```
        ```sexp
        (Blocks
          (Keyed_block (Text foo)
            (Keyed_block (Text bar) (List (Paragraph (Text baz))))))
        ```

        Example 12: nesting
        ----------
        ```md {#original}
        foo:
        - bar:
          - baz
        - qux
        ```
        ```debug-view
        K(foo, List[K(bar, List[baz]),
        qux])
        ```
        ```sexp
        (Blocks
          (Keyed_block (Text foo)
            (List (Keyed_list_item (Text bar) (List (Paragraph (Text baz))))
              (Paragraph (Text qux)))))
        ```

        Example 13: mixed_inline_value
        ----------
        ```md {#original}
        - foo: bar *baz* qux
        ```
        ```debug-view
        List[K(foo, bar *baz* qux)]
        ```
        ```sexp
        (List
          (Keyed_list_item (Text foo)
            (Paragraph (Inlines (Text "bar ") (Emphasis (Text baz)) (Text " qux")))))
        ```

        Example 14: code_span_in_value
        ----------
        ```md {#original}
        - foo: `code: thing`
        ```
        ```debug-view
        List[K(foo, `code: thing`)]
        ```
        ```sexp
        (List (Keyed_list_item (Text foo) (Paragraph (Code_span "code: thing"))))
        ```

        Example 15: non_example_trailing_space
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

        Example 16: non_example_no_space_after_colon
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

        Example 17: url_as_label
        ----------
        ```md {#original}
        - http://x.com: click here
        ```
        ```debug-view
        List[K(http://x.com, click here)]
        ```
        ```sexp
        (List (Keyed_list_item (Text http://x.com) (Paragraph (Text "click here"))))
        ```

        Example 18: escaped_colon
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

        Example 19: emphasis_keyed_item
        ----------
        ```md {#original}
        - *foo*:
          - bar
          - baz
        ```
        ```debug-view
        List[K(*foo*, List[bar,
        baz])]
        ```
        ```sexp
        (List
          (Keyed_list_item (Emphasis (Text foo))
            (List (Paragraph (Text bar)) (Paragraph (Text baz)))))
        ```

        Example 20: emphasis_inline_value
        ----------
        ```md {#original}
        - *foo*: bar
        ```
        ```debug-view
        List[K(*foo*, bar)]
        ```
        ```sexp
        (List (Keyed_list_item (Emphasis (Text foo)) (Paragraph (Text bar))))
        ```

        Example 21: emphasis_chain
        ----------
        ```md {#original}
        - *foo*: bar:
          - baz
        ```
        ```debug-view
        List[K(*foo*, K(bar, List[baz]))]
        ```
        ```sexp
        (List
          (Keyed_list_item (Emphasis (Text foo))
            (Keyed_list_item (Text bar) (List (Paragraph (Text baz))))))
        ```

        Example 22: non_example_mixed_inline
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

        Example 23: non_example_mixed_inline_list_item
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

        Example 24: free_form_value
        ----------
        ```md {#original}
        - foo: *bar* x
        ```
        ```debug-view
        List[K(foo, *bar* x)]
        ```
        ```sexp
        (List
          (Keyed_list_item (Text foo)
            (Paragraph (Inlines (Emphasis (Text bar)) (Text " x")))))
        ```

        Example 25: non_example_blank_line
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

        Example 26: last_item_absorbs_following_text
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

        Example 27: last_item_absorbs_following_text_2
        ----------
        ```md {#original}
        - a: x
        - b:
        text
        ```
        ```debug-view
        List[K(a, x),
        b:
        text]
        ```
        ```sexp
        (List (Keyed_list_item (Text a) (Paragraph (Text x)))
          (Paragraph (Inlines (Text b:) (Break soft) (Text text))))
        ```

        Example 28: keyed_list_item_with_contiguous_blocks
        ----------
        ```md {#original}
        - foo:
        ```
        bar
        ```
        ```
        ```debug-view
        List[K(foo, ```
        bar
        ```)]
        ```
        ```sexp
        (Blocks (List (Keyed_list_item (Text foo) (Code_block no-info bar))))
        ```

        Example 29: non_example_no_colon
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

        Example 30: non_example_colon_in_code_span
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

    let%expect_test _ =
      (* Two levels: A is keyed around the list; each item is keyed
         with an inline value. *)
      let eg =
        {|A:
- B: b
- C: c|}
      in
      eg |> doc_of_string |> pp_doc_sexp;
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
      let eg =
        {|A:
- B: b:
- C: c|}
      in
      eg |> doc_of_string |> pp_doc_sexp;
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
