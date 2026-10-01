(** XPath queries over the Djot XML view. The XML contract is {!Oystermark.Note.Xml}. *)

open Core

let source =
  {|# Top

## Setup

owner: alice

```python
print(1)
```

> [!note] A callout
> Body.

{#target}
Paragraph with _emphasis_.
|}
;;

let of_source source =
  let parsed : Oystermark.Parse.t = Oystermark.Parse.of_string source in
  Oystermark.Note.Xml.of_doc parsed.doc
;;

let doc = of_source source

let eval ?(doc = doc) expression =
  Xpath.run_exn (Xpath.Context.create_exn ()) doc (Xpath.parse_utf8_exn expression)
;;

let nodes ?doc expression =
  eval ?doc expression |> Xpath.Value.Cast.to_node_set |> Option.value_exn |> Map.data
;;

let strings ?doc expression = List.map (nodes ?doc expression) ~f:Xpath.Node.string_value

let%test_unit "XPath selects structured Djot blocks" =
  [%test_result: string list]
    (strings "//section[heading='Setup']//code_block[@lang='python']")
    ~expect:[ "print(1)\n" ];
  [%test_result: string list]
    (strings "//keyed[@key='owner']/paragraph")
    ~expect:[ "alice" ];
  [%test_result: string list]
    (strings "//callout[@type='note']/title")
    ~expect:[ "A callout" ]
;;

let%test_unit "authored attributes, inlines, and locations are queryable" =
  [%test_result: string list] (strings "//*[@id='target']//emph") ~expect:[ "emphasis" ];
  let start = strings "//code_block/@start-byte" |> List.hd_exn |> Int.of_string in
  let last = strings "//code_block/@end-byte" |> List.hd_exn |> Int.of_string in
  let slice = String.sub source ~pos:start ~len:(last - start + 1) in
  assert (String.is_prefix slice ~prefix:"```python");
  assert (String.is_substring slice ~substring:"print(1)")
;;

let%test_unit "XPath scalar expressions are available" =
  [%test_result: string]
    (Xpath.Value.Cast.to_string (eval "count(//code_block)"))
    ~expect:"1"
;;

(* Definitions
   ===========

   Each footnote and reference definition appears exactly once, under
   [footnotes] or [references], wherever it was written. *)

let%test_unit "a reference definition appears only under references" =
  let doc = of_source "[r]: http://x\n" in
  [%test_result: string list]
    (strings ~doc "//reference_definition/@label")
    ~expect:[ "r" ];
  [%test_result: string list]
    (strings ~doc "/doc/references/reference_definition/@label")
    ~expect:[ "r" ]
;;

let%test_unit "a nested footnote definition is its own entry, not its parent's child" =
  let doc = of_source "[^a]: one\n\n    [^b]: two\n" in
  [%test_result: string list]
    (strings ~doc "/doc/footnotes/footnote_definition/@label")
    ~expect:[ "a"; "b" ];
  [%test_result: string list]
    (strings ~doc "//footnote_definition//footnote_definition/@label")
    ~expect:[];
  [%test_result: string list]
    (strings ~doc "//footnote_definition[@label='a']")
    ~expect:[ "one" ]
;;

let%test_unit "a reference definition inside a footnote appears only under references" =
  let doc = of_source "[^a]: one\n\n    [r]: http://y\n" in
  [%test_result: string list]
    (strings ~doc "//reference_definition/@label")
    ~expect:[ "r" ];
  [%test_result: string list]
    (strings ~doc "//footnote_definition//reference_definition/@label")
    ~expect:[]
;;
