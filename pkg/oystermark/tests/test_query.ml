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

let doc =
  let parsed : Oystermark.Parse.t = Oystermark.Parse.of_string source in
  Oystermark.Note.Xml.of_doc parsed.doc
;;

let eval expression =
  Xpath.run_exn (Xpath.Context.create_exn ()) doc (Xpath.parse_utf8_exn expression)
;;

let nodes expression =
  eval expression |> Xpath.Value.Cast.to_node_set |> Option.value_exn |> Map.data
;;

let strings expression = List.map (nodes expression) ~f:Xpath.Node.string_value

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
