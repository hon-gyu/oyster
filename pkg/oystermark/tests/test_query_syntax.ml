(** The text a query is written as, read back and printed again: {!Query.to_string}
    and {!Query.of_string} have to agree on every form. *)

open Core
module Query = Oystermark.Note.Query

(** The steps the text describes, printed back: the two must agree. *)
let round_trip (text : string) : unit =
  match Query.of_string text with
  | Error message -> printf "%s\n  error: %s\n" text message
  | Ok steps ->
    let printed = Query.to_string steps in
    if String.equal printed text
    then printf "%s\n" printed
    else printf "%s\n  -> %s\n" text printed
;;

let%expect_test "every step of the fixture" =
  List.iter
    ~f:round_trip
    [ "section top | child"
    ; "section top/qweioasd sub-path"
    ; "section other sub-path | child kind=code_block"
    ; "section other sub-path | descend kind=code_block nth=1"
    ; "section setup sub-path | field butter | child nth=0 | field foo"
    ; "section setup sub-path | child kind=list | field bird | field two"
    ; "descend has:key"
    ; "child not:kind=heading"
    ; "child level>=2 ordered=true"
    ; "child title=\"A callout\""
    ; "child key=\"12\""
    ; "descend or(kind=list,kind=list_item)"
    ; "descend kind=section exists(descend kind=code_block lang=python)"
    ; "descend count(child)>2"
    ; "self nth=-1"
    ];
  [%expect
    {|
    section top | child
    section top/qweioasd sub-path
    section other sub-path | child kind=code_block
    section other sub-path | descend kind=code_block nth=1
    section setup sub-path | field butter | child nth=0 | field foo
    section setup sub-path | child kind=list | field bird | field two
    descend has:key
    child not:kind=heading
    child level>=2 ordered=true
    child title="A callout"
    child key="12"
    descend or(kind=list,kind=list_item)
    descend kind=section exists(descend kind=code_block lang=python)
    descend count(child)>2
    self nth=-1
    |}]
;;

(* A space instead of a colon, and [str:] and [int:] where the kind of a value
    matters: read the same, printed one way. *)
let%expect_test "what is read but not written" =
  List.iter
    ~f:round_trip
    [ "descend has key"
    ; "child not kind=heading"
    ; "child key=str:12"
    ; "child level=int:2"
    ; "child kind!=paragraph"
    ; "section top"
    ];
  [%expect
    {|
    descend has key
      -> descend has:key
    child not kind=heading
      -> child not:kind=heading
    child key=str:12
      -> child key="12"
    child level=int:2
      -> child level=2
    child kind!=paragraph
    section top
    |}]
;;

let%expect_test "what a bad query says" =
  List.iter
    ~f:round_trip
    [ "kids"
    ; "field"
    ; "section"
    ; "child kind"
    ; "child =paragraph"
    ; "child nth=x"
    ; "child sub-path"
    ; "has"
    ; "descend count(child)"
    ];
  [%expect
    {|
    kids
      error: unknown axis kids; one of self, child, descend, field, section
    field
      error: field needs a key, as: field butter
    section
      error: section needs a path, as: section top/setup
    child kind
      error: expected a predicate such as kind=code_block, got kind
    child =paragraph
      error: a predicate needs a property name: =paragraph
    child nth=x
      error: nth needs a number: nth=x
    child sub-path
      error: sub-path belongs to a section step
    has
      error: unknown axis has; one of self, child, descend, field, section
    descend count(child)
      error: count needs a comparison, as count(child)>0
    |}]
;;
