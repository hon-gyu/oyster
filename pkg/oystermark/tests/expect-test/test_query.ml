(** Querying the blocks of a note: nodes, the steps a query is composed of, and
    the contents of a container. Impl: {!Oystermark.Note.Query} and
    {!Oystermark.Note.Node}.

    This is what [oyster block] is built on; its flags are one syntax for the
    queries written out here, tested in {!Test_query_flags}. *)

open Core
module Node = Oystermark.Note.Node
module Query = Oystermark.Note.Query
open Query.Sugar

let doc_of_string (s : string) : Cmarkit.Doc.t = Oystermark.Parse.of_string ~locs:true s

(** Print every node of [s], indented by depth: its path, kind, info string and
    enclosing heading ids. *)
let survey (s : string) =
  Query.matches (Query.run descendants_or_self (doc_of_string s))
  |> List.iter ~f:(fun (found : Query.found) ->
    printf
      "%s%s\t%s\t%s\t%s\n"
      (String.make (2 * (List.length found.path - 1)) ' ')
      (String.concat ~sep:"." (List.map found.path ~f:Int.to_string))
      (Node.kind found.node)
      (match Node.prop "info" found.node with
       | Some (String info) -> info
       | _ -> "-")
      (match found.headings with
       | [] -> "-"
       | headings ->
         String.concat
           ~sep:"/"
           (List.map headings ~f:(fun (h : Oystermark.Note.Anchor.heading) -> h.slug))))
;;

(** Print each match of [result] the way [oyster block] does: its source, or
    with [~content] what it holds, or why nothing matched. *)
let print_result ?(content = false) (s : string) (result : Query.result) =
  match Query.why_empty result with
  | Some why -> printf "<%s>\n" why
  | None ->
    List.iter (Query.matches result) ~f:(fun (found : Query.found) ->
      if content
      then (
        match found.content with
        | Ok content -> printf "%s\n" content
        | Error kind -> printf "<a %s has no contents to print>\n" kind)
      else (
        match found.span with
        | None -> printf "%s\n" found.markdown
        | Some { first_byte; last_byte; _ } ->
          printf "%s\n" (String.sub s ~pos:first_byte ~len:(last_byte - first_byte + 1))))
;;

(** Run [steps] over [s]. *)
let steps ?content (steps : Query.t) (s : string) =
  Query.run steps (doc_of_string s) |> print_result ?content s
;;

(** Everything in the section of the heading with this id, at any depth. With
    [~nested:false] the section stops at the first subheading. *)
let under ?(nested = true) (slug : string) : Query.t =
  descendants_or_self
  @ [ Query.Filter (Named (Heading slug)); Query.Section { nested } ]
  @ descendants_or_self
;;

(** Only code blocks whose info string is exactly [info]. *)
let lang (info : string) : Query.step = Filter (Prop ("info", Eq, String info))

(** Only the block a link to [ #id ] names. *)
let attr (id : string) : Query.step = Filter (Named (Attr id))

let mixed_note =
  {|
# Top

## Setup

```sh
echo one
```

```python
print("a")
```

> [!note] A callout
> Body line.

- item one
- item two

## Other

```python
print("b")
```
|}
;;

let%expect_test "descendants are in document order, containers before their contents" =
  survey mixed_note;
  [%expect
    {|
    0	heading	-	-
    1	heading	-	top
    2	code_block	sh	top/setup
    3	code_block	python	top/setup
    4	callout	-	top/setup
      4.0	paragraph	-	top/setup
    5	list	-	top/setup
      5.0	list_item	-	top/setup
        5.0.0	paragraph	-	top/setup
      5.1	list_item	-	top/setup
        5.1.0	paragraph	-	top/setup
    6	heading	-	top
    7	code_block	python	top/other
    |}]
;;

let%expect_test "position addresses a block that carries no id" =
  steps (under "top" @ [ lang "python"; Nth 2 ]) mixed_note;
  [%expect
    {|
    ```python
    print("b")
    ```
    |}]
;;

let%expect_test "a section scopes the query" =
  steps (under "other" @ [ Filter (is "code_block") ]) mixed_note;
  steps (under "missing" @ [ Filter (is "code_block") ]) mixed_note;
  [%expect
    {|
    ```python
    print("b")
    ```
    <no heading #missing; headings here: top, setup, other>
    |}]
;;

let%expect_test "the default output is the block's source, verbatim" =
  steps (descendants_or_self @ [ Filter (is "callout") ]) mixed_note;
  [%expect
    {|
    > [!note] A callout
    > Body line.
    |}]
;;

let%expect_test "a callout's contents drop the marker its syntax owns" =
  steps ~content:true (descendants_or_self @ [ Filter (is "callout") ]) mixed_note;
  [%expect {| Body line. |}]
;;

let%expect_test "the id selects the block, not its position" =
  steps
    ~content:true
    (descendants_or_self @ [ attr "wanted" ])
    {|
```python
print("first")
```

{#wanted}
```python
print("second")
```

```python
print("third")
```
|};
  [%expect {| print("second") |}]
;;

let%expect_test "a code block keeps its indentation and blank lines" =
  steps
    ~content:true
    (descendants_or_self @ [ attr "script" ])
    {|
{#script}
```python
def f():
    if True:

        return 1
```
|};
  [%expect
    {|
    def f():
        if True:

            return 1
    |}]
;;

let%expect_test "a paragraph holds inlines, so it has no contents to print" =
  let doc =
    {|
{#prose}
Just a paragraph.

{#code}
```sh
echo hi
```
|}
  in
  steps ~content:true (descendants_or_self @ [ attr "missing" ]) doc;
  steps ~content:true (descendants_or_self @ [ attr "prose" ]) doc;
  steps ~content:true (descendants_or_self @ [ attr "code" ]) doc;
  [%expect
    {|
    <no block named {#missing}; attribute ids here: prose, code>
    <a paragraph has no contents to print>
    echo hi
    |}]
;;

let%expect_test "a fence with no info string has no language to match on" =
  let doc =
    {|
{#bare}
```
some text
```
|}
  in
  steps ~content:true (descendants_or_self @ [ attr "bare" ]) doc;
  steps (descendants_or_self @ [ lang "python" ]) doc;
  [%expect
    {|
    some text
    <no block where info = "python"; none here has info (kinds here: code_block)>
    |}]
;;

let%expect_test "a div's contents are its body, re-rendered" =
  steps
    ~content:true
    (descendants_or_self @ [ Filter (is "div") ])
    {|
::: warning
Inside the div.

Second para.
:::
|};
  [%expect
    {|
    Inside the div.

    Second para.
    |}]
;;

let%expect_test "a list item is a node of its own" =
  survey
    {|
- item one
- item two with `code`
|};
  [%expect
    {|
    0	list	-	-
      0.0	list_item	-	-
        0.0.0	paragraph	-	-
      0.1	list_item	-	-
        0.1.0	paragraph	-	-
    |}]
;;

let%expect_test "a direct query excludes nested subsections" =
  let doc =
    {|
## Setup

```sh
echo direct
```

### Details

```sh
echo nested
```
|}
  in
  steps ~content:true (under "setup" @ [ lang "sh" ]) doc;
  steps ~content:true (under ~nested:false "setup" @ [ lang "sh" ]) doc;
  [%expect
    {|
    echo direct
    echo nested
    echo direct
    |}]
;;

let%expect_test "an id selects the block a link to it resolves to" =
  let doc =
    {|
The [a]{#x} and [b]{#y} terms.

> A quote.

^q1
|}
  in
  steps (descendants_or_self @ [ attr "x" ]) doc;
  steps (descendants_or_self @ [ attr "y" ]) doc;
  steps (descendants_or_self @ [ Filter (Named (Caret "q1")) ]) doc;
  [%expect
    {|
    The [a]{#x} and [b]{#y} terms.
    The [a]{#x} and [b]{#y} terms.
    > A quote.
    |}]
;;

let%expect_test "steps compose: the second item of a list" =
  steps
    (descendants_or_self @ [ Filter (is "list"); Children; Nth 2 ])
    {|
- one
- two
  continued
- three
|};
  [%expect
    {|
    - two
      continued
    |}]
;;

let%expect_test "a heading's section ends with its container" =
  steps
    (under "inside")
    {|
# Top

> [!note]
> ## Inside
> in the callout

after the callout
|};
  [%expect {| in the callout |}]
;;

let%expect_test "a heading's contents are its section" =
  steps
    ~content:true
    (descendants_or_self @ [ Filter (is "heading"); Nth 2 ])
    mixed_note;
  [%expect
    {|
    ```sh
    echo one
    ```

    ```python
    print("a")
    ```

    > \[!note\] A callout
    > Body line.

    - item one
    - item two
    |}]
;;

let%expect_test "an empty result names the step that found nothing" =
  steps (under "setp") mixed_note;
  steps (descendants_or_self @ [ Filter (is "table") ]) mixed_note;
  steps (under "other" @ [ lang "sh" ]) mixed_note;
  steps (descendants_or_self @ [ Filter (is "code_block"); Nth 9 ]) mixed_note;
  steps (descendants_or_self @ [ attr "nope" ]) mixed_note;
  steps (descendants @ [ Filter (is "paragraph"); Section { nested = true } ]) mixed_note;
  steps descendants_or_self "";
  [%expect
    {|
    <no heading #setp; headings here: top, setup, other>
    <no block where kind = "table"; kind here: "heading", "code_block", "callout", "paragraph", "list", "list_item">
    <no block where info = "sh"; info here: "python">
    <no match number 9; 3 matched before it>
    <no block named {#nope}; attribute ids here: none>
    <only a heading has a section; selected: paragraph>
    <the note has no blocks>
    |}]
;;

let%expect_test "a filter can test a property of a node" =
  let doc =
    {|
# One

## Two

### Three

> [!tip] Short
> body

- a
- b
- c
|}
  in
  steps (descendants_or_self @ [ Filter (Prop ("level", Lt, Int 3)) ]) doc;
  steps (descendants_or_self @ [ Filter (Prop ("type", Eq, String "tip")) ]) doc;
  steps (descendants_or_self @ [ Filter (Prop ("length", Ge, Int 3)) ]) doc;
  steps (descendants_or_self @ [ Filter (Prop ("level", Eq, String "1")) ]) doc;
  steps (descendants_or_self @ [ Filter (Prop ("title", Eq, String "Long")) ]) doc;
  [%expect
    {|
    # One
    ## Two
    > [!tip] Short
    > body
    - a
    - b
    - c
    <no block where level = "1"; level here: 1, 2, 3>
    <no block where title = "Long"; title here: "Short">
    |}]
;;

let%expect_test "a filter can run a query from each node" =
  let doc =
    {|
> [!note] Flat
> # A
> # B
> # C

> [!note] Nested
> # A
> ## B
> # C

> [!warning] Flat
> # A
> # B
> # C
|}
  in
  let headings = descendants @ [ Filter (is "heading") ] in
  steps
    (descendants_or_self
     @ [ Filter
           (And
              [ is "callout"
              ; Prop ("type", Eq, String "note")
              ; Count (headings, Eq, 3)
              ; for_all headings (Prop ("level", Lt, Int 2))
              ])
       ])
    doc;
  [%expect
    {|
    > [!note] Flat
    > # A
    > # B
    > # C
    |}]
;;

let%expect_test "until unwraps containers until a condition holds" =
  let doc =
    {|
> [!note]
> ::: warning
> ```sh
> echo deep
> ```
> :::

```sh
echo top
```
|}
  in
  steps ~content:true (until [ Children ] (is "code_block")) doc;
  [%expect {| echo deep |}]
;;

let%expect_test "a keyed list reads as fields" =
  let doc =
    {|
- name: oyster
- deps:
  - cmarkit
  - core
|}
  in
  steps (field "name") doc;
  steps (field "deps" @ [ Children ]) doc;
  steps (field "deps" @ field "missing") doc;
  steps
    (descendants_or_self
     @ [ Filter (is "list_item"); Filter (Prop ("key", Eq, String "deps")) ])
    doc;
  [%expect
    {|
    oyster
    - cmarkit
    - core
    <no block where key = "missing"; none here has key (kinds here: list, list_item)>
    - deps:
      - cmarkit
      - core
    |}]
;;

let%expect_test "each runs its query from one node at a time" =
  let doc =
    {|
- a1
- a2

* b1
* b2
|}
  in
  steps [ Children; Nth 1 ] doc;
  steps [ Each [ [ Children; Nth 1 ] ] ] doc;
  [%expect
    {|
    - a1
    - a1
    * b1
    |}]
;;

let%expect_test "recurse can stop descending" =
  let doc =
    {|
> quoted paragraph
>
> - item paragraph
|}
  in
  steps
    [ Recurse { steps = [ Children ]; emit = is "paragraph"; descend = Not (is "list") } ]
    doc;
  [%expect {| quoted paragraph |}]
;;
