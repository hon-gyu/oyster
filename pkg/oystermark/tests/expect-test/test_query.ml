(** Querying the blocks of a note: cursors, the steps a query is composed of,
    and the contents of a container. Impl: {!Oystermark.Note.Query}.

    This is what [oyster block] is built on. The command's flags are translated
    to steps by {!Oystermark.Note.Query.of_flags}, so the tests below run the
    same query the command runs. *)

open Core
module Query = Oystermark.Note.Query

let doc_of_string (s : string) : Cmarkit.Doc.t = Oystermark.Parse.of_string ~locs:true s

(** Print every cursor of [s], indented by depth: its path, kind, info string and
    enclosing heading ids. *)
let survey (s : string) =
  Query.top (doc_of_string s)
  |> List.concat_map ~f:(fun cursor -> cursor :: Query.descendants cursor)
  |> List.iter ~f:(fun cursor ->
    let path = Query.path cursor in
    printf
      "%s%s\t%s\t%s\t%s\n"
      (String.make (2 * (List.length path - 1)) ' ')
      (String.concat ~sep:"." (List.map path ~f:Int.to_string))
      (Query.kind cursor)
      (Option.value (Query.info_string cursor) ~default:"-")
      (match Query.headings cursor with
       | [] -> "-"
       | headings ->
         String.concat
           ~sep:"/"
           (List.map headings ~f:(fun (h : Oystermark.Note.Anchor.heading) -> h.slug))))
;;

(** Print what [oyster block] would print for [result]: each match's source, or
    with [~content] what the container holds, or why nothing matched. *)
let print_result ?(content = false) (s : string) (result : Query.result) =
  match Query.why_empty result with
  | Some why -> printf "<%s>\n" why
  | None ->
    List.iter result.matches ~f:(fun cursor ->
      if content
      then (
        match Query.content_string cursor with
        | Ok content -> printf "%s\n" content
        | Error kind -> printf "<a %s has no contents to print>\n" kind)
      else (
        let textloc = Cmarkit.Meta.textloc (Query.meta cursor) in
        let first = Cmarkit.Textloc.first_byte textloc in
        let last = Cmarkit.Textloc.last_byte textloc in
        printf "%s\n" (String.sub s ~pos:first ~len:(last - first + 1))))
;;

(** Run the flags of [oyster block] over [s]. *)
let query ?under ?(direct = false) ?kind ?lang ?id ?caret_id ?nth ?content (s : string) =
  Query.run
    (Query.of_flags { under; direct; kind; lang; attr_id = id; caret_id; nth })
    (Query.top (doc_of_string s))
  |> print_result ?content s
;;

(** Run [steps] over [s]. *)
let steps ?content (steps : Query.t) (s : string) =
  Query.run steps (Query.top (doc_of_string s)) |> print_result ?content s
;;

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
  query mixed_note ~under:"top" ~lang:"python" ~nth:2;
  [%expect
    {|
    ```python
    print("b")
    ```
    |}]
;;

let%expect_test "a section scopes the query" =
  query mixed_note ~under:"other" ~kind:"code_block";
  query mixed_note ~under:"missing" ~kind:"code_block";
  [%expect
    {|
    ```python
    print("b")
    ```
    <no heading #missing; headings here: top, setup, other>
    |}]
;;

let%expect_test "the default output is the block's source, verbatim" =
  query mixed_note ~kind:"callout";
  [%expect
    {|
    > [!note] A callout
    > Body line.
    |}]
;;

let%expect_test "a callout's contents drop the marker its syntax owns" =
  query mixed_note ~kind:"callout" ~content:true;
  [%expect {| Body line. |}]
;;

let%expect_test "the id selects the block, not its position" =
  query
    ~id:"wanted"
    ~content:true
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
  query
    ~id:"script"
    ~content:true
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
  query doc ~id:"missing" ~content:true;
  query doc ~id:"prose" ~content:true;
  query doc ~id:"code" ~content:true;
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
  query doc ~id:"bare" ~content:true;
  query doc ~lang:"python";
  [%expect
    {|
    some text
    <no code block with info string python; info strings here: none>
    |}]
;;

let%expect_test "a div's contents are its body, re-rendered" =
  query
    ~kind:"div"
    ~content:true
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

let%expect_test "a list item is a cursor of its own" =
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
  query doc ~under:"Setup" ~lang:"sh" ~content:true;
  query doc ~under:"Setup" ~direct:true ~lang:"sh" ~content:true;
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
  query doc ~id:"x";
  query doc ~id:"y";
  query doc ~caret_id:"q1";
  [%expect
    {|
    The [a]{#x} and [b]{#y} terms.
    The [a]{#x} and [b]{#y} terms.
    > A quote.
    |}]
;;

let%expect_test "steps compose: the second item of a list" =
  steps
    [ Descendants_or_self; Filter (Kind "list"); Children; Nth 2 ]
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
  query
    ~under:"inside"
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
  query mixed_note ~kind:"heading" ~nth:2 ~content:true;
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
  query mixed_note ~under:"setp";
  query mixed_note ~kind:"table";
  query mixed_note ~under:"other" ~lang:"sh";
  query mixed_note ~kind:"code_block" ~nth:9;
  query mixed_note ~id:"nope";
  steps [ Descendants; Filter (Kind "paragraph"); Section { nested = true } ] mixed_note;
  query "";
  [%expect
    {|
    <no heading #setp; headings here: top, setup, other>
    <no block of kind table; kinds here: heading, code_block, callout, paragraph, list, list_item>
    <no code block with info string sh; info strings here: python>
    <no match number 9; 3 matched before it>
    <no block named {#nope}; attribute ids here: none>
    <only a heading has a section; selected: paragraph>
    <the note has no blocks>
    |}]
;;
