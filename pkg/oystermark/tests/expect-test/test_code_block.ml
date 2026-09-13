(** Querying the blocks of a note: the walk, its filters, and the contents of a
    container. Impl: {!Oystermark.Extract.walk} and
    {!Oystermark.Extract.content_string}.

    This is what [oyster block] is built on. The command's flags are filters
    over the records {!Oystermark.Extract.walk} produces, so the tests
    below run the same query the command runs. *)

open Core
module Extract = Oystermark.Extract
module Common = Oystermark.Parse.Common

let doc_of_string (s : string) : Cmarkit.Doc.t = Oystermark.Parse.of_string ~locs:true s

let blocks_of_doc (doc : Cmarkit.Doc.t) : Cmarkit.Block.t list =
  match Cmarkit.Doc.block doc with
  | Cmarkit.Block.Blocks (blocks, _) -> blocks
  | block -> [ block ]
;;

(** Print one line per block of [s]: its position, kind, info string and
    enclosing headings -- the fields the filters select on. *)
let survey (s : string) =
  Extract.walk (blocks_of_doc (doc_of_string s))
  |> List.iter ~f:(fun (located : Extract.located_block) ->
    printf
      "%d\t%s\t%s\t%s\n"
      located.index
      (Extract.kind_of_block located.block)
      (Option.value (Common.info_string_of_block located.block) ~default:"-")
      (match located.heading_path with
       | [] -> "-"
       | path -> String.concat path ~sep:"/"))
;;

(** Run a query over [s] and print what [oyster block] would print: each match's
    source, or with [~content] what the container holds. *)
let query ?under ?kind ?lang ?id ?caret_id ?nth ?(content = false) (s : string) =
  let doc = doc_of_string s in
  let defs = Cmarkit.Doc.defs doc in
  let matches_option option actual =
    match option with
    | None -> true
    | Some wanted ->
      (match actual with
       | Some actual -> String.equal actual wanted
       | None -> false)
  in
  let matches =
    Extract.walk (blocks_of_doc doc)
    |> List.filter ~f:(fun (located : Extract.located_block) ->
      (match under with
       | None -> true
       | Some wanted -> List.mem located.heading_path wanted ~equal:String.equal)
      && matches_option kind (Some (Extract.kind_of_block located.block))
      && matches_option lang (Common.info_string_of_block located.block)
      && matches_option id located.attr_id
      && matches_option caret_id (Common.caret_id_of_block located.block))
  in
  let matches =
    match nth with
    | None -> matches
    | Some n -> List.nth matches (n - 1) |> Option.to_list
  in
  match matches with
  | [] -> printf "<no block matches>\n"
  | matches ->
    List.iter matches ~f:(fun located ->
      if content
      then (
        match Extract.content_string ~defs located with
        | Ok content -> printf "%s\n" content
        | Error kind -> printf "<a %s has no contents to print>\n" kind)
      else (
        let textloc = Cmarkit.Meta.textloc (Common.meta_of_block located.block) in
        let first = Cmarkit.Textloc.first_byte textloc in
        let last = Cmarkit.Textloc.last_byte textloc in
        printf "%s\n" (String.sub s ~pos:first ~len:(last - first + 1))))
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

let%expect_test "the walk reports every addressable block, containers included" =
  survey mixed_note;
  [%expect
    {|
    1	heading	-	-
    2	heading	-	top
    3	code_block	sh	top/setup
    4	code_block	python	top/setup
    5	callout	-	top/setup
    6	paragraph	-	top/setup
    7	list	-	top/setup
    8	paragraph	-	top/setup
    9	paragraph	-	top/setup
    10	heading	-	top
    11	code_block	python	top/other
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
    <no block matches>
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
    <no block matches>
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
    <no block matches>
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

let%expect_test "a list item is not a block; its contents are walked" =
  survey
    {|
- item one
- item two with `code`
|};
  [%expect
    {|
    1	list	-	-
    2	paragraph	-	-
    3	paragraph	-	-
    |}]
;;
