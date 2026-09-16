open Core
module Node = Oystermark.Note.Node
module Query = Oystermark.Note.Query

let doc_of_string (s : string) : Cmarkit.Doc.t = Oystermark.Parse.of_string ~locs:true s

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

butter:
- item one
  - foo: bar
- item two

---

- bird:
  - bar
  - cat: cat1
  - two: three
  - foo
- happy:
  - sad

### Qweioasd

aciouv

## Other

```python
print("b")
```

ttt: hhhh

- aaa
  - bqq:
```rs
rs code
```

|}
;;

(* The tree [mixed_note] parses to, after the keyed rewrite. [---] keeps
   [butter:] from absorbing the [bird]/[happy] list: absorption stops at the
   first blank line. [bqq:] is the last item on its branch, so it absorbs the
   fence that follows the list it is nested in.

   Root
   `- Section "top"
      |- Section "setup"
      |  |- Code_block sh
      |  |- Code_block python
      |  |- Callout note "A callout"
      |  |  `- Paragraph "Body line."
      |  |- Keyed "butter"
      |  |  `- List
      |  |     |- List_item "item one"
      |  |     |  `- List
      |  |     |     `- Keyed "foo" -> Paragraph "bar"
      |  |     `- List_item "item two"
      |  |- Thematic_break
      |  |- List
      |  |  |- Keyed "bird"
      |  |  |  `- List [ "bar"; Keyed "cat"; Keyed "two"; "foo" ]
      |  |  `- Keyed "happy"
      |  |     `- List [ "sad" ]
      |  `- Section "qweioasd"
      |     `- Paragraph "aciouv"
      `- Section "other"
         |- Code_block python
         |- Keyed "ttt" -> Paragraph "hhhh"
         `- List
            `- List_item "aaa"
               `- List
                  `- Keyed "bqq" -> Code_block rs *)

let path_to_string (path : int list) : string =
  String.concat ~sep:"." (List.map path ~f:Int.to_string)
;;

let indented (text : string) : string =
  String.split_lines (String.strip text)
  |> List.map ~f:(fun line -> "    " ^ line)
  |> String.concat ~sep:"\n"
;;

(** Each match as its kind, its path and its Markdown. *)
let show (steps : Query.t) : unit =
  let result = Query.run steps (doc_of_string mixed_note) in
  match result.matches with
  | [] ->
    printf
      "<nothing> %s\n"
      (Option.value_map result.why_empty ~default:"?" ~f:Query.no_match_to_string)
  | matches ->
    List.iter matches ~f:(fun (found : Node.found_t) ->
      printf
        "%s @%s\n%s\n"
        (Node.kind found.node)
        (path_to_string found.path)
        (indented found.markdown))
;;

(** Each match as its kind and its path, for a query whose matches are large. *)
let show_brief (steps : Query.t) : unit =
  let result = Query.run steps (doc_of_string mixed_note) in
  match result.matches with
  | [] ->
    printf
      "<nothing> %s\n"
      (Option.value_map result.why_empty ~default:"?" ~f:Query.no_match_to_string)
  | matches ->
    List.iter matches ~f:(fun (found : Node.found_t) ->
      printf
        "%s @%s%s\n"
        (Node.kind found.node)
        (path_to_string found.path)
        (Option.value_map (Node.prop "text" found.node) ~default:"" ~f:(fun text ->
           " " ^ Node.value_to_string text)))
;;

(** The properties of each match. *)
let show_props (steps : Query.t) : unit =
  let result = Query.run steps (doc_of_string mixed_note) in
  List.iter result.matches ~f:(fun (found : Node.found_t) ->
    List.iter (Node.props found.node) ~f:(fun (name, value) ->
      printf "%s = %s\n" name (Node.value_to_string value)))
;;

(* Sections
   ======== *)

let%expect_test "section: everything under a heading, without the heading" =
  show Query.(empty |> section [ "top" ] |> child);
  [%expect
    {|
    section @0.0
        ## Setup

        ```sh
        echo one
        ```

        ```python
        print("a")
        ```

        > \[!note\] A callout
        > Body line.

        butter:
        - item one
          - foo: bar
        - item two

        ---

        - bird:
          - bar
          - cat: cat1
          - two: three
          - foo
        - happy:
          - sad

        ### Qweioasd

        aciouv
    section @0.1
        ## Other

        ```python
        print("b")
        ```

        ttt: hhhh

        - aaa
          - bqq:
            ```rs
            rs code
            ```
    |}]
;;

let%expect_test "section: a sub-path skips a level" =
  show Query.(empty |> section ~exact:false [ "top"; "qweioasd" ]);
  [%expect
    {|
    section @0.0.6
        ### Qweioasd

        aciouv
    |}]
;;

let%expect_test "section: an exact path must be complete" =
  show Query.(empty |> section [ "top"; "qweioasd" ]);
  [%expect {| <nothing> step 0 (section top/qweioasd): nothing to move to from root |}]
;;

let%expect_test "section: the complete path" =
  show Query.(empty |> section [ "top"; "setup"; "qweioasd" ]);
  [%expect
    {|
    section @0.0.6
        ### Qweioasd

        aciouv
    |}]
;;

(* Child and descendant
   ==================== *)

let%expect_test "child: by position" =
  show Query.(empty |> section ~exact:false [ "other" ] |> child ~nth:0);
  [%expect
    {|
    code_block @0.1.0
        ```python
        print("b")
        ```
    |}]
;;

let%expect_test "child: the rs block is bqq's value, not a child of the section" =
  show
    Query.(empty |> section ~exact:false [ "other" ] |> child ~where:[ is "code_block" ]);
  [%expect
    {|
    code_block @0.1.0
        ```python
        print("b")
        ```
    |}]
;;

let%expect_test "descend: reaches the rs block through the list and bqq" =
  show
    Query.(
      empty
      |> section ~exact:false [ "other" ]
      |> descend ~where:[ is "code_block" ] ~nth:1);
  [%expect
    {|
    code_block @0.1.2.0.1.0.0.0
        ```rs
        rs code
        ```
    |}]
;;

(* Fields
   ====== *)

let%expect_test "field: the keyed node itself, by its key property" =
  show
    Query.(
      empty
      |> section ~exact:false [ "setup" ]
      |> child ~where:[ Prop ("key", Eq, String "butter") ]);
  [%expect
    {|
    keyed_paragraph @0.0.3
        butter:
        - item one
          - foo: bar
        - item two
    |}]
;;

let%expect_test "field: the value of a key" =
  show Query.(empty |> section ~exact:false [ "setup" ] |> field "butter");
  [%expect
    {|
    list @0.0.3.0
        - item one
          - foo: bar
        - item two
    |}]
;;

let%expect_test "field: an unkeyed item, by position" =
  show Query.(empty |> section ~exact:false [ "setup" ] |> field "butter" |> child ~nth:0);
  [%expect
    {|
    list_item @0.0.3.0.0
        item one

        - foo: bar
    |}]
;;

let%expect_test "field: a field of an item, through the list describing it" =
  show
    Query.(
      empty
      |> section ~exact:false [ "setup" ]
      |> field "butter"
      |> child ~nth:0
      |> field "foo");
  [%expect
    {|
    paragraph @0.0.3.0.0.1.0.0.0
        bar
    |}]
;;

let%expect_test "field: under a section a list is content, not fields" =
  show Query.(empty |> section ~exact:false [ "setup" ] |> field "bird");
  [%expect {| <nothing> step 1 (field bird): nothing to move to from section |}]
;;

let%expect_test "field: naming the list gives its items as fields" =
  show
    Query.(
      empty
      |> section ~exact:false [ "setup" ]
      |> child ~where:[ is "list" ]
      |> field "bird"
      |> field "two");
  [%expect
    {|
    paragraph @0.0.5.0.0.0.2.0.0
        three
    |}]
;;

let%expect_test "field: not looked for inside another field's value" =
  show Query.(empty |> section ~exact:false [ "setup" ] |> field "foo");
  [%expect {| <nothing> step 1 (field foo): nothing to move to from section |}]
;;

let%expect_test "field: a keyed paragraph with an inline value" =
  show Query.(empty |> section ~exact:false [ "other" ] |> field "ttt");
  [%expect
    {|
    paragraph @0.1.1.0
        hhhh
    |}]
;;

let%expect_test "field: the section does not adopt the items of a list it holds" =
  show Query.(empty |> section ~exact:false [ "other" ] |> field "bqq");
  [%expect {| <nothing> step 1 (field bqq): nothing to move to from section |}]
;;

let%expect_test "field: bqq is a field of the item aaa" =
  show
    Query.(
      empty
      |> section ~exact:false [ "other" ]
      |> child ~where:[ is "list" ]
      |> child ~nth:0
      |> field "bqq");
  [%expect
    {|
    code_block @0.1.2.0.1.0.0.0
        ```rs
        rs code
        ```
    |}]
;;

(* Properties
   ========== *)

let%expect_test "props: a callout" =
  show_props
    Query.(empty |> section ~exact:false [ "setup" ] |> child ~where:[ is "callout" ]);
  [%expect
    {|
    kind = "callout"
    is_container = true
    type = "note"
    title = "A callout"
    |}]
;;

let%expect_test "child: a callout's body, without its header" =
  show
    Query.(
      empty |> section ~exact:false [ "setup" ] |> child ~where:[ is "callout" ] |> child);
  [%expect
    {|
    paragraph @0.0.2.0
        Body line.
    |}]
;;

(* Predicates
   ========== *)

let%expect_test "exists: the sections holding a python code block" =
  show_brief
    Query.(
      empty
      |> descend
           ~where:
             [ is "section"
             ; Exists
                 (empty
                  |> descend
                       ~where:[ is "code_block"; Prop ("lang", Eq, String "python") ])
             ]);
  [%expect
    {|
    section @0 "Top"
    section @0.0 "Setup"
    section @0.1 "Other"
    |}]
;;

let%expect_test "nth: negative counts from the end" =
  show
    Query.(
      empty
      |> section ~exact:false [ "setup" ]
      |> descend ~where:[ is "keyed_paragraph" ] ~nth:(-1));
  [%expect
    {|
    keyed_paragraph @0.0.5.1.0
        happy:
        - sad
    |}]
;;

(* Syntax
   ====== *)

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

let%expect_test "syntax: every step of the fixture" =
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

let%expect_test "syntax: what a bad query says" =
  List.iter
    ~f:round_trip
    [ "kids"; "field"; "child kind"; "child nth=x"; "child sub-path"; "has" ];
  [%expect
    {|
    kids
      error: unknown axis kids; one of self, child, descend, field, section
    field
      error: field needs a key, as: field butter
    child kind
      error: expected a predicate such as kind=code_block, got kind
    child nth=x
      error: nth needs a number: nth=x
    child sub-path
      error: sub-path belongs to a section step
    has
      error: unknown axis has; one of self, child, descend, field, section
    |}]
;;

let%expect_test "syntax: the parsed query selects the same nodes" =
  (match Query.of_string "section other sub-path | descend kind=code_block nth=1" with
   | Error message -> printf "error: %s\n" message
   | Ok steps -> show steps);
  [%expect
    {|
    code_block @0.1.2.0.1.0.0.0
        ```rs
        rs code
        ```
    |}]
;;
