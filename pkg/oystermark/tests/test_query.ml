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

(** Each match as its kind, its path and its Markdown. *)
let show ?(note = mixed_note) (steps : Query.t) : unit =
  let result = Query.run steps (doc_of_string note) in
  match result.matches with
  | [] ->
    printf
      "<nothing> %s\n"
      (Option.value_map result.why_empty ~default:"?" ~f:Query.no_match_to_string)
  | matches ->
    List.iter matches ~f:(fun (found : Node.found_t) ->
      printf
        "kind: %s, path: %s\n%s\n%s\n"
        (Node.kind found.node)
        (path_to_string found.path)
        (String.make 20 '-')
        found.markdown)
;;

(** Each match as its kind and its path, for a query whose matches are large. *)
let show_brief ?(note = mixed_note) (steps : Query.t) : unit =
  let result = Query.run steps (doc_of_string note) in
  match result.matches with
  | [] ->
    printf
      "<nothing> %s\n"
      (Option.value_map result.why_empty ~default:"?" ~f:Query.no_match_to_string)
  | matches ->
    List.iter matches ~f:(fun (found : Node.found_t) ->
      printf
        "kind: %s, path: %s%s\n"
        (Node.kind found.node)
        (path_to_string found.path)
        (Option.value_map (Node.prop "text" found.node) ~default:"" ~f:(fun text ->
           " " ^ Node.value_to_string text)))
;;

(** The properties of each match. *)
let show_props ?(note = mixed_note) (steps : Query.t) : unit =
  let result = Query.run steps (doc_of_string note) in
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
    kind: section, path: 0.0
    --------------------
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
    kind: section, path: 0.1
    --------------------
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
    kind: section, path: 0.0.6
    --------------------
    ### Qweioasd

    aciouv
    |}]
;;

let%expect_test "section: an exact path must be complete" =
  show Query.(empty |> section [ "top"; "qweioasd" ]);
  [%expect
    {| <nothing> step 0 (Section([top, qweioasd])): nothing to move to from root |}]
;;

let%expect_test "section: the complete path" =
  show Query.(empty |> section [ "top"; "setup"; "qweioasd" ]);
  [%expect
    {|
    kind: section, path: 0.0.6
    --------------------
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
    kind: code_block, path: 0.1.0
    --------------------
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
    kind: code_block, path: 0.1.0
    --------------------
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
    kind: code_block, path: 0.1.2.0.1.0.0.0
    --------------------
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
    kind: keyed_paragraph, path: 0.0.3
    --------------------
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
    kind: list, path: 0.0.3.0
    --------------------
    - item one
      - foo: bar
    - item two
    |}]
;;

let%expect_test "field: an unkeyed item, by position" =
  show Query.(empty |> section ~exact:false [ "setup" ] |> field "butter" |> child ~nth:0);
  [%expect
    {|
    kind: list_item, path: 0.0.3.0.0
    --------------------
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
    kind: paragraph, path: 0.0.3.0.0.1.0.0.0
    --------------------
    bar
    |}]
;;

let%expect_test "field: under a section a list is content, not fields" =
  show Query.(empty |> section ~exact:false [ "setup" ] |> field "bird");
  [%expect {| <nothing> step 1 (Field(bird)): nothing to move to from section |}]
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
    kind: paragraph, path: 0.0.5.0.0.0.2.0.0
    --------------------
    three
    |}]
;;

let%expect_test "field: not looked for inside another field's value" =
  show Query.(empty |> section ~exact:false [ "setup" ] |> field "foo");
  [%expect {| <nothing> step 1 (Field(foo)): nothing to move to from section |}]
;;

let%expect_test "field: a keyed paragraph with an inline value" =
  show Query.(empty |> section ~exact:false [ "other" ] |> field "ttt");
  [%expect
    {|
    kind: paragraph, path: 0.1.1.0
    --------------------
    hhhh
    |}]
;;

let%expect_test "field: the section does not adopt the items of a list it holds" =
  show Query.(empty |> section ~exact:false [ "other" ] |> field "bqq");
  [%expect {| <nothing> step 1 (Field(bqq)): nothing to move to from section |}]
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
    kind: code_block, path: 0.1.2.0.1.0.0.0
    --------------------
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
    kind: paragraph, path: 0.0.2.0
    --------------------
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
    kind: section, path: 0 "Top"
    kind: section, path: 0.0 "Setup"
    kind: section, path: 0.1 "Other"
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
    kind: keyed_paragraph, path: 0.0.5.1.0
    --------------------
    happy:
    - sad
    |}]
;;

(* Syntax
   ====== *)

let%expect_test "syntax: the parsed query selects the same nodes" =
  (match
     Query.of_string
       "[Section([other], exact=false), Descendant(where=[Is(code_block)], nth=1)]"
   with
   | Error message -> printf "error: %s\n" message
   | Ok steps -> show steps);
  [%expect
    {|
    kind: code_block, path: 0.1.2.0.1.0.0.0
    --------------------
    ```rs
    rs code
    ```
    |}]
;;

(* Attributes
   ==========

   [id] and [class] are not properties of a node, so the engine adds them from
   what is written around it: a djot attribute, or a caret marker. *)

let attr_note =
  {|
# Attrs

{#intro}
> A quote named by an attribute.

{.warning .boxed}
A paragraph carrying two classes.

{#snippet .example}
```sh
echo hi
```

A paragraph named by a caret ^caret1
|}
;;

let%expect_test "attribute: an id selects the block it is written on" =
  show ~note:attr_note Query.(empty |> descend ~where:[ Prop ("id", Eq, String "intro") ]);
  [%expect
    {|
    kind: block_quote, path: 0.0
    --------------------
    {#intro}
    > A quote named by an attribute.
    |}]
;;

let%expect_test "attribute: a class selects the block it is written on" =
  show
    ~note:attr_note
    Query.(empty |> descend ~where:[ Prop ("class", Eq, String "warning") ]);
  [%expect
    {|
    kind: paragraph, path: 0.1
    --------------------
    {.warning .boxed}
    A paragraph carrying two classes.
    |}]
;;

let%expect_test "attribute: a block carries every class it is given" =
  show_brief
    ~note:attr_note
    Query.(empty |> descend ~where:[ Prop ("class", Eq, String "boxed") ]);
  [%expect {| kind: paragraph, path: 0.1 |}]
;;

let%expect_test "attribute: which blocks have a class at all" =
  show_brief ~note:attr_note Query.(empty |> descend ~where:[ Has "class" ]);
  [%expect
    {|
    kind: paragraph, path: 0.1
    kind: code_block, path: 0.2 "echo hi"
    |}]
;;

let%expect_test "attribute: an id and a property of the node itself" =
  show
    ~note:attr_note
    Query.(
      empty
      |> descend
           ~where:[ Prop ("id", Eq, String "snippet"); Prop ("lang", Eq, String "sh") ]);
  [%expect
    {|
    kind: code_block, path: 0.2
    --------------------
    {#snippet .example}
    ```sh
    echo hi
    ```
    |}]
;;

let%expect_test "attribute: a caret id is the same namespace as an attribute id" =
  show
    ~note:attr_note
    Query.(empty |> descend ~where:[ Prop ("id", Eq, String "caret1") ]);
  [%expect
    {|
    kind: paragraph, path: 0.3
    --------------------
    A paragraph named by a caret ^caret1
    |}]
;;

(* [id] and [class] are not in {!Node.props}: they belong to the note around
    the node, not to the node. *)
let%expect_test "attribute: not among the node's own properties" =
  show_props
    ~note:attr_note
    Query.(empty |> descend ~where:[ Prop ("id", Eq, String "snippet") ]);
  [%expect
    {|
    kind = "code_block"
    is_container = false
    lang = "sh"
    text = "echo hi"
    |}]
;;

let%test_module "syntax" =
  (module struct
    (** The steps the text describes, printed back. the two should agree. *)
    let parse_and_dump_then_comp (text : string) : unit =
      match Query.of_string text with
      | Error message -> printf "%s\n  error: %s\n" text message
      | Ok steps ->
        let printed = Query.to_string steps in
        if String.equal printed text
        then printf "Ok\n"
        else printf "┌ pre: %s\n└ now: %s\n" text printed
    ;;

    let%expect_test "every step of the fixture" =
      List.iter
        ~f:parse_and_dump_then_comp
        [ "[Section([top]), Child]"
        ; "[Section([top, qweioasd], exact=false)]"
        ; "[Section([other], exact=false), Child(where=[Is(code_block)])]"
        ; "[Section([other], exact=false), Descendant(where=[Is(code_block)], nth=1)]"
        ; "[Section([setup], exact=false), Field(butter), Child(nth=0), Field(foo)]"
        ; "[Section([setup], exact=false), Child(where=[Is(list)]), Field(bird), \
           Field(two)]"
        ; "[Descendant(where=[Has(key)])]"
        ; "[Child(where=[Not(Is(heading))])]"
        ; "[Child(where=[Prop(level, >=, 2), Prop(ordered, =, true)])]"
        ; "[Child(where=[Prop(title, =, \"A callout\")])]"
        ; "[Child(where=[Prop(key, =, 12)])]"
        ; "[Descendant(where=[Or([Is(list), Is(list_item)])])]"
        ; "[Descendant(where=[Is(section), Exists([Descendant(where=[Is(code_block), \
           Prop(lang, =, python)])])])]"
        ; "[Descendant(where=[Count([Child], >, 2)])]"
        ; "[Self(nth=-1)]"
        ; "[]"
        ];
      [%expect
        {|
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        Ok
        |}]
    ;;

    (* Empty parentheses, [Prop] on [kind], quotes and spacing: read the same,
       printed one way. *)
    let%expect_test "what is read but not written" =
      List.iter
        ~f:parse_and_dump_then_comp
        [ "[Child()]"
        ; "[Child(where=[Prop(kind, =, paragraph)])]"
        ; "[Child(where=[Prop(kind, !=, paragraph)])]"
        ; "[Field(\"butter\")]"
        ; "[ Section( [ top ] , exact = true ) ]"
        ; "[Child(where=[Prop(level, <, 2)])]"
        ];
      [%expect
        {|
        ┌ pre: [Child()]
        └ now: [Child]
        ┌ pre: [Child(where=[Prop(kind, =, paragraph)])]
        └ now: [Child(where=[Is(paragraph)])]
        Ok
        ┌ pre: [Field("butter")]
        └ now: [Field(butter)]
        ┌ pre: [ Section( [ top ] , exact = true ) ]
        └ now: [Section([top])]
        Ok
        |}]
    ;;

    let%expect_test "what a bad query says" =
      List.iter
        ~f:parse_and_dump_then_comp
        [ "Child"
        ; "[Kids]"
        ; "[Field]"
        ; "[Child(butter)]"
        ; "[Child(exact=false)]"
        ; "[Child(where=Is(heading))]"
        ; "[Child(where=[Is])]"
        ; "[Child(where=[Prop(level, ~, 2)])]"
        ; "[Child(nth=x)]"
        ; "[Child(nth=0, nth=1)]"
        ; "[Section([top], exact=true, exact=false)]"
        ; "[Descendant(where=[Count([Child])])]"
        ; "[Child(where=[Kind(heading)])]"
        ; "[Child"
        ; "[Field(\"butter)]"
        ; "[Child] [Self]"
        ];
      [%expect
        {|
        Child
          error: expected a list [...], got Child
        [Kids]
          error: unknown Kids; one of Self, Child, Descendant, Field, Section
        [Field]
          error: expected Field(KEY, where=..., nth=...), got Field
        [Child(butter)]
          error: expected Child(where=..., nth=...), got Child(butter)
        [Child(exact=false)]
          error: expected Child(where=..., nth=...), got Child(exact=false)
        [Child(where=Is(heading))]
          error: expected a list [...], got Is(heading)
        [Child(where=[Is])]
          error: expected Is(KIND), got Is
        [Child(where=[Prop(level, ~, 2)])]
          error: expected one of = != < <= > >=, got ~
        [Child(nth=x)]
          error: expected a number, got x
        [Child(nth=0, nth=1)]
          error: Child has duplicate argument nth
        [Section([top], exact=true, exact=false)]
          error: Section has duplicate argument exact
        [Descendant(where=[Count([Child])])]
          error: expected Count([STEP, ...], OP, INT), got Count([Child])
        [Child(where=[Kind(heading)])]
          error: unknown Kind; one of Is, Prop, Has, Not, And, Or, Exists, Count
        [Child
          error: expected , or ], got the end
        [Field("butter)]
          error: unterminated string at 7
        [Child] [Self]
          error: unexpected [ at 8 after the query
        |}]
    ;;
  end)
;;

(* [of_address] finds what {!Oystermark.Note.Private.Addressed_blocks.find}
   names, case for case with that module's tests; the source text is what hover
   shows. *)
let%test_module "of_address" =
  (module struct
    let show content (address : Oystermark.Note.Anchor.Address.t) =
      let doc = doc_of_string content in
      (Query.run (Query.of_address address) doc).matches
      |> List.hd
      |> Option.bind ~f:(Node.source_text content)
      |> Option.value ~default:"<none>"
      |> print_endline
    ;;

    let%expect_test "heading: section of a top-level heading" =
      show "## Sec\n\nContent.\n\n## Other\n\nNot this.\n" (Heading "sec");
      [%expect
        {|
        ## Sec

        Content.
        |}]
    ;;

    let%expect_test "heading: inside a div, the section ends with the div" =
      show "# Top\n\n::: warning\n## Inside\n\nbody\n:::\n\nafter\n" (Heading "inside");
      [%expect
        {|
        ## Inside

        body
        |}]
    ;;

    let%expect_test "heading: a div after the heading belongs to the section whole" =
      show "## A\n\ntext\n\n::: note\n## B\n:::\n\nmore\n\n## C\n" (Heading "a");
      [%expect
        {|
        ## A

        text

        ::: note
        ## B
        :::

        more
        |}]
    ;;

    let%expect_test "heading: inside a block quote" =
      show "> ## Q\n> text\n\nafter\n" (Heading "q");
      [%expect
        {|
        ## Q
        > text
        |}]
    ;;

    let%expect_test "heading: inside a list item" =
      show "- ## L\n  text\n- other\n" (Heading "l");
      [%expect
        {|
        ## L
          text
        |}]
    ;;

    let%expect_test "heading: an authored id starts the section at the heading" =
      show "{#intro}\n# Introduction\n\nbody\n\n# Next\n" (Heading "intro");
      [%expect
        {|
        {#intro}
        # Introduction

        body
        |}]
    ;;

    let%expect_test "heading: a hash inside a code block does not end the section" =
      show "# Alpha\n\n```\n# not a heading\n```\n\ntail\n\n# Beta\n" (Heading "alpha");
      [%expect
        {|
        # Alpha

        ```
        # not a heading
        ```

        tail
        |}]
    ;;

    let%expect_test "caret: the whole paragraph, marker included" =
      show "# H\n\nFirst line\nsecond line ^abc\n\nafter\n" (Caret "abc");
      [%expect
        {|
        First line
        second line ^abc
        |}]
    ;;

    let%expect_test "caret: on a line of its own, the previous block" =
      show "> A quote.\n\n^q1\n" (Caret "q1");
      [%expect {| > A quote. |}]
    ;;

    let%expect_test "caret: in a nested list item" =
      show
        "- a nested list ^firstline\n    - item\n      ^inneritem\n"
        (Caret "inneritem");
      [%expect
        {|
        item
              ^inneritem
        |}]
    ;;

    let%expect_test "attr: on inlines, the containing paragraph as written" =
      show "The [key term]{#kt} is here.\n" (Attr "kt");
      [%expect {| The [key term]{#kt} is here. |}]
    ;;

    let%expect_test "attr: on a block, the wrapped block" =
      show "# H\n\n{#aside}\n> An aside block.\n" (Attr "aside");
      [%expect
        {|
        {#aside}
        > An aside block.
        |}]
    ;;

    let%expect_test "attr: on a heading, its section" =
      show "{#intro}\n# Introduction\n\nbody\n\n# Next\n" (Attr "intro");
      [%expect
        {|
        {#intro}
        # Introduction

        body
        |}]
    ;;

    let%expect_test "not found" =
      show "# H\n\nPlain paragraph.\n" (Heading "missing");
      [%expect {| <none> |}]
    ;;
  end)
;;
