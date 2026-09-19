(* The language and its combinators, its text syntax, and running it over a
   document. The cursor the run walks with is {!Cursor_}. *)

include Common_
include Syntax_
include Eval_
open Core

let%test_module "syntax" =
  (module struct
    (** The steps the text describes, printed back. the two should agree. *)
    let parse_and_dump_then_comp (text : string) : unit =
      match of_string text with
      | Error message -> printf "%s\n  error: %s\n" text message
      | Ok steps ->
        let printed = to_string steps in
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
