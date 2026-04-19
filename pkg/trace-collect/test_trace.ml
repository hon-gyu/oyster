(** Test for {!Trace_collect} and {!Trace_collect.Trace_pp} *)

open Core

let f x = Trace.with_span ~__FILE__ ~__LINE__ "inside-f" @@ fun _sp -> x + 1

let g x =
  Trace.with_span ~__FILE__ ~__LINE__ "g"
  @@ fun _sp ->
  let y = x + 2 in
  Trace.with_span ~__FILE__ ~__LINE__ "right-before-f"
  @@ fun _sp2 ->
  Trace.add_data_to_span _sp2 [ "y", `Int y ];
  let y' = f y in
  y'
;;

let%expect_test "trace_pp" =
  let t = Trace_collect.create () in
  Trace_collect.with_collect t (fun () ->
    let _ = g 3 in
    ());
  print_s [%sexp (Trace_collect.span_names t : string list)];
  [%expect {| (g right-before-f inside-f) |}];
  let spans = Trace_collect.spans t |> Trace_collect.Span_pipeline.normalize_duration in
  print_string (Trace_collect.Trace_pp.format ~tree_chars:Utf8 ~style:Indented spans);
  [%expect
    {|
    g 3us
    └── right-before-f 2us y=5
            └── inside-f 1us
    |}];
  print_string (Trace_collect.Trace_pp.format ~style:Flat spans);
  [%expect
    {|
    g 3us
    right-before-f 2us y=5
    inside-f 1us
    |}];
  print_string (Trace_collect.Trace_pp.format ~style:Indented spans);
  [%expect
    {|
    g 3us
    └── right-before-f 2us y=5
            └── inside-f 1us
    |}];
  print_string (Trace_collect.Trace_pp.format ~style:Show_parents spans);
  [%expect
    {|
    g 3us
    └── right-before-f 2us y=5
        └── inside-f 1us
    |}];
  print_string (Trace_collect.Trace_pp.format ~tree_chars:Ascii ~style:Indented spans);
  [%expect
    {|
    g 3us
    `-- right-before-f 2us y=5
            `-- inside-f 1us
    |}]
;;
