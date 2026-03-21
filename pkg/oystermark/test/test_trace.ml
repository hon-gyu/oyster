(** Test for {!Trace_collect} and {!Trace_pp} *)

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
  [%expect {| (inside-f right-before-f g) |}];
  let spans = Trace_collect.spans t in
  print_string (Trace_collect.Trace_pp.format ~normalize_duration:true Flat spans);
  [%expect
    {|
    g 3
    right-before-f 2 y=5
    inside-f 1
    |}];
  print_string (Trace_collect.Trace_pp.format ~normalize_duration:true Indented spans);
  [%expect
    {|
    g 3
      right-before-f 2 y=5
        inside-f 1
    |}];
  print_string (Trace_collect.Trace_pp.format ~normalize_duration:true Show_parents spans);
  [%expect
    {|
    g 3
      right-before-f 2 y=5
        inside-f 1
    |}]
;;
