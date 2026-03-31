open! Core
open Oystermark

let run_dot ?on_error md =
  let doc = Parse.of_string md in
  let doc' = (Pipeline.dot_render ?on_error ()).on_parse "test.md" doc |> List.hd_exn |> snd in
  print_endline (Parse.commonmark_of_doc doc')
;;

(* dot_render
==================== *)

let%expect_test "basic dot graph" =
  run_dot
    {|```dot
digraph { a -> b }
```|};
  [%expect {|
    ```=html
    <svg width="62pt" height="116pt"
     viewBox="0.00 0.00 62.00 116.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
    <g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 112)">
    <polygon fill="white" stroke="none" points="-4,4 -4,-112 58,-112 58,4 -4,4"/>
    <!-- a -->
    <g id="node1" class="node">
    <title>a</title>
    <ellipse fill="none" stroke="black" cx="27" cy="-90" rx="27" ry="18"/>
    <text xml:space="preserve" text-anchor="middle" x="27" y="-84.95" font-family="Times,serif" font-size="14.00">a</text>
    </g>
    <!-- b -->
    <g id="node2" class="node">
    <title>b</title>
    <ellipse fill="none" stroke="black" cx="27" cy="-18" rx="27" ry="18"/>
    <text xml:space="preserve" text-anchor="middle" x="27" y="-12.95" font-family="Times,serif" font-size="14.00">b</text>
    </g>
    <!-- a&#45;&gt;b -->
    <g id="edge1" class="edge">
    <title>a&#45;&gt;b</title>
    <path fill="none" stroke="black" d="M27,-71.7C27,-64.41 27,-55.73 27,-47.54"/>
    <polygon fill="black" stroke="black" points="30.5,-47.62 27,-37.62 23.5,-47.62 30.5,-47.62"/>
    </g>
    </g>
    </svg>
    ```
    |}]
;;

let%expect_test "non-dot code block unchanged" =
  run_dot
    {|```python
print("hello")
```|};
  [%expect
    {|
    ```python
    print("hello")
    ```
    |}]
;;

let%expect_test "error: keep_original (default)" =
  run_dot
    {|```dot
invalid dot syntax {{{
```|};
  [%expect {|
    ```dot
    invalid dot syntax {{{
    ```
    |}]
;;

let%expect_test "error: show_error" =
  run_dot
    ~on_error:`Show_error
    {|```dot
invalid dot syntax {{{
```|};
  [%expect {|
    ```=html
    <pre class="dot-error"><code>Error: <stdin>: syntax error in line 1 near 'invalid'</code></pre>
    ```
    |}]
;;
