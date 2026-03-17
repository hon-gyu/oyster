(** Jupytext execution pipeline.

    Executes Python code blocks in markdown documents via jupytext,
    inserting outputs as HTML after each code block. *)

open Core

module Config = struct
  type t =
    { eval : bool
    ; deps : string list
    ; env : (string * string) list
    }

  let default = { eval = false; deps = []; env = [] }

  let of_doc (doc : Cmarkit.Doc.t) : t =
    match Parse.Frontmatter.of_doc doc with
    | Some (`O fields) ->
      (match List.Assoc.find fields ~equal:String.equal "jupytext" with
       | Some (`O jt_fields) ->
         let eval =
           match List.Assoc.find jt_fields ~equal:String.equal "eval" with
           | Some (`Bool true) -> true
           | _ -> false
         in
         let deps =
           match List.Assoc.find jt_fields ~equal:String.equal "deps" with
           | Some (`A items) ->
             List.filter_map items ~f:(function
               | `String s -> Some s
               | _ -> None)
           | _ -> []
         in
         let env =
           match List.Assoc.find jt_fields ~equal:String.equal "env" with
           | Some (`O pairs) ->
             List.filter_map pairs ~f:(fun (k, v) ->
               match v with
               | `String s -> Some (k, s)
               | _ -> None)
           | _ -> []
         in
         { eval; deps; env }
       | _ -> default)
    | _ -> default
  ;;
end

let is_python_code_block (cb : Cmarkit.Block.Code_block.t) (meta : Cmarkit.Meta.t) : bool =
  let lang =
    match Cmarkit.Meta.find Parse.Attribute.meta_key meta with
    | Some { lang; _ } -> Some (String.strip lang)
    | None ->
      (match Cmarkit.Block.Code_block.info_string cb with
       | None -> None
       | Some (info, _) ->
         (match Cmarkit.Block.Code_block.language_of_info_string info with
          | None -> None
          | Some (lang, _) -> Some lang))
  in
  match lang with
  | Some l ->
    let ll = String.lowercase l in
    String.equal ll "python" || String.equal ll "py"
  | None -> false
;;

let is_eval_disabled (meta : Cmarkit.Meta.t) : bool =
  match Cmarkit.Meta.find Parse.Attribute.meta_key meta with
  | Some { attribute; _ } ->
    (match List.Assoc.find attribute.kvs ~equal:String.equal "eval" with
     | Some "false" -> true
     | _ -> false)
  | None -> false
;;

let extract_python_blocks (doc : Cmarkit.Doc.t) : string list =
  let block _folder (acc : string list) (b : Cmarkit.Block.t)
    : string list Cmarkit.Folder.result
    =
    match b with
    | Cmarkit.Block.Code_block (cb, meta) ->
      if is_python_code_block cb meta && not (is_eval_disabled meta)
      then (
        let code =
          List.map (Cmarkit.Block.Code_block.code cb) ~f:Cmarkit.Block_line.to_string
          |> String.concat ~sep:"\n"
        in
        Cmarkit.Folder.ret (code :: acc))
      else Cmarkit.Folder.default
    | _ -> Cmarkit.Folder.default
  in
  let folder = Cmarkit.Folder.make ~block_ext_default:(fun _f acc _b -> acc) ~block () in
  Cmarkit.Folder.fold_doc folder [] doc |> List.rev
;;

let build_input (blocks : string list) : string =
  List.map blocks ~f:(fun code -> "```python\n" ^ code ^ "\n```")
  |> String.concat ~sep:"\n\n"
;;

let escape_html (s : string) : string =
  let buf = Buffer.create (String.length s) in
  Cmarkit_html.buffer_add_html_escaped_string buf s;
  Buffer.contents buf
;;

let render_output (output : Yojson.Safe.t) : string =
  let module U = Yojson.Safe.Util in
  let output_type = U.to_string (U.member "output_type" output) in
  match output_type with
  | "stream" ->
    let text =
      U.to_list (U.member "text" output) |> List.map ~f:U.to_string |> String.concat
    in
    sprintf {|<pre class="jt-output">%s</pre>|} (escape_html text)
  | "execute_result" | "display_data" ->
    let data = U.member "data" output in
    let has key =
      match U.member key data with
      | `Null -> false
      | _ -> true
    in
    if has "text/html"
    then U.to_list (U.member "text/html" data) |> List.map ~f:U.to_string |> String.concat
    else if has "image/png"
    then (
      let b64 = U.to_string (U.member "image/png" data) |> String.strip in
      sprintf {|<img src="data:image/png;base64,%s">|} b64)
    else (
      let text =
        U.to_list (U.member "text/plain" data) |> List.map ~f:U.to_string |> String.concat
      in
      sprintf {|<pre class="jt-output">%s</pre>|} (escape_html text))
  | "error" ->
    let traceback =
      U.to_list (U.member "traceback" output)
      |> List.map ~f:U.to_string
      |> String.concat ~sep:"\n"
    in
    sprintf {|<pre class="jt-error">%s</pre>|} (escape_html traceback)
  | _ -> ""
;;

let extract_outputs (notebook : Yojson.Safe.t) : string list =
  let module U = Yojson.Safe.Util in
  let cells = U.to_list (U.member "cells" notebook) in
  let code_cells =
    List.filter cells ~f:(fun c ->
      String.equal (U.to_string (U.member "cell_type" c)) "code")
  in
  List.map code_cells ~f:(fun cell ->
    let outputs = U.to_list (U.member "outputs" cell) in
    List.map outputs ~f:render_output |> String.concat)
;;

let execute (config : Config.t) (input_md : string) : Yojson.Safe.t =
  let tmp = Stdlib.Filename.temp_file "jupytext" ".md" in
  Out_channel.write_all tmp ~data:input_md;
  Exn.protect
    ~f:(fun () ->
      let with_args = List.concat_map config.deps ~f:(fun d -> [ "--with"; d ]) in
      let args =
        [ "uv"; "tool"; "run"; "--with"; "ipykernel" ]
        @ with_args
        @ [ "jupytext"
          ; "--set-kernel"
          ; "python3"
          ; "--from"
          ; "md"
          ; "--to"
          ; "ipynb"
          ; "--execute"
          ; "-o"
          ; "-"
          ; tmp
          ]
      in
      let cmd = String.concat ~sep:" " (List.map args ~f:Filename.quote) in
      let env_prefix =
        match config.env with
        | [] -> ""
        | env ->
          List.map env ~f:(fun (k, v) -> k ^ "=" ^ Filename.quote v)
          |> String.concat ~sep:" "
          |> fun s -> s ^ " "
      in
      let full_cmd = env_prefix ^ cmd in
      let ic = Core_unix.open_process_in full_cmd in
      let stdout_content = In_channel.input_all ic in
      let status = Core_unix.close_process_in ic in
      (match status with
       | Ok () -> ()
       | Error _ -> failwith ("jupytext execution failed for " ^ tmp));
      Yojson.Safe.from_string stdout_content)
    ~finally:(fun () -> Core_unix.unlink tmp)
;;

let insert_outputs (outputs : string list) (doc : Cmarkit.Doc.t) : Cmarkit.Doc.t =
  let outputs_arr = Array.of_list outputs in
  let counter = ref 0 in
  let block _mapper (b : Cmarkit.Block.t) : Cmarkit.Block.t Cmarkit.Mapper.result =
    match b with
    | Cmarkit.Block.Code_block (cb, meta) ->
      if is_python_code_block cb meta && not (is_eval_disabled meta)
      then (
        let idx = !counter in
        incr counter;
        if idx < Array.length outputs_arr && not (String.is_empty outputs_arr.(idx))
        then (
          let html_content = outputs_arr.(idx) in
          let html_cb =
            Cmarkit.Block.Code_block.make
              ~info_string:("=html", Cmarkit.Meta.none)
              (Cmarkit.Block_line.list_of_string html_content)
          in
          let html_block = Cmarkit.Block.Code_block (html_cb, Cmarkit.Meta.none) in
          Cmarkit.Mapper.ret (Cmarkit.Block.Blocks ([ b; html_block ], Cmarkit.Meta.none)))
        else Cmarkit.Mapper.default)
      else Cmarkit.Mapper.default
    | _ -> Cmarkit.Mapper.default
  in
  let mapper =
    Cmarkit.Mapper.make
      ~inline_ext_default:(fun _m i -> Some i)
      ~block_ext_default:(fun _m b -> Some b)
      ~block
      ()
  in
  Cmarkit.Mapper.map_doc mapper doc
;;

let on_parse (path : string) (doc : Cmarkit.Doc.t) : (string * Cmarkit.Doc.t) list =
  let config = Config.of_doc doc in
  if not config.eval
  then [ path, doc ]
  else (
    let blocks = extract_python_blocks doc in
    if List.is_empty blocks
    then [ path, doc ]
    else (
      let input_md = build_input blocks in
      let notebook = execute config input_md in
      let outputs = extract_outputs notebook in
      let doc' = insert_outputs outputs doc in
      [ path, doc' ]))
;;

(* Tests
   ==================== *)

let%test_module "Config" =
  (module struct
    let%expect_test "parse jupytext config from frontmatter" =
      let doc =
        Parse.of_string
          {|---
jupytext:
  eval: true
  deps:
    - matplotlib
    - numpy
  env:
    UV_PYTHON: "3.12"
---
# Hello|}
      in
      let config = Config.of_doc doc in
      printf "eval: %b\n" config.eval;
      printf "deps: %s\n" (String.concat ~sep:", " config.deps);
      List.iter config.env ~f:(fun (k, v) -> printf "env: %s=%s\n" k v);
      [%expect
        {|
        eval: true
        deps: matplotlib, numpy
        env: UV_PYTHON=3.12
        |}]
    ;;

    let%expect_test "no jupytext config" =
      let doc = Parse.of_string "# Hello" in
      let config = Config.of_doc doc in
      printf "eval: %b\n" config.eval;
      [%expect {| eval: false |}]
    ;;

    let%expect_test "jupytext eval false" =
      let doc =
        Parse.of_string
          {|---
jupytext:
  eval: false
---
# Hello|}
      in
      let config = Config.of_doc doc in
      printf "eval: %b\n" config.eval;
      [%expect {| eval: false |}]
    ;;
  end)
;;

let%test_module "extract_python_blocks" =
  (module struct
    let%expect_test "extracts python blocks" =
      let doc =
        Parse.of_string
          {|```python
print("hello")
```

Some text.

```py
x = 1
```

```javascript
console.log("hi")
```|}
      in
      let blocks = extract_python_blocks doc in
      List.iter blocks ~f:(fun code -> printf "---\n%s\n" code);
      [%expect
        {|
        ---
        print("hello")
        ---
        x = 1
        |}]
    ;;

    let%expect_test "skips eval=false blocks" =
      let doc =
        Parse.of_string
          {|```python
print("included")
```

```python {eval=false}
print("excluded")
```|}
      in
      let blocks = extract_python_blocks doc in
      List.iter blocks ~f:(fun code -> printf "---\n%s\n" code);
      [%expect
        {|
        ---
        print("included")
        |}]
    ;;

    let%expect_test "no python blocks" =
      let doc =
        Parse.of_string
          {|```javascript
console.log("hi")
```|}
      in
      let blocks = extract_python_blocks doc in
      printf "count: %d\n" (List.length blocks);
      [%expect {| count: 0 |}]
    ;;
  end)
;;

let%test_module "extract_outputs" =
  (module struct
    let%expect_test "stream output" =
      let notebook =
        Yojson.Safe.from_string
          {|{
        "cells": [
          {
            "cell_type": "code",
            "outputs": [
              {
                "output_type": "stream",
                "text": ["hello\n", "world\n"]
              }
            ]
          }
        ]
      }|}
      in
      let outputs = extract_outputs notebook in
      List.iter outputs ~f:print_endline;
      [%expect
        {|
        <pre class="jt-output">hello
        world
        </pre>
        |}]
    ;;

    let%expect_test "execute_result with text/plain" =
      let notebook =
        Yojson.Safe.from_string
          {|{
        "cells": [
          {
            "cell_type": "code",
            "outputs": [
              {
                "output_type": "execute_result",
                "data": {
                  "text/plain": ["42"]
                }
              }
            ]
          }
        ]
      }|}
      in
      let outputs = extract_outputs notebook in
      List.iter outputs ~f:print_endline;
      [%expect {| <pre class="jt-output">42</pre> |}]
    ;;

    let%expect_test "display_data with image/png" =
      let notebook =
        Yojson.Safe.from_string
          {|{
        "cells": [
          {
            "cell_type": "code",
            "outputs": [
              {
                "output_type": "display_data",
                "data": {
                  "image/png": "abc123",
                  "text/plain": ["<Figure>"]
                }
              }
            ]
          }
        ]
      }|}
      in
      let outputs = extract_outputs notebook in
      List.iter outputs ~f:print_endline;
      [%expect {| <img src="data:image/png;base64,abc123"> |}]
    ;;

    let%expect_test "error output" =
      let notebook =
        Yojson.Safe.from_string
          {|{
        "cells": [
          {
            "cell_type": "code",
            "outputs": [
              {
                "output_type": "error",
                "traceback": ["line 1", "line 2"]
              }
            ]
          }
        ]
      }|}
      in
      let outputs = extract_outputs notebook in
      List.iter outputs ~f:print_endline;
      [%expect
        {|
        <pre class="jt-error">line 1
        line 2</pre>
        |}]
    ;;

    let%expect_test "skips non-code cells" =
      let notebook =
        Yojson.Safe.from_string
          {|{
        "cells": [
          {
            "cell_type": "markdown",
            "source": ["# Hello"]
          },
          {
            "cell_type": "code",
            "outputs": [
              {
                "output_type": "stream",
                "text": ["result"]
              }
            ]
          }
        ]
      }|}
      in
      let outputs = extract_outputs notebook in
      printf "count: %d\n" (List.length outputs);
      List.iter outputs ~f:print_endline;
      [%expect
        {|
        count: 1
        <pre class="jt-output">result</pre>
        |}]
    ;;
  end)
;;

let%test_module "insert_outputs" =
  (module struct
    let%expect_test "inserts output after python blocks" =
      let doc =
        Parse.of_string
          {|```python
print("hello")
```

Some text.

```python
x = 1
```|}
      in
      let outputs = [ "<p>hello</p>"; "<p>1</p>" ] in
      let doc' = insert_outputs outputs doc in
      print_endline (Parse.commonmark_of_doc doc');
      [%expect
        {|
        ```python
        print("hello")
        ```
        ```=html
        <p>hello</p>
        ```

        Some text.

        ```python
        x = 1
        ```
        ```=html
        <p>1</p>
        ```
        |}]
    ;;

    let%expect_test "skips eval=false blocks" =
      let doc =
        Parse.of_string
          {|```python
print("included")
```

```python {eval=false}
print("excluded")
```|}
      in
      let outputs = [ "<p>included</p>" ] in
      let doc' = insert_outputs outputs doc in
      print_endline (Parse.commonmark_of_doc doc');
      [%expect
        {|
        ```python
        print("included")
        ```
        ```=html
        <p>included</p>
        ```

        ```python {eval=false}
        print("excluded")
        ```
        |}]
    ;;

    let%expect_test "empty output not inserted" =
      let doc =
        Parse.of_string
          {|```python
x = 1
```|}
      in
      let outputs = [ "" ] in
      let doc' = insert_outputs outputs doc in
      print_endline (Parse.commonmark_of_doc doc');
      [%expect
        {|
        ```python
        x = 1
        ```
        |}]
    ;;
  end)
;;
