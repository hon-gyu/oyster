open Core
module Attribute = Parse.Attribute
module Frontmatter = Parse.Frontmatter

type cell =
  { id : int
    (** Unique code block id. Most of the time it will be the order of appearance in code blocks in the document *)
  ; lang : string option
  ; info : Attribute.t option
  ; content : string
  }
[@@deriving sexp_of]

type output =
  { id : int
  ; res : [ `Html of string | `Markdown of string | `Error of string ]
  }
[@@deriving sexp_of]

type exec_ctx =
  { config : Yaml.value
  ; inputs : cell list
  }

type exec_result = output list
type executor = exec_ctx -> exec_result

let todo () = failwith "TODO"

let extract_code_blocks (doc : Cmarkit.Doc.t) : cell list =
  let block_id = ref 0 in
  let block _folder (acc : cell list) (b : Cmarkit.Block.t)
    : cell list Cmarkit.Folder.result
    =
    match b with
    | Cmarkit.Block.Code_block (cb, meta) ->
      let (content : string) =
        List.map (Cmarkit.Block.Code_block.code cb) ~f:Cmarkit.Block_line.to_string
        |> String.concat ~sep:"\n"
      in
      let cb_info = Cmarkit.Meta.find Attribute.meta_key meta in
      let cell =
        { id = !block_id
        ; lang = cb_info |> Option.map ~f:(fun ci -> ci.lang)
        ; info = cb_info |> Option.map ~f:(fun ci -> ci.attribute)
        ; content
        }
      in
      incr block_id;
      Cmarkit.Folder.ret (cell :: acc)
    | _ -> Cmarkit.Folder.default
  in
  let folder = Cmarkit.Folder.make ~block_ext_default:(fun _f acc _b -> acc) ~block () in
  Cmarkit.Folder.fold_doc folder [] doc |> List.rev
;;

let%expect_test "extract_code_blocks" =
  let doc =
    Parse.of_string
      {|
Hi
```python
print("Hello")
```
.
```bash {.foo baz=zzz}
bar
```
|}
  in
  let cells = extract_code_blocks doc in
  print_s [%sexp (cells : cell list)];
  [%expect
    {|
    (((id 0) (lang ()) (info ()) (content "print(\"Hello\")"))
     ((id 1) (lang (bash)) (info (((id ()) (classes (.foo)) (kvs ((baz zzz))))))
      (content bar)))
    |}]
;;

let extract_exec_ctx (doc : Cmarkit.Doc.t) : exec_ctx =
  let config =
    match Parse.Frontmatter.of_doc doc with
    | Some (`O fields) ->
      (match List.Assoc.find fields ~equal:String.equal "oyster" with
       | Some (`O oys_fields) -> Yaml.(`O oys_fields)
       | Some _ -> failwith "Invalid frontmatter"
       | None -> Yaml.(`O []))
    | Some _ -> failwith "Invalid frontmatter"
    | None -> Yaml.(`O [])
  in
  let inputs = extract_code_blocks doc in
  { config; inputs }
;;

(* uv
==================== *)

type uv_config =
  { version : float
  ; dependencies : string list
  }
[@@deriving sexp_of]

let default_uv_config = { version = 3.13; dependencies = [] }

let uv_config_of_config (config : Yaml.value) : uv_config =
  match Yaml.Util.find "pyproject" config with
  | Ok (Some (`O fields)) ->
    let version =
      match List.Assoc.find fields ~equal:String.equal "version" with
      | Some (`Float v) -> v
      | Some (`String s) -> Float.of_string s
      | _ -> default_uv_config.version
    in
    let dependencies =
      match List.Assoc.find fields ~equal:String.equal "dependencies" with
      | Some (`A deps) ->
        List.filter_map deps ~f:(function
          | `String s -> Some s
          | _ -> None)
      | _ -> default_uv_config.dependencies
    in
    { version; dependencies }
  | _ -> default_uv_config
;;

let make_notebook (cells : string list) =
  let make_cell source =
    `Assoc
      [ "cell_type", `String "code"
      ; "source", `String source
      ; "metadata", `Assoc []
      ; "outputs", `List []
      ; "execution_count", `Null
      ]
  in
  `Assoc
    [ "nbformat", `Int 4
    ; "nbformat_minor", `Int 5
    ; ( "metadata"
      , `Assoc
          [ ( "kernelspec"
            , `Assoc
                [ "display_name", `String "Python 3"
                ; "language", `String "python"
                ; "name", `String "python3"
                ] )
          ] )
    ; "cells", `List (List.map cells ~f:make_cell)
    ]
;;

let run_notebook ~(uv_config : uv_config) ~(nb_json : Yojson.Basic.t)
  : (Yojson.Basic.t, string) result
  =
  let tmp_in = Filename_unix.temp_file "nb_in" ".ipynb" in
  (* jupyter appends .ipynb to --output, so omit the extension here *)
  let tmp_out_base = Filename_unix.temp_file "nb_out" "" in
  let tmp_out = tmp_out_base ^ ".ipynb" in
  Yojson.Basic.to_file tmp_in nb_json;
  let with_args =
    "jupyter" :: uv_config.dependencies
    |> List.map ~f:(fun dep -> sprintf "--with %s" dep)
    |> String.concat ~sep:" "
  in
  let cmd =
    sprintf
      "JUPYTER_CONFIG_DIR=/dev/null uv run --python %g %s jupyter nbconvert --to notebook \
       --execute %s --output %s 2>/dev/null"
      uv_config.version
      with_args
      tmp_in
      tmp_out_base
  in
  match Core_unix.system cmd with
  | Ok () ->
    let result = Yojson.Basic.from_file tmp_out in
    Sys_unix.remove tmp_in;
    Sys_unix.remove tmp_out_base;
    Sys_unix.remove tmp_out;
    Ok result
  | Error _ -> Error "nbconvert failed"
;;

(** Jupyter multiline_string: either a plain string or an array of strings *)
let multiline_string (j : Yojson.Basic.t) : string =
  let open Yojson.Basic.Util in
  match j with
  | `String s -> s
  | `List _ -> j |> to_list |> List.map ~f:to_string |> String.concat ~sep:""
  | _ -> ""
;;

let cell_outputs (cell : Yojson.Basic.t) : string list =
  let open Yojson.Basic.Util in
  cell
  |> member "outputs"
  |> to_list
  |> List.filter_map ~f:(fun output ->
    match output |> member "output_type" |> to_string with
    | "stream" -> Some (output |> member "text" |> multiline_string)
    | "execute_result" | "display_data" ->
      Some (output |> member "data" |> member "text/plain" |> multiline_string)
    | "error" ->
      let ename = output |> member "ename" |> to_string in
      let evalue = output |> member "evalue" |> to_string in
      Some (sprintf "%s: %s" ename evalue)
    | _ -> None)
;;

let notebook_outputs (nb_json : Yojson.Basic.t) : string list list =
  let open Yojson.Basic.Util in
  nb_json
  |> member "cells"
  |> to_list
  |> List.filter ~f:(fun c -> String.equal (c |> member "cell_type" |> to_string) "code")
  |> List.map ~f:cell_outputs
;;

let uv_executor ?(attr_filter : Attribute.t option -> bool = fun _ -> true) : executor =
  fun ctx ->
  let uv_config = uv_config_of_config ctx.config in
  let (python_cells : cell list) =
    List.filter ctx.inputs ~f:(fun cell ->
      match cell.lang with
      | Some l ->
        let l' = l |> String.lowercase in
        let is_py = String.equal l' "python" || String.equal l' "py" in
        is_py && attr_filter cell.info
      | None -> false)
  in
  let sources = List.map python_cells ~f:(fun cell -> cell.content) in
  let nb_json = make_notebook sources in
  match run_notebook ~uv_config ~nb_json with
  | Error msg -> List.map python_cells ~f:(fun cell -> { id = cell.id; res = `Error msg })
  | Ok executed ->
    let outputs = notebook_outputs executed in
    List.map2_exn python_cells outputs ~f:(fun cell outs ->
      { id = cell.id; res = `Markdown (String.concat ~sep:"\n" outs) })
;;

(* Unit test: dependency config is parsed from frontmatter *)
let%expect_test "uv_config_of_config: parses version and dependencies" =
  let config =
    match
      Yaml.of_string
        {|
pyproject:
  version: "3.11"
  dependencies:
    - numpy
    - pandas
|}
    with
    | Ok v -> v
    | Error (`Msg m) -> failwith m
  in
  let cfg = uv_config_of_config config in
  print_s [%sexp (cfg : uv_config)];
  [%expect {|
    ((version 3.11) (dependencies (numpy pandas)))
  |}]
;;

(* Integration tests: require uv + jupyter in PATH *)

let%expect_test "uv_executor: basic" =
  let ctx = extract_exec_ctx (Parse.of_string {|
```python {}
print("hello")
```
|}) in
  print_s [%sexp (uv_executor ctx : output list)];
  [%expect {|
    (((id 0) (res (Markdown "hello\n"))))
  |}]
;;

let%expect_test "uv_executor: python cells with other language in between" =
  (* bash cell (id=1) is skipped; python cells (ids 0,2) share notebook state *)
  let ctx =
    extract_exec_ctx
      (Parse.of_string
         {|
```python {}
x = 1
print(x)
```
```bash {}
echo hi
```
```python {}
print(x + 1)
```
|})
  in
  print_s [%sexp (uv_executor ctx : output list)];
  [%expect {|
    (((id 0) (res (Markdown "1\n"))) ((id 2) (res (Markdown "2\n"))))
  |}]
;;

let%expect_test "uv_executor: attr_filter excludes matching cells" =
  let ctx =
    extract_exec_ctx
      (Parse.of_string
         {|
```python {}
print("runs")
```
```python {.no-exec}
print("skipped")
```
|})
  in
  let outputs =
    uv_executor
      ~attr_filter:(fun attr ->
        match attr with
        | Some { classes; _ } ->
          not (List.mem classes ".no-exec" ~equal:String.equal)
        | None -> true)
      ctx
  in
  print_s [%sexp (outputs : output list)];
  [%expect {|
    (((id 0) (res (Markdown "runs\n"))))
  |}]
;;
