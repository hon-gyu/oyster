open Core
module Attribute = Parse.Attribute
module Frontmatter = Parse.Frontmatter

(** Config key for Oyster-specific frontmatter in frontmatter *)
let oyster_config_key = "oyster"

(** Config key for uv-specific frontmatter in oyster config *)
let uv_config_key = "pyproject"

type cell =
  { id : int
    (** Unique code block id. Most of the time it will be the order of appearance in code blocks in the document *)
  ; lang : string option
  ; attr : Attribute.t option
  ; content : string
  }
[@@deriving sexp_of]

type output =
  { id : int
  ; res : [ `Html of string | `Markdown of string | `Error of string ]
    (** [`Error] is only used when
    nbconvert itself fails (process-level failure) *)
  }
[@@deriving sexp_of]

type exec_ctx =
  { config : Yaml.value
  ; inputs : cell list
  }

type executor = exec_ctx -> output list

let todo () = failwith "TODO"

(** Walk the Cmarkit AST and collect every fenced code block as a {!cell}.
    Cells are numbered in document order starting from 0.

    [lang] and [attr] are populated from the {!Attribute.code_block_info} attached to the
    block by {!Attribute.tag_cb_attr_meta} during parsing, which tags every fenced code
    block that has a non-empty info string. *)
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
        ; attr = cb_info |> Option.bind ~f:(fun ci -> ci.attribute)
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
    (((id 0) (lang (python)) (attr ()) (content "print(\"Hello\")"))
     ((id 1) (lang (bash)) (attr (((id ()) (classes (.foo)) (kvs ((baz zzz))))))
      (content bar)))
    |}]
;;

(** Build an {!exec_ctx} from a parsed document.
    - [config]: the [oyster] mapping from the YAML frontmatter, or an empty
      mapping if absent. Executors read their own sub-keys from this value.
    - [inputs]: all code blocks in document order via {!extract_code_blocks}. *)
let extract_exec_ctx (doc : Cmarkit.Doc.t) : exec_ctx =
  let config =
    match Parse.Frontmatter.of_doc doc with
    | Some (`O fields) ->
      (match List.Assoc.find fields ~equal:String.equal oyster_config_key with
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

(** Read [uv_config] from the [pyproject] sub-key of the oyster config.
    Both [version: 3.11] (YAML float) and [version: "3.11"] (string) are
    accepted. Falls back to {!default_uv_config} for any missing field.

    Expected frontmatter shape:
    {v
    oyster:
      pyproject:
        version: "3.13"
        dependencies:
          - numpy
          - pandas
    v} *)
let uv_config_of_config (config : Yaml.value) : uv_config =
  match Yaml.Util.find uv_config_key config with
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

(** Build a minimal [.ipynb] JSON with a Python 3 kernelspec.
    Each element of [cells] becomes one code cell; source is stored as a plain
    string (Jupyter's [multiline_string] format accepts both a bare string and
    an array of lines). *)
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

(** Execute a notebook JSON via [uv run ... jupyter nbconvert].
    Dependencies in [uv_config] are passed as [--with <dep>] arguments so uv
    provisions an ephemeral virtual environment — no persistent venv needed.

    Implementation notes:
    - [JUPYTER_CONFIG_DIR=/dev/null] prevents local Jupyter config (e.g.
      contrib extensions) from breaking the clean uv environment.
    - [jupyter nbconvert --output] takes a base name and appends [.ipynb]
      itself, so we strip the extension from the temp output path and
      reconstruct it after the command. *)
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
      "JUPYTER_CONFIG_DIR=/dev/null uv run --python %g %s jupyter nbconvert --to \
       notebook --execute --allow-errors %s --output %s 2>/dev/null"
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

(** Extract text from each output entry of a Jupyter code cell.
    Handles the four output types defined by nbformat:
    - [stream]: stdout/stderr text
    - [execute_result] / [display_data]: [text/plain] from the MIME bundle
    - [error]: formatted as ["ExcType: message"]
    Other output types are silently ignored. *)
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

(** Return the outputs of every code cell in an executed notebook, in order.
    Each element of the returned list corresponds to one code cell and is
    itself a list of output strings (one per output entry). *)
let notebook_outputs (nb_json : Yojson.Basic.t) : string list list =
  let open Yojson.Basic.Util in
  nb_json
  |> member "cells"
  |> to_list
  |> List.filter ~f:(fun c -> String.equal (c |> member "cell_type" |> to_string) "code")
  |> List.map ~f:cell_outputs
;;

(** Executor that runs Python cells via an ephemeral [uv] environment.

    Only cells whose [lang] is ["python"] or ["py"] (case-insensitive) are
    executed. The optional [attr_filter] allows further selection by Pandoc
    attribute (e.g. skip cells tagged [.no-exec]).

    All selected cells are assembled into a single notebook and executed
    together, so they share interpreter state (imports, variables, etc.).
    Outputs are mapped back to the original {!cell} IDs so callers can
    correlate results with source positions even when non-Python cells appear
    in between. *)
let uv_executor ?(attr_filter : Attribute.t option -> bool = fun _ -> true) : executor =
  fun ctx ->
  let uv_config = uv_config_of_config ctx.config in
  let (python_cells : cell list) =
    List.filter ctx.inputs ~f:(fun cell ->
      match cell.lang with
      | Some l ->
        let l' = l |> String.lowercase in
        let is_py = String.equal l' "python" || String.equal l' "py" in
        is_py && attr_filter cell.attr
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

(** Splice executor outputs back into a document, producing a new {!Cmarkit.Doc.t}.

    For every code block that has a matching entry in [outputs] (matched by the
    same integer ID assigned by {!extract_code_blocks}), an output block is
    inserted according to [loc_map]:
    - [`Append] (default): keep the source cell and append the output block
      immediately after it.
    - [`Replace]: replace the source cell entirely with the output block.

    [loc_map] receives the Pandoc {!Attribute.t} of the source cell (if any),
    allowing callers to drive the append/replace decision from cell attributes
    (e.g. [.replace] class → Replace).

    The output block info string depends on the {!output.res} variant:
    - [`Html _]     → ["=html"]  (raw HTML, passed through by the renderer)
    - [`Markdown _] → no info string (preformatted plain-text block)
    - [`Error _]    → ["=html"]  (TBD)

    Cells with no matching entry in [outputs] are left untouched. *)
let merge_outputs
      ?(loc_map : Attribute.t option -> [ `Append | `Replace ] = fun _ -> `Append)
      (outputs : output list)
      (doc : Cmarkit.Doc.t)
  : Cmarkit.Doc.t
  =
  (* Build a map from cell id to its result for O(log n) lookup in the fold. *)
  let output_map =
    List.fold
      outputs
      ~init:(Map.empty (module Int))
      ~f:(fun acc o -> Map.set acc ~key:o.id ~data:o.res)
  in
  (* Counter mirrors extract_code_blocks: increments on every code block so
     IDs assigned here match those in the exec_ctx. *)
  let block_id = ref 0 in
  let block _mapper (b : Cmarkit.Block.t) : Cmarkit.Block.t Cmarkit.Mapper.result =
    match b with
    | Cmarkit.Block.Code_block (_cb, meta) ->
      let id = !block_id in
      incr block_id;
      (match Map.find output_map id with
       | None -> Cmarkit.Mapper.default
       | Some res ->
         let attr =
           Cmarkit.Meta.find Attribute.meta_key meta
           |> Option.bind ~f:(fun ci -> ci.attribute)
         in
         let info_str, content =
           match res with
           | `Html s -> "=html", s
           | `Markdown s -> "", s
           | `Error s -> "=html", s
         in
         let out_cb =
           Cmarkit.Block.Code_block.make
             ~info_string:(info_str, Cmarkit.Meta.none)
             (Cmarkit.Block_line.list_of_string content)
         in
         let out_block = Cmarkit.Block.Code_block (out_cb, Cmarkit.Meta.none) in
         (match loc_map attr with
          | `Append ->
            Cmarkit.Mapper.ret
              (Cmarkit.Block.Blocks ([ b; out_block ], Cmarkit.Meta.none))
          | `Replace -> Cmarkit.Mapper.ret out_block))
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

let%test_module "uv_executor" =
  (module struct
    let%expect_test "uv_executor: basic" =
      let ctx =
        extract_exec_ctx
          (Parse.of_string
             {|
```python {}
print("hello")
```
|})
      in
      print_s [%sexp (uv_executor ctx : output list)];
      [%expect
        {|
    (((id 0) (res (Markdown "hello\n"))))
  |}]
    ;;

    let%expect_test "uv_executor: error" =
      let ctx =
        extract_exec_ctx
          (Parse.of_string
             {|
```py
print("hello")
```

```python {}
gibberish
```

```py
2
```
|})
      in
      print_s [%sexp (uv_executor ctx : output list)];
      [%expect
        {|
        (((id 0) (res (Markdown "hello\n")))
         ((id 1) (res (Markdown "NameError: name 'gibberish' is not defined")))
         ((id 2) (res (Markdown 2))))
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
      [%expect
        {|
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
            | Some { classes; _ } -> not (List.mem classes ".no-exec" ~equal:String.equal)
            | None -> true)
          ctx
      in
      print_s [%sexp (outputs : output list)];
      [%expect
        {|
    (((id 0) (res (Markdown "runs\n"))))
  |}]
    ;;

    let%expect_test "uv_executor: installs and uses dependency from frontmatter" =
      (* Verifies the full path: frontmatter -> uv_config -> uv --with <dep> ->
     importable package inside the notebook. Uses [packaging] (pure-Python,
     no C extensions) so uv can resolve it quickly without network if cached. *)
      let ctx =
        extract_exec_ctx
          (Parse.of_string
             {|---
oyster:
  pyproject:
    dependencies:
      - packaging
---
```python {}
from packaging.version import Version
print(Version("2.1.0").major)
```
|})
      in
      print_s [%sexp (uv_executor ctx : output list)];
      [%expect
        {|
    (((id 0) (res (Markdown "2\n"))))
  |}]
    ;;
  end)
;;
