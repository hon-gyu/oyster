(** Python executor backed by ephemeral [uv] environments and Jupyter [nbconvert]. *)

open Core
module Attribute = Parse.Attribute
open Common

(** Config key for uv-specific frontmatter in oyster config *)
let uv_config_key = "pyproject"

type uv_config =
  { version : float
  ; dependencies : string list
  }
[@@deriving sexp]

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

let is_python (lang : string) : bool =
  let lang' = String.lowercase lang in
  String.equal lang' "python" || String.equal lang' "py"
;;

(** Hash function for Python execution: keys on uv config, attributes, and cell contents.
    Dependencies are sorted before hashing so reordering them is a no-op.

    @param attr_filter filter cells by attribute (e.g. skip cells tagged [.no-exec])
    @param attr_hash_key extract a string from the optional attribute to be used for hashing.
    *)
let hash_fn
      ?(attr_filter : Attribute.t option -> bool = fun _ -> true)
      ?(attr_hash_key : Attribute.t option -> string =
        fun attr_opt -> [%sexp_of: Attribute.t option] attr_opt |> Sexp.to_string)
  : exec_ctx -> string
  =
  Cache.make_hash_fn
    ~config_filter:(fun config ->
      let cfg = uv_config_of_config config in
      let sorted_deps = List.sort cfg.dependencies ~compare:String.compare in
      Some ([%sexp_of: float * string list] (cfg.version, sorted_deps) |> Sexp.to_string))
    ~cell_filter:(fun (c : Common.cell) ->
      match c.lang with
      | Some l when is_python l && attr_filter c.attr ->
        let attr_hash_content = attr_hash_key c.attr in
        let sexp =
          [%sexp_of: string * string] (attr_hash_content, c.content) |> Sexp.to_string
        in
        Some sexp
      | _ -> None)
;;

(** Executor that runs Python cells via an ephemeral [uv] environment.

    Only cells whose [lang] is "python" or "py" (case-insensitive) are
    executed. The optional [attr_filter] allows further selection by Pandoc
    attribute (e.g. skip cells tagged [.no-exec]).

    Cells after filtering are groupped by session id. Each group share
    interpreter state (imports, variables, etc.).

    Outputs are mapped back to the original {!cell} IDs so callers can
    correlate results with source positions even when non-Python cells appear
    in between.

    @param attr_filter see {!filter_group_cells}
    @param attr_session_map see {!filter_group_cells}
    *)
let executor
      ?(attr_filter : Attribute.t option -> bool = fun _ -> true)
      ?(attr_session_map : Attribute.t option -> string = session_id_of_attr)
  : executor
  =
  fun ctx ->
  let uv_config = uv_config_of_config ctx.config in
  let (python_cells_by_session : (string * cell list) list) =
    filter_group_cells ~lang_filter:is_python ~attr_filter ~attr_session_map ctx.inputs
  in
  let (outputs : output list list) =
    List.map python_cells_by_session ~f:(fun (session_id, cells) ->
      let sources = List.map cells ~f:(fun cell -> cell.content) in
      let nb_json = Jupyter.make_notebook sources in
      match
        Jupyter.run_notebook
          ~python_version:uv_config.version
          ~with_args:uv_config.dependencies
          ~nb_json
      with
      | Error msg ->
        List.map cells ~f:(fun cell -> { Common.id = cell.id; res = `Error msg })
      | Ok executed ->
        let outputs = Jupyter.notebook_outputs executed in
        List.map2_exn cells outputs ~f:(fun cell outs ->
          { Common.id = cell.id; res = `Markdown (String.concat ~sep:"\n" outs) }))
  in
  outputs |> List.concat |> List.sort ~compare:(fun a b -> Int.compare a.id b.id)
;;

(* Test
==================== *)

let%test_module "uv_executor" =
  (module struct
    (* Covers: basic output, non-Python cells skipped (bash, id=1),
       shared interpreter state across cells (x = 1 then x + 1),
       and error handling. *)
    let%expect_test "uv_executor: execution" =
      let ctx =
        Common.extract_exec_ctx
          (Parse.of_string
             {|
```python {}
print("hello")
```
```bash {}
echo hi
```
```py
x = 1
print(x)
```
```python {}
gibberish
```
```py
print(x + 1)
```
|})
      in
      print_s [%sexp (executor ctx : Common.output list)];
      [%expect
        {|
        (((id 0) (res (Markdown "hello\n"))) ((id 2) (res (Markdown "1\n")))
         ((id 3) (res (Markdown "NameError: name 'gibberish' is not defined")))
         ((id 4) (res (Markdown "2\n"))))
        |}]
    ;;

    let%expect_test "uv_executor: installs and uses dependency from frontmatter" =
      (* Verifies the full path: frontmatter -> uv_config -> uv --with <dep> ->
     importable package inside the notebook. Uses [packaging] (pure-Python,
     no C extensions) so uv can resolve it quickly without network if cached. *)
      let ctx =
        Common.extract_exec_ctx
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
      print_s [%sexp (executor ctx : Common.output list)];
      [%expect
        {|
    (((id 0) (res (Markdown "2\n"))))
  |}]
    ;;
  end)
;;
