(** Python executor backed by ephemeral [uv] environments and Jupyter [nbconvert]. *)

open Core
module Attribute = Parse.Attribute

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

(** Compute a cache key hash from the filtered python cells and uv config.
    Dependencies are sorted before hashing so reordering them is a no-op. *)
let compute_hash (python_cells : Common.cell list) (cfg : uv_config) : string =
  let contents = List.map python_cells ~f:(fun c -> c.content) in
  let sorted_deps = List.sort cfg.dependencies ~compare:String.compare in
  [%sexp_of: string list * float * string list] (contents, cfg.version, sorted_deps)
  |> Sexp.to_string
  |> Md5.digest_string
  |> Md5.to_hex
;;

(** Executor that runs Python cells via an ephemeral [uv] environment.

    Only cells whose [lang] is "python" or "py" (case-insensitive) are
    executed. The optional [attr_filter] allows further selection by Pandoc
    attribute (e.g. skip cells tagged [.no-exec]).

    All selected cells are assembled into a single notebook and executed
    together, so they share interpreter state (imports, variables, etc.).
    Outputs are mapped back to the original {!Common.cell} IDs so callers can
    correlate results with source positions even when non-Python cells appear
    in between. *)
let uv_executor ?(attr_filter : Attribute.t option -> bool = fun _ -> true)
  : Common.executor
  =
  fun ctx ->
  let uv_config = uv_config_of_config ctx.config in
  let (python_cells : Common.cell list) =
    Common.filter_cells ~lang_filter:is_python ~attr_filter ctx.inputs
  in
  let sources = List.map python_cells ~f:(fun cell -> cell.content) in
  let nb_json = Jupyter.make_notebook sources in
  match
    Jupyter.run_notebook
      ~python_version:uv_config.version
      ~with_args:uv_config.dependencies
      ~nb_json
  with
  | Error msg ->
    List.map python_cells ~f:(fun cell -> { Common.id = cell.id; res = `Error msg })
  | Ok executed ->
    let outputs = Jupyter.notebook_outputs executed in
    List.map2_exn python_cells outputs ~f:(fun cell outs ->
      { Common.id = cell.id; res = `Markdown (String.concat ~sep:"\n" outs) })
;;

(** Execute Python cells in [ctx], consulting [cache] first if provided.
    On a cache miss the notebook is run via {!uv_executor} and the result is
    stored back into [cache] before returning. *)
let run_py
      ?(attr_filter : Attribute.t option -> bool = fun _ -> true)
      ?(cache : Cache.cache option)
      ~(path : string)
      (ctx : Common.exec_ctx)
  : Common.output list
  =
  let uv_config = uv_config_of_config ctx.config in
  let python_cells = Common.filter_cells ~lang_filter:is_python ~attr_filter ctx.inputs in
  let hash = compute_hash python_cells uv_config in
  Cache.run_with ?cache ~path ~hash ~executor:(fun () -> uv_executor ~attr_filter ctx) ()
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
      print_s [%sexp (uv_executor ctx : Common.output list)];
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
      print_s [%sexp (uv_executor ctx : Common.output list)];
      [%expect
        {|
    (((id 0) (res (Markdown "2\n"))))
  |}]
    ;;
  end)
;;
