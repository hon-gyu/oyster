(** Execute fenced code blocks from OysterMark documents and splice results back. *)

open Core
include Common
module Cache = Cache
module Uv = Uv
module Jupyter = Jupyter

(** Hash function that keys on code blocks of language [lang], ignoring config *)
let hash_fn_of_lang (lang : string) : exec_ctx -> string =
  Cache.make_hash_fn
    ~config_filter:(fun _ -> None)
    ~cell_filter:(fun (c : cell) ->
      match c.lang with
      | Some l when String.equal (String.lowercase l) lang -> Some c.content
      | _ -> None)
;;

(** Splice executor outputs back into a document, producing a new [Cmarkit.Doc.t].

    For every code block that has a matching entry in [outputs] (matched by the
    same integer ID assigned by {!extract_code_blocks}), an output block is
    inserted according to [loc_map].

    @param loc_map receives the Pandoc {!Attribute.t} of the source cell (if any),
    allowing callers to drive the append/replace decision from cell attributes
    (e.g. [.replace] class → Replace):
    - [`Append] (default): keep the source cell and append the output block
      immediately after it.
    - [`Replace]: replace the source cell entirely with the output block.
    - [`Silent]: execute the code but drop both the source cell and its output
      from the document.

    @param outputs The output block info string depends on the {!output.res} variant:
    - [`Html _]     → ["=html"]  (raw HTML, passed through by the renderer)
    - [`Markdown _] → no info string (preformatted plain-text block)
    - [`Error _]    → ["=html"]  (TBD)

    Cells with no matching entry in [outputs] are left untouched. *)
let merge_outputs
      ?(loc_map : Attribute.t option -> [ `Append | `Replace | `Silent ] =
        fun _ -> `Append)
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
          | `Replace -> Cmarkit.Mapper.ret out_block
          | `Silent -> Cmarkit.Mapper.ret (Cmarkit.Block.Blocks ([], Cmarkit.Meta.none))))
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

(** Executor for the synthetic [echo] language: returns each cell's source unchanged
    as its output. Intended for tests that need to exercise caching without invoking
    any external process. *)
let echo_executor (ctx : exec_ctx) : output list =
  List.filter_map ctx.inputs ~f:(fun cell ->
    match cell.lang with
    | Some l when String.equal (String.lowercase l) "echo" ->
      Some { id = cell.id; res = `Markdown cell.content }
    | _ -> None)
;;

let is_bash (lang : string) : bool =
  let lang' = String.lowercase lang in
  String.equal lang' "bash" || String.equal lang' "sh"
;;

(** Run a multi-cell bash session.

    Each cell's code is wrapped in [{ ... } > tmpfile 2>&1] so that:
    - All cells run in the same process (shared functions, variables, cwd)
    - Each cell's stdout+stderr is captured to its own temp file

    Returns [Ok outputs] with per-cell strings, or [Error msg] on process failure. *)
let run_bash_session ?(extra_env : string list = []) (cells : cell list)
  : (string list, string) result
  =
  let tmp = Filename_unix.temp_dir "oyster_bash" "" in
  let out_file i = tmp ^/ sprintf "cell_%d.out" i in
  let script =
    List.mapi cells ~f:(fun i cell ->
      sprintf "{\n%s\n} > %s 2>&1" cell.content (out_file i))
    |> String.concat ~sep:"\n"
  in
  try
    let env = Array.append (Core_unix.environment ()) (Array.of_list extra_env) in
    let pc = Core_unix.open_process_full ~env "/bin/bash" in
    Out_channel.output_string pc.stdin script;
    Out_channel.close pc.stdin;
    (* Drain stdout/stderr so the process doesn't block *)
    let _out = In_channel.input_all pc.stdout in
    let proc_err = In_channel.input_all pc.stderr in
    let status = Core_unix.close_process_full pc in
    match status with
    | Ok () | Error _ ->
      let outputs =
        List.mapi cells ~f:(fun i _cell ->
          let path = out_file i in
          if Sys_unix.file_exists_exn path
          then In_channel.read_all path
          else
            (* Cell didn't run (earlier cell caused exit) *)
            proc_err)
      in
      (* Clean up temp files *)
      List.iteri cells ~f:(fun i _ ->
        let path = out_file i in
        if Sys_unix.file_exists_exn path then Sys_unix.remove path);
      (try Core_unix.rmdir tmp with
       | _ -> ());
      Ok outputs
  with
  | exn ->
    (try Core_unix.rmdir tmp with
     | _ -> ());
    Error (Exn.to_string exn)
;;

(** Executor for [bash]/[sh] code blocks.

    Cells are grouped by session ID (from the [session_id] attribute key).
    Cells within the same session run in a single [/bin/bash] process so they
    share shell state (functions, variables, cwd, etc.).  Each cell's
    stdout+stderr is captured individually via temp-file redirection.

    @param attr_filter see {!filter_group_cells}
    @param attr_session_map see {!filter_group_cells} *)
let bash_executor
      ?(attr_filter : Attribute.t option -> bool = fun _ -> true)
      ?(attr_session_map : Attribute.t option -> string = session_id_of_attr)
  : executor
  =
  fun ctx ->
  let bash_cells_by_session =
    filter_group_cells ~lang_filter:is_bash ~attr_filter ~attr_session_map ctx.inputs
  in
  let outputs =
    List.map bash_cells_by_session ~f:(fun (_session_id, cells) ->
      match run_bash_session cells with
      | Ok outs ->
        List.map2_exn cells outs ~f:(fun cell out ->
          { id = cell.id; res = `Markdown out })
      | Error msg -> List.map cells ~f:(fun cell -> { id = cell.id; res = `Error msg }))
  in
  outputs |> List.concat |> List.sort ~compare:(fun a b -> Int.compare a.id b.id)
;;

(** Execute code blocks with trace collection, filling [trace] placeholder blocks
    with formatted span output.

    Wraps the executor in {!Trace_collect.with_collect} to capture OpenTelemetry
    spans emitted during execution. Any code block with [lang = "trace"] is treated
    as a placeholder: its content is replaced with the formatted trace tree. *)
let traced_executor_of_executor
      ?(filter_keys : string list list = [])
      ?(scrub_keys : string list list = [])
      (executor : exec_ctx -> output list)
      (ctx : exec_ctx)
  : output list
  =
  let module OT = Opentelemetry_proto.Trace in
  let tc = Trace_collect.create () in
  let receiver = Trace_collect.Otlp_receiver.create () in
  Trace_collect.Otlp_receiver.start receiver;
  let endpoint = Trace_collect.Otlp_receiver.endpoint receiver in
  Core_unix.putenv ~key:"OTEL_EXPORTER_OTLP_ENDPOINT" ~data:endpoint;
  let outputs = Trace_collect.with_collect tc (fun () -> executor ctx) in
  Core_unix.unsetenv "OTEL_EXPORTER_OTLP_ENDPOINT";
  Trace_collect.Otlp_receiver.stop receiver;
  let (all_spans : OT.span list) =
    Trace_collect.spans tc @ Trace_collect.Otlp_receiver.spans receiver
  in
  let all_spans' =
    all_spans
    |> Trace_collect.Span_pipeline.normalize_duration
    |> Trace_collect.Span_pipeline.filter_attributes ~remove:filter_keys
    |> Trace_collect.Span_pipeline.scrub_attributes ~scrub:scrub_keys
  in
  let trace_text = Trace_collect.Trace_pp.format all_spans' in
  let trace_outputs =
    List.filter_map ctx.inputs ~f:(fun (cell : cell) ->
      match cell.lang with
      | Some l when String.equal (String.lowercase l) "trace" ->
        Some { id = cell.id; res = `Markdown trace_text }
      | _ -> None)
  in
  outputs @ trace_outputs
;;

(* Test
==================== *)

let%test_module "cache" =
  (module struct
    let echo_doc =
      Parse.of_string
        {|
```echo {}
hello
```
|}
    ;;

    let echo_hash doc = hash_fn_of_lang "echo" (extract_exec_ctx doc)

    let%expect_test "run_with: cache hit returns cached output, not real execution" =
      let ctx = extract_exec_ctx echo_doc in
      let hash = echo_hash echo_doc in
      let fake = [ { id = 0; res = `Markdown "FAKE" } ] in
      let cache = Cache.empty_cache () in
      Cache.cache_set cache ~path:"test.md" ~hash ~outputs:fake;
      let result =
        Cache.run_with
          ~cache
          ~path:"test.md"
          ~hash
          ~executor:(fun () -> echo_executor ctx)
          ()
      in
      print_s [%sexp (result : output list)];
      [%expect {| (((id 0) (res (Markdown FAKE)))) |}]
    ;;

    let%expect_test "run_with: cache miss executes and populates cache" =
      let ctx = extract_exec_ctx echo_doc in
      let hash = echo_hash echo_doc in
      let cache = Cache.empty_cache () in
      let _first =
        Cache.run_with
          ~cache
          ~path:"test.md"
          ~hash
          ~executor:(fun () -> echo_executor ctx)
          ()
      in
      let cached = Cache.cache_lookup cache ~path:"test.md" ~hash in
      print_s [%sexp (cached : output list option)];
      [%expect {| ((((id 0) (res (Markdown hello))))) |}]
    ;;

    let%expect_test "full lifecycle: miss → save → load → hit" =
      let tmp = Filename_unix.temp_dir "oyster_cache_test" "" in
      let ctx = extract_exec_ctx echo_doc in
      let hash = echo_hash echo_doc in
      (* Cold start: cache miss → echo execution, no external process *)
      let cache1 = Cache.load_cache ~dir:tmp in
      let real_out =
        Cache.run_with
          ~cache:cache1
          ~path:"test.md"
          ~hash
          ~executor:(fun () -> echo_executor ctx)
          ()
      in
      print_s [%sexp (real_out : output list)];
      (* Tamper in-memory entry so we can tell whether disk roundtrip succeeded *)
      Cache.cache_set
        cache1
        ~path:"test.md"
        ~hash
        ~outputs:[ { id = 0; res = `Markdown "PERSISTED" } ];
      Cache.save_cache cache1 ~dir:tmp;
      (* Warm start: load from disk, run again — must return tampered value *)
      let cache2 = Cache.load_cache ~dir:tmp in
      let cached_out =
        Cache.run_with
          ~cache:cache2
          ~path:"test.md"
          ~hash
          ~executor:(fun () -> echo_executor ctx)
          ()
      in
      print_s [%sexp (cached_out : output list)];
      [%expect
        {|
        (((id 0) (res (Markdown hello))))
        (((id 0) (res (Markdown PERSISTED))))
        |}]
    ;;
  end)
;;

let%test_module "traced_executor_of_executor" =
  (module struct
    let%expect_test "passes through base outputs and fills trace placeholder" =
      let doc =
        Parse.of_string
          {|
```echo {}
hello
```
```trace
```
|}
      in
      let ctx = extract_exec_ctx doc in
      let result = traced_executor_of_executor echo_executor ctx in
      print_s [%sexp (result : output list)];
      [%expect
        {|
        (((id 0) (res (Markdown hello))) ((id 1) (res (Markdown ""))))
        |}]
    ;;

    let%expect_test "no trace block — only base outputs returned" =
      let doc =
        Parse.of_string
          {|
```echo {}
world
```
|}
      in
      let ctx = extract_exec_ctx doc in
      let result = traced_executor_of_executor echo_executor ctx in
      print_s [%sexp (result : output list)];
      [%expect {| (((id 0) (res (Markdown world)))) |}]
    ;;

    let traced_bash_executor =
      traced_executor_of_executor
        ~scrub_keys:[ [ "process.owner" ]; [ "process.pid" ]; [ "process.parent_pid" ] ]
        bash_executor
    ;;

    let%expect_test "bash with otel-cli captures external traces" =
      let doc =
        Parse.of_string
          {|
```bash
otel-cli exec --service oyster-test --name "fetch-data" -- echo "fetched"
```
```trace
```
|}
      in
      let ctx = extract_exec_ctx doc in
      let result = traced_bash_executor ctx in
      let (trace_output : string) =
        List.find_map_exn result ~f:(fun o ->
          match o.res with
          | `Markdown s when o.id = 1 -> Some s
          | _ -> None)
      in
      print_endline trace_output;
      [%expect
        {| fetch-data 1us process.command=echo process.command_args=? process.owner=- process.pid=- process.parent_pid=- |}]
    ;;

    let%expect_test "bash with nested otel-cli spans shows tree" =
      let doc =
        Parse.of_string
          {|
```bash
CARRIER=$(mktemp)
otel-cli exec --service oyster-test --name "parent-op" --tp-carrier $CARRIER -- \
  otel-cli exec --service oyster-test --name "child-step" --tp-carrier $CARRIER -- echo "done"
```
```trace
```
|}
      in
      let ctx = extract_exec_ctx doc in
      let result = traced_bash_executor ctx in
      let trace_output =
        List.find_map_exn result ~f:(fun o ->
          match o.res with
          | `Markdown s when o.id = 1 -> Some s
          | _ -> None)
      in
      print_endline trace_output;
      [%expect
        {|
        parent-op 2us process.command=otel-cli process.command_args=? process.owner=- process.pid=- process.parent_pid=-
        └── child-step 1us process.command=echo process.command_args=? process.owner=- process.pid=- process.parent_pid=-
        |}]
    ;;

    let%expect_test "multiple trace blocks all get same trace text" =
      let doc =
        Parse.of_string
          {|
```echo {}
hi
```
```trace
```
```trace
```
|}
      in
      let ctx = extract_exec_ctx doc in
      let result = traced_executor_of_executor echo_executor ctx in
      print_s [%sexp (result : output list)];
      [%expect
        {|
        (((id 0) (res (Markdown hi))) ((id 1) (res (Markdown "")))
         ((id 2) (res (Markdown ""))))
        |}]
    ;;
  end)
;;

let%test_module "bash_executor" =
  (module struct
    let%expect_test "shared state: define function in one cell, call in another" =
      let ctx =
        extract_exec_ctx
          (Parse.of_string
             {|
```bash
greet() { echo "hello $1"; }
```
```bash
greet world
```
|})
      in
      print_s [%sexp (bash_executor ctx : output list)];
      [%expect
        {|
        (((id 0) (res (Markdown ""))) ((id 1) (res (Markdown "hello world\n"))))
        |}]
    ;;

    let%expect_test "shared variables across cells" =
      let ctx =
        extract_exec_ctx
          (Parse.of_string
             {|
```bash
X=42
```
```bash
echo $X
```
|})
      in
      print_s [%sexp (bash_executor ctx : output list)];
      [%expect
        {|
        (((id 0) (res (Markdown ""))) ((id 1) (res (Markdown "42\n"))))
        |}]
    ;;

    let%expect_test "separate sessions are isolated" =
      let ctx =
        extract_exec_ctx
          (Parse.of_string
             {|
```bash {session_id=a}
Y=aaa
echo $Y
```
```bash {session_id=b}
echo "${Y:-empty}"
```
|})
      in
      print_s [%sexp (bash_executor ctx : output list)];
      [%expect
        {| (((id 0) (res (Markdown "aaa\n"))) ((id 1) (res (Markdown "empty\n")))) |}]
    ;;

    let%expect_test "non-bash cells are skipped" =
      let ctx =
        extract_exec_ctx
          (Parse.of_string
             {|
```bash
echo hi
```
```python
print("hello")
```
```sh
echo bye
```
|})
      in
      print_s [%sexp (bash_executor ctx : output list)];
      [%expect
        {|
        (((id 0) (res (Markdown "hi\n"))) ((id 2) (res (Markdown "bye\n"))))
        |}]
    ;;
  end)
;;
