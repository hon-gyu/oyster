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

(** Execute code blocks with trace collection, filling [trace] placeholder blocks
    with formatted span output.

    Wraps the executor in {!Trace_collect.with_collect} to capture OpenTelemetry
    spans emitted during execution. Any code block with [lang = "trace"] is treated
    as a placeholder: its content is replaced with the formatted trace tree. *)
let traced_executor_of_executor (executor : exec_ctx -> output list) (ctx : exec_ctx)
  : output list
  =
  let tc = Trace_collect.create () in
  let outputs = Trace_collect.with_collect tc (fun () -> executor ctx) in
  let trace_text =
    Trace_collect.Trace_pp.format ~tree_chars:Utf8 Indented (Trace_collect.spans tc)
  in
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
