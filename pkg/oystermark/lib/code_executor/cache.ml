open Core

let cache_file = "_exec_cache.json"

type cache_entry =
  { hash : string
  ; outputs : Common.output list
  }

(** Mutable map from vault-relative file path to its last execution result. *)
type cache = cache_entry String.Map.t ref

let empty_cache () : cache = ref String.Map.empty

(** Return pre-existing execution result for [path] and [hash] if it exists and matches [hash]. *)
let cache_lookup (c : cache) ~(path : string) ~(hash : string) : Common.output list option
  =
  match Map.find !c path with
  | Some entry when String.equal entry.hash hash -> Some entry.outputs
  | _ -> None
;;

(** Store the execution result for [path] with content hash [hash] into the
    in-memory cache. Call [save_cache] afterwards to persist to disk. *)
let cache_set
      (c : cache)
      ~(path : string)
      ~(hash : string)
      ~(outputs : Common.output list)
  : unit
  =
  c := Map.set !c ~key:path ~data:{ hash; outputs }
;;

(** Load cache from [_exec_cache.json] in [dir]. Returns an empty cache if the
    file is missing or malformed. *)
let load_cache ~(dir : string) : cache =
  let path = Filename.concat dir cache_file in
  if not (Sys_unix.file_exists_exn path)
  then empty_cache ()
  else (
    try
      let json = Yojson.Basic.from_file path in
      let open Yojson.Basic.Util in
      let entries =
        json
        |> to_assoc
        |> List.filter_map ~f:(fun (file_path, v) ->
          try
            let hash = v |> member "hash" |> to_string in
            let outputs =
              v
              |> member "outputs"
              |> to_string
              |> Sexp.of_string
              |> [%of_sexp: Common.output list]
            in
            Some (file_path, { hash; outputs })
          with
          | _ -> None)
      in
      ref (String.Map.of_alist_exn entries)
    with
    | _ -> empty_cache ())
;;

(** Persist [cache] to [_exec_cache.json] in [dir]. *)
let save_cache (c : cache) ~(dir : string) : unit =
  Core_unix.mkdir_p dir;
  let path = Filename.concat dir cache_file in
  let json =
    `Assoc
      (Map.to_alist !c
       |> List.map ~f:(fun (file_path, entry) ->
         ( file_path
         , `Assoc
             [ "hash", `String entry.hash
             ; ( "outputs"
               , `String ([%sexp_of: Common.output list] entry.outputs |> Sexp.to_string)
               )
             ] )))
  in
  Yojson.Basic.to_file path json
;;

(** Generic caching wrapper: look up [(path, hash)] in [cache]; on a miss call
    [executor ()] and write the result back before returning. *)
let run_with
      ?(cache : cache option)
      ~(path : string)
      ~(hash : string)
      ~(executor : unit -> Common.output list)
      ()
  : Common.output list
  =
  match Option.bind cache ~f:(cache_lookup ~path ~hash) with
  | Some cached -> cached
  | None ->
    let outs = executor () in
    Option.iter cache ~f:(cache_set ~path ~hash ~outputs:outs);
    outs
;;
