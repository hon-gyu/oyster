(** Link resolution algorithm: resolves link references against a vault index. *)

open Core

type target =
  | File of { path : string }
  | Heading of
      { path : string
      ; heading : string
      ; level : int
      }
  | Block of
      { path : string
      ; block_id : string
      }
  | Curr_file
  | Curr_heading of
      { heading : string
      ; level : int
      }
  | Curr_block of { block_id : string }
  | Unresolved
[@@derving sexp]

let resolved_key : target Cmarkit.Meta.key = Cmarkit.Meta.key ()

(** Check if needle components form a (ordered) subsequence of haystack components. *)
let is_path_subsequence ~(haystack : string list) ~(needle : string list) : bool =
  let hay_len = List.length haystack in
  let hay_arr = Array.of_list haystack in
  let rec loop hay_idx needle_rest =
    match needle_rest with
    | [] -> true
    | n :: ns ->
      let rec find i =
        if i >= hay_len
        then false
        else if String.equal hay_arr.(i) n
        then loop (i + 1) ns
        else find (i + 1)
      in
      find hay_idx
  in
  loop 0 needle
;;

let%expect_test "is_path_subsequence" =
  let haystack = [ "foo"; "bar"; "baz"; "qux" ] in
  let n1 = [ "bar"; "baz" ] in
  let n2 = [ "baz"; "bar" ] in
  printf "%b\n" (is_path_subsequence ~haystack ~needle:n1);
  printf "%b\n" (is_path_subsequence ~haystack ~needle:n2);
  [%expect
    {|
    true
    false
    |}]
;;

(** Resolve a target string to a file entry. Exact match first, then subsequence. *)
let resolve_file (files : Index.file_entry list) (target_str : string)
  : Index.file_entry option
  =
  let normalize_target s = if String.mem s '.' then s else s ^ ".md" in
  let normalized = normalize_target target_str in
  (* Exact match *)
  match List.find files ~f:(fun f -> String.equal f.rel_path normalized) with
  | Some _ as result -> result
  | None ->
    (* Subsequence match: split needle into path components *)
    let needle = String.split normalized ~on:'/' in
    List.find files ~f:(fun f ->
      let haystack = String.split f.rel_path ~on:'/' in
      is_path_subsequence ~haystack ~needle)
;;

(** Resolve a heading query (list of heading texts) against document headings.
    Finds a subsequence where levels strictly increase (backtracking). *)
let resolve_headings (headings : Index.heading_entry list) (query : string list)
  : Index.heading_entry option
  =
  let headings_arr = Array.of_list headings in
  let n_headings = Array.length headings_arr in
  (* Backtracking search: try to match query[qi..] starting from headings[hi..]
     with prev_level constraint. Returns the last matched heading on success. *)
  let rec search hi qi prev_level =
    if qi >= List.length query
    then None (* all matched — but we return from the caller *)
    else if hi >= n_headings
    then None
    else (
      let q = List.nth_exn query qi in
      let h = headings_arr.(hi) in
      if String.equal h.text q && h.level > prev_level
      then
        if
          (* This heading matches query[qi] *)
          qi = List.length query - 1
        then Some h (* last query item matched *)
        else (
          (* Try to match remaining query items *)
          match search (hi + 1) (qi + 1) h.level with
          | Some _ as result -> result
          | None ->
            (* Backtrack: skip this heading, try next *)
            search (hi + 1) qi prev_level)
      else search (hi + 1) qi prev_level)
  in
  search 0 0 0
;;

(** Resolve a link reference against the vault index. *)
let resolve (link_ref : Link_ref.t) (curr_file : string) (index : Index.t) : target =
  (* TODO(refactor): the matches be re-written to use Let_syntax? *)
  let current_entry =
    List.find index.files ~f:(fun f -> String.equal f.rel_path curr_file)
  in
  match link_ref.target with
  | None ->
    (* Self-reference: fragment only *)
    (match link_ref.fragment with
     | None -> Curr_file
     | Some (Link_ref.Heading hs) ->
       (match current_entry with
        | Some entry ->
          (match resolve_headings entry.headings hs with
           | Some h -> Curr_heading { heading = h.text; level = h.level }
           | None -> Curr_file)
        | None -> Curr_file)
     | Some (Link_ref.Block_ref bid) ->
       (match current_entry with
        | Some entry ->
          if List.exists entry.block_ids ~f:(String.equal bid)
          then Curr_block { block_id = bid }
          else Curr_file
        | None -> Curr_file))
  | Some target_str ->
    (match resolve_file index.files target_str with
     | None -> Unresolved
     | Some file ->
       (match link_ref.fragment with
        | None -> File { path = file.rel_path }
        | Some (Link_ref.Heading hs) ->
          (match resolve_headings file.headings hs with
           | Some h -> Heading { path = file.rel_path; heading = h.text; level = h.level }
           | None -> File { path = file.rel_path })
        | Some (Link_ref.Block_ref bid) ->
          if List.exists file.block_ids ~f:(String.equal bid)
          then Block { path = file.rel_path; block_id = bid }
          else File { path = file.rel_path }))
;;

(** Build a [Cmarkit.Mapper.t] that resolves links against the vault index. *)
let resolution_cmarkit_mapper ~(index : Index.t) ~(curr_file : string)
  : Cmarkit.Mapper.t
  =
  Cmarkit.Mapper.make
    ~block_ext_default:(fun _m b -> Some b)
    ~inline_ext_default:(fun _m i ->
      match i with
      | Parse.Wikilink.Ext_wikilink (w, meta) ->
        let link_ref = Link_ref.of_wikilink w in
        let result = resolve link_ref curr_file index in
        let meta' = Cmarkit.Meta.add resolved_key result meta in
        Some (Parse.Wikilink.Ext_wikilink (w, meta'))
      | other -> Some other)
    ~inline:(fun _m i ->
      match i with
      | Cmarkit.Inline.Link (link, meta) ->
        let ref_ = Cmarkit.Inline.Link.reference link in
        (match Link_ref.of_cmark_reference ref_ with
         | Some link_ref ->
           let result = resolve link_ref curr_file index in
           let meta' = Cmarkit.Meta.add resolved_key result meta in
           Cmarkit.Mapper.ret (Cmarkit.Inline.Link (link, meta'))
         | None -> Cmarkit.Mapper.default)
      | Cmarkit.Inline.Image (link, meta) ->
        let ref_ = Cmarkit.Inline.Link.reference link in
        (match Link_ref.of_cmark_reference ref_ with
         | Some link_ref ->
           let result = resolve link_ref curr_file index in
           let meta' = Cmarkit.Meta.add resolved_key result meta in
           Cmarkit.Mapper.ret (Cmarkit.Inline.Image (link, meta'))
         | None -> Cmarkit.Mapper.default)
      | _ -> Cmarkit.Mapper.default)
    ()
;;

(** Resolve links in a list of parsed docs against the vault index. *)
let resolve_docs (docs : (string * Cmarkit.Doc.t) list) (index : Index.t)  : (string * Cmarkit.Doc.t) list =
  List.map docs ~f:(fun (rel_path, doc) ->
    let mapper = resolution_cmarkit_mapper ~index ~curr_file:rel_path in
    rel_path, Cmarkit.Mapper.map_doc mapper doc)
;;
