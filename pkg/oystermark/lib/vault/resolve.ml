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
  | Current_file
  | Current_heading of
      { heading : string
      ; level : int
      }
  | Current_block of { block_id : string }
  | Unresolved

(** Normalize a target string: if it has no '.', append ".md". *)
let normalize_target (s : string) : string = if String.mem s '.' then s else s ^ ".md"

(** Check if needle components form a subsequence of haystack components. *)
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

(** Resolve a target string to a file entry. Exact match first, then subsequence. *)
let resolve_file (files : Index.file_entry list) (target_str : string)
  : Index.file_entry option
  =
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
let resolve ~(index : Index.t) ~(current_file : string) (link_ref : Link_ref.t)
  : target
  =
  let current_entry =
    List.find index.files ~f:(fun f -> String.equal f.rel_path current_file)
  in
  match link_ref.target with
  | None ->
    (* Self-reference: fragment only *)
    (match link_ref.fragment with
     | None -> Current_file
     | Some (Link_ref.Heading hs) ->
       (match current_entry with
        | Some entry ->
          (match resolve_headings entry.headings hs with
           | Some h -> Current_heading { heading = h.text; level = h.level }
           | None -> Current_file)
        | None -> Current_file)
     | Some (Link_ref.Block_ref bid) ->
       (match current_entry with
        | Some entry ->
          if List.exists entry.block_ids ~f:(String.equal bid)
          then Current_block { block_id = bid }
          else Current_file
        | None -> Current_file))
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
