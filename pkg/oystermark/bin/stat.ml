open Core
open Common_

type unresolved =
  { src_path : string
  ; src_loc : int * int (** start and end byte *)
  ; dst_path : string
  ; dst_frag : string option
  ; reason : string
  }

type t =
  { nodes : int
  ; edges : int
  ; self_links : int
  ; unresolved_links : int
  }

let stats (vault : Vault.t) : int * int * int * int =
  let (links : (string * Note.Link.t * Vault.Index.resolution) list) =
    links vault.index
  in
  let edges, self_links, unresolved_links =
    List.fold
      links
      ~init:(0, 0, 0)
      ~f:(fun (edges, self_links, unresolved) (source, _, resolution) ->
        match (resolution : Vault.Index.resolution) with
        | Error _ -> edges, self_links, unresolved + 1
        | Ok target ->
          let destination = Vault.Index.target_path target in
          ( edges + 1
          , self_links + Bool.to_int (String.equal source destination)
          , unresolved ))
  in
  let nodes = List.length (Vault.Index.notes vault.index) in
  nodes, edges, self_links, unresolved_links
;;

let command : Command.t =
  Command.basic
    ~summary:"Show vault graph statistics"
    (let%map_open.Command root = vault_param in
     fun () ->
       let n, e, sl, ul = stats (load_vault root) in
       printf "node-count\t%d\n" n;
       printf "edge-count\t%d\n" e;
       printf "self-link-count\t%d\n" sl;
       printf "unresolved-link-count\t%d\n" ul)
;;
