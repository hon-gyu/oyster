(** Unified link reference extracted from both wikilinks and markdown links. *)

open Core

type fragment =
  | Heading of string list
  | Block_ref of string

type t =
  { target : string option
  ; fragment : fragment option
  }

let is_external (s : string) : bool =
  String.is_prefix s ~prefix:"http://"
  || String.is_prefix s ~prefix:"https://"
  || String.is_prefix s ~prefix:"mailto:"
  || String.is_prefix s ~prefix:"ftp://"
;;

let of_wikilink (w : Wikilink.t) : t =
  let fragment =
    match w.fragment with
    | None -> None
    | Some (Wikilink.Heading hs) -> Some (Heading hs)
    | Some (Wikilink.Block_ref s) -> Some (Block_ref s)
  in
  { target = w.target; fragment }
;;

let percent_decode (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len
    then Buffer.contents buf
    else if Char.equal (String.get s i) '%' && i + 2 < len
    then (
      let hi = String.get s (i + 1) in
      let lo = String.get s (i + 2) in
      match Char.get_hex_digit hi, Char.get_hex_digit lo with
      | Some h, Some l ->
        Buffer.add_char buf (Char.of_int_exn ((h lsl 4) lor l));
        loop (i + 3)
      | _ ->
        Buffer.add_char buf '%';
        loop (i + 1))
    else (
      Buffer.add_char buf (String.get s i);
      loop (i + 1))
  in
  loop 0
;;

let of_markdown_dest (dest : string) : t option =
  let decoded = percent_decode dest in
  if is_external decoded
  then None
  else (
    (* Parse using same logic as Wikilink.make: split on first # *)
    match String.lsplit2 decoded ~on:'#' with
    | None ->
      let target = if String.is_empty decoded then None else Some decoded in
      Some { target; fragment = None }
    | Some (t, frag_str) ->
      let target = if String.is_empty t then None else Some t in
      let fragment =
        if String.is_empty frag_str
        then None
        else if String.is_prefix frag_str ~prefix:"^"
        then (
          let candidate = String.drop_prefix frag_str 1 in
          if Block_id.is_valid_block_id candidate
          then Some (Block_ref candidate)
          else (
            let parts =
              String.split frag_str ~on:'#'
              |> List.filter ~f:(fun s -> not (String.is_empty s))
            in
            if List.is_empty parts then None else Some (Heading parts)))
        else (
          let parts =
            String.split frag_str ~on:'#'
            |> List.filter ~f:(fun s -> not (String.is_empty s))
          in
          if List.is_empty parts then None else Some (Heading parts))
      in
      Some { target; fragment })
;;
