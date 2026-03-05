(** Unified internal link reference extracted from both wikilinks and markdown links. *)

open Core
open Oystermark_base

type fragment =
  | Heading of string list (** Headings *)
  | Block_ref of string (** Block id *)
[@@deriving sexp]

type t =
  { target : string option (** Target file path *)
  ; fragment : fragment option
  }
[@@deriving sexp]

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

let of_cmark_dest (dest : string) : t option =
  let decoded = percent_decode dest in
  if is_external decoded
  then None
  else (
    let wikilink = Wikilink.make ~embed:false decoded in
    Some (of_wikilink wikilink))
;;

let of_cmark_reference (ref : Cmarkit.Inline.Link.reference) : t option =
  match ref with
  | `Ref _ -> None
  | `Inline (ld, _ld_meta) ->
    (match Cmarkit.Link_definition.dest ld with
     | None ->
       (* When destination is empty, Obsidian resolves it to a file named "().md". *)
       Some { target = Some "().md"; fragment = None }
     | Some (dest, dest_meta) -> of_cmark_dest dest)
;;
