open Core

module Ref = struct
  type fragment =
    | Hash_path of string list
    (** May resolves to headings, or Djot attribute anchors (when length = 1). Non-empty *)
    | Caret_id of string (** Obsidian block id *)
  [@@deriving sexp, equal, compare]

  (**
    | Target | Fragment | Meaning |
    |---|---|---|
    | `Some target` | `None` | Another note or asset |
    | `Some target` | `Some fragment` | An anchor in another note |
    | `None` | `Some fragment` | An anchor in the current note |
    | `None` | `None` | Current note or normalized empty reference |
  *)
  type t =
    { target : string option
      (** Authored target name or path. [None] means the current note. *)
    ; fragment : fragment option
    }
  [@@deriving sexp, equal, compare]

  let of_wikilink (w : Cmarkit.Inline.Wikilink.t) : t =
    let fragment =
      match Cmarkit.Inline.Wikilink.fragment w with
      | None -> None
      | Some (Cmarkit.Inline.Wikilink.Heading hs) -> Some (Hash_path hs)
      | Some (Cmarkit.Inline.Wikilink.Block_ref s) -> Some (Caret_id s)
    in
    { target = Cmarkit.Inline.Wikilink.target w; fragment }
  ;;

  let is_external (s : string) : bool =
    String.is_prefix s ~prefix:"http://"
    || String.is_prefix s ~prefix:"https://"
    || String.is_prefix s ~prefix:"mailto:"
    || String.is_prefix s ~prefix:"ftp://"
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
      let wikilink = Cmarkit.Inline.Wikilink.make ~embed:false decoded in
      Some (of_wikilink wikilink))
  ;;

  let of_cmark_reference (ref : Cmarkit.Inline.Link.reference) : t option =
    match ref with
    | `Ref _ ->
      (* TODO: we should support this case? *)
      None
    | `Inline (ld, _ld_meta) ->
      (match Cmarkit.Link_definition.dest ld with
       | None ->
         (* When destination is empty, Obsidian resolves it to a file named "().md". *)
         Some { target = Some "().md"; fragment = None }
       | Some (dest, dest_meta) -> of_cmark_dest dest)
  ;;

  let of_target_address ~(target : string) (address : Anchor.Address.t) : t =
    let fragment =
      match address with
      | Heading id | Attr id -> Hash_path [ id ]
      | Caret id -> Caret_id id
    in
    { target = Some target; fragment = Some fragment }
  ;;

  let string_of_fragment : fragment -> string = function
    | Hash_path segments -> "#" ^ String.concat segments ~sep:"#"
    | Caret_id id -> "#^" ^ id
  ;;

  let resolve_fragment (anchors : Anchor.t list) (fragment : fragment) : Anchor.t option =
    let heading_matches (h : Anchor.heading) q =
      String.equal h.text q || String.equal h.slug (Parse.Common.heading_id_of_text q)
    in
    let resolve_heading query =
      let hs =
        List.filter_map anchors ~f:(fun (a : Anchor.t) ->
          match a.definition with
          | Heading h -> Some (h, a)
          | Caret _ | Attr _ -> None)
        |> Array.of_list
      in
      let qs = Array.of_list query in
      let rec search hi qi prev =
        if hi >= Array.length hs || qi >= Array.length qs
        then None
        else (
          let h, a = hs.(hi) in
          if heading_matches h qs.(qi) && h.level > prev
          then
            if qi = Array.length qs - 1
            then Some a
            else
              Option.first_some
                (search (hi + 1) (qi + 1) h.level)
                (search (hi + 1) qi prev)
          else search (hi + 1) qi prev)
      in
      if Array.is_empty qs then None else search 0 0 0
    in
    match fragment with
    | Hash_path hs ->
      Option.first_some
        (resolve_heading hs)
        (match hs with
         | [ id ] ->
           List.find anchors ~f:(fun (a : Anchor.t) ->
             match a.definition with
             | Attr { id = x; _ } -> String.equal x id
             | Heading _ | Caret _ -> false)
         | _ -> None)
    | Caret_id id ->
      List.find anchors ~f:(fun (a : Anchor.t) ->
        match a.definition with
        | Caret x -> String.equal x id
        | Heading _ | Attr _ -> false)
  ;;
end

type loc = Cmarkit.Textloc.t

let sexp_of_loc = Parse.Textloc_conv.sexp_of_t
let loc_of_sexp = Parse.Textloc_conv.t_of_sexp
let compare_loc = Parse.Textloc_conv.compare
let equal_loc a b = Int.equal (compare_loc a b) 0

type kind =
  | Link
  | Embed
[@@deriving sexp, equal, compare]

type t =
  { reference : Ref.t
  ; kind : kind
  ; loc : loc
  }
[@@deriving sexp, equal, compare]

let of_doc (doc : Cmarkit.Doc.t) : t list =
  let open Cmarkit in
  let links = ref [] in
  let add reference kind meta =
    links := { reference; kind; loc = Meta.textloc meta } :: !links
  in
  let folder =
    Folder.make
      ~block:(fun f acc b ->
        match b with
        | Block.Ext_keyed ((_label, body), _) -> Folder.ret (Folder.fold_block f acc body)
        | Block.Ext_attributes (a, _) ->
          Folder.ret (Folder.fold_block f acc (Block.Attributes.block a))
        | _ -> Folder.default)
      ~inline:(fun f acc i ->
        match i with
        | Inline.Ext_wikilink (w, meta) ->
          add (Ref.of_wikilink w) (if Inline.Wikilink.embed w then Embed else Link) meta;
          Folder.default
        | Inline.Link (l, meta) ->
          Option.iter
            (Ref.of_cmark_reference (Inline.Link.reference l))
            ~f:(fun r -> add r Link meta);
          Folder.default
        | Inline.Image (l, meta) ->
          Option.iter
            (Ref.of_cmark_reference (Inline.Link.reference l))
            ~f:(fun r -> add r Embed meta);
          Folder.default
        | Inline.Ext_attributes (a, _) ->
          Folder.ret (Folder.fold_inline f acc (Inline.Attributes.inline a))
        | _ -> Folder.default)
      ~inline_ext_default:(fun _ acc _ -> acc)
      ~block_ext_default:(fun _ acc _ -> acc)
      ()
  in
  Folder.fold_doc folder () doc;
  List.rev !links
;;
