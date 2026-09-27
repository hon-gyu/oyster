open Core

module Ref = struct
  type fragment = string list [@@deriving sexp, equal, compare]

  type t =
    { target : string option
    ; fragment : fragment option
    }
  [@@deriving sexp, equal, compare]

  let of_wikilink_target (raw : string) : t =
    let non_empty s = Option.some_if (not (String.is_empty s)) s in
    match String.lsplit2 raw ~on:'#' with
    | None -> { target = non_empty (String.strip raw); fragment = None }
    | Some (target, fragment) ->
      { target = non_empty (String.strip target)
      ; fragment =
          (match
             String.split fragment ~on:'#'
             |> List.filter_map ~f:(fun segment -> non_empty (String.strip segment))
           with
           | [] -> None
           | segments -> Some segments)
      }
  ;;

  let string_of_fragment (fragment : fragment) : string =
    "#" ^ String.concat fragment ~sep:"#"
  ;;

  let to_wikilink_target ({ target; fragment } : t) : string =
    Option.value target ~default:""
    ^ Option.value_map fragment ~default:"" ~f:string_of_fragment
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

  let of_destination (dest : string) : t option =
    if String.is_empty (String.strip dest)
    then Some { target = Some "().md"; fragment = None }
    else (
      let decoded = percent_decode dest in
      if is_external decoded then None else Some (of_wikilink_target decoded))
  ;;

  let of_link_target (doc : Djot.Doc.t) (target : Djot.Inline.target) : t option =
    match target with
    | Direct dest -> of_destination dest
    | Reference label ->
      Option.bind (Djot.Doc.reference doc label) ~f:(fun (dest, _) -> of_destination dest)
  ;;

  let of_target_address ~(target : string) (address : Anchor.Address.t) : t =
    { target = Some target; fragment = Some [ Anchor.Address.id address ] }
  ;;

  let resolve_fragment (anchors : Anchor.t list) (fragment : fragment) : Anchor.t option =
    let heading_matches (h : Anchor.heading) q =
      String.equal h.slug q
      || String.equal (Parse.Common.heading_key h.text) (Parse.Common.heading_key q)
    in
    let resolve_heading query =
      let hs =
        List.filter_map anchors ~f:(fun (a : Anchor.t) ->
          match a.definition with
          | Heading h -> Some (h, a)
          | Attr _ -> None)
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
    Option.first_some
      (resolve_heading fragment)
      (match fragment with
       | [ id ] ->
         List.find anchors ~f:(fun (a : Anchor.t) ->
           match a.definition with
           | Attr { id = x; _ } -> String.equal x id
           | Heading _ -> false)
       | _ -> None)
  ;;
end

type loc = Djot.Textloc.t

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

let of_doc (doc : Djot.Doc.t) : t list =
  let links = ref [] in
  let add reference kind node =
    links := { reference; kind; loc = Djot.Doc.textloc doc node } :: !links
  in
  let folder =
    Djot.Folder.make
      ~inline:(fun _ () (Node (_, _, inline) as node) ->
        (match inline with
         | Ext_wikilink (embed, target, _) ->
           add (Ref.of_wikilink_target target) (if embed then Embed else Link) node
         | Link (_, target) ->
           Option.iter (Ref.of_link_target doc target) ~f:(fun r -> add r Link node)
         | Image (_, target) ->
           Option.iter (Ref.of_link_target doc target) ~f:(fun r -> add r Embed node)
         | _ -> ());
        Djot.Folder.default)
      ()
  in
  Djot.Folder.fold_doc folder () doc;
  List.rev !links
;;
