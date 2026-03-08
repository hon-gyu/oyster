(** HTML renderer for oystermark documents.

    Extends {!Cmarkit_html.renderer} to handle wikilinks, resolved link targets,
    and block IDs. Uses tyxml for type-safe HTML construction of custom elements. *)

open Core
open Cmarkit
module C = Cmarkit_renderer.Context
module Resolve = Vault.Resolve
module Block_id = Parse.Block_id
module Wikilink = Parse.Wikilink
module H = Tyxml.Html

let elt_to_string (e : 'a H.elt) : string = Format.asprintf "%a" (H.pp_elt ()) e

let slugify s =
  s
  |> String.lowercase
  |> String.map ~f:(fun c ->
    if Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' then c else '-')
  |> String.split ~on:'-'
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> String.concat ~sep:"-"
;;

(* Strip the .md extension from a path for SSG-friendly URLs. *)
let strip_md_ext : string -> string =
  fun path ->
  match String.chop_suffix path ~suffix:".md" with
  | Some p -> p
  | None -> path
;;

(* Convert a resolved target to an href string. *)
let target_to_href : Resolve.target -> string = function
  | Resolve.Note { path } -> "/" ^ strip_md_ext path ^ "/"
  | Resolve.File { path } -> "/" ^ path
  | Resolve.Heading { path; heading; _ } ->
    "/" ^ strip_md_ext path ^ "/#" ^ slugify heading
  | Resolve.Block { path; block_id } -> "/" ^ strip_md_ext path ^ "/#^" ^ block_id
  | Resolve.Curr_file -> ""
  | Resolve.Curr_heading { heading; _ } -> "#" ^ slugify heading
  | Resolve.Curr_block { block_id } -> "#^" ^ block_id
  | Resolve.Unresolved -> "#"
;;

let is_unresolved (meta : Meta.t) : bool =
  match Meta.find Resolve.resolved_key meta with
  | Some Resolve.Unresolved -> true
  | _ -> false
;;

(* Default display text for a wikilink when no explicit display is given. *)
let wikilink_default_display (w : Wikilink.t) : string =
  match w.target, w.fragment with
  | Some t, None -> t
  | Some t, Some (Wikilink.Heading hs) -> t ^ "#" ^ String.concat ~sep:"#" hs
  | Some t, Some (Wikilink.Block_ref b) -> t ^ "#^" ^ b
  | None, Some (Wikilink.Heading hs) -> String.concat ~sep:"#" hs
  | None, Some (Wikilink.Block_ref b) -> "^" ^ b
  | None, None -> ""
;;

let media_type_of_href (href : string) : [> `Audio | `Iframe | `Image | `Link | `Video ] =
  let has_ext ext = String.is_suffix href ~suffix:ext in
  if List.exists [ ".png"; ".jpg"; ".jpeg"; ".gif"; ".svg"; ".webp" ] ~f:has_ext
  then `Image
  else if List.exists [ ".mp4"; ".webm"; ".mov" ] ~f:has_ext
  then `Video
  else if List.exists [ ".mp3"; ".flac"; ".ogg"; ".wav" ] ~f:has_ext
  then `Audio
  else if List.exists [ ".pdf" ] ~f:has_ext
  then `Iframe
  else `Link
;;

(* Render a wikilink as HTML. Handles embed=true for media content. *)
let render_wikilink (c : Cmarkit_renderer.context) (w : Wikilink.t) (meta : Meta.t) : unit
  =
  let href_of_meta (meta : Meta.t) =
    match Meta.find Resolve.resolved_key meta with
    | Some target -> target_to_href target
    | None -> "#"
  in
  let href = href_of_meta meta in
  let display = Option.value w.display ~default:(wikilink_default_display w) in
  if w.embed
  then (
    let s =
      match media_type_of_href href with
      | `Image -> elt_to_string (H.img ~src:href ~alt:display ())
      | `Video ->
        elt_to_string
          (H.video
             ~a:[ H.a_controls () ]
             [ H.source ~a:[ H.a_src href ] (); H.txt display ])
      | `Audio ->
        elt_to_string
          (H.audio
             ~a:[ H.a_controls () ]
             [ H.source ~a:[ H.a_src href ] (); H.txt display ])
      | `Iframe -> elt_to_string (H.iframe ~a:[ H.a_src href; H.a_title display ] [])
      | `Link ->
        (* Unknown embed type: render as a link *)
        let attrs =
          H.a_href href
          :: (if is_unresolved meta then [ H.a_class [ "unresolved" ] ] else [])
        in
        elt_to_string (H.a ~a:attrs [ H.txt display ])
    in
    C.string c s)
  else (
    let attrs =
      H.a_href href :: (if is_unresolved meta then [ H.a_class [ "unresolved" ] ] else [])
    in
    C.string c (elt_to_string (H.a ~a:attrs [ H.txt display ])))
;;

(* Render a standard link, overriding href if a resolved target is present.
    We render inline content via the cmarkit renderer into a sub-buffer,
    then embed it in the tyxml anchor via Unsafe.data. *)
let render_link (c : Cmarkit_renderer.context) (l : Inline.Link.t) (meta : Meta.t) : bool =
  match Meta.find Resolve.resolved_key meta with
  | Some target ->
    let href = target_to_href target in
    let attrs =
      H.a_href href :: (if is_unresolved meta then [ H.a_class [ "unresolved" ] ] else [])
    in
    (* Render inline children to a sub-buffer *)
    let buf = Buffer.create 128 in
    let sub_ctx = C.make (C.renderer c) buf in
    C.init sub_ctx (C.get_doc c);
    C.inline sub_ctx (Inline.Link.text l);
    let inner_html = Buffer.contents buf in
    C.string c (elt_to_string (H.a ~a:attrs [ H.Unsafe.data inner_html ]));
    true
  | None -> false
;;

(* Render an image with resolved target. *)
let render_image (c : Cmarkit_renderer.context) (l : Inline.Link.t) (meta : Meta.t) : bool
  =
  match Meta.find Resolve.resolved_key meta with
  | Some target ->
    let href = target_to_href target in
    let buf = Buffer.create 64 in
    let rec extract_text = function
      | Inline.Text (s, _) -> Buffer.add_string buf s
      | Inline.Inlines (is, _) -> List.iter is ~f:extract_text
      | Inline.Emphasis (e, _) | Inline.Strong_emphasis (e, _) ->
        extract_text (Inline.Emphasis.inline e)
      | _ -> ()
    in
    extract_text (Inline.Link.text l);
    let alt = Buffer.contents buf in
    C.string c (elt_to_string (H.img ~src:href ~alt ()));
    true
  | None -> false
;;

let inline (c : Cmarkit_renderer.context) : Inline.t -> bool = function
  | Wikilink.Ext_wikilink (w, meta) ->
    render_wikilink c w meta;
    true
  | Inline.Link (l, meta) -> render_link c l meta
  | Inline.Image (l, meta) -> render_image c l meta
  | _ -> false
;;

(** Render a paragraph with block-id. The ^blockid text stays visible
    (it's part of the inline content). We add an id to the <p> for linking. *)
let block (c : Cmarkit_renderer.context) : Block.t -> bool = function
  | Block.Paragraph (p, meta) ->
    (match Meta.find Block_id.meta_key meta with
     | Some (block_id : Block_id.t) ->
       let id = "^" ^ block_id.id in
       C.string c (Format.asprintf "<p id=\"%s\">" id);
       C.inline c (Block.Paragraph.inline p);
       C.string c "</p>\n";
       true
     | None -> false)
  | Parse.Frontmatter.Frontmatter y ->
    C.string c (Parse.Frontmatter.to_html (Some y));
    true
  | _ -> false
;;

let renderer ~(backend_blocks : bool) ~(safe : bool) () : Cmarkit_renderer.t =
  let custom = Cmarkit_renderer.make ~inline ~block () in
  let default = Cmarkit_html.renderer ~backend_blocks ~safe () in
  Cmarkit_renderer.compose default custom
;;

let of_doc ~(backend_blocks : bool) ~(safe : bool) (doc : Doc.t) : string =
  Cmarkit_renderer.doc_to_string (renderer ~backend_blocks ~safe ()) doc
;;
