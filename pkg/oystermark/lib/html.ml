(** HTML renderer for oystermark documents.

    Extends {!Cmarkit_html.renderer} to handle wikilinks, resolved link targets,
    and block IDs. Uses tyxml for type-safe HTML construction of custom elements. *)

open Core
open Cmarkit
module C = Cmarkit_renderer.Context
module Resolve = Vault.Resolve
module Embed = Vault.Embed
module Block_id = Parse.Block_id
module Callout = Parse.Callout
module Wikilink = Parse.Wikilink
module H = Tyxml.Html

let elt_to_string (e : 'a H.elt) : string = Format.asprintf "%a" (H.pp_elt ()) e

(** URL path for a note: "foo/bar.md" → "/foo/bar/", "foo/index.md" → "/foo/". *)
let note_url_path (rel_path : string) : string =
  let base = String.chop_suffix_exn rel_path ~suffix:".md" in
  if String.is_suffix base ~suffix:"/index"
  then "/" ^ String.chop_suffix_exn base ~suffix:"/index" ^ "/"
  else "/" ^ base ^ "/"
;;

(** Output file path for a note: derived from its URL path.
    "foo/bar.md" → "foo/bar/index.html", "foo/index.md" → "foo/index.html". *)
let note_output_path (rel_path : string) : string =
  let base = String.chop_suffix_exn rel_path ~suffix:".md" in
  if String.is_suffix base ~suffix:"/index" then base ^ ".html" else base ^ "/index.html"
;;

(** URL path for any file: notes get pretty URLs, others get literal paths. *)
let file_url_path (path : string) : string =
  if String.is_suffix path ~suffix:".md" then note_url_path path else "/" ^ path
;;

(* Convert a resolved target to an href string. *)
let target_to_href : Resolve.target -> string = function
  | Resolve.Note { path } -> note_url_path path
  | Resolve.File { path } -> "/" ^ path
  | Resolve.Heading { path; slug; _ } -> file_url_path path ^ "#" ^ slug
  | Resolve.Block { path; block_id } -> file_url_path path ^ "#^" ^ block_id
  | Resolve.Curr_file -> ""
  | Resolve.Curr_heading { slug; _ } -> "#" ^ slug
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

(** Parse an Obsidian image dimension spec: "100x145" → Some (100, Some 145),
    "100" → Some (100, None), anything else → None. *)
let parse_image_dims (s : string) : (int * int option) option =
  let s = String.strip s in
  match String.lsplit2 s ~on:'x' with
  | Some (w, h) ->
    (try Some (Int.of_string (String.strip w), Some (Int.of_string (String.strip h))) with
     | _ -> None)
  | None ->
    (try Some (Int.of_string s, None) with
     | _ -> None)
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
      | `Image ->
        let dim_attrs, alt =
          match parse_image_dims display with
          | Some (iw, Some ih) ->
            [ H.a_width iw; H.a_height ih ], wikilink_default_display w
          | Some (iw, None) -> [ H.a_width iw ], wikilink_default_display w
          | None -> [], display
        in
        let img = H.img ~src:href ~alt ~a:dim_attrs () in
        elt_to_string (H.a ~a:[ H.a_href href ] [ img ])
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
    let raw_alt = Buffer.contents buf in
    let dim_attrs, alt =
      match parse_image_dims raw_alt with
      | Some (iw, Some ih) -> [ H.a_width iw; H.a_height ih ], ""
      | Some (iw, None) -> [ H.a_width iw ], ""
      | None -> [], raw_alt
    in
    let img = H.img ~src:href ~alt ~a:dim_attrs () in
    C.string c (elt_to_string (H.a ~a:[ H.a_href href ] [ img ]));
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
let render_callout
      (c : Cmarkit_renderer.context)
      (bq : Block.Block_quote.t)
      (callout : Callout.t)
  : unit
  =
  let body = Block.Block_quote.block bq in
  match callout.fold with
  | None ->
    C.string c (sprintf "<div class=\"callout\" data-callout=\"%s\">\n" callout.kind);
    C.string c (sprintf "<div class=\"callout-title\">%s</div>\n" callout.title);
    C.string c "<div class=\"callout-content\">\n";
    C.block c body;
    C.string c "</div>\n</div>\n"
  | Some fold ->
    let open_attr =
      match fold with
      | Callout.Foldable_open -> " open"
      | Callout.Foldable_closed -> ""
    in
    C.string
      c
      (sprintf
         "<details class=\"callout\" data-callout=\"%s\"%s>\n"
         callout.kind
         open_attr);
    C.string c (sprintf "<summary class=\"callout-title\">%s</summary>\n" callout.title);
    C.string c "<div class=\"callout-content\">\n";
    C.block c body;
    C.string c "</div>\n</details>\n"
;;

module Index = Vault.Index

let renderer ~(backend_blocks : bool) ~(safe : bool) () : Cmarkit_renderer.t =
  let slug_seen = Hashtbl.create (module String) in
  let block (c : Cmarkit_renderer.context) : Block.t -> bool = function
    | Block.Heading (h, _meta) ->
      let text = Parse.inline_to_plain_text (Block.Heading.inline h) in
      let slug = Index.dedup_slug slug_seen text in
      let level = Block.Heading.level h in
      C.string c (sprintf "<h%d id=\"%s\">" level slug);
      C.inline c (Block.Heading.inline h);
      C.string c (sprintf "</h%d>\n" level);
      true
    | Block.Block_quote (bq, meta) ->
      (match Meta.find Callout.meta_key meta with
       | Some callout ->
         render_callout c bq callout;
         true
       | None -> false)
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
    | Block.Blocks (blocks, meta) ->
      (match Meta.find Embed.embed_meta_key meta with
       | None -> false
       | Some { depth; _ } ->
         C.string c (sprintf "<div class=\"embed\" data-embed-depth=\"%d\">\n" depth);
         List.iter blocks ~f:(C.block c);
         C.string c "</div>\n";
         true)
    | _ -> false
  in
  let custom = Cmarkit_renderer.make ~inline ~block () in
  let default = Cmarkit_html.renderer ~backend_blocks ~safe () in
  Cmarkit_renderer.compose default custom
;;

let of_doc ~(backend_blocks : bool) ~(safe : bool) (doc : Doc.t) : string =
  Cmarkit_renderer.doc_to_string (renderer ~backend_blocks ~safe ()) doc
;;
