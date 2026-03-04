(** HTML renderer for oystermark documents.

    Extends {!Cmarkit_html.renderer} to handle wikilinks, resolved link targets,
    and block IDs. *)

open Core
open Cmarkit
module C = Cmarkit_renderer.Context
module Resolve = Vault.Resolve
module Block_id = Oystermark_base.Block_id
module Wikilink = Oystermark_base.Wikilink

(* Slugify a heading string for use as an HTML fragment identifier. *)
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
let strip_md_ext : string -> string = fun path ->
  match String.chop_suffix path ~suffix:".md" with
  | Some p -> p
  | None -> path
;;

(** Convert a resolved target to an href string. *)
let target_to_href = function
  | Resolve.File { path } -> strip_md_ext path
  | Resolve.Heading { path; heading; _ } -> strip_md_ext path ^ "#" ^ slugify heading
  | Resolve.Block { path; block_id } -> strip_md_ext path ^ "#^" ^ block_id
  | Resolve.Curr_file -> ""
  | Resolve.Curr_heading { heading; _ } -> "#" ^ slugify heading
  | Resolve.Curr_block { block_id } -> "#^" ^ block_id
  | Resolve.Unresolved -> "#"
;;

(** Default display text for a wikilink when no explicit display is given. *)
let wikilink_default_display (w : Wikilink.t) =
  match w.target, w.fragment with
  | Some t, None -> t
  | Some t, Some (Wikilink.Heading hs) -> t ^ "#" ^ String.concat ~sep:"#" hs
  | Some t, Some (Wikilink.Block_ref b) -> t ^ "#^" ^ b
  | None, Some (Wikilink.Heading hs) -> String.concat ~sep:"#" hs
  | None, Some (Wikilink.Block_ref b) -> "^" ^ b
  | None, None -> ""
;;

(** Render a wikilink as an HTML anchor. *)
let render_wikilink c (w : Wikilink.t) meta =
  let href =
    match Meta.find Resolve.resolved_key meta with
    | Some target -> target_to_href target
    | None -> "#"
  in
  let display = Option.value (w.display) ~default:(wikilink_default_display w) in
  let unresolved =
    match Meta.find Resolve.resolved_key meta with
    | Some Resolve.Unresolved -> true
    | _ -> false
  in
  C.string c "<a href=\"";
  Cmarkit_html.pct_encoded_string c href;
  if unresolved then C.string c "\" class=\"unresolved";
  C.string c "\">";
  Cmarkit_html.html_escaped_string c display;
  C.string c "</a>"
;;

(** Render a standard link, overriding href if a resolved target is present. *)
let render_link c (l : Inline.Link.t) meta =
  match Meta.find Resolve.resolved_key meta with
  | Some target ->
    let href = target_to_href target in
    let unresolved = match target with Resolve.Unresolved -> true | _ -> false in
    C.string c "<a href=\"";
    Cmarkit_html.pct_encoded_string c href;
    if unresolved then C.string c "\" class=\"unresolved";
    C.string c "\">";
    C.inline c (Inline.Link.text l);
    C.string c "</a>";
    true
  | None -> false (* fall through to default renderer *)
;;

(** Extract plain text from an inline tree. *)
let rec plain_text buf = function
  | Inline.Text (s, _) -> Buffer.add_string buf s
  | Inline.Inlines (is, _) -> List.iter is ~f:(plain_text buf)
  | Inline.Emphasis (e, _) | Inline.Strong_emphasis (e, _) ->
    plain_text buf (Inline.Emphasis.inline e)
  | Inline.Code_span (cs, _) ->
    let lines = Inline.Code_span.code_layout cs in
    List.iter lines ~f:(fun l ->
      Buffer.add_string buf (Cmarkit.Block_line.tight_to_string l))
  | _ -> ()
;;

(** Render an image, overriding src if a resolved target is present. *)
let render_image c (l : Inline.Link.t) _meta =
  match Meta.find Resolve.resolved_key _meta with
  | Some target ->
    let href = target_to_href target in
    C.string c "<img src=\"";
    Cmarkit_html.pct_encoded_string c href;
    C.string c "\" alt=\"";
    let buf = Buffer.create 64 in
    plain_text buf (Inline.Link.text l);
    Cmarkit_html.html_escaped_string c (Buffer.contents buf);
    C.string c "\" />";
    true
  | None -> false
;;

let inline c = function
  | Wikilink.Ext_wikilink (w, meta) -> render_wikilink c w meta; true
  | Inline.Link (l, meta) -> render_link c l meta
  | Inline.Image (l, meta) -> render_image c l meta
  | _ -> false
;;

(** Render a paragraph, appending a block-id anchor if present. *)
let block c = function
  | Block.Paragraph (p, meta) ->
    (match Meta.find Block_id.meta_key meta with
     | Some (block_id : Block_id.t) ->
       C.string c "<p>";
       C.inline c (Block.Paragraph.inline p);
       C.string c
         (Printf.sprintf {| <span id="^%s" class="block-id">^%s</span>|}
            block_id.id block_id.id);
       C.string c "</p>\n";
       true
     | None -> false)
  | _ -> false
;;

let renderer ~safe () =
  let custom = Cmarkit_renderer.make ~inline ~block () in
  let default = Cmarkit_html.renderer ~safe () in
  Cmarkit_renderer.compose default custom
;;

let of_doc ~safe doc = Cmarkit_renderer.doc_to_string (renderer ~safe ()) doc
