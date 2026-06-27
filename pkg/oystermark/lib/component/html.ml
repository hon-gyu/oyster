(** HTML renderer for oystermark documents.

    Extends [Cmarkit_html.renderer] to handle wikilinks, resolved link targets,
    and block IDs. Uses tyxml for type-safe HTML construction of custom elements. *)

open Core
open Cmarkit
module C = Cmarkit_renderer.Context
module Resolve = Vault.Resolve
module Embed = Vault.Embed
module Cb_attribute = Parse.Cb_attribute
module Heading_slug = Parse.Heading_slug
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

(* Attribute-to-HTML helpers
---------------------------- *)

let buffer_add_attr_value (buf : Buffer.t) (s : string) : unit =
  String.iter s ~f:(fun c ->
    match c with
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | _ -> Buffer.add_char buf c)
;;

(** Render id/classes/key-values as a leading-space-prefixed sequence of HTML
    attributes: [` id="x" class="a b" key="value"`]. With [~key_prefix],
    every attribute name (including [id] and [class]) is prefixed — used for
    the data-* path on code blocks. Identifiers and classes are expected to be
    marker-free (no [#] / [.]), as produced by {!Cmarkit.Attribute}. *)
let emit_html_attrs
      ?(key_prefix = "")
      ~(id : string option)
      ~(classes : string list)
      ~(kvs : (string * string) list)
      ()
  : string
  =
  let buf = Buffer.create 32 in
  let emit (k : string) (v : string) =
    Buffer.add_char buf ' ';
    Buffer.add_string buf key_prefix;
    Buffer.add_string buf k;
    Buffer.add_string buf "=\"";
    buffer_add_attr_value buf v;
    Buffer.add_char buf '"'
  in
  Option.iter id ~f:(fun id -> emit "id" id);
  if not (List.is_empty classes)
  then emit "class" (String.concat ~sep:" " classes);
  List.iter kvs ~f:(fun (k, v) -> emit k v);
  Buffer.contents buf
;;

(** HTML attributes for a fork {!Cmarkit.Attribute.t} (Djot block/inline
    attribute). *)
let cmarkit_attr_html ?key_prefix (a : Attribute.t) : string =
  emit_html_attrs
    ?key_prefix
    ~id:(Attribute.id a)
    ~classes:(Attribute.classes a)
    ~kvs:(Attribute.key_values a)
    ()
;;

(** HTML attributes for a Pandoc code-block {!Cb_attribute.t}. *)
let cb_attr_html ?key_prefix (a : Cb_attribute.t) : string =
  emit_html_attrs ?key_prefix ~id:a.id ~classes:a.classes ~kvs:a.kvs ()
;;

(* Default display text for a wikilink when no explicit display is given. *)
let wikilink_default_display (w : Cmarkit.Inline.Wikilink.t) : string =
  match Cmarkit.Inline.Wikilink.target w, Cmarkit.Inline.Wikilink.fragment w with
  | Some t, None -> t
  | Some t, Some (Cmarkit.Inline.Wikilink.Heading hs) -> t ^ "#" ^ String.concat ~sep:"#" hs
  | Some t, Some (Cmarkit.Inline.Wikilink.Block_ref b) -> t ^ "#^" ^ b
  | None, Some (Cmarkit.Inline.Wikilink.Heading hs) -> String.concat ~sep:"#" hs
  | None, Some (Cmarkit.Inline.Wikilink.Block_ref b) -> "^" ^ b
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
let render_wikilink (c : Cmarkit_renderer.context) (w : Cmarkit.Inline.Wikilink.t) (meta : Meta.t) : unit
  =
  let href_of_meta (meta : Meta.t) =
    match Meta.find Resolve.resolved_key meta with
    | Some target -> target_to_href target
    | None -> "#"
  in
  let href = href_of_meta meta in
  let display =
    Option.value (Cmarkit.Inline.Wikilink.display w) ~default:(wikilink_default_display w)
  in
  if Cmarkit.Inline.Wikilink.embed w
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

(* Render a standard link, overriding href if a resolved target is present. *)
let render_link
      ?(attr : Attribute.t option)
      (c : Cmarkit_renderer.context)
      (l : Inline.Link.t)
      (meta : Meta.t)
  : bool
  =
  match Meta.find Resolve.resolved_key meta with
  | Some target ->
    let href = target_to_href target in
    let buf = Buffer.create 128 in
    let sub_ctx = C.make (C.renderer c) buf in
    C.init sub_ctx (C.get_doc c);
    C.inline sub_ctx (Inline.Link.text l);
    let inner_html = Buffer.contents buf in
    let href_buf = Buffer.create 64 in
    buffer_add_attr_value href_buf href;
    let href_esc = Buffer.contents href_buf in
    let attrs_str =
      let id = Option.bind attr ~f:Attribute.id in
      let classes = Option.value_map attr ~default:[] ~f:Attribute.classes in
      let kvs = Option.value_map attr ~default:[] ~f:Attribute.key_values in
      let classes = if is_unresolved meta then classes @ [ "unresolved" ] else classes in
      emit_html_attrs ~id ~classes ~kvs ()
    in
    C.string c (sprintf "<a href=\"%s\"%s>%s</a>" href_esc attrs_str inner_html);
    true
  | None -> false
;;

(* Render an image with resolved target. *)
let render_image
      ?(attr : Attribute.t option)
      (c : Cmarkit_renderer.context)
      (l : Inline.Link.t)
      (meta : Meta.t)
  : bool
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
    let dim_str, alt =
      match parse_image_dims raw_alt with
      | Some (iw, Some ih) -> sprintf " width=\"%d\" height=\"%d\"" iw ih, ""
      | Some (iw, None) -> sprintf " width=\"%d\"" iw, ""
      | None -> "", raw_alt
    in
    let href_buf = Buffer.create 64 in
    buffer_add_attr_value href_buf href;
    let href_esc = Buffer.contents href_buf in
    let alt_buf = Buffer.create 32 in
    buffer_add_attr_value alt_buf alt;
    let alt_esc = Buffer.contents alt_buf in
    let extra_attrs = Option.value_map attr ~default:"" ~f:cmarkit_attr_html in
    C.string c
      (sprintf
         "<a href=\"%s\"><img src=\"%s\" alt=\"%s\"%s%s/></a>"
         href_esc
         href_esc
         alt_esc
         dim_str
         extra_attrs);
    true
  | None -> false
;;

(** Render an inline, optionally carrying a Djot [attr] from an enclosing
    {!Cmarkit.Inline.Ext_attributes} wrapper. Returns [false] (defer to the
    default renderer) for inlines that need no oystermark-specific handling. *)
let render_inline ?(attr : Attribute.t option) (c : Cmarkit_renderer.context)
  : Inline.t -> bool
  =
  let with_attr ~tag render =
    match attr with
    | None -> false
    | Some a ->
      C.string c (sprintf "<%s%s>" tag (cmarkit_attr_html a));
      render ();
      C.string c (sprintf "</%s>" tag);
      true
  in
  function
  | Cmarkit.Inline.Ext_wikilink (w, meta) ->
    render_wikilink c w meta;
    true
  | Inline.Link (l, meta) -> render_link ?attr c l meta
  | Inline.Image (l, meta) -> render_image ?attr c l meta
  | Inline.Text (s, _) ->
    with_attr ~tag:"span" (fun () -> Cmarkit_html.html_escaped_string c s)
  | Inline.Emphasis (e, _) ->
    with_attr ~tag:"em" (fun () -> C.inline c (Inline.Emphasis.inline e))
  | Inline.Strong_emphasis (e, _) ->
    with_attr ~tag:"strong" (fun () -> C.inline c (Inline.Emphasis.inline e))
  | Inline.Code_span (cs, _) ->
    with_attr ~tag:"code" (fun () ->
      Cmarkit_html.html_escaped_string c (Inline.Code_span.code cs))
  | Inline.Autolink (a, _) ->
    (match attr with
     | None -> false
     | Some attr ->
       let link, _ = Inline.Autolink.link a in
       let is_email = Inline.Autolink.is_email a in
       let href = if is_email then "mailto:" ^ link else link in
       let href_buf = Buffer.create 64 in
       buffer_add_attr_value href_buf href;
       let href_esc = Buffer.contents href_buf in
       C.string c (sprintf "<a href=\"%s\"%s>" href_esc (cmarkit_attr_html attr));
       Cmarkit_html.html_escaped_string c link;
       C.string c "</a>";
       true)
  | Inline.Ext_strikethrough (s, _) ->
    with_attr ~tag:"del" (fun () -> C.inline c (Inline.Strikethrough.inline s))
  | _ -> false
;;

let inline (c : Cmarkit_renderer.context) : Inline.t -> bool = function
  | Inline.Ext_attributes (a, _) ->
    render_inline
      ~attr:(Inline.Attributes.attributes a)
      c
      (Inline.Attributes.inline a)
  | i -> render_inline c i
;;

let render_callout
      (c : Cmarkit_renderer.context)
      (bq : Block.Block_quote.t)
      (callout : Block.Callout.t)
  : unit
  =
  let inner = Block.Block_quote.block bq in
  let body = Block.Callout.strip_header inner in
  let kind = Block.Callout.kind callout in
  let render_title () =
    match Block.Callout.title callout inner with
    | Some title -> C.inline c title
    | None -> C.string c (String.capitalize kind)
  in
  match Block.Callout.fold callout with
  | None ->
    C.string c (sprintf "<div class=\"callout\" data-callout=\"%s\">\n" kind);
    C.string c "<div class=\"callout-title\">";
    render_title ();
    C.string c "</div>\n";
    C.string c "<div class=\"callout-content\">\n";
    C.block c body;
    C.string c "</div>\n</div>\n"
  | Some fold ->
    let open_attr =
      match fold with
      | Block.Callout.Foldable_open -> " open"
      | Block.Callout.Foldable_closed -> ""
    in
    C.string
      c
      (sprintf
         "<details class=\"callout\" data-callout=\"%s\"%s>\n"
         kind
         open_attr);
    C.string c "<summary class=\"callout-title\">";
    render_title ();
    C.string c "</summary>\n";
    C.string c "<div class=\"callout-content\">\n";
    C.block c body;
    C.string c "</div>\n</details>\n"
;;

type struct_style =
  [ `Plain (** no visible styling, as close to plain CommonMark as possible *)
  | `Basic (** boxed label with hover highlighting of label and body *)
  | `Graph (** basic style plus box/arrow layout for nested structs *)
  ]

let struct_style_attr : struct_style -> string = function
  | `Plain -> "plain"
  | `Basic -> "basic"
  | `Graph -> "graph"
;;

(* Recognize `:::struct-style-{plain,basic,graph}` div fences as local
   style overrides. Returns [Some style] if the class selects a known
   struct style, [None] otherwise. *)
let struct_style_of_class : string -> struct_style option = function
  | "struct-style-plain" -> Some `Plain
  | "struct-style-basic" -> Some `Basic
  | "struct-style-graph" -> Some `Graph
  | _ -> None
;;

let label_is_empty : Inline.t -> bool = function
  | Inline.Text ("", _) -> true
  | _ -> false
;;

(* Render a keyed block (or keyed list item) with a uniform HTML shape:
     <div class="keyed" data-style="..." [data-body="..."] [data-empty-label] [data-single-list-item]>
       <span class="keyed-label">label</span>   (always emitted; empty when label is empty)
       <div class="keyed-body">body-as-rendered</div>
     </div>

    The effective style is resolved by the renderer (from config default,
    optionally overridden by an enclosing `:::struct-style-*` div) and
    emitted as a single [data-style] attribute, so CSS selectors key off
    [.keyed[data-style="..."]] without any ancestor dance.

    The body is rendered via [C.block], so [<ul>]/[<li>] wrapping is
    preserved — nested keyed-list-items remain inside their parent list
    rather than being unwrapped. *)
let render_struct
      ~(style : struct_style)
      (label_kind : [ `Paragraph | `List_item ])
      c
      label
      body
  =
  let label_empty = label_is_empty label in
  let label_kind_attr =
    match label_kind with
    | `Paragraph -> "paragraph"
    | `List_item -> "list-item"
  in
  let body_attr, single_attr =
    match body with
    | Block.Paragraph _ -> " data-body=\"paragraph\"", ""
    | Block.List (l, _) ->
      let single =
        match Block.List'.items l with
        | [ _ ] -> " data-single-list-item"
        | _ -> ""
      in
      " data-body=\"list\"", single
    | _ -> "", ""
  in
  let emtpy_label_attr = if label_empty then " data-empty-label" else "" in
  let struct_style_attr_str = struct_style_attr style in
  C.string
    c
    {%string|<div class="keyed" data-label-kind="%{label_kind_attr}" data-style="%{struct_style_attr_str}"%{body_attr}%{emtpy_label_attr}%{single_attr}>|};
  (* Always emit a label span, even when empty *)
  C.string c "<span class=\"keyed-label\">";
  if not label_empty then C.inline c label;
  C.string c "</span>\n<div class=\"keyed-body\">\n";
  C.block c body;
  C.string c "</div>\n</div>\n"
;;

(* List-item body rendering for non-keyed items inside a mixed keyed list.
   [Cmarkit_html] does not expose its list-item helper, and once oystermark
   handles a list for semantic keyed markup it must emit every [<li>] itself.
   Nested blocks still go through [C.block], so nested keyed lists compose. *)
let rec item_block ~(tight : bool) c : Block.t -> unit = function
  | Block.Blank_line _ -> ()
  | Block.Ext_keyed _ as b -> item_block ~tight c (Struct.unkey b)
  | Block.Paragraph (p, _) when tight -> C.inline c (Block.Paragraph.inline p)
  | Block.Blocks (bs, _) ->
    let rec loop add_nl = function
      | Block.Blank_line _ :: bs -> loop add_nl bs
      | Block.Paragraph (p, _) :: bs when tight ->
        C.inline c (Block.Paragraph.inline p);
        loop true bs
      | b :: bs ->
        if add_nl then C.byte c '\n';
        C.block c b;
        loop false bs
      | [] -> ()
    in
    loop true bs
  | b ->
    C.byte c '\n';
    C.block c b
;;

let list_has_keyed (l : Block.List'.t) : bool =
  List.exists (Block.List'.items l) ~f:(fun (item, _) ->
    match Block.List_item.block item with
    | Block.Ext_keyed _ -> true
    | _ -> false)
;;

let keyed_list_item ~(style : struct_style) ~(tight : bool) c (item, _) =
  let render_body () =
    match Block.List_item.block item with
    | Block.Ext_keyed ((label, body), _) ->
      C.byte c '\n';
      render_struct ~style `List_item c (Struct.label_key label) body
    | b -> item_block ~tight c b
  in
  C.string c "<li>";
  match Block.List_item.ext_task_marker item with
  | None ->
    render_body ();
    C.string c "</li>\n"
  | Some (mark, _) ->
    let close =
      match Block.List_item.task_status_of_task_marker mark with
      | `Unchecked ->
        C.string c "<div class=\"task\"><input type=\"checkbox\" disabled><div>";
        "</div></div></li>\n"
      | `Checked | `Other _ ->
        C.string
          c
          "<div class=\"task\"><input type=\"checkbox\" disabled checked><div>";
        "</div></div></li>\n"
      | `Cancelled ->
        C.string c "<div class=\"task\"><input type=\"checkbox\" disabled><del>";
        "</del></div></li>\n"
    in
    render_body ();
    C.string c close
;;

(* Render a list that contains keyed items. Plain lists are left to
   [Cmarkit_html.renderer], but a mixed keyed list needs positional control so a
   keyed list item can become semantic HTML inside its own [<li>]. *)
let render_keyed_list ~(style : struct_style) c (l : Block.List'.t) =
  let tight = Block.List'.tight l in
  let opening, closing =
    match Block.List'.type' l with
    | `Unordered _ -> "<ul>\n", "</ul>\n"
    | `Ordered (start, _) ->
      (if start = 1 then "<ol>\n" else sprintf "<ol start=\"%d\">\n" start), "</ol>\n"
  in
  C.string c opening;
  List.iter (Block.List'.items l) ~f:(keyed_list_item ~style ~tight c);
  C.string c closing
;;

(** Render a block, optionally carrying a Djot [attr] from an enclosing
    {!Cmarkit.Block.Ext_attributes} wrapper. Returns [false] (defer) for
    blocks that need no oystermark-specific handling and no attribute. *)
let render_block
      ~(struct_style : struct_style ref)
      ?(attr : Attribute.t option)
      (c : Cmarkit_renderer.context)
  : Block.t -> bool
  =
  let attr_id = Option.bind attr ~f:Attribute.id in
  let attr_non_id_html =
    match attr with
    | None -> ""
    | Some a ->
      emit_html_attrs ~id:None ~classes:(Attribute.classes a) ~kvs:(Attribute.key_values a) ()
  in
  function
  | Block.Heading (h, meta) ->
    let slug = Meta.find Heading_slug.meta_key meta in
    (match slug, attr with
     | None, None -> false
     | _, _ ->
       let level = Block.Heading.level h in
       (* The attribute id wins over the auto slug if both present (djot says
          last id wins; the user-written attribute is more specific). *)
       let id_attr =
         match attr_id, slug with
         | Some id, _ -> sprintf " id=\"%s\"" id
         | None, Some s -> sprintf " id=\"%s\"" s
         | None, None -> ""
       in
       C.string c (sprintf "<h%d%s%s>" level id_attr attr_non_id_html);
       C.inline c (Block.Heading.inline h);
       C.string c (sprintf "</h%d>\n" level);
       true)
  | Block.Block_quote (bq, meta) ->
    (match Block.Callout.find meta with
     | Some callout ->
       render_callout c bq callout;
       true
     | None ->
       (match attr with
        | None -> false
        | Some a ->
          C.string c (sprintf "<blockquote%s>\n" (cmarkit_attr_html a));
          C.block c (Block.Block_quote.block bq);
          C.string c "</blockquote>\n";
          true))
  | Block.Paragraph (p, meta) ->
    let block_id = Block.Block_id.find meta in
    (match block_id, attr with
     | None, None -> false
     | _, _ ->
       let id_attr =
         match attr_id, block_id with
         | Some id, _ -> sprintf " id=\"%s\"" id
         | None, Some (b : Block.Block_id.t) -> sprintf " id=\"^%s\"" (Block.Block_id.id b)
         | None, None -> ""
       in
       C.string c (sprintf "<p%s%s>" id_attr attr_non_id_html);
       C.inline c (Block.Paragraph.inline p);
       C.string c "</p>\n";
       true)
  | Block.Code_block (cb, meta) ->
    (* Render with [data-attr-*] when a Pandoc-style attribute is
       attached. Otherwise let the default cmarkit_html renderer
       handle it. *)
    (match Meta.find Cb_attribute.meta_key meta with
     | None | Some { attribute = None; _ } -> false
     | Some { lang; attribute = Some attr } ->
       let data_attrs = cb_attr_html ~key_prefix:"data-attr-" attr in
       C.string c "<pre><code class=\"language-";
       C.string c lang;
       C.string c "\"";
       C.string c data_attrs;
       C.string c ">";
       List.iter (Block.Code_block.code cb) ~f:(fun bl ->
         Cmarkit_html.html_escaped_string c (Block_line.to_string bl);
         C.byte c '\n');
       C.string c "</code></pre>\n";
       true)
  | Parse.Frontmatter.Frontmatter y ->
    let inner = Parse.Frontmatter.to_html (Some y) in
    C.string c (sprintf "<div class=\"frontmatter\">%s</div>\n" inner);
    true
  | Block.Ext_div (d, _) ->
    let class_name = Option.map (Block.Div.class' d) ~f:fst in
    let body = Block.Div.block d in
    (match class_name with
     | Some cls -> C.string c (sprintf "<div class=\"%s\">\n" cls)
     | None -> C.string c "<div>\n");
    let override = Option.bind class_name ~f:struct_style_of_class in
    (match override with
     | None -> C.block c body
     | Some s ->
       let prev = !struct_style in
       struct_style := s;
       C.block c body;
       struct_style := prev);
    C.string c "</div>\n";
    true
  | Cmarkit.Block.Ext_keyed ((label, body), _) ->
    (* A keyed node reached here is a {e free} block (a top-level keyed node, or
       the body of another keyed node). A keyed node that is a list item's block
       is handled positionally by [render_keyed_list], not here. *)
    render_struct ~style:!struct_style `Paragraph c (Struct.label_key label) body;
    true
  | Block.List (l, _) when list_has_keyed l ->
    render_keyed_list ~style:!struct_style c l;
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
;;

(* [struct_style] is threaded as a ref so that [Ext_div] can push a
   local override for the duration of its body and restore it afterwards.
   Rendering is depth-first and synchronous, so a single ref is safe. *)
let block ~(struct_style : struct_style ref) (c : Cmarkit_renderer.context)
  : Block.t -> bool
  = function
  | Block.Ext_attributes (a, _) ->
    let attr = Block.Attributes.attributes a in
    (match Block.Attributes.block a with
     (* Orphan attribute (e.g. [{#x}] before a blank line): no target, no HTML. *)
     | Block.Blocks ([], _) -> true
     | inner ->
       if render_block ~struct_style ~attr c inner
       then true
       else (
         (* Generic fallback for a target with no dedicated rendering: wrap in
            a [<div>] carrying the attributes, matching the fork default. *)
         C.string c (sprintf "<div%s>\n" (cmarkit_attr_html attr));
         C.block c inner;
         C.string c "</div>\n";
         true))
  | b -> render_block ~struct_style c b
;;

let renderer
      ~(backend_blocks : bool)
      ~(safe : bool)
      ?(struct_style : struct_style = `Plain)
      ()
  : Cmarkit_renderer.t
  =
  let style_ref = ref struct_style in
  let custom = Cmarkit_renderer.make ~inline ~block:(block ~struct_style:style_ref) () in
  let default = Cmarkit_html.renderer ~backend_blocks ~safe () in
  Cmarkit_renderer.compose default custom
;;

let of_doc
      ~(backend_blocks : bool)
      ~(safe : bool)
      ?(config = Config.default)
      (* ?(struct_style : struct_style = `Plain) *)
        (doc : Doc.t)
  : string
  =
  let struct_style : struct_style =
    match config.ext_struct.style with
    | Config.Struct_style_def.Plain -> `Plain
    | Config.Struct_style_def.Basic -> `Basic
    | Config.Struct_style_def.Graph -> `Graph
  in
  Cmarkit_renderer.doc_to_string (renderer ~backend_blocks ~safe ~struct_style ()) doc
;;

module For_test = struct
  let html_of_doc struct_style doc =
    Cmarkit_renderer.doc_to_string
      (renderer ~backend_blocks:false ~safe:false ~struct_style ())
      doc
  ;;

  let pp_doc struct_style ppf doc =
    html_of_doc struct_style doc |> Format.pp_print_string ppf
  ;;
end

let%expect_test "block attribute on paragraph" =
  let open For_test in
  let doc = Parse.of_string "{#water .important key=\"my val\"}\nDon't forget!" in
  Format.printf "%a%!" (pp_doc `Plain) doc;
  [%expect {| <p id="water" class="important" key="my val">Don't forget!</p> |}]
;;

let%expect_test "block attribute on heading combines with slug; attr id wins" =
  let open For_test in
  let doc = Parse.of_string "{#custom .big}\n# Hello world" in
  Format.printf "%a%!" (pp_doc `Plain) doc;
  [%expect {| <h1 id="custom" class="big">Hello world</h1> |}]
;;

let%expect_test "block attribute on blockquote" =
  let open For_test in
  let doc = Parse.of_string "{source=\"Iliad\"}\n> Sing, muse" in
  Format.printf "%a%!" (pp_doc `Plain) doc;
  [%expect
    {|
    <blockquote source="Iliad">
    <p>Sing, muse</p>
    </blockquote>
    |}]
;;

let%expect_test "code block pandoc attribute renders as data-attr-*" =
  let open For_test in
  let src = "```python {#snippet .runnable timeout=30}\nprint('hi')\n```" in
  let doc = Parse.of_string src in
  Format.printf "%a%!" (pp_doc `Plain) doc;
  [%expect
    {|
    <pre><code class="language-python" data-attr-id="snippet" data-attr-class="runnable" data-attr-timeout="30">print('hi')
    </code></pre>
    |}]
;;

let%expect_test "orphan block attribute emits no HTML" =
  let open For_test in
  (* Orphan attribute paragraph (followed by blank line, no target) *)
  let doc = Parse.of_string "{#orphan}\n\nA paragraph." in
  Format.printf "%a%!" (pp_doc `Plain) doc;
  [%expect {| <p>A paragraph.</p> |}]
;;

let%expect_test "struct: unified HTML across styles" =
  let open For_test in
  let src =
    {|
Architecture:
- : encoder–decoder
- encoder:
  - self-attention: multi-head
  - feed-forward: position-wise MLP
- decoder:
  - masked self-attention:
    - autoregressive
    - attends positions ≤ i
  - cross-attention: over encoder output

Single:
- only-child: sole entry
|}
  in
  let doc = Parse.of_string src in
  Format.printf "%a%!" (pp_doc `Plain) doc;
  [%expect
    {|
    <div class="keyed" data-label-kind="paragraph" data-style="plain" data-body="list"><span class="keyed-label">Architecture</span>
    <div class="keyed-body">
    <ul>
    <li>
    <div class="keyed" data-label-kind="list-item" data-style="plain" data-body="paragraph" data-empty-label><span class="keyed-label"></span>
    <div class="keyed-body">
    <p>encoder–decoder</p>
    </div>
    </div>
    </li>
    <li>
    <div class="keyed" data-label-kind="list-item" data-style="plain" data-body="list"><span class="keyed-label">encoder</span>
    <div class="keyed-body">
    <ul>
    <li>
    <div class="keyed" data-label-kind="list-item" data-style="plain" data-body="paragraph"><span class="keyed-label">self-attention</span>
    <div class="keyed-body">
    <p>multi-head</p>
    </div>
    </div>
    </li>
    <li>
    <div class="keyed" data-label-kind="list-item" data-style="plain" data-body="paragraph"><span class="keyed-label">feed-forward</span>
    <div class="keyed-body">
    <p>position-wise MLP</p>
    </div>
    </div>
    </li>
    </ul>
    </div>
    </div>
    </li>
    <li>
    <div class="keyed" data-label-kind="list-item" data-style="plain" data-body="list"><span class="keyed-label">decoder</span>
    <div class="keyed-body">
    <ul>
    <li>
    <div class="keyed" data-label-kind="list-item" data-style="plain" data-body="list"><span class="keyed-label">masked self-attention</span>
    <div class="keyed-body">
    <ul>
    <li>autoregressive</li>
    <li>attends positions ≤ i</li>
    </ul>
    </div>
    </div>
    </li>
    <li>
    <div class="keyed" data-label-kind="list-item" data-style="plain" data-body="paragraph"><span class="keyed-label">cross-attention</span>
    <div class="keyed-body">
    <p>over encoder output</p>
    </div>
    </div>
    </li>
    </ul>
    </div>
    </div>
    </li>
    </ul>
    </div>
    </div>
    <div class="keyed" data-label-kind="paragraph" data-style="plain" data-body="list" data-single-list-item><span class="keyed-label">Single</span>
    <div class="keyed-body">
    <ul>
    <li>
    <div class="keyed" data-label-kind="list-item" data-style="plain" data-body="paragraph"><span class="keyed-label">only-child</span>
    <div class="keyed-body">
    <p>sole entry</p>
    </div>
    </div>
    </li>
    </ul>
    </div>
    </div>
    |}]
;;

let%expect_test "struct: data-style differs across plain/basic/graph" =
  let open For_test in
  let src = {|Key: value|} in
  let doc = Parse.of_string src in
  let first_line s =
    match String.split_lines s with
    | first :: _ -> first
    | [] -> ""
  in
  List.iter [ `Plain; `Basic; `Graph ] ~f:(fun style ->
    let rendered = html_of_doc style doc in
    print_endline (first_line rendered));
  [%expect
    {|
    <div class="keyed" data-label-kind="paragraph" data-style="plain" data-body="paragraph"><span class="keyed-label">Key</span>
    <div class="keyed" data-label-kind="paragraph" data-style="basic" data-body="paragraph"><span class="keyed-label">Key</span>
    <div class="keyed" data-label-kind="paragraph" data-style="graph" data-body="paragraph"><span class="keyed-label">Key</span>
    |}]
;;

let%test_module "don't throw" =
  (module struct
    let%test_unit _ =
      let examples : string list =
        List.concat
          [ List.map ~f:(fun ex -> ex.content) Parse.Struct.For_test.examples
          ; (* div smoke-test inputs *)
            [ "::: warning\nbody\n:::"
            ; ":::: outer\n::: inner\ncontent\n:::\n::::"
            ; "::: warning\nunclosed content"
            ; "- foo:\n::: warning\ncontent\n:::\n- bar"
            ]
          ; (* callout smoke-test inputs *)
            [ "> [!info] Here's a callout title"
            ; "> [!tip]"
            ; "> [!faq]- Are callouts foldable?"
            ; "> [!note]+ Expanded"
            ; "> [!WARNING] Watch out"
            ; "> not a callout"
            ; "> [!] empty kind"
            ; "> [!tip]\n> Body content here"
            ]
          ; Parse.Cb_attribute.For_test.examples
          ]
      in
      List.iter examples ~f:(fun src ->
        let doc = Parse.of_string src in
        ignore (of_doc ~backend_blocks:false ~safe:false doc))
    ;;
  end)
;;
