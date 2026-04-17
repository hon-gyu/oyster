(** HTML renderer for oystermark documents.

    Extends [Cmarkit_html.renderer] to handle wikilinks, resolved link targets,
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

module Heading_slug = Parse.Heading_slug

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

(* [struct_style] is threaded as a ref so that [Ext_div] can push a
   local override for the duration of its body and restore it afterwards.
   Rendering is depth-first and synchronous, so a single ref is safe. *)
let block ~(struct_style : struct_style ref) (c : Cmarkit_renderer.context)
  : Block.t -> bool
  = function
  | Block.Heading (h, meta) ->
    (match Meta.find Heading_slug.meta_key meta with
     | Some slug ->
       let level = Block.Heading.level h in
       C.string c (sprintf "<h%d id=\"%s\">" level slug);
       C.inline c (Block.Heading.inline h);
       C.string c (sprintf "</h%d>\n" level);
       true
     | None -> false)
  | Block.Block_quote (bq, meta) ->
    (match Meta.find Callout.meta_key meta with
     | Some callout ->
       render_callout c bq callout;
       true
     | None -> false)
  | Block.Paragraph (p, meta) ->
    (* Render a paragraph with block-id. The ^blockid text stays visible
        (it's part of the inline content). We add an id to the <p> for linking. *)
    (match Meta.find Block_id.meta_key meta with
     | Some (block_id : Block_id.t) ->
       let id = "^" ^ block_id.id in
       C.string c (Format.asprintf "<p id=\"%s\">" id);
       C.inline c (Block.Paragraph.inline p);
       C.string c "</p>\n";
       true
     | None -> false)
  | Parse.Frontmatter.Frontmatter y ->
    let inner = Parse.Frontmatter.to_html (Some y) in
    C.string c (sprintf "<div class=\"frontmatter\">%s</div>\n" inner);
    true
  | Parse.Div.Ext_div (div, body) ->
    (match div.class_name with
     | Some cls -> C.string c (sprintf "<div class=\"%s\">\n" cls)
     | None -> C.string c "<div>\n");
    let override = Option.bind div.class_name ~f:struct_style_of_class in
    (match override with
     | None -> C.block c body
     | Some s ->
       let prev = !struct_style in
       struct_style := s;
       C.block c body;
       struct_style := prev);
    C.string c "</div>\n";
    true
  | Parse.Struct.Ext_keyed_block ({ label }, body) ->
    render_struct ~style:!struct_style `Paragraph c label body;
    true
  | Parse.Struct.Ext_keyed_list_item ({ label }, body) ->
    render_struct ~style:!struct_style `List_item c label body;
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

  let pp_doc struct_style doc = html_of_doc struct_style doc |> print_string
end

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
  pp_doc `Plain doc;
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
          ; List.map ~f:(fun (_, content, _) -> content) Parse.Div.For_test.examples
          ; Parse.Callout.For_test.examples
          ; Parse.Attribute.For_test.examples
          ]
      in
      List.iter examples ~f:(fun src ->
        let doc = Parse.of_string src in
        ignore (of_doc ~backend_blocks:false ~safe:false doc))
    ;;
  end)
;;
