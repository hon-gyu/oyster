(* mdast of a djot document: the [root] node oyster-publish's remark pipeline
   reads in place of remark-parse's.

   Syntax without an mdast type becomes an [oyElement] node carrying
   [data.hName]/[data.hProperties], which mdast-util-to-hast renders without a
   custom handler. Attributes become the [hProperties] of the node they are on,
   or of a wrapping [span]/[div] when that node renders as raw content. *)

open Core
module Json = Yojson.Safe

type block = Djot.Block.t Djot.node
type inline = Djot.Inline.t Djot.node

(* mdast positions are 1-based line, 1-based column and 0-based offset; a
   location's last byte is inclusive, an mdast end point sits after it. *)
let position (doc : Djot.Doc.t) (node : _ Djot.node) : (string * Json.t) list =
  let loc = Djot.Doc.textloc doc node in
  if Djot.Textloc.is_none loc
  then []
  else (
    let first_line, first_line_start = Djot.Textloc.first_line loc in
    let last_line, last_line_start = Djot.Textloc.last_line loc in
    let first = Djot.Textloc.first_byte loc in
    let last = Djot.Textloc.last_byte loc in
    let point line column offset =
      `Assoc [ "line", `Int line; "column", `Int column; "offset", `Int offset ]
    in
    [ ( "position"
      , `Assoc
          [ "start", point first_line (first - first_line_start + 1) first
          ; "end", point last_line (last - last_line_start + 2) (last + 1)
          ] )
    ])
;;

let mk ?(pos = []) type_ fields : Json.t =
  `Assoc ((("type", `String type_) :: fields) @ pos)
;;

let text ?pos value = mk ?pos "text" [ "value", `String value ]
let class_name classes = "className", `List (List.map classes ~f:(fun c -> `String c))

let element ?pos ?(properties = []) ~children tag =
  mk
    ?pos
    "oyElement"
    [ ( "data"
      , `Assoc
          (("hName", `String tag)
           ::
           (if List.is_empty properties then [] else [ "hProperties", `Assoc properties ])
          ) )
    ; "children", `List children
    ]
;;

let hproperties (attrs : Djot.Attr.t) : (string * Json.t) list =
  List.map attrs ~f:(fun (k, v) ->
    match k with
    | "class" -> class_name (Djot.Attr.classes attrs)
    | k -> k, `String v)
;;

(** [nodes] with [attrs] as the hProperties of the single element among them,
    or wrapped in [wrapper] carrying them. *)
let with_attrs ?pos ~wrapper (attrs : Djot.Attr.t) (nodes : Json.t list) : Json.t list =
  match attrs, nodes with
  | [], _ -> nodes
  | _, [ `Assoc (("type", `String type_) :: rest) ]
    when (not (String.equal type_ "text"))
         && (not (String.equal type_ "html"))
         && not (List.Assoc.mem rest "data" ~equal:String.equal) ->
    [ `Assoc
        (("type", `String type_)
         :: ("data", `Assoc [ "hProperties", `Assoc (hproperties attrs) ])
         :: rest)
    ]
  | _ -> [ element ?pos ~properties:(hproperties attrs) ~children:nodes wrapper ]
;;

let footnote_identifier label = String.chop_prefix_if_exists label ~prefix:"^"

(* Inlines
   ======= *)

let rec inlines doc (is : inline list) : Json.t list = List.concat_map is ~f:(inline doc)

and inline doc (Node (_, attrs, contents) as node : inline) : Json.t list =
  let pos = position doc node in
  let children l = "children", `List (inlines doc l) in
  let tag name l = [ element ~pos ~children:(inlines doc l) name ] in
  let destination target =
    match (target : Djot.Inline.target) with
    | Direct url -> url
    | Reference label ->
      Option.value_map (Djot.Doc.reference doc label) ~default:"" ~f:fst
  in
  let nodes =
    match contents with
    | Str s -> [ text ~pos s ]
    | SoftBreak -> [ text ~pos "\n" ]
    | HardBreak -> [ mk ~pos "break" [] ]
    | NonBreakingSpace -> [ text ~pos "\u{00A0}" ]
    | Verbatim s -> [ mk ~pos "inlineCode" [ "value", `String s ] ]
    | Emph l -> [ mk ~pos "emphasis" [ children l ] ]
    | Strong l -> [ mk ~pos "strong" [ children l ] ]
    | Delete l -> [ mk ~pos "delete" [ children l ] ]
    | Highlight l -> tag "mark" l
    | Insert l -> tag "ins" l
    | Superscript l -> tag "sup" l
    | Subscript l -> tag "sub" l
    | Span l -> inlines doc l
    | Quoted (quote, l) ->
      let open_, close =
        match quote with
        | SingleQuotes -> "\u{2018}", "\u{2019}"
        | DoubleQuotes -> "\u{201C}", "\u{201D}"
      in
      (text ~pos open_ :: inlines doc l) @ [ text ~pos close ]
    | Symbol name ->
      [ mk
          ~pos
          "text"
          [ "value", `String (":" ^ name ^ ":")
          ; "data", `Assoc [ "oySymbol", `Assoc [ "name", `String name ] ]
          ]
      ]
    | Math (style, tex) ->
      let cls =
        match style with
        | DisplayMath -> "math-display"
        | InlineMath -> "math-inline"
      in
      [ element
          ~pos
          ~properties:[ class_name [ "math"; cls ] ]
          ~children:[ text ~pos tex ]
          "span"
      ]
    | Link (l, target) ->
      [ mk ~pos "link" [ "url", `String (destination target); children l ] ]
    | Image (l, target) ->
      [ mk
          ~pos
          "image"
          [ "url", `String (destination target)
          ; "alt", `String (Parse.Common.plain_text l)
          ]
      ]
    | UrlLink url ->
      [ mk ~pos "link" [ "url", `String url; "children", `List [ text ~pos url ] ] ]
    | EmailLink email ->
      [ mk
          ~pos
          "link"
          [ "url", `String ("mailto:" ^ email); "children", `List [ text ~pos email ] ]
      ]
    | FootnoteReference label ->
      let id = footnote_identifier label in
      [ mk ~pos "footnoteReference" [ "identifier", `String id; "label", `String id ] ]
    | RawInline ("html", s) -> [ mk ~pos "html" [ "value", `String s ] ]
    | RawInline _ -> []
    | Ext_wikilink (embed, raw, alias) ->
      let reference = Note.Link.Ref.of_wikilink_target raw in
      let fragment : Json.t =
        match reference.fragment with
        | None -> `Null
        | Some path ->
          `Assoc
            [ "kind", `String "heading"
            ; "path", `List (List.map path ~f:(fun s -> `String s))
            ]
      in
      let target = Option.value reference.target ~default:"" in
      [ mk
          ~pos
          "link"
          [ "url", `String (Note.Link.Ref.to_wikilink_target reference)
          ; ( "data"
            , `Assoc
                [ "hProperties", `Assoc [ class_name [ "wikilink" ] ]
                ; ( "oyWikilink"
                  , `Assoc
                      [ "target", `String target
                      ; "fragment", fragment
                      ; "embed", `Bool embed
                      ] )
                ] )
          ; ( "children"
            , `List
                [ text
                    ~pos
                    (match alias with
                     | Some alias -> alias
                     | None -> if String.is_empty target then raw else target)
                ] )
          ]
      ]
  in
  with_attrs ~pos ~wrapper:"span" attrs nodes
;;

(* Blocks
   ====== *)

(** A heading's or a section's attributes without the id djot derives when
    none is written: the site slugs headings itself, and links are resolved
    against those slugs. An attribute spec on the node means the id is the
    author's. *)
let authored_attrs doc (node : block) (attrs : Djot.Attr.t) : Djot.Attr.t =
  let written =
    List.exists (Djot.Doc.syntax_locs doc node) ~f:(fun (role, _) ->
      match role with
      | RAttrSpec -> true
      | ROpenFence | RCloseFence -> false)
  in
  if written then attrs else List.Assoc.remove attrs "id" ~equal:String.equal
;;

let rec blocks doc (bs : block list) : Json.t list = List.concat_map bs ~f:(block doc)

and block doc (Node (_, attrs, contents) as node : block) : Json.t list =
  let pos = position doc node in
  let children l = "children", `List (blocks doc l) in
  let item_node ~spread ~checked bs =
    mk "listItem" [ "spread", `Bool spread; "checked", checked; children bs ]
  in
  match contents with
  | Section ((Node (_, heading_attrs, Heading (level, l)) as heading) :: rest) ->
    (* mdast has no sections: the heading carries the section's attributes. *)
    let attrs = authored_attrs doc node attrs in
    let heading_attrs =
      authored_attrs doc heading heading_attrs
      @ List.filter attrs ~f:(fun (k, _) ->
        not (List.Assoc.mem heading_attrs k ~equal:String.equal))
    in
    with_attrs
      ~pos:(position doc heading)
      ~wrapper:"div"
      heading_attrs
      [ mk
          ~pos:(position doc heading)
          "heading"
          [ "depth", `Int level; "children", `List (inlines doc l) ]
      ]
    @ blocks doc rest
  | Section l -> with_attrs ~pos ~wrapper:"div" attrs (blocks doc l)
  | _ ->
    let nodes =
      match contents with
      | Section _ -> assert false
      | Para l -> [ mk ~pos "paragraph" [ "children", `List (inlines doc l) ] ]
      | Heading (level, l) ->
        [ mk ~pos "heading" [ "depth", `Int level; "children", `List (inlines doc l) ] ]
      | CodeBlock (lang, code) ->
        let lang, meta =
          match String.lsplit2 (String.strip lang) ~on:' ' with
          | _ when String.is_empty (String.strip lang) -> `Null, `Null
          | None -> `String (String.strip lang), `Null
          | Some (lang, meta) -> `String lang, `String meta
        in
        [ mk
            ~pos
            "code"
            [ "lang", lang
            ; "meta", meta
            ; "value", `String (String.chop_suffix_if_exists code ~suffix:"\n")
            ]
        ]
      | RawBlock ("html", s) -> [ mk ~pos "html" [ "value", `String s ] ]
      | RawBlock _ -> []
      | BlockQuote l -> [ mk ~pos "blockquote" [ children l ] ]
      | Div l -> [ element ~pos ~children:(blocks doc l) "div" ]
      | ThematicBreak -> [ mk ~pos "thematicBreak" [] ]
      | BulletList (spacing, items) ->
        let spread = Poly.equal spacing Djot.Block.Loose in
        [ mk
            ~pos
            "list"
            [ "ordered", `Bool false
            ; "start", `Null
            ; "spread", `Bool spread
            ; "children", `List (List.map items ~f:(item_node ~spread ~checked:`Null))
            ]
        ]
      | OrderedList ({ ol_start; _ }, spacing, items) ->
        let spread = Poly.equal spacing Djot.Block.Loose in
        [ mk
            ~pos
            "list"
            [ "ordered", `Bool true
            ; "start", `Int ol_start
            ; "spread", `Bool spread
            ; "children", `List (List.map items ~f:(item_node ~spread ~checked:`Null))
            ]
        ]
      | TaskList (spacing, items) ->
        let spread = Poly.equal spacing Djot.Block.Loose in
        [ mk
            ~pos
            "list"
            [ "ordered", `Bool false
            ; "start", `Null
            ; "spread", `Bool spread
            ; ( "children"
              , `List
                  (List.map items ~f:(fun (status, bs) ->
                     item_node
                       ~spread
                       ~checked:(`Bool (Poly.equal status Djot.Block.Complete))
                       bs)) )
            ]
        ]
      | DefinitionList (_, items) ->
        [ element
            ~pos
            ~children:
              (List.concat_map items ~f:(fun (term, bs) ->
                 [ element ~children:(inlines doc term) "dt"
                 ; element ~children:(blocks doc bs) "dd"
                 ]))
            "dl"
        ]
      | Table (caption, rows) ->
        let align (Djot.Block.Cell (_, align, _)) : Json.t =
          match align with
          | AlignLeft -> `String "left"
          | AlignRight -> `String "right"
          | AlignCenter -> `String "center"
          | AlignDefault -> `Null
        in
        let row cells =
          mk
            "tableRow"
            [ ( "children"
              , `List
                  (List.map cells ~f:(fun (Djot.Block.Cell (_, _, l)) ->
                     mk "tableCell" [ "children", `List (inlines doc l) ])) )
            ]
        in
        [ mk
            ~pos
            "table"
            ([ ( "align"
               , `List
                   (Option.value_map (List.hd rows) ~default:[] ~f:(List.map ~f:align)) )
             ; "children", `List (List.map rows ~f:row)
             ]
             @ Option.value_map caption ~default:[] ~f:(fun l ->
               [ "caption", `List (inlines doc l) ]))
        ]
      | FootnoteDef (label, l) ->
        let id = footnote_identifier label in
        [ mk
            ~pos
            "footnoteDefinition"
            [ "identifier", `String id; "label", `String id; children l ]
        ]
      | RefDef _ -> []
      | Ext_keyed (label, value) ->
        [ element
            ~pos
            ~properties:[ class_name [ "keyed" ] ]
            ~children:
              [ element ~children:(inlines doc label) "dt"
              ; element ~children:(block doc value) "dd"
              ]
            "dl"
        ]
      | Ext_callout (kind, fold, title, body) ->
        let title =
          match title with
          | [] -> [ text ~pos (String.capitalize kind) ]
          | title -> inlines doc title
        in
        let fold =
          match fold with
          | None -> []
          | Some FoldExpanded -> [ "dataCalloutFold", `String "open" ]
          | Some FoldCollapsed -> [ "dataCalloutFold", `String "closed" ]
        in
        [ mk
            ~pos
            "blockquote"
            [ ( "data"
              , `Assoc
                  [ "hName", `String "blockquote"
                  ; ( "hProperties"
                    , `Assoc
                        (class_name [ "callout" ] :: ("dataCallout", `String kind) :: fold)
                    )
                  ] )
            ; ( "children"
              , `List
                  [ element
                      ~properties:[ class_name [ "callout-title" ] ]
                      ~children:title
                      "div"
                  ; element
                      ~properties:[ class_name [ "callout-content" ] ]
                      ~children:(blocks doc body)
                      "div"
                  ] )
            ]
        ]
    in
    let attrs =
      match contents with
      | Heading _ -> authored_attrs doc node attrs
      | _ -> attrs
    in
    with_attrs ~pos ~wrapper:"div" attrs nodes
;;

let of_doc (doc : Djot.Doc.t) : Json.t =
  let footnotes =
    List.map (Djot.Doc.footnotes doc) ~f:(fun (label, bs) ->
      Djot.Node.make (Djot.Block.FootnoteDef (label, bs)))
  in
  mk "root" [ "children", `List (blocks doc (Djot.Doc.blocks doc @ footnotes)) ]
;;
