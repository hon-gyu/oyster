open Core
module X = Simple_xml

let attr key value : X.Attribute.t = { ns = ""; key; value }

let element ?(attributes = []) tag children : X.element =
  { tag = { ns = ""; tag }; attributes; children }
;;

let node ?(attributes = []) tag children : X.t =
  X.Element (element ~attributes tag children)
;;

let string key value = attr key value
let number key value = attr key (Int.to_string value)

let textloc (loc : Djot.Textloc.t) : X.Attribute.t list =
  if Djot.Textloc.is_none loc
  then []
  else
    [ number "start-byte" (Djot.Textloc.first_byte loc)
    ; number "end-byte" (Djot.Textloc.last_byte loc)
    ; number "start-line" (fst (Djot.Textloc.first_line loc))
    ; number "end-line" (fst (Djot.Textloc.last_line loc))
    ]
;;

let location (doc : Djot.Doc.t) (n : _ Djot.node) : X.Attribute.t list =
  textloc (Djot.Doc.textloc doc n)
;;

(* [label] and [title] are not nodes: they span their inlines. *)
let inlines_location (doc : Djot.Doc.t) (ns : Djot.Inline.t Djot.node list)
  : X.Attribute.t list
  =
  match List.hd ns, List.last ns with
  | Some first, Some last ->
    let first = Djot.Doc.textloc doc first
    and last = Djot.Doc.textloc doc last in
    if Djot.Textloc.is_none first || Djot.Textloc.is_none last
    then []
    else textloc (Djot.Textloc.reloc ~first ~last)
  | _ -> []
;;

let with_locations (locs : 'loc list) (parts : 'a list) : ('loc option * 'a) list =
  match List.zip locs parts with
  | Ok pairs -> List.map pairs ~f:(fun (loc, part) -> Some loc, part)
  | Unequal_lengths -> List.map parts ~f:(fun part -> None, part)
;;

let part_location : Djot.Textloc.t option -> X.Attribute.t list =
  Option.value_map ~default:[] ~f:textloc
;;

let span (element : X.element) : (int * int) option =
  let value key =
    List.find_map element.attributes ~f:(fun (a : X.Attribute.t) ->
      if String.equal a.key key then Int.of_string_opt a.value else None)
  in
  match value "start-byte", value "end-byte" with
  | Some first, Some last when 0 <= first && first <= last -> Some (first, last)
  | _ -> None
;;

let authored_attrs (attrs : Djot.Attr.t) : X.Attribute.t list * X.t list =
  let id = Option.to_list (Option.map (Djot.Attr.id attrs) ~f:(string "id")) in
  let classes = Djot.Attr.classes attrs in
  let class_attr =
    if List.is_empty classes
    then []
    else [ string "class" (String.concat ~sep:" " classes) ]
  in
  let other =
    Djot.Attr.key_values attrs
    |> List.map ~f:(fun (name, value) ->
      node "attribute" ~attributes:[ string "name" name; string "value" value ] [])
  in
  id @ class_attr, other
;;

let with_node doc (n : _ Djot.node) extra =
  let attrs = Djot.Node.attrs n in
  let head, tail = authored_attrs attrs in
  extra @ head @ location doc n, tail
;;

let rec inlines doc nodes = List.map nodes ~f:(inline doc)

and inline doc (Djot.Node (_, _, value) as n : Djot.Inline.t Djot.node) : X.t =
  let tag, extra, children =
    match value with
    | Str s -> "text", [], [ X.Text s ]
    | Emph ns -> "emph", [], inlines doc ns
    | Strong ns -> "strong", [], inlines doc ns
    | Highlight ns -> "highlight", [], inlines doc ns
    | Insert ns -> "insert", [], inlines doc ns
    | Delete ns -> "delete", [], inlines doc ns
    | Superscript ns -> "superscript", [], inlines doc ns
    | Subscript ns -> "subscript", [], inlines doc ns
    | Span ns -> "span", [], inlines doc ns
    | Verbatim s -> "verbatim", [], [ X.Text s ]
    | Symbol s -> "symbol", [], [ X.Text s ]
    | Math (style, s) ->
      ( "math"
      , [ string
            "style"
            (match style with
             | DisplayMath -> "display"
             | InlineMath -> "inline")
        ]
      , [ X.Text s ] )
    | Link (ns, target) -> "link", target_attrs target, inlines doc ns
    | Image (ns, target) -> "image", target_attrs target, inlines doc ns
    | FootnoteReference label -> "footnote_reference", [ string "label" label ], []
    | UrlLink url -> "url_link", [ string "url" url ], [ X.Text url ]
    | EmailLink address -> "email_link", [ string "address" address ], [ X.Text address ]
    | Ext_wikilink (embed, target, alias) ->
      ( "wikilink"
      , [ string "target" target; string "embed" (Bool.to_string embed) ]
        @ Option.to_list (Option.map alias ~f:(string "alias"))
      , [] )
    | RawInline (format, s) -> "raw_inline", [ string "format" format ], [ X.Text s ]
    | NonBreakingSpace -> "non_breaking_space", [], [ X.Text "\194\160" ]
    | Quoted (style, ns) ->
      ( "quoted"
      , [ string
            "style"
            (match style with
             | SingleQuotes -> "single"
             | DoubleQuotes -> "double")
        ]
      , inlines doc ns )
    | SoftBreak -> "soft_break", [], [ X.Text "\n" ]
    | HardBreak -> "hard_break", [], [ X.Text "\n" ]
  in
  let attributes, authored = with_node doc n extra in
  node tag ~attributes (authored @ children)

and target_attrs : Djot.Inline.target -> X.Attribute.t list = function
  | Direct target -> [ string "target" target; string "target-kind" "direct" ]
  | Reference target -> [ string "target" target; string "target-kind" "reference" ]
;;

(* Definitions are listed once, under [footnotes] and [references], so the tree
   skips them wherever they were written: the document pass leaves reference
   definitions in place, and {!Djot.Doc.footnote_defs} keeps nested ones in
   their parent. *)
let is_definition (Djot.Node (_, _, value) : Djot.Block.t Djot.node) =
  match value with
  | FootnoteDef _ | RefDef _ -> true
  | _ -> false
;;

let rec blocks doc ns =
  List.filter_map ns ~f:(fun n -> if is_definition n then None else Some (block doc n))

and block doc (Djot.Node (_, _, value) as n : Djot.Block.t Djot.node) : X.t =
  let parts = Djot.Doc.parts doc n in
  let item_locations =
    match parts with
    | Items locs -> locs
    | NoParts | DefItems _ | TableRows _ -> []
  in
  let tag, extra, children =
    match value with
    | Para ns -> "paragraph", [], inlines doc ns
    | Section ns -> "section", [], blocks doc ns
    | Heading (level, ns) -> "heading", [ number "level" level ], inlines doc ns
    | BlockQuote ns -> "block_quote", [], blocks doc ns
    | CodeBlock (lang, text) -> "code_block", [ string "lang" lang ], [ X.Text text ]
    | Div ns -> "div", [], blocks doc ns
    | OrderedList (settings, spacing, items) ->
      ( "list"
      , [ string "kind" "ordered"
        ; string "spacing" (spacing_string spacing)
        ; number "start" settings.ol_start
        ; string
            "style"
            (match settings.ol_style with
             | Decimal -> "decimal"
             | LetterUpper -> "letter-upper"
             | LetterLower -> "letter-lower"
             | RomanUpper -> "roman-upper"
             | RomanLower -> "roman-lower")
        ; string
            "delimiter"
            (match settings.ol_delim with
             | RightPeriod -> "period"
             | RightParen -> "right-paren"
             | LeftRightParen -> "left-right-paren")
        ]
      , list_items doc item_locations items )
    | BulletList (spacing, items) ->
      ( "list"
      , [ string "kind" "bullet"; string "spacing" (spacing_string spacing) ]
      , list_items doc item_locations items )
    | TaskList (spacing, items) ->
      ( "list"
      , [ string "kind" "task"; string "spacing" (spacing_string spacing) ]
      , with_locations item_locations items
        |> List.map ~f:(fun (loc, (status, bs)) ->
          node
            "item"
            ~attributes:
              (string
                 "task"
                 (match status with
                  | Djot.Block.Complete -> "checked"
                  | Incomplete -> "unchecked")
               :: part_location loc)
            (blocks doc bs)) )
    | DefinitionList (spacing, items) ->
      ( "definition_list"
      , [ string "spacing" (spacing_string spacing) ]
      , with_locations
          (match parts with
           | DefItems locs -> locs
           | NoParts | Items _ | TableRows _ -> [])
          items
        |> List.map ~f:(fun (locs, (term, definition)) ->
          let part f = part_location (Option.map locs ~f) in
          node
            "item"
            ~attributes:(part (fun (item, _, _) -> item))
            [ node "term" ~attributes:(part (fun (_, term, _) -> term)) (inlines doc term)
            ; node
                "definition"
                ~attributes:(part (fun (_, _, definition) -> definition))
                (blocks doc definition)
            ]) )
    | ThematicBreak -> "thematic_break", [], []
    | Table (caption, rows) ->
      ( "table"
      , []
      , let caption_location, row_locations =
          match parts with
          | TableRows (caption, rows) -> caption, rows
          | NoParts | Items _ | DefItems _ -> None, []
        in
        (if List.is_empty caption
         then []
         else
           [ node
               "caption"
               ~attributes:(part_location caption_location)
               (inlines doc caption)
           ])
        @ (with_locations row_locations rows
           |> List.map ~f:(fun (locs, row) ->
             node
               "row"
               ~attributes:(part_location (Option.map locs ~f:fst))
               (with_locations (Option.value_map locs ~default:[] ~f:snd) row
                |> List.map ~f:(fun (loc, c) -> cell doc (part_location loc) c)))) )
    | RawBlock (format, text) -> "raw_block", [ string "format" format ], [ X.Text text ]
    | FootnoteDef (label, ns) ->
      "footnote_definition", [ string "label" label ], blocks doc ns
    | RefDef (label, destination) ->
      ( "reference_definition"
      , [ string "label" label; string "destination" destination ]
      , [] )
    | Ext_keyed (label, value) ->
      ( "keyed"
      , [ string "key" (String.strip (Djot.Inline.to_plain_text label)) ]
      , node "label" ~attributes:(inlines_location doc label) (inlines doc label)
        :: blocks doc [ value ] )
    | Ext_callout (kind, fold, title, body) ->
      ( "callout"
      , [ string "type" kind ]
        @ Option.to_list
            (Option.map fold ~f:(function
               | FoldExpanded -> string "fold" "expanded"
               | FoldCollapsed -> string "fold" "collapsed"))
      , node "title" ~attributes:(inlines_location doc title) (inlines doc title)
        :: blocks doc body )
  in
  let attributes, authored = with_node doc n extra in
  node tag ~attributes (authored @ children)

and list_items doc locations (items : Djot.Block.t Djot.node list list) : X.t list =
  with_locations locations items
  |> List.map ~f:(fun (loc, bs) ->
    node "item" ~attributes:(part_location loc) (blocks doc bs))

and cell doc location (Djot.Block.Cell (kind, align, ns)) : X.t =
  node
    "cell"
    ~attributes:
      ([ string
           "kind"
           (match kind with
            | HeadCell -> "head"
            | BodyCell -> "body")
       ; string
           "align"
           (match align with
            | AlignLeft -> "left"
            | AlignRight -> "right"
            | AlignCenter -> "center"
            | AlignDefault -> "default")
       ]
       @ location)
    (inlines doc ns)

and spacing_string : Djot.Block.list_spacing -> string = function
  | Tight -> "tight"
  | Loose -> "loose"
;;

let rec yaml : Yaml.value -> X.Attribute.t list * X.t list = function
  | `Null -> [ string "type" "null" ], []
  | `Bool b -> [ string "type" "bool" ], [ X.Text (Bool.to_string b) ]
  | `Float f ->
    ( [ string "type" "number" ]
    , [ X.Text
          (if Float.is_integer f
           then Int.to_string (Float.to_int f)
           else Float.to_string f)
      ] )
  | `String s -> [ string "type" "string" ], [ X.Text s ]
  | `A values ->
    ( [ string "type" "list" ]
    , List.map values ~f:(fun value ->
        let attributes, children = yaml value in
        node "entry" ~attributes children) )
  | `O fields ->
    ( [ string "type" "map" ]
    , List.map fields ~f:(fun (name, value) ->
        let attributes, children = yaml value in
        node "field" ~attributes:(string "name" name :: attributes) children) )
;;

let of_doc ?(frontmatter : Yaml.value option) (doc : Djot.Doc.t) : X.element =
  let frontmatter =
    Option.to_list
      (Option.map frontmatter ~f:(fun value ->
         let attributes, children = yaml value in
         node "frontmatter" ~attributes children))
  in
  let content = blocks doc (Djot.Doc.blocks doc) in
  let footnotes =
    Djot.Doc.footnote_defs doc |> List.map ~f:(block doc) |> node "footnotes"
  in
  let references =
    Djot.Doc.references doc
    |> List.map ~f:(fun (label, (destination, attrs)) ->
      let authored, tail = authored_attrs attrs in
      node
        "reference_definition"
        ~attributes:([ string "label" label; string "destination" destination ] @ authored)
        tail)
    |> node "references"
  in
  element "doc" (frontmatter @ content @ [ footnotes; references ])
;;
