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

let span (element : X.element) : (int * int) option =
  let value key =
    List.find_map element.attributes ~f:(fun (a : X.Attribute.t) ->
      if String.equal a.key key then Int.of_string_opt a.value else None)
  in
  match value "start-byte", value "end-byte" with
  | Some first, Some last when 0 <= first && first <= last -> Some (first, last)
  | _ -> None
;;

(* The name of a div or span; empty means unnamed. *)
let name_attr (name : string) : X.Attribute.t list =
  if String.is_empty name then [] else [ string "name" name ]
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
    Djot.Attr.kvs attrs
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
    | Span (name, ns) -> "span", name_attr name, inlines doc ns
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
    | Hole expression -> "hole", [], [ X.Text expression ]
  in
  let attributes, authored = with_node doc n extra in
  node tag ~attributes (authored @ children)

and target_attrs : Djot.Inline.target -> X.Attribute.t list = function
  | Direct target -> [ string "target" target; string "target-kind" "direct" ]
  | Reference target -> [ string "target" target; string "target-kind" "reference" ]
;;

(* Definitions are listed once, under [footnotes] and [references], so the tree
   skips them wherever they were written. *)
let is_definition (Djot.Node (_, _, value) : Djot.Block.t Djot.node) =
  match value with
  | FootnoteDef _ | RefDef _ -> true
  | _ -> false
;;

let rec blocks doc ns =
  List.filter_map ns ~f:(fun n -> if is_definition n then None else Some (block doc n))

and block doc (Djot.Node (_, _, value) as n : Djot.Block.t Djot.node) : X.t =
  let tag, extra, children =
    match value with
    | Para ns -> "paragraph", [], inlines doc ns
    | Section ns -> "section", [], blocks doc ns
    | Heading (level, ns) -> "heading", [ number "level" level ], inlines doc ns
    | BlockQuote ns -> "block_quote", [], blocks doc ns
    | CodeBlock (lang, text) -> "code_block", [ string "lang" lang ], [ X.Text text ]
    | Div (name, ns) -> "div", name_attr name, blocks doc ns
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
      , list_items doc items )
    | BulletList (_, spacing, items) ->
      ( "list"
      , [ string "kind" "bullet"; string "spacing" (spacing_string spacing) ]
      , list_items doc items )
    | TaskList (spacing, items) ->
      ( "list"
      , [ string "kind" "task"; string "spacing" (spacing_string spacing) ]
      , List.map items ~f:(fun (Djot.Node (_, _, (status, bs)) as item) ->
          node
            "item"
            ~attributes:
              (string
                 "task"
                 (match status with
                  | Djot.Block.Complete -> "checked"
                  | Incomplete -> "unchecked")
               :: location doc item)
            (blocks doc bs)) )
    | DefinitionList (spacing, items) ->
      ( "definition_list"
      , [ string "spacing" (spacing_string spacing) ]
      , List.map items ~f:(fun (Djot.Node (_, _, (term, definition)) as item) ->
          node
            "item"
            ~attributes:(location doc item)
            [ node
                "term"
                ~attributes:(location doc term)
                (inlines doc (Djot.Node.content term))
            ; node
                "definition"
                ~attributes:(location doc definition)
                (blocks doc (Djot.Node.content definition))
            ]) )
    | ThematicBreak -> "thematic_break", [], []
    | Table (caption, rows) ->
      ( "table"
      , []
      , (match Djot.Node.content caption with
         | [] -> []
         | ns -> [ node "caption" ~attributes:(location doc caption) (inlines doc ns) ])
        @ List.map rows ~f:(fun row ->
          node
            "row"
            ~attributes:(location doc row)
            (List.map (Djot.Node.content row) ~f:(cell doc))) )
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

and list_items doc (items : Djot.Block.t Djot.node list Djot.node list) : X.t list =
  List.map items ~f:(fun item ->
    node "item" ~attributes:(location doc item) (blocks doc (Djot.Node.content item)))

and cell doc (Djot.Node (_, _, Djot.Block.Cell (kind, align, ns)) as c) : X.t =
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
       @ location doc c)
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

let of_doc (doc : Djot.Doc.t) : X.element =
  let frontmatter =
    Option.to_list
      (Option.map (Parse.frontmatter doc) ~f:(fun value ->
         let attributes, children = yaml value in
         node "frontmatter" ~attributes children))
  in
  let content = blocks doc (Djot.Doc.blocks doc) in
  let footnotes =
    Parse.Common.footnote_defs doc |> List.map ~f:(block doc) |> node "footnotes"
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
