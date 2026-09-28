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

let location (doc : Djot.Doc.t) (n : _ Djot.node) : X.Attribute.t list =
  let loc = Djot.Doc.textloc doc n in
  if Djot.Textloc.is_none loc
  then []
  else
    [ number "start-byte" (Djot.Textloc.first_byte loc)
    ; number "end-byte" (Djot.Textloc.last_byte loc)
    ; number "start-line" (fst (Djot.Textloc.first_line loc))
    ; number "end-line" (fst (Djot.Textloc.last_line loc))
    ]
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

let rec blocks doc ns = List.map ns ~f:(block doc)

and block doc (Djot.Node (_, _, value) as n : Djot.Block.t Djot.node) : X.t =
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
      , List.map items ~f:(fun bs -> node "item" (blocks doc bs)) )
    | BulletList (spacing, items) ->
      ( "list"
      , [ string "kind" "bullet"; string "spacing" (spacing_string spacing) ]
      , List.map items ~f:(fun bs -> node "item" (blocks doc bs)) )
    | TaskList (spacing, items) ->
      ( "list"
      , [ string "kind" "task"; string "spacing" (spacing_string spacing) ]
      , List.map items ~f:(fun (status, bs) ->
          node
            "item"
            ~attributes:
              [ string
                  "task"
                  (match status with
                   | Complete -> "checked"
                   | Incomplete -> "unchecked")
              ]
            (blocks doc bs)) )
    | DefinitionList (spacing, items) ->
      ( "definition_list"
      , [ string "spacing" (spacing_string spacing) ]
      , List.map items ~f:(fun (term, definition) ->
          node
            "item"
            [ node "term" (inlines doc term); node "definition" (blocks doc definition) ])
      )
    | ThematicBreak -> "thematic_break", [], []
    | Table (caption, rows) ->
      ( "table"
      , []
      , Option.to_list (Option.map caption ~f:(fun ns -> node "caption" (inlines doc ns)))
        @ List.map rows ~f:(fun row -> node "row" (List.map row ~f:(cell doc))) )
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
      , [ node "label" (inlines doc label); block doc value ] )
    | Ext_callout (kind, fold, title, body) ->
      ( "callout"
      , [ string "type" kind ]
        @ Option.to_list
            (Option.map fold ~f:(function
               | FoldExpanded -> string "fold" "expanded"
               | FoldCollapsed -> string "fold" "collapsed"))
      , node "title" (inlines doc title) :: blocks doc body )
  in
  let attributes, authored = with_node doc n extra in
  node tag ~attributes (authored @ children)

and cell doc (Djot.Block.Cell (kind, align, ns)) : X.t =
  node
    "cell"
    ~attributes:
      [ string
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
    (inlines doc ns)

and spacing_string : Djot.Block.list_spacing -> string = function
  | Tight -> "tight"
  | Loose -> "loose"
;;

let of_doc (doc : Djot.Doc.t) : X.element =
  let content = blocks doc (Djot.Doc.blocks doc) in
  let footnotes = Djot.Doc.footnote_defs doc |> blocks doc |> node "footnotes" in
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
  element "doc" (content @ [ footnotes; references ])
;;
