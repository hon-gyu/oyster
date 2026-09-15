open Core
module B = Cmarkit.Block

type task =
  [ `Unchecked
  | `Checked
  | `Cancelled
  | `Other of Uchar.t
  ]

type t =
  | Heading of
      { level : int
      ; id : string
      ; text : string
      }
  | Paragraph
  | Code_block of
      { info : string option
      ; text : string
      }
  | Math_block of { text : string }
  | Raw_block of
      { format : string
      ; text : string
      }
  | Callout of
      { type_ : string
      ; title : string option
      }
  | Block_quote
  | List of
      { ordered : bool
      ; tight : bool
      ; length : int
      }
  | List_item of
      { task : task option
      ; key : string option
      }
  | Keyed of { key : string }
  | Div of { class_ : string option }
  | Footnote_definition of { label : string }
  | Table
  | Definition_list
  | Thematic_break
  | Html_block of { text : string }

let kind : t -> string = function
  | Heading _ -> "heading"
  | Paragraph -> "paragraph"
  | Code_block _ -> "code_block"
  | Math_block _ -> "math_block"
  | Html_block _ -> "html_block"
  | Raw_block _ -> "raw_block"
  | Callout _ -> "callout"
  | Block_quote -> "block_quote"
  | List _ -> "list"
  | List_item _ -> "list_item"
  | Keyed _ -> "keyed"
  | Div _ -> "div"
  | Footnote_definition _ -> "footnote_definition"
  | Table -> "table"
  | Definition_list -> "definition_list"
  | Thematic_break -> "thematic_break"
;;

let kinds =
  [ "heading"
  ; "paragraph"
  ; "code_block"
  ; "math_block"
  ; "html_block"
  ; "raw_block"
  ; "callout"
  ; "block_quote"
  ; "list"
  ; "list_item"
  ; "keyed"
  ; "div"
  ; "footnote_definition"
  ; "table"
  ; "definition_list"
  ; "thematic_break"
  ]
;;

let lines_text lines =
  List.map lines ~f:Cmarkit.Block_line.to_string |> String.concat ~sep:"\n"
;;

let key_of_label (label : Cmarkit.Inline.t) : string =
  Parse.Common.inline_to_plain_text (Cmarkit.Struct.label_key label) |> String.strip
;;

let of_block (block : B.t) : t option =
  match block with
  | B.Heading (h, _) ->
    Some
      (Heading
         { level = B.Heading.level h
         ; id = Option.value (Parse.Common.heading_id h) ~default:""
         ; text = Parse.Common.inline_to_plain_text (B.Heading.inline h)
         })
  | B.Paragraph _ -> Some Paragraph
  | B.Code_block (cb, _) ->
    Some
      (Code_block
         { info = Parse.Common.info_string_of_block block
         ; text = lines_text (B.Code_block.code cb)
         })
  | B.Ext_math_block (cb, _) ->
    Some (Math_block { text = lines_text (B.Code_block.code cb) })
  | B.Html_block (lines, _) -> Some (Html_block { text = lines_text lines })
  | B.Ext_raw_block (rb, _) ->
    Some
      (Raw_block
         { format = B.Raw_block.format rb
         ; text = lines_text (B.Code_block.code (B.Raw_block.code_block rb))
         })
  | B.Block_quote (bq, meta) ->
    (match B.Callout.find meta with
     | Some callout ->
       Some
         (Callout
            { type_ = B.Callout.kind callout
            ; title =
                B.Callout.title callout (B.Block_quote.block bq)
                |> Option.map ~f:Parse.Common.inline_to_plain_text
            })
     | None -> Some Block_quote)
  | B.List (l, _) ->
    Some
      (List
         { ordered =
             (match B.List'.type' l with
              | `Unordered _ -> false
              | _ -> true)
         ; tight = B.List'.tight l
         ; length = List.length (B.List'.items l)
         })
  | B.Ext_keyed ((label, _body), _) -> Some (Keyed { key = key_of_label label })
  | B.Ext_div (d, _) -> Some (Div { class_ = Option.map (B.Div.class' d) ~f:fst })
  | B.Ext_footnote_definition (fn, _) ->
    Some (Footnote_definition { label = Cmarkit.Label.key (B.Footnote.label fn) })
  | B.Ext_table _ -> Some Table
  | B.Ext_definition_list _ -> Some Definition_list
  | B.Thematic_break _ -> Some Thematic_break
  | B.Ext_jsx_block _ -> invalid_arg "Node.of_block: JSX blocks are not supported"
  | _ -> None
;;

let of_item ((item, _meta) : B.List_item.t Cmarkit.node) : t =
  let rec blocks (block : B.t) : B.t list =
    match block with
    | B.Blocks (bs, _) -> List.concat_map bs ~f:blocks
    | B.Ext_attributes (a, _) -> blocks (B.Attributes.block a)
    | B.Blank_line _ | B.Link_reference_definition _ -> []
    | block -> [ block ]
  in
  List_item
    { task =
        Option.map (B.List_item.ext_task_marker item) ~f:(fun (marker, _) ->
          B.List_item.task_status_of_task_marker marker)
    ; key =
        (match blocks (B.List_item.block item) with
         | [ B.Ext_keyed ((label, _), _) ] -> Some (key_of_label label)
         | _ -> None)
    }
;;

(* Properties
   ========== *)

type value =
  | Int of int
  | String of string
  | Bool of bool

let value_to_string = function
  | Int n -> Int.to_string n
  | String s -> sprintf "%S" s
  | Bool b -> Bool.to_string b
;;

let task_to_string : task -> string = function
  | `Unchecked -> "unchecked"
  | `Checked -> "checked"
  | `Cancelled -> "cancelled"
  | `Other u -> sprintf "U+%04X" (Stdlib.Uchar.to_int u)
;;

let fields (node : t) : (string * value) list =
  let optional name value = Option.map value ~f:(fun value -> name, value) in
  match node with
  | Heading { level; id; text } ->
    [ "level", Int level; "id", String id; "text", String text ]
  | Code_block { info; text } ->
    List.filter_opt
      [ optional "info" (Option.map info ~f:(fun info -> String info))
      ; Some ("text", String text)
      ]
  | Math_block { text } | Html_block { text } -> [ "text", String text ]
  | Raw_block { format; text } -> [ "format", String format; "text", String text ]
  | Callout { type_; title } ->
    List.filter_opt
      [ Some ("type", String type_)
      ; optional "title" (Option.map title ~f:(fun title -> String title))
      ]
  | List { ordered; tight; length } ->
    [ "ordered", Bool ordered; "tight", Bool tight; "length", Int length ]
  | List_item { task; key } ->
    List.filter_opt
      [ optional "task" (Option.map task ~f:(fun task -> String (task_to_string task)))
      ; optional "key" (Option.map key ~f:(fun key -> String key))
      ]
  | Keyed { key } -> [ "key", String key ]
  | Div { class_ } ->
    Option.to_list (optional "class" (Option.map class_ ~f:(fun c -> String c)))
  | Footnote_definition { label } -> [ "label", String label ]
  | Paragraph | Block_quote | Table | Definition_list | Thematic_break -> []
;;

let props (node : t) : (string * value) list = ("kind", String (kind node)) :: fields node

let prop (name : string) (node : t) : value option =
  List.Assoc.find (props node) name ~equal:String.equal
;;
