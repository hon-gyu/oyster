(** AST for a note. It builds on top of Cmarkit's AST and differs
    from it by attaching more semantics and excluding some non-content
    structures (breaks)
*)
open Core

module B = Cmarkit.Block

type task =
  [ `Unchecked
  | `Checked
  | `Cancelled
  | `Other of Uchar.t
  ]

type t =
  | Root
  | Heading of
      { level : int
      ; id : string
      ; text : string
      }
  | Section of
      { heading : t
      ; children : t list
      }
  | Paragraph
  | Keyed_paragraph of { key : string }
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
  | Div of { class_ : string option }
  | Footnote_definition of { label : string }
  | Table
  | Definition_list
  | Thematic_break
  | Html_block of { text : string }

type span =
  { first_line : int
  ; last_line : int
  ; first_byte : int
  ; last_byte : int
  }

let kind : t -> string = function
  | Root -> "root"
  | Heading _ -> "heading"
  | Section _ -> "section"
  | Paragraph -> "paragraph"
  | Keyed_paragraph _ -> "keyed_paragraph"
  | Code_block _ -> "code_block"
  | Math_block _ -> "math_block"
  | Html_block _ -> "html_block"
  | Raw_block _ -> "raw_block"
  | Callout _ -> "callout"
  | Block_quote -> "block_quote"
  | List _ -> "list"
  | List_item _ -> "list_item"
  | Div _ -> "div"
  | Footnote_definition _ -> "footnote_definition"
  | Table -> "table"
  | Definition_list -> "definition_list"
  | Thematic_break -> "thematic_break"
;;

let kinds =
  [ "root"
  ; "heading"
  ; "section"
  ; "paragraph"
  ; "keyed_paragraph"
  ; "code_block"
  ; "math_block"
  ; "html_block"
  ; "raw_block"
  ; "callout"
  ; "block_quote"
  ; "list"
  ; "list_item"
  ; "div"
  ; "footnote_definition"
  ; "table"
  ; "definition_list"
  ; "thematic_break"
  ]
;;

(** Whether the node can hold others, so that a child step can move into it. A
    heading cannot: its section holds the blocks under it. *)
let is_container : t -> bool = function
  | Root
  | Section _
  | Keyed_paragraph _
  | Callout _
  | Block_quote
  | List _
  | List_item _
  | Div _
  | Footnote_definition _ -> true
  | Heading _
  | Paragraph
  | Code_block _
  | Math_block _
  | Html_block _
  | Raw_block _
  | Table
  | Definition_list
  | Thematic_break -> false
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
  | B.Ext_keyed ((label, _body), _) -> Some (Keyed_paragraph { key = key_of_label label })
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

type value =
  | Int of int
  | String of string
  | Bool of bool

let value_to_string = function
  | Int n -> Int.to_string n
  | String s -> sprintf "%S" s
  | Bool b -> Bool.to_string b
;;

type found_t =
  { node : t
  ; path : int list
  ; names : Anchor.Address.t list
  ; headings : Anchor.heading list
  ; span : span option
  ; markdown : string
  ; props : (string * value) list
  }

let source_text (content : string) (found : found_t) : string option =
  Option.map found.span ~f:(fun { first_byte; last_byte; _ } ->
    let stop = Int.min (last_byte + 1) (String.length content) in
    String.sub content ~pos:first_byte ~len:(stop - first_byte) |> String.rstrip)
;;

(* Properties
   ========== *)

let task_to_string : task -> string = function
  | `Unchecked -> "unchecked"
  | `Checked -> "checked"
  | `Cancelled -> "cancelled"
  | `Other u -> sprintf "U+%04X" (Stdlib.Uchar.to_int u)
;;

let rec fields (node : t) : (string * value) list =
  let optional name value = Option.map value ~f:(fun value -> name, value) in
  match node with
  | Heading { level; id = _; text } -> [ "level", Int level; "text", String text ]
  | Section { heading; children = _ } -> fields heading
  | Code_block { info; text } ->
    List.filter_opt
      [ optional "lang" (Option.map info ~f:(fun info -> String info))
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
  | Keyed_paragraph { key } -> [ "key", String key ]
  | Div { class_ } ->
    Option.to_list (optional "class" (Option.map class_ ~f:(fun c -> String c)))
  | Footnote_definition { label } -> [ "label", String label ]
  | Root | Paragraph | Block_quote | Table | Definition_list | Thematic_break -> []
;;

let props (node : t) : (string * value) list =
  ("kind", String (kind node))
  :: ("is_container", Bool (is_container node))
  :: fields node
;;

let prop (name : string) (node : t) : value option =
  List.Assoc.find (props node) name ~equal:String.equal
;;
