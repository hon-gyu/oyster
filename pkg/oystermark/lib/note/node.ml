open Core

let kinds =
  [ "root"
  ; "section"
  ; "heading"
  ; "paragraph"
  ; "keyed"
  ; "code_block"
  ; "raw_block"
  ; "callout"
  ; "block_quote"
  ; "div"
  ; "list"
  ; "list_item"
  ; "footnote_definition"
  ; "table"
  ; "definition_list"
  ; "thematic_break"
  ]
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

type span =
  { first_line : int
  ; last_line : int
  ; first_byte : int
  ; last_byte : int
  }

type found_t =
  { kind : string
  ; path : int list
  ; names : Anchor.Address.t list
  ; headings : Anchor.heading list
  ; span : span option
  ; props : (string * value) list
  ; blocks : Djot.Block.t Djot.node list
  }

let prop (name : string) (found : found_t) : value option =
  List.Assoc.find found.props name ~equal:String.equal
;;

let source_text (content : string) (found : found_t) : string option =
  Option.map found.span ~f:(fun { first_byte; last_byte; _ } ->
    let stop = Int.min (last_byte + 1) (String.length content) in
    String.sub content ~pos:first_byte ~len:(stop - first_byte) |> String.rstrip)
;;
