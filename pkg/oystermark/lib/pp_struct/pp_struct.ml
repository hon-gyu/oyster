open Core
open Cmarkit
open Parse.Struct
open For_test

let example =
  {|
Architecture:
- : encoder–decoder. each half is a stack of six identical layers. self-attention and position-wise feed-forward replace recurrent and convolutional layers
- encoder:
  - self-attention: multi-head, every position attends to every other input position
  - feed-forward: position-wise MLP, applied independently at each step
  - residual + norm: a residual connection wraps each sub-layer, followed by layer normalization
- decoder:
  - masked self-attention sub-layer:
    - like the encoder's, but autoregressive
    - position `i` can only attend to positions `≤ i`
  - cross-attention: multi-head attention over the encoder output.
  - feed-forward sub-layer: same as on the encoder side
  - residual + norm:
    - as in the encoder

Attention:
- scaled dot-product:
  - definition: `softmax(QKᵀ / √dₖ) V`
  - `√dₖ`:
    - dot products grow like `√dₖ` as the key dimension grows. dividing keeps the softmax out of its saturation regime, so gradients don't vanish
- multi-head:
  - procedure:
    - run `h = 8` attention functions in parallel over learned linear projections of `Q`, `K`, `V`.
    - concatenate the heads and project.
  - why multiple heads:
    - each head can attend to a different representation subspace. a single softmax over averaged features cannot recover this.
|}
;;

let%expect_test _ =
  let doc = example |> doc_of_string in
  doc |> pp_doc_debug;
  [%expect
    {|
    K(Architecture, List[K(,
    encoder–decoder. each half is a stack of six identical layers. self-attention and position-wise feed-forward replace recurrent and convolutional layers), K(encoder, List[K(self-attention,
    multi-head, every position attends to every other input position), K(feed-forward,
    position-wise MLP, applied independently at each step), K(residual + norm,
    a residual connection wraps each sub-layer, followed by layer normalization)]), K(decoder, List[K(masked self-attention sub-layer, List[
    like the encoder's, but autoregressive,
    position `i` can only attend to positions `≤ i`]), K(cross-attention,
    multi-head attention over the encoder output.), K(feed-forward sub-layer,
    same as on the encoder side), K(residual + norm, List[
    as in the encoder])])])
    K(Attention, List[K(scaled dot-product, List[K(definition,
    `softmax(QKᵀ / √dₖ) V`), K(`√dₖ`, List[
    dot products grow like `√dₖ` as the key dimension grows. dividing keeps the softmax out of its saturation regime, so gradients don't vanish])]), K(multi-head, List[K(procedure, List[
    run `h = 8` attention functions in parallel over learned linear projections of `Q`, `K`, `V`.,
    concatenate the heads and project.]), K(why multiple heads, List[
    each head can attend to a different representation subspace. a single softmax over averaged features cannot recover this.])])])
    |}];
  doc |> pp_doc_sexp;
  [%expect
    {|
    (Blocks Blank_line
      (Keyed_block (Text Architecture)
        (List
          (Keyed_list_item (Text "")
            (Paragraph
              (Text
                "encoder\226\128\147decoder. each half is a stack of six identical layers. self-attention and position-wise feed-forward replace recurrent and convolutional layers")))
          (Keyed_list_item (Text encoder)
            (List
              (Keyed_list_item (Text self-attention)
                (Paragraph
                  (Text
                    "multi-head, every position attends to every other input position")))
              (Keyed_list_item (Text feed-forward)
                (Paragraph
                  (Text "position-wise MLP, applied independently at each step")))
              (Keyed_list_item (Text "residual + norm")
                (Paragraph
                  (Text
                    "a residual connection wraps each sub-layer, followed by layer normalization")))))
          (Keyed_list_item (Text decoder)
            (List
              (Keyed_list_item (Text "masked self-attention sub-layer")
                (List (Paragraph (Text "like the encoder's, but autoregressive"))
                  (Paragraph
                    (Inlines (Text "position ") (Code_span i)
                      (Text " can only attend to positions ")
                      (Code_span "\226\137\164 i")))))
              (Keyed_list_item (Text cross-attention)
                (Paragraph
                  (Text "multi-head attention over the encoder output.")))
              (Keyed_list_item (Text "feed-forward sub-layer")
                (Paragraph (Text "same as on the encoder side")))
              (Keyed_list_item (Text "residual + norm")
                (List (Paragraph (Text "as in the encoder"))))))))
      Blank_line
      (Keyed_block (Text Attention)
        (List
          (Keyed_list_item (Text "scaled dot-product")
            (List
              (Keyed_list_item (Text definition)
                (Paragraph
                  (Code_span
                    "softmax(QK\225\181\128 / \226\136\154d\226\130\150) V")))
              (Keyed_list_item (Code_span "\226\136\154d\226\130\150")
                (List
                  (Paragraph
                    (Inlines (Text "dot products grow like ")
                      (Code_span "\226\136\154d\226\130\150")
                      (Text
                        " as the key dimension grows. dividing keeps the softmax out of its saturation regime, so gradients don't vanish")))))))
          (Keyed_list_item (Text multi-head)
            (List
              (Keyed_list_item (Text procedure)
                (List
                  (Paragraph
                    (Inlines (Text "run ") (Code_span "h = 8")
                      (Text
                        " attention functions in parallel over learned linear projections of ")
                      (Code_span Q) (Text ", ") (Code_span K) (Text ", ")
                      (Code_span V) (Text .)))
                  (Paragraph (Text "concatenate the heads and project."))))
              (Keyed_list_item (Text "why multiple heads")
                (List
                  (Paragraph
                    (Text
                      "each head can attend to a different representation subspace. a single softmax over averaged features cannot recover this."))))))))
      Blank_line)
    |}]
;;

(* Graph pretty-printing
   ===================== *)

(** Approximate display width of a UTF-8 string.
    Counts codepoints, each treated as 1 column. *)
let display_width (s : string) : int =
  let len = String.length s in
  let rec go i w =
    if i >= len
    then w
    else (
      let b = Char.to_int s.[i] in
      let skip =
        if b land 0x80 = 0
        then 1
        else if b land 0xE0 = 0xC0
        then 2
        else if b land 0xF0 = 0xE0
        then 3
        else 4
      in
      go (i + skip) (w + 1))
  in
  go 0 0
;;

let max_text_w = 56

let word_wrap max_w s =
  if display_width s <= max_w
  then [ s ]
  else (
    let words = String.split s ~on:' ' in
    let rec go cur cw acc = function
      | [] -> List.rev (if String.is_empty cur then acc else cur :: acc)
      | word :: rest ->
        let ww = display_width word in
        if cw = 0
        then go word ww acc rest
        else if cw + 1 + ww <= max_w
        then go (cur ^ " " ^ word) (cw + 1 + ww) acc rest
        else go word ww (cur :: acc) rest
    in
    go "" 0 [] words)
;;

(* Inline to text
   -------------- *)

let rec inline_to_text (i : Inline.t) : string =
  match i with
  | Inline.Text (s, _) -> s
  | Inline.Code_span (cs, _) -> "`" ^ Inline.Code_span.code cs ^ "`"
  | Inline.Emphasis (em, _) -> inline_to_text (Inline.Emphasis.inline em)
  | Inline.Strong_emphasis (em, _) -> inline_to_text (Inline.Emphasis.inline em)
  | Inline.Inlines (is, _) -> String.concat (List.map is ~f:inline_to_text)
  | Inline.Break _ -> " "
  | _ -> ""
;;

(* Visual tree
   ----------- *)

type visual =
  | Box of string * visual list
  | Arrow of string * string
  | Txt of string

let rec block_to_visuals (b : Block.t) : visual list =
  match b with
  | Ext_keyed_block ({ label }, body) | Ext_keyed_list_item ({ label }, body) ->
    keyed_to_visual (inline_to_text label) body
  | Block.List (l, _) ->
    List.concat_map (Block.List'.items l) ~f:(fun (item, _) ->
      block_to_visuals (Block.List_item.block item))
  | Block.Blocks (bs, _) -> List.concat_map bs ~f:block_to_visuals
  | Block.Paragraph (p, _) -> [ Txt (inline_to_text (Block.Paragraph.inline p)) ]
  | Block.Code_block (cb, _) ->
    Block.Code_block.code cb |> List.map ~f:(fun (line, _) -> Txt line)
  | Block.Block_quote (bq, _) -> block_to_visuals (Block.Block_quote.block bq)
  | Block.Blank_line _ -> []
  | _ -> []

and keyed_to_visual label body =
  match body with
  | Block.List _ -> [ Box (label, block_to_visuals body) ]
  | Block.Paragraph (p, _) ->
    let v = inline_to_text (Block.Paragraph.inline p) in
    [ Arrow (label, v) ]
  | _ ->
    let children = block_to_visuals body in
    (match children with
     | [ Arrow (k, v) ] when not (String.is_empty label) ->
       [ Arrow (label ^ " ▸ " ^ k, v) ]
     | [ Txt v ] when not (String.is_empty label) -> [ Arrow (label, v) ]
     | _ when String.is_empty label -> children
     | _ -> [ Box (label, children) ])
;;

(* Render
   ------ *)

let repeat s n =
  let buf = Buffer.create (String.length s * n) in
  for _ = 1 to n do
    Buffer.add_string buf s
  done;
  Buffer.contents buf
;;

let pad s w =
  let cur = display_width s in
  if cur >= w then s else s ^ String.make (w - cur) ' '
;;

let rec render (v : visual) : string list =
  match v with
  | Txt s -> word_wrap max_text_w s
  | Arrow (label, value) ->
    let prefix = if String.is_empty label then "──> " else "──" ^ label ^ "──> " in
    let prefix_w = display_width prefix in
    if prefix_w + display_width value <= max_text_w
    then [ prefix ^ value ]
    else (
      let head = if String.is_empty label then "─>" else "──" ^ label ^ "──>" in
      let value_lines = word_wrap (max_text_w - 4) value in
      head :: List.map value_lines ~f:(fun l -> "    " ^ l))
  | Box (title, children) ->
    let has_box =
      List.exists children ~f:(function
        | Box _ -> true
        | _ -> false)
    in
    let child_lines =
      List.concat_mapi children ~f:(fun i c ->
        let lines = render c in
        if i > 0 && has_box then "" :: lines else lines)
    in
    let max_line_w =
      List.fold child_lines ~init:0 ~f:(fun acc l -> max acc (display_width l))
    in
    let title_w = display_width title in
    let inner_w = max max_line_w title_w in
    let total_w = inner_w + 6 in
    let top =
      if String.is_empty title
      then "╭" ^ repeat "─" (total_w - 2) ^ "╮"
      else (
        let dashes = max 1 (total_w - title_w - 5) in
        "╭─ " ^ title ^ " " ^ repeat "─" dashes ^ "╮")
    in
    let bot = "╰" ^ repeat "─" (total_w - 2) ^ "╯" in
    let content =
      List.map child_lines ~f:(fun l ->
        if String.is_empty l
        then "│" ^ String.make (total_w - 2) ' ' ^ "│"
        else "│  " ^ pad l inner_w ^ "  │")
    in
    [ top ] @ content @ [ bot ]
;;

let pp_doc_graph (doc : Doc.t) : unit =
  let visuals = block_to_visuals (Doc.block doc) in
  let all_lines =
    List.concat_mapi visuals ~f:(fun i v ->
      let lines = render v in
      if i > 0 then "" :: lines else lines)
  in
  List.iter all_lines ~f:print_endline
;;

let%expect_test "graph" =
  let doc = example |> doc_of_string in
  doc |> pp_doc_graph;
  [%expect
    {|
    ╭─ Architecture ───────────────────────────────────────────────────╮
    │  ─>                                                              │
    │      encoder–decoder. each half is a stack of six                │
    │      identical layers. self-attention and position-wise          │
    │      feed-forward replace recurrent and convolutional            │
    │      layers                                                      │
    │                                                                  │
    │  ╭─ encoder ──────────────────────────────────────────────────╮  │
    │  │  ──self-attention──>                                       │  │
    │  │      multi-head, every position attends to every other     │  │
    │  │      input position                                        │  │
    │  │  ──feed-forward──>                                         │  │
    │  │      position-wise MLP, applied independently at each      │  │
    │  │      step                                                  │  │
    │  │  ──residual + norm──>                                      │  │
    │  │      a residual connection wraps each sub-layer, followed  │  │
    │  │      by layer normalization                                │  │
    │  ╰────────────────────────────────────────────────────────────╯  │
    │                                                                  │
    │  ╭─ decoder ─────────────────────────────────────────────────╮   │
    │  │  ╭─ masked self-attention sub-layer ─────────────────╮    │   │
    │  │  │  like the encoder's, but autoregressive           │    │   │
    │  │  │  position `i` can only attend to positions `≤ i`  │    │   │
    │  │  ╰───────────────────────────────────────────────────╯    │   │
    │  │                                                           │   │
    │  │  ──cross-attention──>                                     │   │
    │  │      multi-head attention over the encoder output.        │   │
    │  │                                                           │   │
    │  │  ──feed-forward sub-layer──> same as on the encoder side  │   │
    │  │                                                           │   │
    │  │  ╭─ residual + norm ───╮                                  │   │
    │  │  │  as in the encoder  │                                  │   │
    │  │  ╰─────────────────────╯                                  │   │
    │  ╰───────────────────────────────────────────────────────────╯   │
    ╰──────────────────────────────────────────────────────────────────╯

    ╭─ Attention ────────────────────────────────────────────────────────────╮
    │  ╭─ scaled dot-product ─────────────────────────────────────────────╮  │
    │  │  ──definition──> `softmax(QKᵀ / √dₖ) V`                          │  │
    │  │                                                                  │  │
    │  │  ╭─ `√dₖ` ────────────────────────────────────────────────────╮  │  │
    │  │  │  dot products grow like `√dₖ` as the key dimension grows.  │  │  │
    │  │  │  dividing keeps the softmax out of its saturation regime,  │  │  │
    │  │  │  so gradients don't vanish                                 │  │  │
    │  │  ╰────────────────────────────────────────────────────────────╯  │  │
    │  ╰──────────────────────────────────────────────────────────────────╯  │
    │                                                                        │
    │  ╭─ multi-head ─────────────────────────────────────────────────────╮  │
    │  │  ╭─ procedure ────────────────────────────────────────────────╮  │  │
    │  │  │  run `h = 8` attention functions in parallel over learned  │  │  │
    │  │  │  linear projections of `Q`, `K`, `V`.                      │  │  │
    │  │  │  concatenate the heads and project.                        │  │  │
    │  │  ╰────────────────────────────────────────────────────────────╯  │  │
    │  │                                                                  │  │
    │  │  ╭─ why multiple heads ───────────────────────────────────────╮  │  │
    │  │  │  each head can attend to a different representation        │  │  │
    │  │  │  subspace. a single softmax over averaged features cannot  │  │  │
    │  │  │  recover this.                                             │  │  │
    │  │  ╰────────────────────────────────────────────────────────────╯  │  │
    │  ╰──────────────────────────────────────────────────────────────────╯  │
    ╰────────────────────────────────────────────────────────────────────────╯
    |}]
;;
