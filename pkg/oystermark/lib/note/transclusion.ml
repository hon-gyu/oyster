open Core

type embed_meta =
  { depth : int
  ; source_path : string
  ; fragment : Cmarkit.Inline.Wikilink.fragment option
  }

let embed_meta_key : embed_meta Cmarkit.Meta.key = Cmarkit.Meta.key ()
let embed_class = "embed"

(* Embed metadata as text
   ====================== *)

(** A fragment as it is written after the ['#'] of a wikilink, which is how the
    [fragment] attribute stores it. Parsing goes back through
    {!Cmarkit.Inline.Wikilink}, so the attribute and the wikilink cannot drift
    apart: a heading whose text contains a ['#'] is split by both alike. *)
let fragment_to_string : Cmarkit.Inline.Wikilink.fragment -> string = function
  | Heading path -> String.concat ~sep:"#" path
  | Block_ref id -> "^" ^ id
;;

let fragment_of_string (text : string) : Cmarkit.Inline.Wikilink.fragment option =
  Cmarkit.Inline.Wikilink.fragment
    (Cmarkit.Inline.Wikilink.make ~embed:false ("#" ^ text))
;;

let attribute_of_embed_meta ({ depth; source_path; fragment } : embed_meta)
  : Cmarkit.Attribute.t
  =
  Cmarkit.Attribute.of_bindings
    ([ `Key_value ("source", source_path) ]
     @ Option.value_map fragment ~default:[] ~f:(fun fragment ->
       [ `Key_value ("fragment", fragment_to_string fragment) ])
     @ [ `Key_value ("depth", Int.to_string depth) ])
;;

let embed_meta_of_attribute (attribute : Cmarkit.Attribute.t) : embed_meta option =
  let key_values = Cmarkit.Attribute.key_values attribute in
  let find name = List.Assoc.find key_values name ~equal:String.equal in
  Option.map (find "source") ~f:(fun source_path ->
    { depth = Option.value_map (find "depth") ~default:1 ~f:Int.of_string
    ; source_path
    ; fragment = Option.bind (find "fragment") ~f:fragment_of_string
    })
;;

(** The transclusion [block] is, from the meta a same-process expansion left on
    it or, failing that, from the attribute written on it, which is all a
    document read back from text has. *)
let embed_meta_of_block (block : Cmarkit.Block.t) : embed_meta option =
  let rec go (block : Cmarkit.Block.t) (attribute : Cmarkit.Attribute.t option) =
    match block with
    | Cmarkit.Block.Ext_attributes (a, _) ->
      let attributes = Cmarkit.Block.Attributes.attributes a in
      go
        (Cmarkit.Block.Attributes.block a)
        (Some
           (Option.value_map attribute ~default:attributes ~f:(fun attribute ->
              Cmarkit.Attribute.merge attribute attributes)))
    | Cmarkit.Block.Ext_div (d, meta) ->
      let is_embed =
        Option.value_map (Cmarkit.Block.Div.class' d) ~default:false ~f:(fun (c, _) ->
          String.equal c embed_class)
      in
      if not is_embed
      then None
      else (
        match Cmarkit.Meta.find embed_meta_key meta with
        | Some meta -> Some meta
        | None -> Option.bind attribute ~f:embed_meta_of_attribute)
    | _ -> None
  in
  go block None
;;

let non_fm_blocks (doc : Cmarkit.Doc.t) : Cmarkit.Block.t list =
  match Cmarkit.Doc.block doc with
  | block when Option.is_some (embed_meta_of_block block) -> [ block ]
  | Cmarkit.Block.Blocks (bs, _) ->
    (match bs with
     | Parse.Frontmatter.Frontmatter _ :: rest -> rest
     | _ -> bs)
  | other -> [ other ]
;;

type embed_source =
  | Wikilink_embed of Cmarkit.Inline.Wikilink.t * Cmarkit.Meta.t
  | Image_embed of Link.Ref.t

let embed_source_of_inline (inline : Cmarkit.Inline.t) : embed_source option =
  let check_one (i : Cmarkit.Inline.t) : embed_source option =
    match i with
    | Cmarkit.Inline.Ext_wikilink (w, meta) when Cmarkit.Inline.Wikilink.embed w ->
      Some (Wikilink_embed (w, meta))
    | Cmarkit.Inline.Image (link, _) ->
      Link.Ref.of_cmark_reference (Cmarkit.Inline.Link.reference link)
      |> Option.map ~f:(fun link_ref -> Image_embed link_ref)
    | _ -> None
  in
  match inline with
  | Cmarkit.Inline.Inlines ([ i ], _) -> check_one i
  | i -> check_one i
;;

let is_expandable_embed_paragraph
      (block : Cmarkit.Block.t)
      ~(siblings : Cmarkit.Block.t list)
  : embed_source option
  =
  let all_siblings_blank : bool =
    List.for_all siblings ~f:(fun b ->
      match b with
      | Cmarkit.Block.Blank_line _ -> true
      | b' -> phys_equal b' block)
  in
  match block with
  | Cmarkit.Block.Paragraph (p, _) when all_siblings_blank ->
    embed_source_of_inline (Cmarkit.Block.Paragraph.inline p)
  | _ -> None
;;

let fallback_block (wl : Cmarkit.Inline.Wikilink.t) (meta : Cmarkit.Meta.t)
  : Cmarkit.Block.t
  =
  let wl_link =
    Cmarkit.Inline.Wikilink.make ~embed:false (Cmarkit.Inline.Wikilink.content wl)
  in
  let link_inline = Cmarkit.Inline.Ext_wikilink (wl_link, meta) in
  let p =
    Cmarkit.Block.Paragraph.make
      (Cmarkit.Inline.Inlines ([ link_inline ], Cmarkit.Meta.none))
  in
  Cmarkit.Block.Paragraph (p, Cmarkit.Meta.none)
;;

let fragment : Anchor.definition -> Cmarkit.Inline.Wikilink.fragment option = function
  | Heading heading -> Some (Heading [ heading.text ])
  | Caret id -> Some (Block_ref id)
  | Attr _ -> None
;;

let transclude ~depth ~source_path ~fragment (blocks : Cmarkit.Block.t list)
  : Cmarkit.Block.t
  =
  let embed_meta = { depth; source_path; fragment } in
  let div =
    Cmarkit.Block.Div.make
      ~class':(embed_class, Cmarkit.Meta.none)
      (Cmarkit.Block.Blocks (blocks, Cmarkit.Meta.none))
  in
  (* The meta is what the same-process consumers read; the attribute is the
     same thing in text, for whoever only gets the rendered note back. *)
  let div =
    Cmarkit.Block.Ext_div
      (div, Cmarkit.Meta.add embed_meta_key embed_meta Cmarkit.Meta.none)
  in
  Cmarkit.Block.Ext_attributes
    ( Cmarkit.Block.Attributes.make ~specs:[ attribute_of_embed_meta embed_meta ] div
    , Cmarkit.Meta.none )
;;

let reverse_embed_doc (doc : Cmarkit.Doc.t) : Cmarkit.Doc.t =
  let strip_md (path : string) : string =
    match String.chop_suffix path ~suffix:".md" with
    | Some s -> s
    | None -> path
  in
  let mapper =
    Cmarkit.Mapper.make
      ~block_ext_default:(fun _m b -> Some b)
      ~inline_ext_default:(fun _m i -> Some i)
      ~block:(fun _mapper block ->
        match block with
        | Cmarkit.Block.Ext_attributes _ | Cmarkit.Block.Ext_div _ ->
          (match embed_meta_of_block block with
           | None -> Cmarkit.Mapper.default
           | Some { source_path; fragment; _ } ->
             let target =
               if String.is_empty source_path then None else Some (strip_md source_path)
             in
             let wl =
               Parse.Common.wikilink_of_fields ~target ~fragment ~display:None ~embed:true
             in
             let inline = Cmarkit.Inline.Ext_wikilink (wl, Cmarkit.Meta.none) in
             let p =
               Cmarkit.Block.Paragraph.make
                 (Cmarkit.Inline.Inlines ([ inline ], Cmarkit.Meta.none))
             in
             Cmarkit.Mapper.ret (Cmarkit.Block.Paragraph (p, Cmarkit.Meta.none)))
        | _ -> Cmarkit.Mapper.default)
      ()
  in
  Cmarkit.Mapper.map_doc mapper doc
;;

(* Test
   ==== *)

module For_test = struct
  let parse_blocks (md : string) : Cmarkit.Block.t list =
    non_fm_blocks (Parse.of_string md)
  ;;

  let doc_of_blocks (blocks : Cmarkit.Block.t list) : Cmarkit.Doc.t =
    Cmarkit.Doc.make (Cmarkit.Block.Blocks (blocks, Cmarkit.Meta.none))
  ;;

  let print_blocks (blocks : Cmarkit.Block.t list) : unit =
    let doc = doc_of_blocks blocks in
    print_endline (Parse.commonmark_of_doc doc)
  ;;
end

let%expect_test "is_expandable_embed_paragraph: sole embed paragraph" =
  let blocks = For_test.parse_blocks "![[target]]" in
  let block = List.hd_exn blocks in
  let result = is_expandable_embed_paragraph block ~siblings:blocks in
  printf "%b\n" (Option.is_some result);
  [%expect {| true |}]
;;

let%expect_test "is_expandable_embed_paragraph: embed mixed with text" =
  let blocks = For_test.parse_blocks "See ![[target]] here." in
  let block = List.hd_exn blocks in
  let result = is_expandable_embed_paragraph block ~siblings:blocks in
  printf "%b\n" (Option.is_some result);
  [%expect {| false |}]
;;

let%expect_test "is_expandable_embed_paragraph: embed with blank siblings only" =
  let blocks = For_test.parse_blocks "\n![[target]]\n" in
  let block =
    List.find_exn blocks ~f:(fun b ->
      match b with
      | Cmarkit.Block.Paragraph _ -> true
      | _ -> false)
  in
  let result = is_expandable_embed_paragraph block ~siblings:blocks in
  printf "%b\n" (Option.is_some result);
  [%expect {| true |}]
;;

let%expect_test "is_expandable_embed_paragraph: non-embed wikilink" =
  let blocks = For_test.parse_blocks "[[target]]" in
  let block = List.hd_exn blocks in
  let result = is_expandable_embed_paragraph block ~siblings:blocks in
  printf "%b\n" (Option.is_some result);
  [%expect {| false |}]
;;

let%expect_test "is_expandable_embed_paragraph: embed among other blocks" =
  let blocks = For_test.parse_blocks "Some text.\n\n![[target]]\n\nMore text." in
  let embed_block = List.nth_exn blocks 1 in
  let result = is_expandable_embed_paragraph embed_block ~siblings:blocks in
  printf "%b\n" (Option.is_some result);
  [%expect {| false |}]
;;
