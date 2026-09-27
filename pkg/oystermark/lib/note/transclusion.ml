open Core

type block = Djot.Block.t Djot.node

type embed_meta =
  { depth : int
  ; source_path : string
  ; fragment : Link.Ref.fragment option
  }

let embed_class = "embed"

(* Embed metadata as attributes
   ============================ *)

(** The [fragment] attribute holds a fragment as it is written after the first
    ['#'] of a wikilink, and is read back the same way, so the attribute and the
    wikilink cannot drift apart. *)
let attrs_of_embed_meta ({ depth; source_path; fragment } : embed_meta) : Djot.Attr.t =
  [ "class", embed_class; "source", source_path ]
  @ Option.value_map fragment ~default:[] ~f:(fun fragment ->
    [ "fragment", String.concat ~sep:"#" fragment ])
  @ [ "depth", Int.to_string depth ]
;;

let embed_meta_of_block (Djot.Node (_, attrs, block) : block) : embed_meta option =
  match block with
  | Div _ when List.mem (Djot.Attr.classes attrs) embed_class ~equal:String.equal ->
    Option.map (Djot.Attr.find "source" attrs) ~f:(fun source_path ->
      { depth =
          Option.value_map (Djot.Attr.find "depth" attrs) ~default:1 ~f:Int.of_string
      ; source_path
      ; fragment =
          Option.bind (Djot.Attr.find "fragment" attrs) ~f:(fun text ->
            (Link.Ref.of_wikilink_target ("#" ^ text)).fragment)
      })
  | _ -> None
;;

type embed_source =
  | Wikilink_embed of string
  | Image_embed of Link.Ref.t

let embed_source_of_inlines (doc : Djot.Doc.t) (inlines : Djot.Inline.t Djot.node list)
  : embed_source option
  =
  match inlines with
  | [ Node (_, _, Ext_wikilink (true, target, _)) ] -> Some (Wikilink_embed target)
  | [ Node (_, _, Image (_, target)) ] ->
    Link.Ref.of_link_target doc target
    |> Option.map ~f:(fun link_ref -> Image_embed link_ref)
  | _ -> None
;;

let is_expandable_embed_paragraph
      (doc : Djot.Doc.t)
      (Djot.Node (_, _, contents) as block : block)
      ~(siblings : block list)
  : embed_source option
  =
  match contents with
  | Para inlines when List.for_all siblings ~f:(phys_equal block) ->
    embed_source_of_inlines doc inlines
  | _ -> None
;;

let fallback_block (target : string) : block =
  Djot.Node.make
    (Djot.Block.Para [ Djot.Node.make (Djot.Inline.Ext_wikilink (false, target, None)) ])
;;

let fragment : Anchor.definition -> Link.Ref.fragment = function
  | Heading heading -> [ heading.text ]
  | Attr { id; _ } -> [ id ]
;;

let transclude ~depth ~source_path ~fragment (blocks : block list) : block =
  Djot.Node.make
    ~attrs:(attrs_of_embed_meta { depth; source_path; fragment })
    (Djot.Block.Div blocks)
;;

let reverse_embed_doc (doc : Djot.Doc.t) : Djot.Doc.t =
  let strip_md (path : string) : string =
    Option.value (String.chop_suffix path ~suffix:".md") ~default:path
  in
  let mapper =
    Djot.Mapper.make
      ~block:(fun _ block ->
        match embed_meta_of_block block with
        | None -> Djot.Mapper.default
        | Some { source_path; fragment; _ } ->
          let target =
            Link.Ref.to_wikilink_target
              { target =
                  (if String.is_empty source_path
                   then None
                   else Some (strip_md source_path))
              ; fragment
              }
          in
          Djot.Mapper.ret
            (Djot.Node.make
               (Djot.Block.Para
                  [ Djot.Node.make (Djot.Inline.Ext_wikilink (true, target, None)) ])))
      ()
  in
  Djot.Mapper.map_doc mapper doc
;;

(* Test
   ==== *)

let%test_module "is_expandable_embed_paragraph" =
  (module struct
    let expandable content ~nth =
      let { Parse.doc; _ } = Parse.of_string content in
      let blocks = Djot.Doc.blocks doc in
      is_expandable_embed_paragraph doc (List.nth_exn blocks nth) ~siblings:blocks
      |> Option.is_some
      |> printf "%b\n"
    ;;

    let%expect_test "sole embed paragraph" =
      expandable "![[target]]" ~nth:0;
      [%expect {| true |}]
    ;;

    let%expect_test "embed mixed with text" =
      expandable "See ![[target]] here." ~nth:0;
      [%expect {| false |}]
    ;;

    let%expect_test "blank lines around" =
      expandable "\n![[target]]\n" ~nth:0;
      [%expect {| true |}]
    ;;

    let%expect_test "non-embed wikilink" =
      expandable "[[target]]" ~nth:0;
      [%expect {| false |}]
    ;;

    let%expect_test "embed among other blocks" =
      expandable "Some text.\n\n![[target]]\n\nMore text." ~nth:1;
      [%expect {| false |}]
    ;;
  end)
;;

let%expect_test "transclude, then reverse" =
  let { Parse.doc; _ } = Parse.of_string "# A\n\nbody\n" in
  let div =
    transclude
      ~depth:1
      ~source_path:"notes/a.md"
      ~fragment:(Some [ "A" ])
      (Djot.Doc.blocks doc)
  in
  print_string (Parse.source_of_blocks [ div ]);
  [%expect
    {|
    {source="notes/a.md" fragment="A" depth="1"}
    ::: embed
    # A

    body
    :::
    |}];
  print_s
    [%sexp
      (Option.map (embed_meta_of_block div) ~f:(fun m ->
         m.depth, m.source_path, m.fragment)
       : (int * string * string list option) option)];
  [%expect {| ((1 notes/a.md ((A)))) |}];
  print_string (Djot.Source.of_doc (reverse_embed_doc (Parse.doc_of_blocks [ div ])));
  [%expect {| ![[notes/a#A]] |}]
;;
