(** Pre-resolution file-level parsing  *)

open Core
module Block_id = Block_id
module Callout = Callout
module Frontmatter = Frontmatter
module Heading_slug = Heading_slug
module Wikilink = Wikilink
module Extract = Extract

type block_id =
  | Caret of Block_id.t
  | Heading of string

(** Render inlines to plain text, losing their markdown syntax. Used in rendering
    heading to plain text. *)
let inline_to_plain_text (inline : Cmarkit.Inline.t) : string =
  let lines =
    Cmarkit.Inline.to_plain_text
      ~ext:(fun ~break_on_soft inline ->
        match inline with
        | Wikilink.Ext_wikilink (wl, _meta) ->
          let text = Wikilink.to_plain_text wl in
          Cmarkit.Inline.Text (text, Cmarkit.Meta.none)
        | other -> other)
      ~break_on_soft:false
      inline
  in
  String.concat ~sep:"\n" (List.map lines ~f:(String.concat ~sep:""))
;;

(** Create the single-pass mapper that:
    - parses wikilinks in inline text nodes
    - tags block identifiers at paragraph ends
    - tags callout metadata on block quotes
    - stamps deduplicated heading slugs onto heading block meta

    Returns a fresh mapper each time (heading slug dedup requires per-document state). *)
let make_mapper () : Cmarkit.Mapper.t =
  let slug_seen = Hashtbl.create (module String) in
  let map_block (mapper : Cmarkit.Mapper.t) (block : Cmarkit.Block.t)
    : Cmarkit.Block.t Cmarkit.Mapper.result
    =
    match block with
    | Cmarkit.Block.Heading (h, meta) ->
      let orig_inline = Cmarkit.Block.Heading.inline h in
      let mapped_inline =
        Cmarkit.Mapper.map_inline mapper orig_inline |> Option.value ~default:orig_inline
      in
      let text = inline_to_plain_text mapped_inline in
      let slug = Heading_slug.dedup_slug slug_seen text in
      let meta' = Cmarkit.Meta.add Heading_slug.meta_key slug meta in
      let h' =
        Cmarkit.Block.Heading.make
          ?id:(Cmarkit.Block.Heading.id h)
          ~layout:(Cmarkit.Block.Heading.layout h)
          ~level:(Cmarkit.Block.Heading.level h)
          mapped_inline
      in
      Cmarkit.Mapper.ret (Cmarkit.Block.Heading (h', meta'))
    | _ ->
      (match Callout.map_callout mapper block with
       | `Map _ as result -> result
       | `Default -> Block_id.tag_block_id_meta mapper block)
  in
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i -> Some i)
    ~inline:Wikilink.parse
    ~block:map_block
    ()
;;

(** [of_string ?strict ?layout s] parses markdown string [s] into a
    {!Cmarkit.Doc.t} with frontmatter embedded as a {!Frontmatter.Frontmatter}
    block and wikilinks/block IDs parsed. Heading slugs are stamped onto
    heading block metadata. *)
let of_string ?(strict = false) ?(layout = false) (s : string) : Cmarkit.Doc.t =
  let open Cmarkit in
  let yaml_opt, body = Frontmatter.of_string s in
  let cmarkit_doc = Doc.of_string ~strict ~layout body in
  let body_doc = Mapper.map_doc (make_mapper ()) cmarkit_doc in
  match yaml_opt, Doc.block body_doc with
  | None, _ -> body_doc
  | Some yaml, Block.Blocks (blocks, meta) ->
    let blocks' = Frontmatter.Frontmatter yaml :: blocks in
    Doc.make (Block.Blocks (blocks', meta))
  | Some yaml, other ->
    Doc.make (Block.Blocks ([ Frontmatter.Frontmatter yaml; other ], Meta.none))
;;

let commonmark_of_doc (doc : Cmarkit.Doc.t) : string =
  let custom =
    let inline (c : Cmarkit_renderer.context) = function
      | Wikilink.Ext_wikilink (wl, _) ->
        Cmarkit_renderer.Context.string c (Wikilink.to_commonmark wl);
        true
      | _ -> false
    in
    let block (c : Cmarkit_renderer.context) = function
      | Frontmatter.Frontmatter y ->
        Cmarkit_renderer.Context.string c (Frontmatter.to_commonmark y);
        true
      | _ -> false
    in
    Cmarkit_renderer.make ~inline ~block ()
  in
  let default = Cmarkit_commonmark.renderer () in
  let r = Cmarkit_renderer.compose default custom in
  Cmarkit_renderer.doc_to_string r doc
;;

let%expect_test "get_heading_section" =
  let make_block s : Cmarkit.Block.t =
    let doc = of_string s in
    Cmarkit.Doc.block doc
  in
  let block =
    make_block
      {|\
# Heading 1
## Heading 2
### Heading 3
#### Heading 4
##### Heading 5
###### Heading 6
# Heading 7
## Heading 8
### Heading 9
## Heading 10
#### Heading 11
### Heading 12|}
  in
  let heading_id = "heading-1" in
  let extracted = Extract.get_heading_section [ block ] heading_id in
  print_endline
    (commonmark_of_doc
       (Cmarkit.Doc.make (Cmarkit.Block.Blocks (extracted, Cmarkit.Meta.none))));
  [%expect
    {|
    # Heading 1
    ## Heading 2
    ### Heading 3
    #### Heading 4
    ##### Heading 5
    ###### Heading 6
    |}];
  let heading_id = "heading-8" in
  let extracted = Extract.get_heading_section [ block ] heading_id in
  print_endline
    (commonmark_of_doc
       (Cmarkit.Doc.make (Cmarkit.Block.Blocks (extracted, Cmarkit.Meta.none))));
  [%expect
    {|
    ## Heading 8
    ### Heading 9
    |}]
;;

let%expect_test "get_block_by_caret_id" =
  let render_block (block : Cmarkit.Block.t option) : unit =
    match block with
    | None -> print_endline "<none>"
    | Some b -> print_endline (commonmark_of_doc (Cmarkit.Doc.make b))
  in
  (* Case 1: inline block ID at end of paragraph *)
  let doc1 =
    of_string
      {|\
First paragraph.

Second paragraph text ^abc123|}
  in
  render_block (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc1 ] "abc123");
  [%expect {| Second paragraph text ^abc123 |}];

  (* Case 2: standalone block ID referencing previous block (blockquote) *)
  let doc2 =
    of_string
      {|\
> A blockquote here.

^bq001
|}
  in
  render_block (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc2 ] "bq001");
  [%expect {| > A blockquote here. |}];

  (* Case 3: not found *)
  let doc3 = of_string {|
Some text ^exists
|} in
  render_block (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc3 ] "nope");
  [%expect {| <none> |}]
;;
