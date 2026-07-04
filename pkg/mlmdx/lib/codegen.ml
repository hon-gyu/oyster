(** mlmdx codegen: an oymarkit {!Cmarkit.Doc.t} becomes a compiler-native
    [Parsetree.structure] exposing [let make () = <element>].

    Structural Markdown (headings, paragraphs, emphasis, ...) is machine
    generated, so it is emitted as already-lowered [JSX.node]/[JSX.string]/
    [JSX.list] calls — no [@JSX] and no mlx involved. Only the leaf code the
    author wrote inside a [ {expr} ] span is parsed, by the real OCaml parser
    ({!Parse.expression}) on a lexbuf primed to the span's true source position
    so type errors and hovers land on the right byte of the [.mlmdx].

    The generated wrapper nodes carry no source location ([Location.none], the
    {!Ast_helper} default), so tooling never maps onto them — only the embedded
    expression underneath. *)

open Cmarkit

let mknoloc txt : _ Location.loc = { txt; loc = Location.none }
let lident s = mknoloc (Longident.Lident s)
let jsx_id name = Ast_helper.Exp.ident (mknoloc (Longident.Ldot (Lident "JSX", name)))
let estring s = Ast_helper.Exp.constant (Ast_helper.Const.string s)

(* An OCaml list expression [ [e1; e2; ...] ]. *)
let rec elist = function
  | [] -> Ast_helper.Exp.construct (lident "[]") None
  | e :: es -> Ast_helper.Exp.construct (lident "::") (Some (Ast_helper.Exp.tuple [ e; elist es ]))

let jsx name args =
  Ast_helper.Exp.apply (jsx_id name) (List.map (fun a -> (Asttypes.Nolabel, a)) args)

(* [JSX.node "tag" [] children] — attributes are empty for now (v1). *)
let node tag ~children = jsx "node" [ estring tag; elist []; elist children ]
let jstring s = jsx "string" [ estring s ]
let jlist es = jsx "list" [ elist es ]
let jnull = jsx_id "null"

(* Parse an embedded [ {expr} ] body with the real OCaml parser, priming the
   lexbuf so the resulting [Parsetree] locations are absolute in [file]. The
   node's textloc spans the whole [ {expr} ], starting at the '{', so the
   expression begins one byte later. *)
let embedded_expr ~file ~meta expr_src =
  let tl = Meta.textloc meta in
  let cnum = Textloc.first_byte tl + 1 (* skip '{' *) in
  let lnum, bol = Textloc.first_line tl in
  let lb = Lexing.from_string expr_src in
  Lexing.set_filename lb file;
  Lexing.set_position lb { Lexing.pos_fname = file; pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum };
  Parse.expression lb

let rec inline ~file (i : Inline.t) : Parsetree.expression =
  match i with
  | Inline.Text (s, _) -> jstring s
  | Inline.Emphasis (e, _) -> node "em" ~children:[ inline ~file (Inline.Emphasis.inline e) ]
  | Inline.Strong_emphasis (e, _) ->
    node "strong" ~children:[ inline ~file (Inline.Emphasis.inline e) ]
  | Inline.Code_span (cs, _) -> node "code" ~children:[ jstring (Inline.Code_span.code cs) ]
  | Inline.Inlines (is, _) -> jlist (List.map (inline ~file) is)
  | Inline.Break (_, _) -> jstring " "
  | Inline.Ext_jsx_expr (j, m) -> embedded_expr ~file ~meta:m (Inline.Jsx_expr.expr j)
  | _ -> jnull

let rec block ~file (b : Block.t) : Parsetree.expression =
  match b with
  | Block.Heading (h, _) ->
    let tag = Printf.sprintf "h%d" (Block.Heading.level h) in
    node tag ~children:[ inline ~file (Block.Heading.inline h) ]
  | Block.Paragraph (p, _) -> node "p" ~children:[ inline ~file (Block.Paragraph.inline p) ]
  | Block.Blocks (bs, _) -> jlist (List.map (block ~file) bs)
  | Block.Block_quote (bq, _) ->
    node "blockquote" ~children:[ block ~file (Block.Block_quote.block bq) ]
  | _ -> jnull

(* [let make () = <body>] (OCaml 5.2+ unified-function AST). *)
let structure ~file (doc : Doc.t) : Parsetree.structure =
  let body = block ~file (Doc.block doc) in
  let param : Parsetree.function_param =
    { pparam_loc = Location.none
    ; pparam_desc = Pparam_val (Asttypes.Nolabel, None, Ast_helper.Pat.construct (lident "()") None)
    }
  in
  let make = Ast_helper.Exp.function_ [ param ] None (Pfunction_body body) in
  [ Ast_helper.Str.value Asttypes.Nonrecursive
      [ Ast_helper.Vb.mk (Ast_helper.Pat.var (mknoloc "make")) make ]
  ]

let of_string ~file s : Parsetree.structure =
  structure ~file (Doc.of_string ~strict:false ~jsx_expr:true ~locs:true ~file s)

let%expect_test "codegen: heading with embedded expr" =
  print_string (Pprintast.string_of_structure (of_string ~file:"t.mlmdx" "# {JSX.int (2 + 2)}"));
  [%expect {| let make () = JSX.node "h1" [] [JSX.int (2 + 2)] |}]

let%expect_test "codegen: paragraph with prose, bold, and embedded expr" =
  print_string
    (Pprintast.string_of_structure
       (of_string ~file:"t.mlmdx" "Some **bold** and {JSX.string name}."));
  [%expect
    {|
    let make () =
      JSX.node "p" []
        [JSX.list
           [JSX.string "Some ";
           JSX.node "strong" [] [JSX.string "bold"];
           JSX.string " and ";
           JSX.string name;
           JSX.string "."]]
    |}]
