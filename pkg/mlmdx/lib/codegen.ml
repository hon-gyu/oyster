(** mlmdx codegen: an oymarkit {!Cmarkit.Doc.t} becomes a compiler-native
    [Parsetree.structure] exposing [let make () = <element>].

    Structural Markdown is machine-generated, so it is emitted as already-lowered
    [JSX.node]/[JSX.string]/[JSX.list] calls. Author-written OCaml inside
    [ {expr} ] spans and JSX attribute expressions is parsed with the real OCaml
    parser on a lexbuf primed to the original [.mlmdx] source position.

    oymarkit owns Markdown/JSX boundary detection and stores raw JSX tag source
    plus locations. mlmdx reparses only that small raw opening-tag language to
    decide whether to emit a host node, a component call, or a fragment. *)

open Cmarkit

type origin =
  { byte : int
  ; line : int
  }

type split =
  { prelude : string
  ; body : string
  ; body_origin : origin
  }

let source_start = { byte = 0; line = 1 }

let mknoloc txt : _ Location.loc = { txt; loc = Location.none }
let lident s = mknoloc (Longident.Lident s)
let jsx_id name = Ast_helper.Exp.ident (mknoloc (Longident.Ldot (Lident "JSX", name)))
let estring s = Ast_helper.Exp.constant (Ast_helper.Const.string s)
let ebool b = Ast_helper.Exp.construct (lident (if b then "true" else "false")) None

let rec elist = function
  | [] -> Ast_helper.Exp.construct (lident "[]") None
  | e :: es ->
    Ast_helper.Exp.construct (lident "::") (Some (Ast_helper.Exp.tuple [ e; elist es ]))

let jsx name args =
  Ast_helper.Exp.apply (jsx_id name) (List.map (fun a -> (Asttypes.Nolabel, a)) args)

let node ?(attrs = []) tag ~children = jsx "node" [ estring tag; elist attrs; elist children ]
let jstring s = jsx "string" [ estring s ]
let jlist es = jsx "list" [ elist es ]
let jnull = jsx_id "null"

let parse_expr ~file ~lnum ~bol ~cnum src =
  let lb = Lexing.from_string src in
  Lexing.set_filename lb file;
  Lexing.set_position lb { Lexing.pos_fname = file; pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum };
  Parse.expression lb

let parse_structure ~file ~origin src =
  let lb = Lexing.from_string src in
  Lexing.set_filename lb file;
  Lexing.set_position lb
    { Lexing.pos_fname = file
    ; pos_lnum = origin.line
    ; pos_bol = origin.byte
    ; pos_cnum = origin.byte
    };
  Parse.implementation lb

let loc_in_source origin meta =
  let tl = Meta.textloc meta in
  let lnum, bol = Textloc.first_line tl in
  lnum + origin.line - 1, bol + origin.byte, Textloc.first_byte tl + origin.byte

let embedded_expr ~origin ~file ~meta expr_src =
  let lnum, bol, first = loc_in_source origin meta in
  parse_expr ~file ~lnum ~bol ~cnum:(first + 1) expr_src

let brace_close s i =
  let n = String.length s in
  let rec code k depth =
    if k >= n then failwith "mlmdx: unterminated { in JSX attribute" else
    match s.[k] with
    | '{' -> code (k + 1) (depth + 1)
    | '}' -> if depth = 1 then k else code (k + 1) (depth - 1)
    | '"' -> string '"' (k + 1) depth
    | '\'' -> string '\'' (k + 1) depth
    | '(' when k + 1 < n && s.[k + 1] = '*' -> comment (k + 2) depth 1
    | _ -> code (k + 1) depth
  and string quote k depth =
    if k >= n then failwith "mlmdx: unterminated string in JSX attribute" else
    match s.[k] with
    | '\\' when k + 1 < n -> string quote (k + 2) depth
    | c when c = quote -> code (k + 1) depth
    | _ -> string quote (k + 1) depth
  and comment k depth nest =
    if k >= n then failwith "mlmdx: unterminated comment in JSX attribute" else
    if k + 1 < n && s.[k] = '*' && s.[k + 1] = ')'
    then (if nest = 1 then code (k + 2) depth else comment (k + 2) depth (nest - 1))
    else if k + 1 < n && s.[k] = '(' && s.[k + 1] = '*'
    then comment (k + 2) depth (nest + 1)
    else comment (k + 1) depth nest
  in
  code (i + 1) 1

let quote_close s quote i =
  let n = String.length s in
  let rec go k =
    if k >= n then failwith "mlmdx: unterminated quoted JSX attribute" else
    match s.[k] with
    | '\\' when k + 1 < n -> go (k + 2)
    | c when c = quote -> k
    | _ -> go (k + 1)
  in
  go (i + 1)

let longident_of_dotted s =
  match String.split_on_char '.' s with
  | [] -> Longident.Lident s
  | x :: xs -> List.fold_left (fun acc part -> Longident.Ldot (acc, part)) (Longident.Lident x) xs

type attr_value =
  | Absent
  | String_value of string
  | Expr_value of Parsetree.expression

type attr = { name : string; value : attr_value }

type raw_tag =
  { name : string
  ; attrs : attr list
  ; self_closing : bool
  }

let is_ws = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false
let is_name_stop = function ' ' | '\t' | '\r' | '\n' | '=' | '/' | '>' -> true | _ -> false

let parse_raw_tag ~origin ~file ~meta raw =
  let lnum, bol, base = loc_in_source origin meta in
  let expr_span a b = parse_expr ~file ~lnum ~bol ~cnum:(base + a) (String.sub raw a (b - a)) in
  let n = String.length raw in
  if n < 2 || raw.[0] <> '<' then failwith "mlmdx: invalid JSX opening tag";
  if n >= 2 && raw.[1] = '>' then { name = ""; attrs = []; self_closing = false } else
  let i = ref 1 in
  let name_start = !i in
  while !i < n && not (is_name_stop raw.[!i]) do incr i done;
  let name = String.sub raw name_start (!i - name_start) in
  let attrs = ref [] in
  let self_closing = ref false in
  let done_ = ref false in
  while not !done_ do
    while !i < n && is_ws raw.[!i] do incr i done;
    if !i >= n then done_ := true
    else match raw.[!i] with
      | '>' ->
        incr i;
        done_ := true
      | '/' when !i + 1 < n && raw.[!i + 1] = '>' ->
        i := !i + 2;
        self_closing := true;
        done_ := true
      | '/' ->
        failwith "mlmdx: unexpected '/' in JSX opening tag"
      | _ ->
        let attr_start = !i in
        while !i < n && not (is_name_stop raw.[!i]) do incr i done;
        let attr_name = String.sub raw attr_start (!i - attr_start) in
        while !i < n && is_ws raw.[!i] do incr i done;
        let value =
          if !i < n && raw.[!i] = '=' then begin
            incr i;
            while !i < n && is_ws raw.[!i] do incr i done;
            if !i >= n then failwith "mlmdx: missing JSX attribute value";
            match raw.[!i] with
            | '{' ->
              let close = brace_close raw !i in
              let expr = expr_span (!i + 1) close in
              i := close + 1;
              Expr_value expr
            | '"' | '\'' as quote ->
              let close = quote_close raw quote !i in
              let value = String.sub raw (!i + 1) (close - !i - 1) in
              i := close + 1;
              String_value value
            | _ ->
              failwith "mlmdx: JSX attribute values must be quoted or braced"
          end else Absent
        in
        if attr_name = "" then failwith "mlmdx: empty JSX attribute name";
        attrs := { name = attr_name; value } :: !attrs
  done;
  { name; attrs = List.rev !attrs; self_closing = !self_closing }

let is_component_name = function
  | "" -> false
  | name ->
    let c = name.[0] in
    c >= 'A' && c <= 'Z'

let html_attr_name = function
  | "className" | "class_" -> "class"
  | "htmlFor" | "for_" -> "for"
  | name -> name

let is_boolean_html_attr = function
  | "allowfullscreen" | "async" | "autofocus" | "autoplay" | "checked"
  | "controls" | "default" | "defer" | "disabled" | "formnovalidate"
  | "hidden" | "ismap" | "itemscope" | "loop" | "multiple" | "muted"
  | "nomodule" | "novalidate" | "open" | "playsinline" | "readonly"
  | "required" | "reversed" | "selected" -> true
  | _ -> false

let variant tag expr = Ast_helper.Exp.variant tag (Some expr)

let host_attr { name; value } =
  let name = html_attr_name name in
  let value =
    match value with
    | Absent -> variant "Bool" (ebool true)
    | String_value s -> variant "String" (estring s)
    | Expr_value e when is_boolean_html_attr name -> variant "Bool" e
    | Expr_value e -> variant "String" e
  in
  Ast_helper.Exp.tuple [ estring name; value ]

let is_ocaml_label name =
  let n = String.length name in
  let is_start = function 'a' .. 'z' | '_' -> true | _ -> false in
  let is_part = function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true | _ -> false in
  n > 0 && is_start name.[0] &&
  let rec loop i = i = n || (is_part name.[i] && loop (i + 1)) in
  loop 1

let component_attr { name; value } =
  if not (is_ocaml_label name)
  then failwith ("mlmdx: component prop is not a valid OCaml label: " ^ name);
  let value =
    match value with
    | Absent -> ebool true
    | String_value s -> estring s
    | Expr_value e -> e
  in
  (Asttypes.Labelled name, value)

let component_call tag attrs ?children () =
  let make = Ast_helper.Exp.ident (mknoloc (Longident.Ldot (longident_of_dotted tag, "make"))) in
  let unit = Ast_helper.Exp.construct (lident "()") None in
  let args = List.map component_attr attrs in
  let args = match children with None -> args | Some e -> args @ [ Asttypes.Labelled "children", e ] in
  Ast_helper.Exp.apply make (args @ [ Asttypes.Nolabel, unit ])

let rec inline ~origin ~file (i : Inline.t) : Parsetree.expression =
  match i with
  | Inline.Text (s, _) -> jstring s
  | Inline.Emphasis (e, _) -> node "em" ~children:[ inline ~origin ~file (Inline.Emphasis.inline e) ]
  | Inline.Strong_emphasis (e, _) ->
    node "strong" ~children:[ inline ~origin ~file (Inline.Emphasis.inline e) ]
  | Inline.Code_span (cs, _) -> node "code" ~children:[ jstring (Inline.Code_span.code cs) ]
  | Inline.Inlines (is, _) -> jlist (List.map (inline ~origin ~file) is)
  | Inline.Break (_, _) -> jstring " "
  | Inline.Ext_jsx_expr (j, m) -> embedded_expr ~origin ~file ~meta:m (Inline.Jsx_expr.expr j)
  | Inline.Ext_jsx_element (e, m) -> jsx_inline_element ~origin ~file ~meta:m e
  | _ -> jnull

and jsx_inline_element ~origin ~file ~meta e =
  let tag = parse_raw_tag ~origin ~file ~meta (Inline.Jsx_element.raw e) in
  let children = Option.map (inline ~origin ~file) (Inline.Jsx_element.children e) in
  match tag.name, children with
  | "", None -> jlist []
  | "", Some child -> jlist [ child ]
  | name, children when is_component_name name ->
    component_call name tag.attrs ?children ()
  | name, None ->
    node name ~attrs:(List.map host_attr tag.attrs) ~children:[]
  | name, Some child ->
    node name ~attrs:(List.map host_attr tag.attrs) ~children:[ child ]

and block ~origin ~file (b : Block.t) : Parsetree.expression =
  match b with
  | Block.Heading (h, _) ->
    let tag = Printf.sprintf "h%d" (Block.Heading.level h) in
    node tag ~children:[ inline ~origin ~file (Block.Heading.inline h) ]
  | Block.Paragraph (p, _) -> node "p" ~children:[ inline ~origin ~file (Block.Paragraph.inline p) ]
  | Block.Blocks (bs, _) -> jlist (List.map (block ~origin ~file) bs)
  | Block.Block_quote (bq, _) ->
    node "blockquote" ~children:[ block ~origin ~file (Block.Block_quote.block bq) ]
  | Block.Ext_jsx_block (j, _) -> jsx_block ~origin ~file j
  | _ -> jnull

and jsx_block ~origin ~file j =
  let raw_open, meta = Block.Jsx_block.raw_open j in
  let tag = parse_raw_tag ~origin ~file ~meta raw_open in
  let child = block ~origin ~file (Block.Jsx_block.block j) in
  match tag.name with
  | "" -> jlist [ child ]
  | name when is_component_name name -> component_call name tag.attrs ~children:child ()
  | name -> node name ~attrs:(List.map host_attr tag.attrs) ~children:[ child ]

let structure ~origin ~file (doc : Doc.t) : Parsetree.structure =
  let body = block ~origin ~file (Doc.block doc) in
  let param : Parsetree.function_param =
    { pparam_loc = Location.none
    ; pparam_desc = Pparam_val (Asttypes.Nolabel, None, Ast_helper.Pat.construct (lident "()") None)
    }
  in
  let make = Ast_helper.Exp.function_ [ param ] None (Pfunction_body body) in
  [ Ast_helper.Str.value Asttypes.Nonrecursive
      [ Ast_helper.Vb.mk (Ast_helper.Pat.var (mknoloc "make")) make ]
  ]

let is_blank_line s first last =
  let rec loop i =
    i >= last || ((s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\r') && loop (i + 1))
  in
  loop first

let line_end s i =
  let n = String.length s in
  let rec loop j = if j >= n || s.[j] = '\n' then j else loop (j + 1) in
  loop i

let next_line s i =
  let e = line_end s i in
  if e < String.length s then e + 1 else e

let count_newlines s first last =
  let rec loop i acc =
    if i >= last then acc else loop (i + 1) (if s.[i] = '\n' then acc + 1 else acc)
  in
  loop first 0

let line_starts_with s first last prefix =
  let plen = String.length prefix in
  last - first >= plen && String.sub s first plen = prefix &&
  let after = first + plen in
  after = last || is_ws s.[after]

let prelude_starter s first last =
  line_starts_with s first last "open" ||
  line_starts_with s first last "let" ||
  line_starts_with s first last "module"

let split_initial_prelude s =
  let n = String.length s in
  let rec skip_blank_lines i =
    if i >= n then i else
    let e = line_end s i in
    if is_blank_line s i e then skip_blank_lines (next_line s i) else i
  in
  let rec block_end i =
    if i >= n then n else
    let e = line_end s i in
    if is_blank_line s i e then i else block_end (next_line s i)
  in
  let rec after_blank_run i =
    if i >= n then i else
    let e = line_end s i in
    if is_blank_line s i e then after_blank_run (next_line s i) else i
  in
  let rec consume i consumed_any =
    let first = skip_blank_lines i in
    if first >= n then n, n else
    let first_end = line_end s first in
    if prelude_starter s first first_end then begin
      let b_end = block_end first in
      let after = after_blank_run b_end in
      if after = b_end && after < n
      then failwith "mlmdx: top-of-file OCaml prelude must be separated from Markdown by a blank line";
      consume after true
    end else if consumed_any then first, first else 0, 0
  in
  let prelude_end, body_start = consume 0 false in
  { prelude = String.sub s 0 prelude_end
  ; body = String.sub s body_start (n - body_start)
  ; body_origin = { byte = body_start; line = 1 + count_newlines s 0 body_start }
  }

let of_string ~file s : Parsetree.structure =
  let split = split_initial_prelude s in
  let prelude =
    if split.prelude = "" then [] else parse_structure ~file ~origin:source_start split.prelude
  in
  let doc = Doc.of_string ~strict:false ~jsx_expr:true ~jsx_element:true ~locs:true ~file split.body in
  prelude @ structure ~origin:split.body_origin ~file doc

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

let%expect_test "codegen: self-closing component with attributes" =
  print_string
    (Pprintast.string_of_structure
       (of_string ~file:"t.mlmdx" {|<Greeting name="World" count={1 + 2} loud />|}));
  [%expect
    {|
    let make () =
      JSX.node "p" [] [Greeting.make ~name:"World" ~count:(1 + 2) ~loud:true ()]
    |}]

let%expect_test "codegen: host inline JSX" =
  print_string
    (Pprintast.string_of_structure
       (of_string ~file:"t.mlmdx" {|a <b class="x">bold</b> c|}));
  [%expect
    {|
    let make () =
      JSX.node "p" []
        [JSX.list
           [JSX.string "a ";
           JSX.node "b" [("class", (`String "x"))] [JSX.string "bold"];
           JSX.string " c"]]
    |}]

let%expect_test "codegen: component inline JSX children" =
  print_string
    (Pprintast.string_of_structure
       (of_string ~file:"t.mlmdx" {|<Callout kind="warn">Read **this**</Callout>|}));
  [%expect
    {|
    let make () =
      JSX.node "p" []
        [Callout.make ~kind:"warn"
           ~children:(JSX.list
                        [JSX.string "Read ";
                        JSX.node "strong" [] [JSX.string "this"]]) ()]
    |}]

let%expect_test "codegen: block host and fragment JSX" =
  print_string
    (Pprintast.string_of_structure
       (of_string ~file:"t.mlmdx" {|<div className="box">

# Title

</div>

<>

text

</>|}));
  [%expect
    {|
    let make () =
      JSX.list
        [JSX.node "div" [("class", (`String "box"))]
           [JSX.list [JSX.null; JSX.node "h1" [] [JSX.string "Title"]; JSX.null]];
        JSX.null;
        JSX.list
          [JSX.list [JSX.null; JSX.node "p" [] [JSX.string "text"]; JSX.null]]]
    |}]

let%expect_test "codegen: top-of-file OCaml prelude" =
  print_string
    (Pprintast.string_of_structure
       (of_string ~file:"t.mlmdx" {|open Components

let title = "Prelude"

# {JSX.string title}

<Panel title={title}>

Body

</Panel>|}));
  [%expect
    {|
    open Components
    let title = "Prelude"
    let make () =
      JSX.list
        [JSX.node "h1" [] [JSX.string title];
        JSX.null;
        Panel.make ~title
          ~children:(JSX.list
                       [JSX.null; JSX.node "p" [] [JSX.string "Body"]; JSX.null])
          ()]
    |}]

let%expect_test "codegen: prelude stops at first markdown block" =
  print_string
    (Pprintast.string_of_structure
       (of_string ~file:"t.mlmdx" {|# Title

let x = 1|}));
  [%expect
    {|
    let make () =
      JSX.list
        [JSX.node "h1" [] [JSX.string "Title"];
        JSX.null;
        JSX.node "p" [] [JSX.string "let x = 1"]]
    |}]

let%expect_test "codegen: split_initial_prelude" =
  let split = split_initial_prelude {|open Components

let title = "x"

# Title
|} in
  Printf.printf "prelude=%S\nbody=%S\norigin=%d:%d\n"
    split.prelude split.body split.body_origin.line split.body_origin.byte;
  [%expect
    {|
    prelude="open Components\n\nlet title = \"x\"\n\n"
    body="# Title\n"
    origin=5:34
    |}]

let find_ident_loc name structure =
  let found = ref None in
  let expr iterator e =
    begin match e.Parsetree.pexp_desc with
    | Pexp_ident { txt = Longident.Lident n; loc } when n = name ->
      if !found = None then found := Some loc
    | _ -> ()
    end;
    Ast_iterator.default_iterator.expr iterator e
  in
  let iterator = { Ast_iterator.default_iterator with expr } in
  iterator.structure iterator structure;
  match !found with
  | Some loc -> loc
  | None -> failwith ("missing ident: " ^ name)

let print_loc loc =
  Printf.printf "%s:%d:%d-%d\n"
    loc.Location.loc_start.pos_fname
    loc.loc_start.pos_lnum
    (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
    (loc.loc_end.pos_cnum - loc.loc_start.pos_bol)

let%expect_test "codegen: embedded expr location after prelude" =
  let structure = of_string ~file:"t.mlmdx" {|let title = "x"

# {JSX.string title}
|} in
  print_loc (find_ident_loc "title" structure);
  [%expect {| t.mlmdx:3:14-19 |}]

let%expect_test "codegen: JSX prop expr location after prelude" =
  let structure = of_string ~file:"t.mlmdx" {|let title = "x"

<Panel title={title} />
|} in
  print_loc (find_ident_loc "title" structure);
  [%expect {| t.mlmdx:3:14-19 |}]
