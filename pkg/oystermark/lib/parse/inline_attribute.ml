(** {0 Inline attributes}

    Implements the Djot inline attribute syntax. A [{...}] specifier
    immediately following an inline element (with no intervening
    whitespace) attaches the parsed {!Attribute.t} to that element's
    metadata. The same internal syntax as block attributes (see
    {!Attribute}) is used.

    {1 Attachment targets}

    The element to the immediate left of the [{...}] is the target:

    {ul
    {- {b Structured inline} (emphasis, strong, code span, link, image,
       autolink, raw HTML, oystermark extensions): the brace must begin
       at position [0] of the [Text] node that follows the structured
       inline, with the structured inline as the previous sibling.}
    {- {b Bare text}: the brace appears mid-[Text]. The prefix text up
       to the brace becomes the target — emitted as a fresh [Text] node
       carrying the attribute meta. The renderer chooses whether to
       wrap in a span.}}

    In both cases, the character immediately before the [{] must be
    non-whitespace.

    {1 Stacking}

    Consecutive [{...}{...}] runs all bind to the same target and are
    merged via {!Attribute.merge}.

    {1 Multi-line specs}

    The spec body may straddle a soft or hard break in the source. The
    scanner walks forward across [Break] siblings until the matching [}]
    is found in a later [Text] node, joining their contents (breaks
    become spaces inside the spec).

    {1 Limitations}

    {ul
    {- A [{] in the AST may have come from a CommonMark-escaped [\\{] in
       the source — Cmarkit unescapes before we see it. Such literal
       braces will be misinterpreted as attribute openers if their
       contents happen to parse.}
    {- Bare-text attachment requires a non-whitespace character
       immediately before the [{]; standalone [{...}] runs (e.g. at the
       very start of an inline) are left as literal text.}}
*)

open Core
open Cmarkit
open Common

(** Attached to the target inline (Text/Emphasis/Code_span/...) when
    one or more [{...}] specifiers immediately follow it. Stacked
    specifiers are merged via {!Attribute.merge}. *)
let meta_key : Attribute.t Meta.key = Meta.key ()

let sexp_of_meta : meta_sexp =
  fun meta ->
  Meta.find meta_key meta
  |> Option.map ~f:(fun a -> Sexp.List [ Atom "inline_attribute"; Attribute.sexp_of_t a ])
;;

(* Brace scanning
   ==============

   CommonMark has already consumed one level of backslash escapes by the
   time we see the inline AST, so [\\{] in the source appears as [{]
   here with no preceding backslash. We therefore do not perform any
   backslash-escape accounting on AST text content — every literal [{]
   and [}] in the AST is a candidate brace. The one exception is
   double-quoted runs inside a spec: [key="...}..."] suppresses [}] as
   a closer, matching {!Attribute.tokenize}. *)

(** Locate a [}] in [s] at or after [from] that is not inside a
    [".."] quoted run. Returns [None] if not found. *)
let find_close_brace (s : string) ~(from : int) : int option =
  let len = String.length s in
  let rec go i in_quote =
    if i >= len
    then None
    else (
      match s.[i] with
      | '"' -> go (i + 1) (not in_quote)
      | '}' when not in_quote -> Some i
      | _ -> go (i + 1) in_quote)
  in
  go from false
;;

(** Result of a scan that started at [{] in some [Text] node. *)
type scan =
  { spec : Attribute.t
  ; end_idx : int (** index of the [Text] containing the closing [}] *)
  ; end_pos : int (** one past the [}] in that [Text]'s content *)
  }

(** Try to parse a [{...}] starting at position [pos] in
    [children.(start_idx)] (which must be [Text]). May consume following
    [Break] + [Text] siblings to reach the closing [}]. Returns [None]
    if the close is missing or the contents don't parse. *)
let try_scan_brace (children : Inline.t array) ~(start_idx : int) ~(pos : int)
  : scan option
  =
  let n = Array.length children in
  let get_text idx =
    if idx >= n
    then None
    else (
      match children.(idx) with
      | Inline.Text (s, _) -> Some s
      | _ -> None)
  in
  let is_break idx =
    idx < n
    &&
    match children.(idx) with
    | Inline.Break _ -> true
    | _ -> false
  in
  match get_text start_idx with
  | None -> None
  | Some s0 when pos >= String.length s0 || not (Char.equal s0.[pos] '{') -> None
  | Some s0 ->
    let buf = Buffer.create 32 in
    let rec scan idx pos =
      match get_text idx with
      | None -> None
      | Some s ->
        (match find_close_brace s ~from:pos with
         | Some close_pos ->
           Buffer.add_substring buf s ~pos ~len:(close_pos - pos);
           Some (idx, close_pos + 1)
         | None ->
           Buffer.add_substring buf s ~pos ~len:(String.length s - pos);
           if is_break (idx + 1) && idx + 2 < n
           then (
             Buffer.add_char buf '\n';
             scan (idx + 2) 0)
           else None)
    in
    (match scan start_idx (pos + 1) with
     | None -> None
     | Some (end_idx, end_pos) ->
       let inside = Buffer.contents buf in
       (match Attribute.of_string_or_error inside with
        | Error _ -> None
        | Ok spec -> Some { spec; end_idx; end_pos }))
;;

(* Target validation
   ================= *)

(** Is this inline a structured target that can carry inline attribute
    meta directly (i.e. the brace at the start of a following [Text] can
    bind to it)? Excludes [Text] (handled via the bare-text path) and
    [Inlines] / [Break]. *)
let is_structured_target : Inline.t -> bool = function
  | Inline.Emphasis _
  | Inline.Strong_emphasis _
  | Inline.Code_span _
  | Inline.Link _
  | Inline.Image _
  | Inline.Autolink _
  | Inline.Raw_html _
  | Inline.Ext_strikethrough _
  | Inline.Ext_math_span _ -> true
  | _ -> false
;;

(** Stamp [attr] onto [block]'s meta, merging with any existing inline
    attribute already attached. *)
let attach_attr (attr : Attribute.t) (i : Inline.t) : Inline.t =
  let add meta =
    let merged =
      match Meta.find meta_key meta with
      | None -> attr
      | Some prev -> Attribute.merge prev attr
    in
    Meta.add meta_key merged meta
  in
  match i with
  | Inline.Text (s, m) -> Inline.Text (s, add m)
  | Inline.Emphasis (e, m) -> Inline.Emphasis (e, add m)
  | Inline.Strong_emphasis (e, m) -> Inline.Strong_emphasis (e, add m)
  | Inline.Code_span (cs, m) -> Inline.Code_span (cs, add m)
  | Inline.Link (l, m) -> Inline.Link (l, add m)
  | Inline.Image (l, m) -> Inline.Image (l, add m)
  | Inline.Autolink (a, m) -> Inline.Autolink (a, add m)
  | Inline.Raw_html (h, m) -> Inline.Raw_html (h, add m)
  | Inline.Ext_strikethrough (s, m) -> Inline.Ext_strikethrough (s, add m)
  | Inline.Ext_math_span (s, m) -> Inline.Ext_math_span (s, add m)
  | other -> other
;;

(* Children-list rewrite
   ===================== *)

(** Find the next [{] in [s] at or after [from]. *)
let find_open_brace (s : string) ~(from : int) : int option =
  String.index_from s from '{'
;;

(** Push a slice of [s] (from [emit_from] to [stop], exclusive) as a
    [Text] inline onto [acc]. Skip if empty. *)
let push_slice acc (s : string) ~emit_from ~stop meta =
  if stop <= emit_from
  then acc
  else (
    let chunk = String.sub s ~pos:emit_from ~len:(stop - emit_from) in
    Inline.Text (chunk, meta) :: acc)
;;

(** Walk a children list, attaching attribute specs.

    State per [Text] node: ([emit_from], [search_from]).
    - [emit_from] is the position from which un-emitted source text
      begins; it advances only when we successfully attach an attribute
      (consuming a prefix as target text or jumping past a closing brace).
    - [search_from] is where to look for the next [{]; it advances past
      any failed brace attempt without dropping the literal text.

    On a successful attachment, stacked [{...}{...}] runs are walked and
    merged onto the same target. *)
let rewrite_children (children : Inline.t list) : Inline.t list =
  let arr = Array.of_list children in
  let n = Array.length arr in
  let rec emit acc idx =
    if idx >= n
    then List.rev acc
    else (
      match arr.(idx) with
      | Inline.Text (s, meta) -> handle_text acc idx s meta ~emit_from:0 ~search_from:0
      | other -> emit (other :: acc) (idx + 1))
  and handle_text acc idx s meta ~emit_from ~search_from =
    match find_open_brace s ~from:search_from with
    | None ->
      let acc = push_slice acc s ~emit_from ~stop:(String.length s) meta in
      emit acc (idx + 1)
    | Some open_pos ->
      (match try_scan_brace arr ~start_idx:idx ~pos:open_pos with
       | None ->
         (* The brace doesn't close, or contents don't parse. Leave it
            literal and keep scanning. *)
         handle_text acc idx s meta ~emit_from ~search_from:(open_pos + 1)
       | Some scan ->
         try_attach acc idx s meta ~emit_from ~open_pos scan)
  and try_attach acc idx s meta ~emit_from ~open_pos scan =
    let prefix_in_emit = open_pos > emit_from in
    let prefix_last_nonws =
      open_pos > 0 && not (Char.is_whitespace s.[open_pos - 1])
    in
    if prefix_in_emit && prefix_last_nonws
    then (
      (* Bare-text target: the unemitted prefix becomes the target. *)
      let target_text = String.sub s ~pos:emit_from ~len:(open_pos - emit_from) in
      let target = Inline.Text (target_text, meta) in
      let target = attach_attr scan.spec target in
      after_attach (target :: acc) idx s meta scan)
    else if (not prefix_in_emit) && open_pos = 0
    then (
      (* [{] at the start of this text. Try the previous sibling. *)
      match acc with
      | prev :: rest when is_structured_target prev ->
        let prev' = attach_attr scan.spec prev in
        after_attach (prev' :: rest) idx s meta scan
      | _ ->
        (* No valid target — orphan; leave the [{] literal. *)
        handle_text acc idx s meta ~emit_from ~search_from:(open_pos + 1))
    else
      (* Whitespace separates target from [{], or the prefix is already
         emitted (impossible given how state advances, but defensive).
         Orphan — leave literal. *)
      handle_text acc idx s meta ~emit_from ~search_from:(open_pos + 1)
  and after_attach acc idx s meta scan =
    if scan.end_idx = idx
    then (
      (* Same Text. Try stacking another [{...}] immediately after. *)
      let len = String.length s in
      let pos = scan.end_pos in
      if pos < len && Char.equal s.[pos] '{'
      then (
        match try_scan_brace arr ~start_idx:idx ~pos with
        | Some scan' ->
          (match acc with
           | target :: rest ->
             let target' = attach_attr scan'.spec target in
             after_attach (target' :: rest) idx s meta scan'
           | [] -> handle_text acc idx s meta ~emit_from:pos ~search_from:pos)
        | None -> handle_text acc idx s meta ~emit_from:pos ~search_from:pos)
      else handle_text acc idx s meta ~emit_from:pos ~search_from:pos)
    else (
      (* Brace ended in a later child. Drop bridging breaks/text and
         resume from the close position. *)
      match arr.(scan.end_idx) with
      | Inline.Text (s', meta') ->
        let pos = scan.end_pos in
        let len = String.length s' in
        if pos < len && Char.equal s'.[pos] '{'
        then (
          match try_scan_brace arr ~start_idx:scan.end_idx ~pos with
          | Some scan' ->
            (match acc with
             | target :: rest ->
               let target' = attach_attr scan'.spec target in
               after_attach (target' :: rest) scan.end_idx s' meta' scan'
             | [] ->
               handle_text acc scan.end_idx s' meta' ~emit_from:pos ~search_from:pos)
          | None -> handle_text acc scan.end_idx s' meta' ~emit_from:pos ~search_from:pos)
        else handle_text acc scan.end_idx s' meta' ~emit_from:pos ~search_from:pos
      | _ -> emit acc (scan.end_idx + 1))
  in
  emit [] 0
;;

(* Mapper / doc rewrite
   ==================== *)

let wrap_children (meta : Meta.t) : Inline.t list -> Inline.t = function
  | [] -> Inline.Text ("", meta)
  | [ single ] -> single
  | many -> Inline.Inlines (many, meta)
;;

let inline_map : Inline.t Mapper.mapper =
  fun mapper i ->
  match i with
  | Inline.Inlines (children, meta) ->
    (* Recurse first so nested [Inlines] (inside Emphasis, etc.) are
       processed before we look at this level's siblings. *)
    let children = List.filter_map children ~f:(Mapper.map_inline mapper) in
    let children = rewrite_children children in
    Mapper.ret (wrap_children meta children)
  | Inline.Text (s, _) when String.contains s '{' ->
    (* A standalone [Text] (no surrounding [Inlines]) appears when the
       paragraph content is a single text run.  Run [rewrite_children]
       on the singleton list so the bare-text case can fire. *)
    let meta =
      match i with
      | Inline.Text (_, m) -> m
      | _ -> Meta.none
    in
    Mapper.ret (wrap_children meta (rewrite_children [ i ]))
  | _ -> Mapper.default
;;

(* Tests
   ===== *)

module For_test = struct
  let sexp_of_ = Common.make_sexp_of ~metas:[ sexp_of_meta ] ()

  let pp_inline (i : Inline.t) : unit =
    print_endline (Sexp.to_string_hum ~indent:2 (sexp_of_.inline i))
  ;;
end

let%test_module "Inline_attribute" =
  (module struct
    let parse_inline (s : string) : Inline.t =
      let doc = Doc.of_string ~strict:false ~layout:false ~locs:true s in
      let mapper =
        Mapper.make ~inline:inline_map ~inline_ext_default:(fun _ i -> Some i) ()
      in
      let doc = Mapper.map_doc mapper doc in
      match Doc.block doc with
      | Block.Paragraph (p, _) -> Block.Paragraph.inline p
      | Block.Blocks (Block.Paragraph (p, _) :: _, _) -> Block.Paragraph.inline p
      | _ -> Inline.Text ("<no-paragraph>", Meta.none)
    ;;

    let pp s = For_test.pp_inline (parse_inline s)

    let%expect_test "attach to emphasis" =
      pp "An attribute on _emphasized text_{#foo}";
      [%expect
        {|
        (Inlines (Text "An attribute on ")
          ((Emphasis (Text "emphasized text"))
            (meta (inline_attribute ((id (#foo)) (classes ()) (kvs ()))))))
        |}]
    ;;

    let%expect_test "attach to strong" =
      pp "**bold**{.b}";
      [%expect
        {|
        ((Strong_emphasis (Text bold))
          (meta (inline_attribute ((id ()) (classes (.b)) (kvs ())))))
        |}]
    ;;

    let%expect_test "attach to bare text" =
      pp "avant{lang=fr}";
      [%expect
        {|
        ((Text avant)
          (meta (inline_attribute ((id ()) (classes ()) (kvs ((lang fr)))))))
        |}]
    ;;

    let%expect_test "stacked specs merge" =
      pp "avant{lang=fr}{.blue}";
      [%expect
        {|
        ((Text avant)
          (meta (inline_attribute ((id ()) (classes (.blue)) (kvs ((lang fr)))))))
        |}]
    ;;

    let%expect_test "multi-line spec" =
      pp "_emphasized text_{#foo\n.bar .baz key=\"my value\"}";
      [%expect
        {|
        ((Emphasis (Text "emphasized text"))
          (meta
            (inline_attribute
              ((id (#foo)) (classes (.bar .baz)) (kvs ((key "my value")))))))
        |}]
    ;;

    let%expect_test "whitespace before brace = orphan" =
      pp "hello {.x}";
      [%expect {| (Text "hello {.x}") |}]
    ;;

    let%expect_test "brace at start = orphan" =
      pp "{.x}hello";
      [%expect {| (Text {.x}hello) |}]
    ;;

    let%expect_test "invalid contents = literal" =
      pp "foo{not a spec}";
      [%expect {| (Text "foo{not a spec}") |}]
    ;;

    let%expect_test "code span target" =
      pp "`code`{.lang}";
      [%expect
        {|
        ((Code_span code)
          (meta (inline_attribute ((id ()) (classes (.lang)) (kvs ())))))
        |}]
    ;;

    let%expect_test "trailing literal text after attached attr" =
      pp "_em_{.x} after";
      [%expect
        {|
        (Inlines
          ((Emphasis (Text em))
            (meta (inline_attribute ((id ()) (classes (.x)) (kvs ())))))
          (Text " after"))
        |}]
    ;;
  end)
;;
