(** Djot div block extension.

    A div begins with a line of three or more consecutive colons, optionally
    followed by white space and a class name (but nothing else). It ends with
    a line of consecutive colons at least as long as the opening fence, or
    with the end of the document or containing block.

    The contents of a div are interpreted as block-level content.

    {1 Syntax}

{v
::: warning
Here is a paragraph.

And here is another.
:::
v}

    {1 Parsing}

    Parsing is a two-phase process:
    {ol
    {- {b Pre-processing}: {!ensure_fence_isolation} inserts blank lines
       around div fence lines so Cmarkit parses them as standalone paragraphs.}
    {- {b Post-processing}: {!rewrite_doc} walks the parsed AST, detects
       fence paragraphs, and restructures surrounding blocks into
       {!Ext_div} nodes.}}

    A fence paragraph with a class name always opens a new div. A fence
    paragraph without a class name closes the nearest matching open div
    (i.e. one whose opening colon count {m \le} this fence's colon count).

    {1 Data types}

    {!type-t} holds the parsed div metadata:
    {ul
    {- [class_name] -- optional class from the opening fence}
    {- [colons] -- number of colons in the opening fence}} *)

open Core

type t =
  { class_name : string option (** Optional class from the opening fence. *)
  ; colons : int (** Number of colons in the opening fence. *)
  }
[@@deriving sexp]

type Cmarkit.Block.t += Ext_div of t * Cmarkit.Block.t

(** Parse a div fence line.  Returns [(colons, class_name option)] or [None].
    A valid fence is 3+ consecutive colons, optionally followed by whitespace
    and a single non-whitespace token (the class name). *)
let parse_fence (s : string) : (int * string option) option =
  let s = String.strip s in
  let len = String.length s in
  if len < 3
  then None
  else (
    let n = ref 0 in
    while !n < len && Char.equal s.[!n] ':' do
      incr n
    done;
    if !n < 3
    then None
    else (
      let rest = String.strip (String.drop_prefix s !n) in
      if String.is_empty rest
      then Some (!n, None)
      else if String.exists rest ~f:Char.is_whitespace
      then None (* "but nothing else" -- must be a single word *)
      else Some (!n, Some rest)))
;;

(** Extract fence info from a paragraph whose inline is a single [Text] node. *)
let paragraph_fence (block : Cmarkit.Block.t) : (int * string option) option =
  match block with
  | Cmarkit.Block.Paragraph (p, _) ->
    (match Cmarkit.Block.Paragraph.inline p with
     | Cmarkit.Inline.Text (s, _) -> parse_fence s
     | _ -> None)
  | _ -> None
;;

(** Count leading occurrences of [c] in [s]. *)
let count_leading_char (s : string) (c : char) : int =
  let n = ref 0 in
  let len = String.length s in
  while !n < len && Char.equal s.[!n] c do
    incr n
  done;
  !n
;;

(** Pre-process a markdown string so that div fence lines (lines of 3+ colons)
    become standalone paragraphs.  Inserts blank lines before and after fence
    lines when absent.  Skips lines inside fenced code blocks. *)
let ensure_fence_isolation (s : string) : string =
  let lines = String.split_lines s in
  let buf = Buffer.create (String.length s + 64) in
  let in_code_block = ref false in
  let code_fence_char = ref '`' in
  let code_fence_len = ref 0 in
  let prev_blank = ref true in
  let first = ref true in
  let add_line line =
    if not !first then Buffer.add_char buf '\n';
    Buffer.add_string buf line;
    first := false
  in
  List.iter lines ~f:(fun line ->
    let stripped = String.lstrip line in
    if !in_code_block
    then (
      (* Check for closing code fence *)
      let n = count_leading_char stripped !code_fence_char in
      if n >= !code_fence_len
         && String.for_all (String.drop_prefix stripped n) ~f:Char.is_whitespace
      then in_code_block := false;
      add_line line;
      prev_blank := false)
    else (
      let first_char = if String.length stripped > 0 then Some stripped.[0] else None in
      match first_char with
      | Some (('`' | '~') as c) ->
        let n = count_leading_char stripped c in
        if n >= 3
        then (
          in_code_block := true;
          code_fence_char := c;
          code_fence_len := n);
        add_line line;
        prev_blank := false
      | _ ->
        (match parse_fence stripped with
         | Some _ ->
           if not !prev_blank then add_line "";
           add_line line;
           add_line "";
           prev_blank := true
         | None ->
           add_line line;
           prev_blank := String.is_empty (String.strip line))));
  Buffer.contents buf
;;

(** Rewrite a list of sibling blocks, collecting div fences into [Ext_div] nodes. *)
let rec rewrite_block_list (blocks : Cmarkit.Block.t list) : Cmarkit.Block.t list =
  let arr = Array.of_list blocks in
  let len = Array.length arr in
  let result = ref [] in
  let i = ref 0 in
  while !i < len do
    match paragraph_fence arr.(!i) with
    | Some (colons, class_name) ->
      incr i;
      let body_blocks, new_i = collect_body colons arr !i len in
      i := new_i;
      let body_blocks = rewrite_block_list body_blocks in
      let body =
        match body_blocks with
        | [] -> Cmarkit.Block.Blocks ([], Cmarkit.Meta.none)
        | [ single ] -> single
        | multiple -> Cmarkit.Block.Blocks (multiple, Cmarkit.Meta.none)
      in
      result := Ext_div ({ class_name; colons }, body) :: !result
    | None ->
      result := rewrite_within_block arr.(!i) :: !result;
      incr i
  done;
  List.rev !result

(** Collect blocks until a closing fence matching [open_colons] is found.
    Tracks nested named opening fences so their matching closing fences are
    not mistaken for ours. *)
and collect_body
      (open_colons : int)
      (arr : Cmarkit.Block.t array)
      (start : int)
      (len : int)
  : Cmarkit.Block.t list * int
  =
  let collected = ref [] in
  let i = ref start in
  let nesting : int Stack.t = Stack.create () in
  let found_close = ref false in
  while !i < len && not !found_close do
    match paragraph_fence arr.(!i) with
    | Some (colons, Some _) ->
      (* Named opening fence -- track for nesting *)
      Stack.push nesting colons;
      collected := arr.(!i) :: !collected;
      incr i
    | Some (colons, None) ->
      if (not (Stack.is_empty nesting)) && colons >= Stack.top_exn nesting
      then (
        (* Closes the innermost nested div *)
        ignore (Stack.pop_exn nesting : int);
        collected := arr.(!i) :: !collected;
        incr i)
      else if colons >= open_colons
      then (
        (* Closes our div *)
        found_close := true;
        incr i)
      else (
        (* Doesn't match anything -- treat as content *)
        collected := arr.(!i) :: !collected;
        incr i)
    | None ->
      collected := arr.(!i) :: !collected;
      incr i
  done;
  (List.rev !collected, !i)

(** Recurse into block containers to rewrite div fences in their children. *)
and rewrite_within_block (block : Cmarkit.Block.t) : Cmarkit.Block.t =
  match block with
  | Cmarkit.Block.Blocks (blocks, meta) ->
    Cmarkit.Block.Blocks (rewrite_block_list blocks, meta)
  | Cmarkit.Block.Block_quote (bq, meta) ->
    let inner = Cmarkit.Block.Block_quote.block bq in
    let inner' = rewrite_within_block inner in
    Cmarkit.Block.Block_quote (Cmarkit.Block.Block_quote.make inner', meta)
  | _ -> block
;;

(** Post-process a document, rewriting div fence paragraphs into [Ext_div] blocks. *)
let rewrite_doc (doc : Cmarkit.Doc.t) : Cmarkit.Doc.t =
  let block = Cmarkit.Doc.block doc in
  let block' = rewrite_within_block block in
  if phys_equal block block' then doc else Cmarkit.Doc.make block'
;;
