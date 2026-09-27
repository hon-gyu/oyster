(** Command blocks: fenced [oysterlsp] blocks whose lines name server
    commands.

    Spec: {!page-"feature-command-block"}.

    The pure layer — finding the blocks and reading their lines. What a command
    {e does} lives in {!Server}; this module only says which line asked for
    which one, so the surfaces built on it (lens, action, completion,
    diagnostic) all agree about what is where. *)

open Core

(** {1:catalogue Catalogue}

    The closed set of names a line may hold.  See
    {!page-"feature-command-block".commands}. *)

type command =
  | Daily_today
  | Daily_yesterday
  | Daily_tomorrow
  | Daily_prev
  | Daily_next
[@@deriving sexp, equal, compare, enumerate]

let name_of_command : command -> string = function
  | Daily_today -> "daily/today"
  | Daily_yesterday -> "daily/yesterday"
  | Daily_tomorrow -> "daily/tomorrow"
  | Daily_prev -> "daily/prev"
  | Daily_next -> "daily/next"
;;

(** What the command is for, one line, shown while completing.  Deliberately
    not the lens title: that one reports what will {e happen} to this vault
    today ("Create yesterday's daily note"), which only {!Server} knows. *)
let doc_of_command : command -> string = function
  | Daily_today -> "Open today's daily note, creating it when missing"
  | Daily_yesterday -> "Open yesterday's daily note, creating it when missing"
  | Daily_tomorrow -> "Open tomorrow's daily note, creating it when missing"
  | Daily_prev -> "Open the previous existing daily note, relative to this note"
  | Daily_next -> "Open the next existing daily note, relative to this note"
;;

let command_of_name (s : string) : command option =
  List.find all_of_command ~f:(fun c -> String.equal (name_of_command c) s)
;;

(** {1:reading Reading a block}

    A line is exactly its command name: fenced content is literal text, not
    markdown.  See {!page-"feature-command-block".syntax}. *)

(** One meaningful line of a command block. *)
type entry =
  { line : int (** 0-based, as LSP counts. *)
  ; text : string (** The line, stripped. *)
  ; command : command option (** [None] when the name is unknown. *)
  }
[@@deriving sexp_of, equal, compare]

let info_string = "oysterlsp"

(** Whether a code block's info string opens a command block. *)
let is_command_block (info : string) : bool =
  match String.split (String.strip info) ~on:' ' with
  | first :: _ -> String.equal first info_string
  | [] -> false
;;

(** Blank, or a comment: not a command, and not an error either. *)
let is_ignorable (text : string) : bool =
  String.is_empty text || String.is_prefix text ~prefix:"#"
;;

(** Fold [f] over every command block in [content], in document order.

    Blocks are located through the parser rather than by scanning for fences:
    indented, nested and differently-fenced blocks are the parser's problem,
    and it has already solved them. *)
let fold_command_blocks
      (content : string)
      ~(init : 'a)
      ~(f : 'a -> Djot.Doc.t -> Djot.Block.t Djot.node -> code:string -> 'a)
  : 'a
  =
  let { Oystermark.Parse.doc; _ } = Lsp_util.parse_doc content in
  let folder =
    Djot.Folder.make
      ~block:(fun _folder acc (Node (_, _, block) as node) ->
        match block with
        | CodeBlock (info, code) when is_command_block info ->
          Djot.Folder.ret (f acc doc node ~code)
        | _ -> Djot.Folder.default) (* Inlines cannot contain a code block. *)
      ~inline:(fun _folder acc _i -> Djot.Folder.ret acc)
      ()
  in
  Djot.Folder.fold_doc folder init doc
;;

(** Every line of every command block in [content], blanks and comments
    included, as [(line, stripped text)]. A block's code starts on the line
    after its opening fence. *)
let block_lines (content : string) : (int * string) list =
  List.rev
    (fold_command_blocks content ~init:[] ~f:(fun acc doc node ~code ->
       match
         List.find_map (Djot.Doc.syntax_locs doc node) ~f:(fun (role, loc) ->
           match role with
           | ROpenFence -> Some loc
           | RAttrSpec | RCloseFence -> None)
       with
       | None -> acc
       | Some fence ->
         (* 1-based fence line = 0-based line of the first code line. *)
         let first = fst (Djot.Textloc.first_line fence) in
         String.chop_suffix_if_exists code ~suffix:"\n"
         |> String.split ~on:'\n'
         |> List.foldi ~init:acc ~f:(fun i acc line ->
           (first + i, String.strip line) :: acc)))
;;

(** Whether [content] already holds a block.  Asked before offering to insert
    one; not [block_lines], which cannot tell an empty block from no block.
    See {!page-"feature-command-block".insert}. *)
let has_command_block (content : string) : bool =
  fold_command_blocks content ~init:false ~f:(fun _acc _doc _node ~code:_ -> true)
;;

(** [entries content] is every meaningful line of every command block, in
    document order. *)
let entries (content : string) : entry list =
  block_lines content
  |> List.filter_map ~f:(fun (line, text) ->
    if is_ignorable text
    then None
    else Some { line; text; command = command_of_name text })
;;

(** [entry_at content ~line] is the command-block line the cursor sits on, if
    any.  Used by the code-action surface, which is anchored to a position
    rather than to the whole document. *)
let entry_at (content : string) ~(line : int) : entry option =
  List.find (entries content) ~f:(fun e -> Int.equal e.line line)
;;

(** Whether [line] falls inside a command block, meaningful or not.  Completion
    asks this: a blank line in a block is exactly where a name is about to be
    typed, and {!entries} skips those. *)
let in_command_block (content : string) ~(line : int) : bool =
  List.exists (block_lines content) ~f:(fun (l, _) -> Int.equal l line)
;;

(** {1:render Writing a block}

    The inverse of {!entries}, used by the action that seeds a note with a
    panel.  See {!page-"feature-command-block".insert}. *)

(** A fresh block listing [commands], one per line, fences included.  Ends in a
    blank line so that whatever the insertion point pushed down stays a
    paragraph of its own. *)
let render (commands : command list) : string =
  String.concat ~sep:"\n" (("```" ^ info_string) :: List.map commands ~f:name_of_command)
  ^ "\n```\n\n"
;;

(** {1:test Test} *)

let%test_module "command_block" =
  (module struct
    let show (content : string) =
      List.iter (entries content) ~f:(fun e ->
        printf
          "%d %-18s %s\n"
          e.line
          e.text
          (match e.command with
           | Some c -> name_of_command c
           | None -> "<unknown>"))
    ;;

    let%expect_test "a block's lines, with their positions" =
      show
        {|# Journal

```oysterlsp
daily/today
daily/prev
```

Some prose.
|};
      [%expect
        {|
        3 daily/today        daily/today
        4 daily/prev         daily/prev
        |}]
    ;;

    (* Blank lines and comments are structure, not mistakes; an unrecognized
       name is a mistake, and is kept so a diagnostic can point at it. *)
    let%expect_test "blanks, comments, and unknown names" =
      show
        {|```oysterlsp
# navigation

daily/today
  daily/next
daily/yesterdya
```
|};
      [%expect
        {|
        3 daily/today        daily/today
        4 daily/next         daily/next
        5 daily/yesterdya    <unknown>
        |}]
    ;;

    let%expect_test "several blocks, and blocks that are not ours" =
      show
        {|```oysterlsp
daily/today
```

```ocaml
daily/today
```

{#panel}
```oysterlsp
daily/next
```
|};
      [%expect
        {|
        1 daily/today        daily/today
        10 daily/next         daily/next
        |}]
    ;;

    (* An indented block is still a block; scanning for fences by hand would
       have to know that. *)
    let%expect_test "inside a list item" =
      show
        {|- a list item:

  ```oysterlsp
  daily/today
  ```
|};
      [%expect {| 3 daily/today        daily/today |}]
    ;;

    (* Container extensions are the parser's business, and it descends into
       them: a panel inside a callout is found like any other. *)
    let%expect_test "inside a callout" =
      show
        {|> [!note]
> ```oysterlsp
> daily/today
> ```
|};
      [%expect {| 2 daily/today        daily/today |}]
    ;;

    let%expect_test "no block at all" =
      show "# Just a note\n\nwith prose.\n";
      [%expect {| |}]
    ;;

    let%expect_test "entry_at and in_command_block" =
      let content =
        {|```oysterlsp
daily/today

```
prose
|}
      in
      let at line =
        printf
          "%d: entry=%s in_block=%b\n"
          line
          (match entry_at content ~line with
           | Some e -> e.text
           | None -> "-")
          (in_command_block content ~line)
      in
      List.iter [ 0; 1; 2; 4 ] ~f:at;
      [%expect
        {|
        0: entry=- in_block=false
        1: entry=daily/today in_block=true
        2: entry=- in_block=true
        4: entry=- in_block=false
        |}]
    ;;

    (* An empty block is a block: the insert action must not offer to add a
       second one just because the first holds no lines yet. *)
    let%expect_test "has_command_block" =
      let check content = printf "%b\n" (has_command_block content) in
      check "```oysterlsp\ndaily/today\n```\n";
      check "```oysterlsp\n```\n";
      check "```ocaml\ndaily/today\n```\n";
      check "# prose only\n";
      [%expect
        {|
        true
        true
        false
        false
        |}]
    ;;

    let%expect_test "render" =
      print_string (render [ Daily_today; Daily_prev ]);
      [%expect
        {|
        ```oysterlsp
        daily/today
        daily/prev
        ```
        |}]
    ;;

    (* What is written can be read back: rendering then parsing recovers the
       commands, at the lines the block put them on. *)
    let%expect_test "render round-trips through entries" =
      show (render all_of_command);
      [%expect
        {|
        1 daily/today        daily/today
        2 daily/yesterday    daily/yesterday
        3 daily/tomorrow     daily/tomorrow
        4 daily/prev         daily/prev
        5 daily/next         daily/next
        |}]
    ;;

    let%expect_test "catalogue" =
      List.iter all_of_command ~f:(fun c ->
        printf "%-16s %s\n" (name_of_command c) (doc_of_command c));
      [%expect
        {|
        daily/today      Open today's daily note, creating it when missing
        daily/yesterday  Open yesterday's daily note, creating it when missing
        daily/tomorrow   Open tomorrow's daily note, creating it when missing
        daily/prev       Open the previous existing daily note, relative to this note
        daily/next       Open the next existing daily note, relative to this note
        |}]
    ;;
  end)
;;
