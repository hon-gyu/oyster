open Odoc_document.Types
module Url = Odoc_document.Url

type t =
  { path : string
  ; body : string
  ; subpages : Page.t list
  }

(* The state of one page being rendered. [refs] accumulates the references
   found in the current declaration's signature and is emptied by each
   [Refs:] line; [page] is the note being written, so that a reference into
   it renders as a bare fragment. *)
type state =
  { buf : Buffer.t
  ; page : string
  ; mutable refs : string list
  ; mutable subpages : Page.t list
  }

let pr st fmt = Printf.ksprintf (Buffer.add_string st.buf) fmt

(* Fences
   ====== *)

(* A fence must be longer than any backtick run in its content, or content
   that is itself fenced markdown closes it early. *)
let fence content =
  let longest = ref 0
  and run = ref 0 in
  String.iter
    (fun c ->
       if Char.equal c '`'
       then (
         incr run;
         if !run > !longest then longest := !run)
       else run := 0)
    content;
  String.make (max 3 (!longest + 1)) '`'
;;

let code_fence ?(indent = "") ?(info = "") content =
  let f = fence content in
  Printf.sprintf "%s%s%s\n%s\n%s%s\n\n" indent f info content indent f
;;

(* Targets
   ======= *)

let url_target st (url : Url.t) =
  let page = Address.note url.page in
  let page = if String.equal page st.page then "" else page in
  if String.equal url.anchor "" then page else page ^ "#" ^ Address.anchor url.anchor
;;

let target_href st (target : Target.t) =
  match target with
  | External href -> href
  | Internal (Resolved url) -> url_target st url
  | Internal Unresolved -> ""
;;

(* Inline
   ====== *)

(* odoc stores entities bare: ["gt"], ["#45"]. [->] is ["#45"] then ["gt"]. *)
let entity e =
  match e with
  | "gt" -> ">"
  | "lt" -> "<"
  | "amp" -> "&"
  | "quot" -> "\""
  | _ when String.length e > 1 && Char.equal e.[0] '#' ->
    (match int_of_string_opt (String.sub e 1 (String.length e - 1)) with
     | Some c when c < 128 -> String.make 1 (Char.chr c)
     | _ -> e)
  | _ -> e
;;

(* Inline content in a code position: links flatten to their text and are
   recorded, to be emitted as wikilinks beneath the fence. *)
let rec text st (inline : Inline.t) =
  String.concat "" (List.map (fun (one : Inline.one) -> text_one st one.desc) inline)

and text_one st = function
  | Inline.Text t -> t
  | Entity e -> entity e
  | Linebreak -> "\n"
  | Styled (_, t) -> text st t
  | Source s -> source st s
  | Math m -> m
  | Raw_markup (_, s) -> s
  | Link { content; target; _ } ->
    (match target with
     | Internal (Resolved url) ->
       let t = url_target st url in
       if not (List.mem t st.refs) then st.refs <- t :: st.refs
     | Internal Unresolved | External _ -> ());
    text st content

and source st (s : Source.t) =
  String.concat
    ""
    (List.map
       (function
         | Source.Elt i -> text st i
         | Source.Tag (_, s) -> source st s)
       s)
;;

(* Inline content in a prose position, where links can be rendered. *)
let rec inline st (i : Inline.t) =
  String.concat "" (List.map (fun (one : Inline.one) -> inline_one st one.desc) i)

and inline_one st = function
  | Inline.Text t -> t
  | Entity e -> entity e
  | Linebreak -> "\\\n"
  | Styled (`Bold, t) -> "**" ^ inline st t ^ "**"
  | Styled ((`Italic | `Emphasis), t) -> "*" ^ inline st t ^ "*"
  | Styled (`Superscript, t) -> "^" ^ inline st t ^ "^"
  | Styled (`Subscript, t) -> "~" ^ inline st t ^ "~"
  | Source s -> "`" ^ source st s ^ "`"
  | Math m -> "$" ^ m ^ "$"
  | Raw_markup (target, s) -> Printf.sprintf "`%s`{=%s}" s target
  | Link { content; target; _ } ->
    (match target with
     | Internal (Resolved url) ->
       let t = url_target st url
       and display = text st content in
       if String.equal t display
       then Printf.sprintf "[[%s]]" t
       else Printf.sprintf "[[%s|%s]]" t display
     | Internal Unresolved -> inline st content
     | External href -> Printf.sprintf "[%s](%s)" (inline st content) href)
;;

(* Blocks
   ====== *)

let rec block ?(indent = "") st (b : Block.t) =
  List.iter (fun (one : Block.one) -> block_one ~indent st one) b

(* The first block of a list item or a keyed value continues the line the
   marker opened, so it is rendered unindented and the rest indented under it. *)
and body ~indent st = function
  | [] -> pr st "\n"
  | first :: rest ->
    block_one ~indent:"" st first;
    block ~indent:(indent ^ "  ") st rest

and block_one ~indent st (one : Block.one) =
  match one.desc with
  | Block.Inline i | Paragraph i -> pr st "%s%s\n\n" indent (inline st i)
  | List (kind, items) ->
    List.iteri
      (fun n item ->
         let marker =
           match kind with
           | Ordered -> string_of_int (n + 1) ^ "."
           | Unordered -> "-"
         in
         pr st "%s%s " indent marker;
         body ~indent st item)
      items;
    pr st "\n"
  | Description items ->
    List.iter
      (fun (d : Description.one) ->
         let part attr =
           List.filter (fun (o : Inline.one) -> List.mem attr o.attr) d.key
         in
         let labels =
           match part "at-tag", part "value" with
           | [], [] -> [ Printf.sprintf "`%s`" (inline st d.key) ]
           | tag, [] -> [ inline st tag ]
           | tag, value -> [ inline st tag; Printf.sprintf "`%s`" (inline st value) ]
         in
         pr st "%s- %s: " indent (String.concat ": " labels);
         body ~indent st d.definition)
      items;
    pr st "\n"
  | Source (lang, s) -> pr st "%s" (code_fence ~indent ~info:lang (source st s))
  | Verbatim v -> pr st "%s" (code_fence ~indent v)
  | Math m -> pr st "%s$$%s$$\n\n" indent m
  | Raw_markup (target, s) -> pr st "%s" (code_fence ~indent ~info:("=" ^ target) s)
  | Table t -> table ~indent st t
  | Image (target, alt) -> pr st "%s![%s](%s)\n\n" indent alt (target_href st target)
  | Video (target, alt) ->
    pr st "%s![%s](%s){.video}\n\n" indent alt (target_href st target)
  | Audio (target, alt) ->
    pr st "%s![%s](%s){.audio}\n\n" indent alt (target_href st target)

(* A cell holds blocks, but a pipe-table row is one line: render the cell
   aside and take the text back off the buffer. *)
and cell st (blocks, _) =
  let mark = Buffer.length st.buf in
  block st blocks;
  let s = Buffer.sub st.buf mark (Buffer.length st.buf - mark) in
  Buffer.truncate st.buf mark;
  String.trim s

and table ~indent st (t : Block.t Table.t) =
  let row cells =
    pr st "%s| %s |\n" indent (String.concat " | " (List.map (cell st) cells))
  in
  (match t.data with
   | [] -> ()
   | header :: rows ->
     row header;
     let align (a : Table.alignment) =
       match a with
       | Left -> ":---"
       | Center -> ":---:"
       | Right -> "---:"
       | Default -> "---"
     in
     let rule =
       match t.align with
       | [] -> List.map (fun _ -> "---") header
       | aligns -> List.map align aligns
     in
     pr st "%s| %s |\n" indent (String.concat " | " rule);
     List.iter row rows);
  pr st "\n"
;;

(* Declarations
   ============ *)

(* The code lines of a declaration, all inside one fence. An expansion is
   collapsed to [ ... ] and queued as a note of its own. *)
let rec declaration_code st (d : DocumentedSrc.t) =
  let is_member (one : DocumentedSrc.one) =
    match one with
    | Documented _ | Nested _ -> true
    | Code _ | Subpage _ | Alternative _ -> false
  in
  let ends_with_newline s =
    String.length s > 0 && Char.equal s.[String.length s - 1] '\n'
  in
  let rec go = function
    | [] -> ""
    | (one : DocumentedSrc.one) :: rest ->
      let this =
        match one with
        | Code s ->
          let s = source st s in
          (match rest with
           | next :: _ when is_member next && not (ends_with_newline s) -> s ^ "\n"
           | _ -> s)
        | Documented { code; _ } -> "  " ^ text st code ^ "\n"
        | Nested { code; _ } ->
          let c = declaration_code st code in
          if ends_with_newline c || String.equal c "" then c else c ^ "\n"
        | Subpage { content; _ } ->
          st.subpages <- content :: st.subpages;
          let t = Address.note content.url in
          if not (List.mem t st.refs) then st.refs <- t :: st.refs;
          " ... "
        | Alternative (Expansion { expansion; _ }) -> declaration_code st expansion
      in
      this ^ go rest
  in
  go d
;;

(* Every member gets a line beneath the fence, documented or not: odoc anchors
   them all, and an attribute line with no block under it is dropped, taking
   the anchor with it. *)
let rec members st (d : DocumentedSrc.t) =
  List.iter
    (fun (one : DocumentedSrc.one) ->
       let anchored label (anchor : Url.Anchor.t option) doc =
         Option.iter
           (fun (a : Url.Anchor.t) -> pr st "%s\n" (Address.attribute a.anchor))
           anchor;
         pr st "- `%s`" label;
         match doc with
         | [] -> pr st "\n\n"
         | _ ->
           pr st ": ";
           body ~indent:"" st doc
       in
       match one with
       | DocumentedSrc.Documented { code; doc; anchor; _ } ->
         anchored (String.trim (text st code)) anchor doc
       | Nested { code; doc; anchor; _ } ->
         let label =
           match String.split_on_char '\n' (String.trim (declaration_code st code)) with
           | line :: _ -> String.trim line
           | [] -> ""
         in
         anchored label anchor doc;
         members st code
       | Alternative (Expansion { expansion; _ }) -> members st expansion
       | Code _ | Subpage _ -> ())
    d
;;

let emit_refs st =
  match List.sort compare st.refs with
  | [] -> ()
  | refs ->
    pr st "Refs: %s\n\n" (String.concat " " (List.map (Printf.sprintf "[[%s]]") refs));
    st.refs <- []
;;

(* Items
   ===== *)

let rec item st (i : Item.t) =
  match i with
  | Item.Text t -> block st t
  | Heading h ->
    Option.iter (fun label -> pr st "%s\n" (Address.attribute label)) h.label;
    pr st "%s %s\n\n" (String.make (min 6 (h.level + 1)) '#') (inline st h.title)
  | Declaration { anchor; content; doc; _ } ->
    st.refs <- [];
    let code = declaration_code st content in
    Option.iter
      (fun (a : Url.Anchor.t) -> pr st "%s\n" (Address.attribute a.anchor))
      anchor;
    pr st "%s" (code_fence ~info:"ocaml" (String.trim code));
    emit_refs st;
    members st content;
    block st doc
  | Include { content = { content; status; summary }; _ } ->
    let status =
      match status with
      | `Inline -> "inline"
      | `Open -> "open"
      | `Closed -> "closed"
      | `Default -> "default"
    in
    pr
      st
      "{.include status=%s}\n%s"
      status
      (code_fence ~info:"ocaml" (String.trim (source st summary)));
    List.iter (item st) content
;;

let page (p : Page.t) =
  let st =
    { buf = Buffer.create 4096; page = Address.note p.url; refs = []; subpages = [] }
  in
  List.iter (item st) p.preamble;
  List.iter (item st) p.items;
  { path = st.page; body = Buffer.contents st.buf; subpages = List.rev st.subpages }
;;
