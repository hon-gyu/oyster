open Core
module Common = Common
module Frontmatter = Frontmatter
module Textloc_conv = Textloc_conv

let profile : Djot.Profile.t =
  Djot.Profile.(
    markdown_like
    |> with_ext_wikilinks true
    |> with_ext_keyed true
    |> with_ext_callouts true)
;;

type t =
  { frontmatter : Yaml.value option
  ; doc : Djot.Doc.t
  }

let of_string ?(locs = true) (s : string) : t =
  let frontmatter, body = Frontmatter.blank_frontmatter s in
  { frontmatter; doc = Djot.Doc.of_string ~profile ~locs body }
;;

let to_string ({ frontmatter; doc } : t) : string =
  Option.value_map frontmatter ~default:"" ~f:Frontmatter.to_source
  ^ Djot.Source.of_doc doc
;;

let doc_of_blocks (blocks : Djot.Block.t Djot.node list) : Djot.Doc.t =
  Djot.Doc.of_blocks ~profile blocks
;;

let source_of_blocks (blocks : Djot.Block.t Djot.node list) : string =
  Djot.Source.of_doc (doc_of_blocks blocks)
;;

(* Sexp
   ==== *)

let with_attrs (attrs : Djot.Attr.t) (sexp : Sexp.t) : Sexp.t =
  match attrs, sexp with
  | [], _ -> sexp
  | _, Sexp.List items ->
    Sexp.List
      (items
       @ [ Sexp.List
             (Atom "@" :: List.map attrs ~f:(fun (k, v) -> Sexp.List [ Atom k; Atom v ]))
         ])
  | _, Sexp.Atom _ ->
    Sexp.List
      [ sexp
      ; Sexp.List
          (Atom "@" :: List.map attrs ~f:(fun (k, v) -> Sexp.List [ Atom k; Atom v ]))
      ]
;;

let rec sexp_of_inline (Djot.Node (_, attrs, inline) : Djot.Inline.t Djot.node) : Sexp.t =
  let tag name children = Sexp.List (Atom name :: List.map children ~f:sexp_of_inline) in
  let target : Djot.Inline.target -> Sexp.t = function
    | Direct url -> Atom url
    | Reference label -> List [ Atom "ref"; Atom label ]
  in
  with_attrs
    attrs
    (match inline with
     | Str s -> Atom s
     | Emph l -> tag "Emph" l
     | Strong l -> tag "Strong" l
     | Highlight l -> tag "Highlight" l
     | Insert l -> tag "Insert" l
     | Delete l -> tag "Delete" l
     | Superscript l -> tag "Superscript" l
     | Subscript l -> tag "Subscript" l
     | Verbatim s -> List [ Atom "Verbatim"; Atom s ]
     | Symbol s -> List [ Atom "Symbol"; Atom s ]
     | Math (DisplayMath, s) -> List [ Atom "Display_math"; Atom s ]
     | Math (InlineMath, s) -> List [ Atom "Math"; Atom s ]
     | Link (l, t) -> List [ Atom "Link"; target t; tag "text" l ]
     | Image (l, t) -> List [ Atom "Image"; target t; tag "alt" l ]
     | Span l -> tag "Span" l
     | FootnoteReference label -> List [ Atom "Footnote_ref"; Atom label ]
     | UrlLink url -> List [ Atom "Url"; Atom url ]
     | EmailLink email -> List [ Atom "Email"; Atom email ]
     | Ext_wikilink (embed, target, alias) ->
       List
         (Atom (if embed then "Embed" else "Wikilink")
          :: Atom target
          :: Option.value_map alias ~default:[] ~f:(fun a -> [ Sexp.Atom a ]))
     | RawInline (format, s) -> List [ Atom "Raw"; Atom format; Atom s ]
     | NonBreakingSpace -> Atom "Nbsp"
     | Quoted (SingleQuotes, l) -> tag "Single_quoted" l
     | Quoted (DoubleQuotes, l) -> tag "Double_quoted" l
     | SoftBreak -> Atom "Soft_break"
     | HardBreak -> Atom "Hard_break")
;;

let rec sexp_of_block (Djot.Node (_, attrs, block) : Djot.Block.t Djot.node) : Sexp.t =
  let inlines l = List.map l ~f:sexp_of_inline in
  let blocks l = List.map l ~f:sexp_of_block in
  let item bs = Sexp.List (Atom "item" :: blocks bs) in
  with_attrs
    attrs
    (match block with
     | Para l -> List (Atom "Para" :: inlines l)
     | Section l -> List (Atom "Section" :: blocks l)
     | Heading (level, l) ->
       List (Atom "Heading" :: Atom (Int.to_string level) :: inlines l)
     | BlockQuote l -> List (Atom "Block_quote" :: blocks l)
     | CodeBlock (lang, text) -> List [ Atom "Code_block"; Atom lang; Atom text ]
     | Div l -> List (Atom "Div" :: blocks l)
     | OrderedList (_, _, items) -> List (Atom "Ordered_list" :: List.map items ~f:item)
     | BulletList (_, items) -> List (Atom "Bullet_list" :: List.map items ~f:item)
     | TaskList (_, items) ->
       List
         (Atom "Task_list"
          :: List.map items ~f:(fun (status, bs) ->
            Sexp.List
              (Atom
                 (match status with
                  | Complete -> "[x]"
                  | Incomplete -> "[ ]")
               :: blocks bs)))
     | DefinitionList (_, items) ->
       List
         (Atom "Definition_list"
          :: List.map items ~f:(fun (term, bs) ->
            Sexp.List [ List (Atom "term" :: inlines term); item bs ]))
     | ThematicBreak -> Atom "Thematic_break"
     | Table _ -> Atom "Table"
     | RawBlock (format, s) -> List [ Atom "Raw_block"; Atom format; Atom s ]
     | FootnoteDef (label, l) -> List (Atom "Footnote_def" :: Atom label :: blocks l)
     | RefDef (label, dest) -> List [ Atom "Ref_def"; Atom label; Atom dest ]
     | Ext_keyed (label, b) ->
       List [ Atom "Keyed"; List (Atom "label" :: inlines label); sexp_of_block b ]
     | Ext_callout (kind, fold, title, body) ->
       List
         ((Sexp.Atom "Callout"
           :: Sexp.Atom kind
           ::
           (match fold with
            | None -> []
            | Some FoldExpanded -> [ Sexp.Atom "+" ]
            | Some FoldCollapsed -> [ Sexp.Atom "-" ]))
          @ [ Sexp.List (Atom "title" :: inlines title) ]
          @ blocks body))
;;

let sexp_of_doc (doc : Djot.Doc.t) : Sexp.t =
  let footnotes =
    List.map (Djot.Doc.footnotes doc) ~f:(fun (label, bs) ->
      Sexp.List (Atom "Footnote" :: Atom label :: List.map bs ~f:sexp_of_block))
  in
  Sexp.List (List.map (Djot.Doc.blocks doc) ~f:sexp_of_block @ footnotes)
;;

module For_test = struct
  let pp_doc (ppf : Format.formatter) (doc : Djot.Doc.t) : unit =
    Format.fprintf ppf "%s@\n" (Sexp.to_string_hum ~indent:2 (sexp_of_doc doc))
  ;;
end

let%expect_test "frontmatter is kept out of the tree, locations stay in the file" =
  let { frontmatter; doc } = of_string "---\ntitle: T\n---\n# Kap\n" in
  print_s [%sexp (Option.is_some frontmatter : bool)];
  For_test.pp_doc Format.std_formatter doc;
  let (Djot.Node (_, _, section) as node) = List.hd_exn (Djot.Doc.blocks doc) in
  ignore (section : Djot.Block.t);
  Format.printf "%a@." Djot.Textloc.pp (Djot.Doc.textloc doc node);
  [%expect
    {|
    true
    ((Section (Heading 1 Kap) (@ (id Kap))))
    4.0-4.4
    |}]
;;

let%expect_test "extensions" =
  of_string "[[note#A#B|alias]] ![[img.png]]\n\n> [!note]- Title\n> body\n\nkey: value\n"
  |> fun { doc; _ } ->
  For_test.pp_doc Format.std_formatter doc;
  [%expect
    {|
    ((Para (Wikilink note#A#B alias) " " (Embed img.png))
      (Callout note - (title Title) (Para body))
      (Keyed (label key) (Para value)))
    |}]
;;
