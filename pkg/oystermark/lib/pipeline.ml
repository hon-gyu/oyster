(** Processing pipeline for vault rendering.

    A {!mapper} is analogous to {!Cmarkit.Mapper.t} but operates on
    {!Parse.doc} (which carries frontmatter alongside the AST) and
    receives a vault {!ctx}. Mappers compose left-to-right via {!compose}.

    Pre-index exclusion (e.g. draft files that should not appear in
    the vault index at all) uses {!filter}, a lighter-weight predicate
    that runs before the index is built. *)

open Core

(** Vault context available to mappers after indexing. *)
type ctx =
  { rel_path : string
  ; index : Vault.Index.t
  }

(** A mapper transforms a document within vault context.
    Returns [None] to drop the document from output. *)
type mapper = ctx -> Parse.doc -> Parse.doc option

(** A pre-index filter: decides whether a parsed file enters the vault index.
    Runs before the index exists, so no vault context is available. *)
type filter = string -> Parse.doc -> bool

(* {1 Composition} *)

(** Compose two mappers: run [a] then [b]. Short-circuits on [None]. *)
let compose (a : mapper) (b : mapper) : mapper =
  fun ctx doc ->
  match a ctx doc with
  | None -> None
  | Some doc' -> b ctx doc'
;;

(** Compose a list of mappers left-to-right. Identity on empty list. *)
let compose_all (mappers : mapper list) : mapper =
  fun ctx doc ->
  List.fold_until
    mappers
    ~init:doc
    ~f:(fun doc m ->
      match m ctx doc with
      | None -> Stop None
      | Some doc' -> Continue doc')
    ~finish:(fun doc -> Some doc)
;;

(* {1 Lifting from Cmarkit} *)

(** Lift a context-free [Cmarkit.Mapper.t] into a pipeline mapper.
    Applies it to the doc's AST; frontmatter passes through unchanged. *)
let of_cmarkit (m : Cmarkit.Mapper.t) : mapper =
  fun _ctx pdoc -> Some { pdoc with doc = Cmarkit.Mapper.map_doc m pdoc.doc }
;;

(** Lift a context-dependent [Cmarkit.Mapper.t] builder into a pipeline mapper. *)
let of_cmarkit_with_ctx (f : ctx -> Cmarkit.Mapper.t) : mapper =
  fun ctx pdoc -> Some { pdoc with doc = Cmarkit.Mapper.map_doc (f ctx) pdoc.doc }
;;

(* {1 Frontmatter helpers} *)

(** Test whether a document has [draft: true] in its frontmatter. *)
let is_draft (doc : Parse.doc) : bool =
  match doc.frontmatter with
  | Some (`O fields) ->
    (match List.Assoc.find fields ~equal:String.equal "draft" with
     | Some (`Bool true) -> true
     | _ -> false)
  | _ -> false
;;

(* {1 Built-in mappers} *)

(** Drop documents where frontmatter has [draft: true]. *)
let filter_drafts : mapper = fun _ctx doc -> if is_draft doc then None else Some doc

(* {1 Built-in filters} *)

(** Pre-index filter that excludes drafts from the vault index entirely. *)
let draft_filter : filter = fun _rel_path doc -> not (is_draft doc)

(* {1 Components} *)

module H = Tyxml.Html

(** A component produces HTML elements to inject during rendering. *)
type component = ctx -> Html_types.flow5 H.elt list

(** Rule that determines which components to inject before/after content. *)
type component_rule =
  { before : component list
  ; after : component list
  }

(** Determine component rules for a page. Returns empty lists by default. *)
type component_selector = ctx -> component_rule

let empty_rule : component_rule = { before = []; after = [] }

(* {2 Built-in components} *)

(** Welcome banner for the home page. *)
let welcome_banner : component =
  fun _ctx -> [ H.p ~a:[ H.a_class [ "welcome" ] ] [ H.txt "Welcome to Home" ] ]
;;

(** Directory listing: unordered list of links to sibling .md files in the same dir. *)
let dir_toc : component =
  fun ctx ->
  let dir = Filename.dirname ctx.rel_path in
  let children : Vault.Index.file_entry list =
    List.filter ctx.index.files ~f:(fun (f : Vault.Index.file_entry) ->
      String.is_suffix f.rel_path ~suffix:".md"
      && (not (String.equal f.rel_path ctx.rel_path))
      && String.equal (Filename.dirname f.rel_path) dir)
  in
  match children with
  | [] -> []
  | _ ->
    let items =
      List.map children ~f:(fun (f : Vault.Index.file_entry) ->
        let name = Filename.chop_extension (Filename.basename f.rel_path) in
        let href = Filename.chop_extension f.rel_path in
        H.li [ H.a ~a:[ H.a_href href ] [ H.txt name ] ])
    in
    [ H.nav ~a:[ H.a_class [ "dir-toc" ] ] [ H.ul items ] ]
;;

(** Default component selector: home.md gets a welcome banner,
    index.md in subdirs gets a directory listing. *)
let default_components : component_selector =
  fun ctx ->
  let basename = Filename.basename ctx.rel_path in
  let dir = Filename.dirname ctx.rel_path in
  let before =
    (if String.equal basename "home.md" then [ welcome_banner ] else [])
    @ (if String.equal basename "index.md" && not (String.equal dir ".")
       then [ dir_toc ]
       else [])
  in
  { before; after = [] }
;;

(** Render component elements to an HTML string. *)
let render_components (components : component list) (ctx : ctx) : string =
  let elts = List.concat_map components ~f:(fun c -> c ctx) in
  String.concat
    (List.map elts ~f:(fun e -> Format.asprintf "%a" (H.pp_elt ()) e))
;;
