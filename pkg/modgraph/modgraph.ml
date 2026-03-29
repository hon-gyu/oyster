(* tools/modgraph/modgraph.ml *)
open Core

(* A node is either a compilation unit ("Foo") or a submodule ("Foo.Bar") *)
(* An edge is (src, dst) meaning src references dst *)

module String_pair = struct
  module T = struct
    type t = string * string [@@deriving compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)
end

type graph =
  { nodes : Hash_set.M(String).t
  ; edges : Hash_set.M(String_pair).t
  }

let make_graph () =
  { nodes = Hash_set.create (module String)
  ; edges = Hash_set.create (module String_pair)
  }
;;

let add_edge g src dst =
  Hash_set.add g.nodes src;
  Hash_set.add g.nodes dst;
  if not (String.equal src dst) then Hash_set.add g.edges (src, dst)
;;

(* Convert a Path.t to a dot-separated string *)
let path_to_string path =
  Path.name path
  |> String.map ~f:(function
    | '(' | ')' -> '_'
    | c -> c)
;;

(* Collect all module references inside a module expression *)
let collect_mod_refs ~iterator ~refs mod_expr =
  let open Typedtree in
  match mod_expr.mod_desc with
  | Tmod_ident (path, _) -> Hash_set.add refs (path_to_string path)
  | _ -> iterator.Tast_iterator.module_expr iterator mod_expr
;;

(* Given a structure, find all submodule names defined at the top level *)
let top_level_submodules str =
  let open Typedtree in
  List.filter_map str.str_items ~f:(fun item ->
    match item.str_desc with
    | Tstr_module { mb_name = { txt = Some name; _ }; _ } -> Some name
    | _ -> None)
;;

(* Walk a structure item and collect (submodule_name, set_of_referenced_modules) *)
let extract_submodule_refs unit_name str =
  let open Typedtree in
  let local_modules = top_level_submodules str |> String.Set.of_list in
  List.filter_map str.str_items ~f:(fun item ->
    match item.str_desc with
    | Tstr_module { mb_name = { txt = Some name; _ }; mb_expr; _ } ->
      let refs = Hash_set.create (module String) in
      (* Walk the module body collecting Tmod_ident and Texp_ident *)
      let iterator =
        { Tast_iterator.default_iterator with
          module_expr =
            (fun self me ->
              (match me.mod_desc with
               | Tmod_ident (path, _) -> Hash_set.add refs (path_to_string path)
               | _ -> ());
              Tast_iterator.default_iterator.module_expr self me)
        ; expr =
            (fun self e ->
              (match e.exp_desc with
               | Texp_ident (path, _, _) ->
                 (* Only care about qualified paths — unqualified are local values *)
                 (match path with
                  | Path.Pdot (Path.Pident _, _) ->
                    Hash_set.add refs (path_to_string path)
                  | _ -> ())
               | _ -> ());
              Tast_iterator.default_iterator.expr self e)
        }
      in
      iterator.module_expr iterator mb_expr;
      (* Filter refs to only locally-defined submodules *)
      let local_refs =
        Hash_set.filter refs ~f:(fun r ->
          (* r might be "Bar" or "Foo.Bar" — check if it starts with a local name *)
          let top = String.split r ~on:'.' |> List.hd_exn in
          Set.mem local_modules top || String.equal top unit_name)
      in
      Some (unit_name ^ "." ^ name, local_refs)
    | _ -> None)
;;

let process_cmt g cmt_path =
  let cmt = Cmt_format.read_cmt cmt_path in
  let unit_name = cmt.Cmt_format.cmt_modname in
  Hash_set.add g.nodes unit_name;
  match cmt.Cmt_format.cmt_annots with
  | Cmt_format.Implementation str ->
    let submod_refs = extract_submodule_refs unit_name str in
    List.iter submod_refs ~f:(fun (submod, refs) ->
      (* unit -> submodule containment edge *)
      add_edge g unit_name submod;
      (* submodule -> referenced submodule edges *)
      Hash_set.iter refs ~f:(fun ref_name ->
        (* Normalise: if ref is just "Bar", qualify to "Unit.Bar" *)
        let dst =
          if String.is_prefix ref_name ~prefix:(unit_name ^ ".")
          then ref_name
          else if String.mem ref_name '.'
          then ref_name
          else unit_name ^ "." ^ ref_name
        in
        add_edge g submod dst))
  | _ -> ()
;;

let emit_dot g =
  print_endline "digraph modules {";
  print_endline "  rankdir=LR;";
  print_endline "  node [shape=box fontname=monospace];";
  Hash_set.iter g.nodes ~f:(fun n -> Printf.printf "  \"%s\";\n" n);
  Hash_set.iter g.edges ~f:(fun (src, dst) ->
    Printf.printf "  \"%s\" -> \"%s\";\n" src dst);
  print_endline "}"
;;

let () =
  let cmt_files = Sys.get_argv () |> Array.to_list |> List.tl_exn in
  let g = make_graph () in
  List.iter cmt_files ~f:(process_cmt g);
  emit_dot g
;;
