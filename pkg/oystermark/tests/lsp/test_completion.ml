(** Spec: {!page-"feature-completion"}.
    Impl: {!Lsp_lib.Completion}. *)

open Core
open Lsp_helper

let vault_root =
  let cwd = Core_unix.getcwd () in
  Filename.concat cwd "data"
;;

(* complete-src.md line 2 is "See [[anchor-target#]]"; the [#] is at character
   19, so character 20 is inside the fragment. anchor-target.md has heading
   "Target" and the inline attribute anchor [{#key-term}], so both — the
   heading slug and the attribute id — are offered.
   See {!page-"feature-attribute-anchors"}. *)
let%expect_test "e2e: fragment completion offers heading and attribute id" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"complete-src.md";
  let response = completion s ~rel_path:"complete-src.md" ~line:2 ~character:20 in
  let items = parse_completion_result response in
  List.iter items ~f:(fun (label, insert) -> printf "%s -> %s\n" label insert);
  shutdown s;
  [%expect
    {|
    Target -> target
    #key-term -> key-term
    |}]
;;
