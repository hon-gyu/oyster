(** LSP-facing adapter for vault-aware renaming.

    The rename engine and byte-edit contract live in
    {!Oystermark.Vault.Rename}. This module only translates the target found
    at an editor position into that library contract. *)

open Core

type edit = Oystermark.Vault.Rename.edit =
  { rel_path : string
  ; first_byte : int
  ; last_byte : int
  ; new_text : string
  }
[@@deriving sexp, equal, compare]

let valid_note_name = Oystermark.Vault.Rename.valid_note_name
let renamed_note_path = Oystermark.Vault.Rename.renamed_note_path

let plan_target ~index ~docs ~read_file target ~new_name =
  Oystermark.Vault.Rename.plan ~index ~docs ~read_file target ~new_name
;;

let rename ~index ~docs ~read_file ~rel_path ~content ~line ~character ~new_name () =
  Find_references.detect_target ~index ~rel_path ~content ~line ~character
  |> Option.value_map ~default:[] ~f:(fun target ->
    match plan_target ~index ~docs ~read_file target ~new_name with
    | Error _ -> []
    | Ok change -> change.edits)
;;

module For_test = struct
  let rename = rename
end
