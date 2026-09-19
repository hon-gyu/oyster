open Core
module Node = Oystermark.Note.Node
module Query = Oystermark.Note.Query
module Parse = Oystermark.Parse
module Vault = Oystermark.Vault

let load_vault (root : string) : Vault.t = Vault_fs.of_root_path ~skip_expand:true root
let (vault_param : string Command.Param.t) = Command.Param.(anon ("vault" %: string))

let links (index : Vault.Index.t) : (string * Note.Link.t * Vault.Index.resolution) list =
  Vault.Index.notes index
  |> List.concat_map ~f:(fun note ->
    let source = Vault.Index.Entry.path note in
    Vault.Index.Entry.links note
    |> List.map ~f:(fun link ->
      source, link, Vault.Index.resolve index source link.reference))
;;
