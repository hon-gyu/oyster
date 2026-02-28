(** Oystermark — Obsidian-flavored CommonMark.

    Extends cmarkit with Obsidian wikilinks and block identifiers. *)

module Wikilink = Wikilink
module Block_id = Block_id

(** [of_string ?strict ?layout s] parses markdown string [s] into a cmarkit
    Doc with wikilinks and block IDs resolved via the mapper. *)
val of_string : ?strict:bool -> ?layout:bool -> string -> Cmarkit.Doc.t

(** The mapper that transforms a cmarkit Doc, resolving wikilinks in inline
    text nodes and block identifiers at paragraph ends. *)
val mapper : Cmarkit.Mapper.t
