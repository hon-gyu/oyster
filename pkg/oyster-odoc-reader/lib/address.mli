(** Note paths and anchor identifiers for a converted odoc page. *)

(** [note url] is the vault-relative note path for the odoc page at [url],
    without the [.md] extension: [oystermark/Vault/Index]. Modules nest as
    directories, mirroring odoc's own page tree, and each component carries
    odoc's disambiguating [kind-] prefix so that a module and a module type of
    the same name do not collide. *)
val note : Odoc_document.Url.Path.t -> string

(** [anchor a] is the odoc anchor [a] reduced to the characters an oystermark
    identifier admits. A ['.'], as in odoc's field and constructor anchors
    ([type-t.field]), would read as a class separator, so every character
    outside \[A-Za-z0-9_-\] becomes ['-']. *)
val anchor : string -> string

(** [attribute a] is the djot attribute line introducing the block anchored at
    [a]. When {!anchor} had to rewrite the identifier, the odoc original is
    kept verbatim in an [odoc-anchor] key. *)
val attribute : string -> string
