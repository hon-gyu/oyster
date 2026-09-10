(** Rendering one odoc page into oystermark.

    The odoc constructs that have no markup counterpart are handled as follows.

    - A signature stays a fenced code block, because a fence's content is
      literal and cannot hold a link. The cross-references odoc resolved inside
      it are collected and emitted as wikilinks on a [Refs:] line beneath the
      fence, so the edges reach the vault graph even though their position
      within the signature does not survive.
    - A member with its own anchor and documentation
      ([Odoc_document.Types.DocumentedSrc.Documented]) keeps its code line
      inside that same fence; its anchor and prose follow the fence as a keyed
      item, since no anchor can attach to a line inside a code block.
    - An odoc tag arrives as a [Description] whose key splits into a tag name
      and an optional argument, which is the label chain a keyed node wants:
      [- parameter: `x`: ...].
    - An expansion becomes a note of its own, whatever odoc's inline
      preference was, and the fence that held it collapses to [sig ... end]. *)

type t =
  { path : string (** the note's vault-relative path, without [.md] *)
  ; body : string
  ; subpages : Odoc_document.Types.Page.t list
    (** expansions that each become a note of their own *)
  }

val page : Odoc_document.Types.Page.t -> t
