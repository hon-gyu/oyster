(** CSS class name constants shared between HTML emitters and theme CSS.

    Every class that appears in rendered HTML should be defined here so that
    both the emitting code and the stylesheet reference the same string. *)

(* Layout *)
let layout : string = "layout"
let sidebar : string = "sidebar"
let sidebar_handle : string = "sidebar-handle"
let sidebar_collapsed : string = "sidebar-collapsed"
let page_title : string = "page-title"

(* Lightbox *)
let lightbox : string = "lightbox"
let lightbox_close : string = "lightbox-close"

(* Content *)
let frontmatter : string = "frontmatter"
let callout : string = "callout"
let callout_title : string = "callout-title"
let callout_content : string = "callout-content"
let embed : string = "embed"
let unresolved : string = "unresolved"

(* Components *)
let backlinks : string = "backlinks"
let backlink_context : string = "backlink-context"
let breadcrumb : string = "breadcrumb"
let sep : string = "sep"

(* Struct (keyed blocks) — variant info lives in data-* attributes, not classes. *)
let keyed : string = "keyed"
let keyed_label : string = "keyed-label"
let keyed_body : string = "keyed-body"
