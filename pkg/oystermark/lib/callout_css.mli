(** Expand Obsidian-style callout CSS into browser-ready CSS.

    Obsidian lets users define custom callout types with two CSS custom properties:

    {v
    .callout[data-callout="llm"] {
      --callout-color: 127, 134, 193;
      --callout-icon: lucide-bot-message-square;
    }
    v}

    Browsers cannot render these as-is — [--callout-icon] is not a real CSS
    property, and bare [r, g, b] is not a valid color value.  {!expand}
    post-processes the assembled CSS string to produce working rules:

    {ul
    {- Each [--callout-icon: lucide-{i name}] declaration is replaced by a
       companion [{i selector} .callout-title::before] rule with
       [-webkit-mask-image] / [mask-image] set to an inline SVG data URI.}
    {- Bare comma-separated RGB values (e.g. [127, 134, 193]) in
       [--callout-color] are wrapped in [rgb(...)].}
    {- Valid CSS colors ([var(...)], [#hex], [rgb(...)]) pass through
       unchanged.}}

    {1 Adding a new icon}

    To support a new Lucide icon, add a case to {!lucide_icon_body} with the
    icon name (including the [lucide-] prefix) and its SVG body — the elements
    inside the [\<svg\>...\</svg\>] tag, using {b single-quoted} attributes.
    The SVG wrapper and percent-encoding are handled automatically.

    {1 Supported icons}

    {v
    lucide-pencil               note
    lucide-info                 info
    lucide-flame                tip / hint / important
    lucide-clipboard-list       abstract / summary / tldr
    lucide-circle-check         todo
    lucide-check                success / check / done
    lucide-circle-help          question / help / faq
    lucide-triangle-alert       warning / caution / attention
    lucide-x                    failure / fail / missing
    lucide-zap                  danger / error
    lucide-bug                  bug
    lucide-list                 example
    lucide-quote                quote / cite
    lucide-bot-message-square   llm
    lucide-square-chevron-right prompt
    v}

    Unknown icon names emit a CSS comment ([/* unknown icon: ... */]) and no
    mask-image rule, so the callout degrades gracefully (text only, no icon). *)

(** [lucide_icon_body name] returns the inner SVG elements for the Lucide icon
    [name] (e.g. ["lucide-info"]), or [None] if the icon is not in the
    built-in table. *)
val lucide_icon_body : string -> string option

(** [svg_data_uri body] wraps raw SVG elements in a full [\<svg\>] tag and
    returns a percent-encoded [url("data:image/svg+xml,...")] string ready for
    use in CSS [mask-image] declarations. *)
val svg_data_uri : string -> string

(** [expand css] post-processes a CSS string, expanding every
    [--callout-icon: lucide-{i name}] declaration into a [-webkit-mask-image] /
    [mask-image] rule and wrapping bare RGB color triples in [rgb(...)].
    Non-callout CSS blocks pass through unchanged. *)
val expand : string -> string
