// OysterMark-Core PEG Grammar (Peggy format)
//
// The subset of CommonMark that is PEG-compatible.
// Excludes emphasis delimiter runs that are both left-flanking and right-flanking.
//
// Linting rules (enforced outside, not in this grammar):
//   - Reject emphasis delimiter runs that are BOTH left-flanking AND right-flanking
//   - Reference links whose labels are not in the definitions

// ============================================================
// DOCUMENT
// ============================================================

document = block* !.

// ============================================================
// BLOCK STRUCTURE
// ============================================================

block
  = blank_lines
  / atx_heading
  / setext_heading
  / thematic_break
  / fenced_code_block
  / indented_code_block
  / block_quote
  / ordered_list
  / bullet_list
  / link_ref_definition
  / paragraph              // fallback: must be last

blank_lines = BLANK_LINE+

// ---- Thematic break ----
thematic_break
  = space03 (
      ("*" ws*)  ("*" ws*)  ("*" ws*)+
    / ("-" ws*)  ("-" ws*)  ("-" ws*)+
    / ("_" ws*)  ("_" ws*)  ("_" ws*)+
  ) NEWLINE

// ---- ATX Heading ----
atx_heading
  = space03 atx_marker (ws+ atx_heading_content atx_heading_close? / ws*) NEWLINE

atx_marker = $( "#" "#"? "#"? "#"? "#"? "#"? ) &{ return text().length <= 6; }

atx_heading_content
  = (!NEWLINE !(ws+ "#"+ ws* NEWLINE) .)*

atx_heading_close
  = ws+ "#"+ ws*

// ---- Setext Heading ----
setext_heading
  = setext_heading_line+ setext_underline

setext_heading_line
  = !thematic_break
    !atx_heading
    !fenced_code_fence_start
    !blockquote_marker
    !bullet_list_marker
    !ordered_list_marker
    !setext_underline
    space03 line_content NEWLINE

setext_underline
  = space03 ("="+ / "-"+) ws* NEWLINE

// ---- Indented Code Block ----
indented_code_block
  = indented_chunk (BLANK_LINE* indented_chunk)*

indented_chunk
  = (indent4 line_content NEWLINE)+

// ---- Fenced Code Block ----
fenced_code_block
  = fenced_code_fence_open fenced_code_body (fenced_code_fence_close / &{ return offset() >= input.length; })

fenced_code_fence_start
  = space03 ("```" "`"* / "~~~" "~"*)

fenced_code_fence_open
  = space03 fence_open_char info_string? NEWLINE

fence_open_char = $("```" "`"* / "~~~" "~"*)

info_string
  = ws* (!NEWLINE !"`" .)+ ws*

fenced_code_body
  = (!fenced_code_fence_close line_content NEWLINE)*

fenced_code_fence_close
  = space03 fence_close_char ws* NEWLINE

fence_close_char = $("```" "`"* / "~~~" "~"*)

// ---- Block Quote ----
block_quote = blockquote_line+

blockquote_marker = space03 ">" " "?

blockquote_line = blockquote_marker line_content NEWLINE

// ---- Lists ----
bullet_list = bullet_list_item+

bullet_list_marker = space03 ("-" / "+" / "*") (" " / "\t")+

bullet_list_item = bullet_list_marker list_item_content

ordered_list = ordered_list_item+

ordered_list_marker = space03 $([0-9] [0-9]? [0-9]? [0-9]? [0-9]? [0-9]? [0-9]? [0-9]? [0-9]?) ("." / ")") (" " / "\t")+

ordered_list_item = ordered_list_marker list_item_content

list_item_content
  = line_content NEWLINE list_continuation*

list_continuation
  = ws+ line_content NEWLINE
  / BLANK_LINE

// ---- Link Reference Definition ----
link_ref_definition
  = space03 link_label ":" ws* NEWLINE? ws* link_destination (ws+ link_title)? ws* NEWLINE

// ---- Paragraph ----
paragraph = paragraph_line+

paragraph_line
  = !BLANK_LINE
    !atx_heading
    !thematic_break
    !fenced_code_fence_start
    !blockquote_marker
    !bullet_list_marker
    !ordered_list_marker
    space03 line_content NEWLINE

// ============================================================
// INLINE STRUCTURE
// ============================================================

inline = inline_element*

inline_element
  = code_span
  / autolink
  / hard_line_break
  / backslash_escape
  / entity_ref
  / image
  / inline_link
  / full_reference_link
  / collapsed_ref_link
  / shortcut_ref_link
  / strong_star
  / strong_under
  / emph_star
  / emph_under
  / soft_line_break
  / plain_text

// ---- Code span ----
code_span = backtick_string code_span_body backtick_string

backtick_string = $( "`"+ )

code_span_body = (!backtick_string .)*

// ---- Line breaks ----
hard_line_break = ("  " " "* / "\\") NEWLINE ws*

soft_line_break = NEWLINE !BLANK_LINE ws*

// ---- Backslash escape ----
backslash_escape = "\\" ascii_punct

// ---- Entity reference ----
entity_ref = "&" (named_entity / hex_entity / decimal_entity) ";"

named_entity  = $( [a-zA-Z] [a-zA-Z0-9]|1..31| )
decimal_entity = $( "#" [0-9]|1..7| )
hex_entity     = $( "#" [xX] [0-9a-fA-F]|1..6| )

// ============================================================
// LINKS AND IMAGES
// ============================================================

inline_link
  = "[" link_text "]" "(" ws* link_destination (ws+ link_title)? ws* ")"

full_reference_link
  = "[" link_text "]" "[" link_label "]"

collapsed_ref_link
  = "[" link_text "]" "[]"

shortcut_ref_link
  = "[" link_label "]"

image
  = "!" "[" link_text "]" (
      "(" ws* link_destination (ws+ link_title)? ws* ")"
    / "[" link_label "]"
    / "[]"
  )

link_text = inline

link_label = $((!"]" !"[" !("\n" "\n") .)|1..999|)

link_destination = angle_link_destination / bare_link_destination

angle_link_destination = "<" (!">" !NEWLINE .)* ">"

bare_link_destination = bare_link_char+

bare_link_char
  = "(" bare_link_char* ")"
  / (!unicode_space !"(" !")" !">" .)

link_title
  = "\"" (!"\"" .)* "\""
  / "'"  (!"'"  .)* "'"
  / "("  (!")"  .)* ")"

// ============================================================
// AUTOLINKS
// ============================================================

autolink = "<" (uri_autolink / email_autolink) ">"

uri_autolink
  = [a-zA-Z] [a-zA-Z0-9]|0..31| "://" (!unicode_space !">" .)*

email_autolink
  = email_local_char+ "@" email_domain ("." email_domain)*

email_local_char
  = [a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]

email_domain = [a-zA-Z0-9-]+

// ============================================================
// EMPHASIS (CommonMark only)
// ============================================================

// ---- CommonMark star ----
strong_star = star_strong_open inline star_strong_close
emph_star   = star_emph_open  inline star_emph_close

star_strong_open  = "**" !unicode_space
star_strong_close = "**"
star_emph_open    = "*"  !"*" !unicode_space
star_emph_close   = !"*" "*"

// ---- CommonMark underscore ----
strong_under = under_strong_open inline under_strong_close
emph_under   = under_emph_open  inline under_emph_close

under_strong_open  = "__" !unicode_space
under_strong_close = "__"
under_emph_open    = "_"  !"_" !unicode_space
under_emph_close   = !"_" "_"

// ============================================================
// PLAIN TEXT
// ============================================================

plain_text = plain_char+

plain_char
  = !("*" / "_" / "`" / "[" / "]" / "!" / "<" / "&" / "\\" / "\n" / "\r") .

// ============================================================
// PRIMITIVES
// ============================================================

ws = " " / "\t"

NEWLINE = "\r\n" / "\n" / "\r"

BLANK_LINE = ws* NEWLINE

line_content = (!NEWLINE .)*

space03 = " "? " "? " "?

indent4 = "    "

ascii_punct
  = [!"#$%&'()*+,\-./:;<=>?@\[\\\]^_`{|}~]

unicode_space = [ \t\n\r\x0C\x0B]
