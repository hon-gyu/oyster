(** {0 Struct: colon-keyed tree restructuring}

    {1 Specification}

    A {b keyed node} is a list item or paragraph whose inline content
    carries a colon-delimited label / value relationship.  There are
    two mutually exclusive forms:

    {ul
    {- {b Trailing-colon form.}  The inline ends with an unescaped
       [:] (no space after).  The labels are all the segments
       separated by [: ] (colon-space).  The body comes from the
       node's sub-blocks, or — if there are none — from content that
       is {e absorbed} from the surrounding context (see
       {{!section:restructuring}Tree restructuring}).}
    {- {b Inline-value form.}  The inline does {e not} end with a
       colon, but contains at least one [: ] split.  The last
       segment is the {b value} (unrestricted free-form inline); all
       preceding segments are labels.  Unlike the trailing-colon
       form, this node does {e not} absorb following content.}}

    In both forms, each {b label} segment must be a single inline
    unit: pure text, emphasis, strong emphasis, or code span.  Mixed
    content — e.g. emphasis followed by text in the same segment —
    disqualifies the entire decomposition.  The {b value} segment in
    the inline-value form has no such restriction.

    For cross-line absorption (trailing-colon form), there must be no
    whitespace between the colon and the line break.  For inline
    splits (both forms), there must be exactly [": "] — a colon
    immediately followed by a space.  [- foo:bar] (no space) and
    [- foo: ] (trailing space) are both non-keying.

    {2 Label detection}

    A trailing colon is detected by walking the inline tree rightward:
    follow the last child of each [Inlines] container until a [Text]
    leaf is reached.  If that leaf's raw content ends with [:]
    (no trailing whitespace), the node is keyed in trailing-colon
    form.

    Only [Text] and [Inlines] nodes are traversed.  Emphasis, code
    spans, links, images, raw HTML, breaks, and extension inlines are
    {b opaque} — a colon inside them does not participate in
    keying.

    {ul
    {- [foo bar:] → trailing-colon, label [Text "foo bar"].}
    {- [*foo*:] → trailing-colon, label [Emphasis "foo"].}
    {- [**foo**:] → trailing-colon, label [Strong_emphasis "foo"].}
    {- [*foo* bar:] → {b not keyed}.  Mixed content.}
    {- [*foo:*] → {b not keyed}.  Emphasis is opaque.}
    {- [`code:`] → {b not keyed}.  Code spans are opaque.}
    {- [foo: bar] → inline-value, label [Text "foo"], value
       [Text "bar"].}
    {- [foo: `code: thing`] → inline-value, label [Text "foo"],
       value [Code_span "code: thing"] (code spans inside values are
       free-form).}}

    {2 Escaped colons}

    A backslash immediately before the trailing colon in the {e parsed}
    inline text (i.e. [Text] node content) suppresses keying.

    Because CommonMark already consumes one level of backslash
    escaping ([\\:] in source becomes [\:] in the AST), write [\\\\:]
    in source to get [\\:] in the AST — which struct treats as a
    literal colon.

    More precisely, the number of consecutive backslashes immediately
    before the colon is counted.  An odd count means the colon is
    escaped; an even count means the backslashes pair up and the colon
    is structural.

    {2 Colon chains}

    Chains combine with both forms.  [a: b: c:] yields three labels
    in trailing-colon form; [a: b: c] yields two labels and a
    value [c] in inline-value form.

    {ul
    {- [- foo: bar:] with body [baz] →
       [Keyed_list_item "foo" (Keyed_list_item "bar" (... baz ...))].}
    {- [- a: b: c] (no trailing colon, no sub-blocks) →
       [Keyed_list_item "a" (Keyed_list_item "b" (Paragraph "c"))].}
    {- [- *foo*: bar:] with body [baz] →
       [Keyed_list_item (Emphasis "foo") (Keyed_list_item "bar" (...))].}
    {- [- http://example.com:] → single label ["http://example.com"].
       The [:] after [http] has no trailing space, so no split.}}

    {2:restructuring Tree restructuring rules}

    {ol
    {- {b Keyed list item with indented content.}  The indented
       sub-blocks become the body of an {!Ext_keyed_list_item}.
       Applies to both forms; in the inline-value form the value
       paragraph is prepended to the body.}
    {- {b Keyed list item followed by a blank line.}  No
       transformation — the trailing colon is treated as literal
       punctuation.}
    {- {b Keyed list item followed by contiguous blocks.}  For
       trailing-colon form only, unindented blocks immediately after
       the list are reparented under the last item as an
       {!Ext_keyed_list_item}.}
    {- {b Middle-item absorption.}  A non-last list item whose
       paragraph has a bare trailing colon (no inline value, no
       sub-blocks) absorbs all remaining sibling items of the same
       list as a nested list under its label.}
    {- {b Keyed paragraph.}  In trailing-colon form, a paragraph
       ending with [:] claims all immediately following contiguous
       blocks (no blank-line separation) as children, producing an
       {!Ext_keyed_block}.  In inline-value form, the paragraph is
       rewritten to an {!Ext_keyed_block} whose body is the value
       paragraph; no following content is absorbed.  The
       inline-value rewrite on paragraphs is gated by the
       [paragraph_inline_value] parameter of {!rewrite_doc}.}
    {- {b Nesting.}  Keyed nodes nest: a keyed paragraph can contain
       a list whose items are themselves keyed.}}

    {1 Parsing}

    Parsing is a single-pass rewrite on the already-parsed Cmarkit
    AST.  [decompose] classifies each candidate inline into one of
    the two forms, and the sibling-block walker in [Rewrite] applies
    the restructuring rules above. *)

include Struct_common
module For_test = Struct_for_test
