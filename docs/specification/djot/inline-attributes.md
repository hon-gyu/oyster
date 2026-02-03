# Inline attributes

Attributes are put inside curly braces and must immediately follow the inline element to which they are attached (with no intervening whitespace).

Inside the curly braces, the following syntax is possible:

    .foo specifies foo as a class. Multiple classes may be given in this way; they will be combined.
    #foo specifies foo as an identifier. An element may have only one identifier; if multiple identifiers are given, the last one is used.
    key="value" or key=value specifies a key-value attribute. Quotes are not needed when the value consists entirely of ASCII alphanumeric characters or _ or : or -. Backslash escapes may be used inside quoted values.
    % begins a comment, which ends with the next % or the end of the attribute (}).

Attribute specifiers may contain line breaks.

Example:

An attribute on _emphasized text_{#foo
.bar .baz key="my value"}

<p>An attribute on <em class="bar baz" id="foo" key="my value">emphasized text</em></p>

Attribute specifiers may be “stacked,” in which case they will be combined. Thus,

avant{lang=fr}{.blue}

<p><span class="blue" lang="fr">avant</span></p>

is the same as

avant{lang=fr .blue}

<p><span class="blue" lang="fr">avant</span></p>
