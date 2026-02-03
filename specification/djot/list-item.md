The following basic types of list markers are available:

| Marker | List type                                                |
| ------ | -------------------------------------------------------- |
| -      | bullet                                                   |
| +      | bullet                                                   |
| \*     | bullet                                                   |
| 1.     | ordered, decimal-enumerated, followed by period          |
| 1)     | ordered, decimal-enumerated, followed by parenthesis     |
| (1)    | ordered, decimal-enumerated, enclosed in parentheses     |
| a.     | ordered, lower-alpha-enumerated, followed by period      |
| a)     | ordered, lower-alpha-enumerated, followed by parenthesis |
| (a)    | ordered, lower-alpha-enumerated, enclosed in parentheses |
| A.     | ordered, upper-alpha-enumerated, followed by period      |
| A)     | ordered, upper-alpha-enumerated, followed by parenthesis |
| (A)    | ordered, upper-alpha-enumerated, enclosed in parentheses |
| i.     | ordered, lower-roman-enumerated, followed by period      |
| i)     | ordered, lower-roman-enumerated, followed by parenthesis |
| (i)    | ordered, lower-roman-enumerated, enclosed in parentheses |
| I.     | ordered, upper-roman-enumerated, followed by period      |
| I)     | ordered, upper-roman-enumerated, followed by parenthesis |
| (I)    | ordered, upper-roman-enumerated, enclosed in parentheses |
| :      | definition                                               |
| - [ ]  | task                                                     |

Ordered list markers can use any number in the series: thus, (xix) and v) are both valid lower-roman-enumerated markers, and v) is also a valid lower-alpha-enumerated marker.
Task list item

A bullet list item that begins with [ ], [X], or [x] followed by a space is a task list item, either unchecked ([ ]) or checked ([X] or [x]).
Definition list item

In a definition list item, the first line or lines after the : marker is parsed as inline content and taken to be the term defined. Any further blocks included in the item are assumed to be the definition.

```djot
: orange

A citrus fruit.
```

```html
<dl>
<dt>orange</dt>
<dd>
<p>A citrus fruit.</p>
</dd>
</dl>
```
