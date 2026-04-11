# Struct: edge cases specification

Notation: `K(label, body)` is a keyed item/block, `P("text")` is a paragraph.
Answer each question inline (pick a letter, or write the desired AST).

## Basics

1. `- B: b`
   → `K(B, P("b"))`

2. ```
   - B:
     - b
   ```
   → `K(B, List[P(b)])`

3. `- B:` alone (no body, last in doc)
   → not keyed 

4. ```
   - B: b:
   - C: c
   ```
   → `K(B, Blocks[ K(b, List[ K(C, P("c")) ]) ] )` or `K(B, K(b, List[ K(C, P("c")) ]))`
   Explanation: 4 levels of nesting: B -> b -> C -> c.

## Chains — the big question

5. `- a: b: c` (no trailing colon)
   → `K(a, K(b, P("c")))` — chain of 2 labels + value

6. ```
   - a: b:
     - baz
   ```
   - (a) chain: `K(a, K(b, List[P(baz)]))`

7. ```
   - a: b: c:
     - baz
   ```
   → `K(a, K(b, K(c, List[baz])))` — chain of 3 labels + value

8. ```
   - a: b: c
   - x: y
   ```
   (no trailing colon anywhere)
   Does `- a: b: c` somehow claim `- x: y`, or are they just two independent siblings?
   **Answer:** no, they are just two independent siblings. The first sibling has a chain of 2 labels (a and b) and a value (c), while the second sibling has a label (x) and a value (y).

## Paragraphs (not list items)

9. Standalone paragraph `foo: bar` (no list marker)
   - (a) `K(foo, P("bar"))` — apply inline-value mode to paragraphs too
   - (b) leave paragraphs alone; inline-value mode only applies inside list items

   **Answer:** (a) is correct. But I'd like to have a configuration option to switch between the two behaviors if possible (skip this if it's too complicated for now)

10. ```
    foo: bar:
    - baz
    ```
    (paragraph with both inline value and trailing colon that absorbs following)
    Wrong answer: `K(foo, Blocks[P("bar"); List[baz]])` 
    Correct answer: `K(foo, Blocks[K(bar, List[baz])])` (two nested labels: foo and bar) or `K(foo, K(bar, List[baz]))`

## Value contents

11. `- foo: bar *baz* qux`
    Value has mixed inline. Keep as-is inside `P(...)` — value is unrestricted, unlike labels?
    **Answer:** Yes

12. `` - foo: `code: thing` ``
    Code span in value. Keyed with value `Code_span`?
    **Answer:** Yes, we should only key single inline units. Here we only have ONE keyed item: foo. "code" will not be keyed (current behavior)

13. `- foo: ` (trailing space)
    **Answer:** will not be keyed even if there's contiguous blocks in the next line. Because there's space after the colon. I.e., for cross-line absorption to happen, there must be no space after the colon.

14. `- foo:bar` (no space after colon)
    **Answer:** → not keyed. For single line splitting, we requires space after the colon. Comparing this to the cross-line absorption, we can say that we always require space or linebreak after the colon (but not a mix of space and linebreak). But how is this implemented is a different question.

15. `- http://x.com: click here`
    → `K(Text "http://x.com", P("click here"))`
    **Answer:** We require space after the colon or cross-line linebreak.

## Escapes

16. `- foo\: bar`
    **Answer:** label `foo` because the escaping is a bit tricky for commonmark. Cmarkit will escape the colon for us so we will only see `foo: bar` in the AST. For the actual escaping to happen, we need double backslash before the colon.

## Emphasis / labels

18. `- *foo*: bar`
    → `K(Emphasis foo, P("bar"))`

19. `- *foo* x: bar`
    → not keyed — label is mixed inline

20. `- foo: *bar* x`
    → keyed, value is free-form (no mixed-inline restriction on values)

## Trailing colon on last item

21. ```
    - a
    - b:
    text
    ```
    Does `b` absorb `text`? *(current: yes)*
    **Answer:** Yes

22. ```
    - a: x
    - b:
    text
    ```
    Same — does `b` absorb `text`?
    **Answer:** Yes
