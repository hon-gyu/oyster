# Block Identifier Specification

Block identifiers mark referenceable blocks:

```markdown
(* Inline at end of paragraph *)
Some paragraph text ^blockid

(* Inline at end of list item *)
- List item text ^blockid

(* Separate line after block - references previous block *)
| Table |
| ----- |
| Cell  |

^tableid

(* Inline in nested list *)
- Parent item ^parentid
    - Child item ^childid
```

### Block Identifier Syntax
```ebnf
block_marker = "^" , block_id ;
block_id     = ( letter | digit ) , { letter | digit | "-" } ;
