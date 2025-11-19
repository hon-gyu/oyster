paragraph 1 ^paragraph

paragraph with `code` inside ^p-with-code

paragraph 2

^paragraph2

- some list
	- item 1
	- item 2

^fulllist

| Col 1  | Col 2  |
| ------ | ------ |
| Cell 1 | Cell 2 |
| Cell 3 | Cell 4 |

^table

> quotation

^quotation

[[#^quotation]]


> [!info] this is a info callout

^callout

[[#^callout]]


---

reference
- paragraph: [[#^paragraph]]
- paragraph with code: [[#^p-with-code]]
- separate line caret: [[#^paragraph2]]
	- for paragraph the caret doesn't need to have a blank line before and after it
- table: [[#^table]] 

---

- a nested list ^firstline
	-  item
	  ^inneritem
- inside a list
	- [[#^firstline]]: points to the first line
	-  [[#^inneritem]]: points to the first inner item



---
######  Edge case: a later block identifier invalidate previous one

| Col 1  | Col 2  |
| ------ | ------ |
| Cell 1 | Cell 2 |
| Cell 3 | Cell 4 |

^tableref

- this works fine [[#^tableref]]


| Col 1  | Col 2  |
| ------ | ------ |
| Cell 1 | Cell 2 |
| Cell 3 | Cell 4 |

^tableref2

^tableref3

- now the above table can only be referenced by [[#^tableref3]]
- [[#^tableref2]] is invalid and will fallback to the whole note

######  Edge case: the number of blank lines before identifier doesn't matter

this
^works

[[#^works]]

- 1 blank line after the identifier is required
- however, 0-n blank line before the identifier works fine
	- for clarity, we should always require at least 1 blank line before the identifier (so that the identifier won't be parsed as part of the previous struct)

######  Edge case: full reference to a list make its inner state not refereceable

- a nested list ^firstline
	-  item
	  ^inneritem
- inside a list
	- [[#^firstline]]: points to the first line
	-  [[#^inneritem]]: points to the first inner item

- a nested list ^firstline1
	-  item
	  ^inneritem1
^fulllist1

- inside a list
	- [[#^firstline1]]: this now breaks and fallback to the full note
	-  [[#^inneritem1]]: this now breaks and fallback to the full note
	- [[#^fulllst1]]: points to the full list


######  Edge case: When there are more than one identical identifiers

Obsidian doesn't guarantee to points to the first one 