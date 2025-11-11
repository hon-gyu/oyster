- Note in Obsidian cannot have # ^ [ ] | in the heading.
### Level 3 title
#### Level 4 title

### Example (level 3)

###### Markdown link: `[x](y)`
- percent encoding: [Three laws of motion 11](Three%20laws%20of%20motion.md)
- heading  in the same file:  [Level 3 title](#Level%203%20title)
	- `[Level 3 title](#Level%203%20title)`
- different file heading [22](Note%202#Some%20level%202%20title)
	- `[22](Note%202#Some%20level%202%20title)`
	- the heading is level 2 but we don't need to specify it
- empty link 1 [www]()
	- empty markdown link `[]()` points to note `().md`
- empty link 2 [](ww)
	- `[](ww)`
	- points to note `ww`
- empty link 3 []()
	- `[]()`
	- points to note `()`
- empty link 4 [](Three%20laws%20of%20motion.md)
	- `[](Three%20laws%20of%20motion.md)`
	- points to note `Three laws of motion`
	- the first part of markdown link is displayed text and doesn't matter

###### Wiki link: `[[x#]]` | `[[x#^block_identifier]]`
- basic: [[Three laws of motion]]
- explicit markdown extension in name: [[Three laws of motion.md]]
- with pipe for displayed text: [[Note 2 | Note two]]
- heading in the same note: [[#Level 3 title]]
	- `[[#Level 3 title]]`
- nested heading in the same note: [[#Level 4 title]]
	- `[[#Level 4 title]]`
- invalid heading in the same note: [[#random]]
	- `[[#random]]`
	- fallback to note
- heading in another note: [[Note 2#Some level 2 title]]
	- `[[Note 2#Some level 2 title]]`
- nested heading in another note: [[Note 2#Some level 2 title#Level 3 title]]
- invalid heading in another note: [[Note 2#random#Level 3 title]]
	- fallback to note if the heading doesn't exist 
- heading in another note: [[Note 2#Level 3 title]]
- heading in another note: [[Note 2#L4]]
- nested heading in another note: [[Note 2#Some level 2 title#L4]]
	- when there's multiple levels, the level doesn't need to be specified
	- it will match as long as the ancestor-descendant relationship holds
- non-existing note: [[Non-existing note 4]]
- empty link: [[]]
	- points to current note
- empty heading: [[#]]
	- `[[#]]` points to current note
- incorrect heading level
	- `[[#######Link to figure]]`: [[#######Link to figure]]
	- `[[######Link to figure]]`: [[######Link to figure]]
	- `[[####Link to figure]]`: [[####Link to figure]]
	- `[[###Link to figure]]`: [[###Link to figure]]
	- `[[#Link to figure]]`: [[#Link to figure]]
 - ambiguous pipe and heading: [[#L2 | #L4]]
	 - `[[#L2 | #L4]]`
	 - points to L2
	 - things after the pipe is escaped 
- incorrect nested heading 
	- `[[###L2#L4]]`:  [[###L2#L4]]
		- points to L4 heading correctly
	- `[[##L2######L4]]`: [[##L2######L4]]
		- points to L4 heading correctly
	- `[[##L2#####L4]]`: [[##L2#####L4]]
		- points to L4 heading correctly
	- `[[##L2#####L4#L3]]`: [[##L2#####L4#L3]]
		- fallback to current note
	- `[[##L2#####L4#L3]]`: [[##L2#####L4#Another L3]]
		- fallback to current note
	- for displayed text, the first hash is removed, the subsequent nesting ones are not affected
- ↳ it looks like whenever there's multiple hash, it's all stripped. only the ancestor-descendant relationship matter
- I don't think there's a different between Wikilink and Markdown link
	- `[1](##L2######L4)`: [1](##L2######L4)
		- points to L4 heading correctly
	- `[2](##L2#####L4)`: [2](##L2#####L4)
		- points to L4 heading correctly
	- `[3](##L2#####L4#L3)`: [3](##L2#####L4#L3)
		- fallback to current note

##### Link to figure
- `[[Figure 1.jpg]]`: [[Figure 1.jpg]]
	- even if there exists a note called `Figure 1.jpg`, the asset will take precedence
- `[[Figure 1.jpg.md]]`: [[Figure 1.jpg.md]]
	- with explicit `.md` ending, we seek for note `Figure 1.jpg`
- `[[Figure 1.jpg.md.md]]`: [[Figure 1.jpg.md.md]]
- ↳ when there's `.md`, it's removed and limit to the searching of notes

![[Figure 1.jpg]]

[[empty_video.mp4]]

## L2

### L3
#### L4
### Another L3

---
## 
