- Note in Obsidian cannot have # ^ [ ] | in the title.
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
- empty heading:
	- `[[#]]`: [[#]] 
		- points to current note
	- `[[Note 2##]]`:  [[Note 2##]]
		- points to Note 2
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
 - multiple pipe: [[Note 2 | 2 | 3]]
	 - `[[Note 2 | 2 | 3]]`
	 - this points to Note 2
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

##### Link to asset
- `[[Figure1.jpg]]`: [[Figure1.jpg]]
	- even if there exists a note called `Figure1.jpg`, the asset will take precedence
- `[[Figure1.jpg#2]]`: [[Figure1.jpg#2]]
	- points to image
- `[[Figure1.jpg | 2]]`: [[Figure1.jpg | 2]]
	- points to image
	- leading and ending spaces are stripped
- `[[Figure1.jpg.md]]`: [[Figure1.jpg.md]]
	- with explicit `.md` ending, we seek for note `Figure1.jpg`
- `[[Figure1.jpg.md.md]]`: [[Figure1.jpg.md.md]]
- `[[Figure1#2.jpg]]`: [[Figure1#2.jpg]]
	- understood as note and points to note Figure1 (fallback to note after failing finding heading)
- `[[Figure1|2.jpg]]`: [[Figure1|2.jpg]]
	- understood as note and points to note Figure1 (fallback to note after failing finding heading)
- `[[Figure1^2.jpg]]`: [[Figure1^2.jpg]]
	- points to image
- ↳ when there's `.md`, it's removed and limit to the searching of notes
- `[[dir/]]`: [[dir/]]
	- BUG
	- when clicking it, it will create `dir` note if not exists
	- create `dir 1.md` if `dir` exists
	- create `dir {n+1}.md` if `dir {n}.md` exists
	- I guess the logic is:
		- there's no file named `dir/`, Obsidian try to create a note
		- it removes `/` and `\`
		- if there exists one, it add integer suffix
- matching of nested dirs only match ancestor-descendant relationship
	- `[[dir/inner_dir/note_in_inner_dir]]`: [[dir/inner_dir/note_in_inner_dir]]
	- `[[inner_dir/note_in_inner_dir]]`: [[inner_dir/note_in_inner_dir]]
	- `[[dir/note_in_inner_dir]]`: [[dir/note_in_inner_dir]]
	- ↳ all points to the same note
	- `[[random/note_in_inner_dir]]`: [[random/note_in_inner_dir]]
		- this has no match
		- it will try to understand the file name and path
		- mkdir and touch file (in contrast to the case of `dir/`)
- `[[dir/indir_same_name]]`: [[dir/indir_same_name]]
- `[[indir_same_name]]`: [[indir_same_name]]
	- points to `indir_same_name`, not the in dir one
-  `[[indir2]]`: [[indir2]]
	- points to `dir/indir2`
- `[[Something]]`: [[Something]]
	- there exists a `Something` file, but this will points to note `Something.md`
- `[[unsupported_text_file.txt]]`: [[unsupported_text_file.txt]]
	- points to text file, which is of unsupported format
- `[[a.joiwduvqneoi]]`: [[a.joiwduvqneoi]]
	- points to file
- `[[Note 1]]`: [[Note 1]]
	- even if there exists a file named `Note 1`, this points to the note

`![[Figure1.jpg]]`: ![[Figure1.jpg]]
`[[empty_video.mp4]]`: [[empty_video.mp4]]

## L2

### L3
#### L4
### Another L3

---
## 
