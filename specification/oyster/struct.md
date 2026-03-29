# 
```
- foo:

bar
```
bar will not be child of foo. foo will have no child because the next element is empty


#
````md
- foo:
```
bar
```
````

the codeblock that contains bar will be a child of foo

expected: this will generate different tree hierarhcy as ordinary commonmark

#
```
foo:
- bar
- baz

bee
```

- the list bar and baz will be the children of foo
- bee will be not
- A rule that "cannot contain hardbreak"?


# Goal: An OLOG can be extracted from a list

How to declare path equilvalence?

---

```mli
(* Disallow single-line nesting*)
module type Struct1 : sig
    type t = {
		name: string
	    body: string list
	    children: t list
    }
	
	(* no children *)
	val is_leaf : t -> bool
end
	
(* Allow single-line nesting (flow-style) *)
module type Struct2 : sig
    type t = {
		name: string
	    body: body list
	    children: t list
    }
	
	type body = Leaf of string | Def of t
end

(* Allow `->` to specify process. Also `( -> )` is of higher priority than `( : )`  *)
module type Struct3 : sig
    type process = string list
    type t = {
		name: string
	    body: body list
	    children: t list
    }
	type body = Leaf of process | Def of t
end



val parse_flow : Struct1 -> Struct2
```



- A: stuff
	- : B
	- C: D
	- E: F
		- G: H
		- : I


- A: stuff: T: \[x, y, z]
	- : B
	- C: D
	- E: F
		- G: H
		- : I

- the first line
	- in Struct1
		- `{name: "A", body: "stuff: T: [x, y, z]", children: ...}`
	- in Struct2
		- `{name: "A", body: Def {name: "stuff", body: {...}}, children: ...}`



A -> B -> C
