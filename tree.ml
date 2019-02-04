type tree = 
        | Abstraction of string * tree
	| Application of tree * tree
        | Variable of string;;