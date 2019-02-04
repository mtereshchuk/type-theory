%{
  open Tree;;
%}
%token <string> VARIABLE
%token LPAREN RPAREN
%token SLASH DOT
%token EOF
%start main
%type <Tree.tree> main
%%

main:
        expr EOF           		{ $1 }
expr:
	SLASH VARIABLE DOT expr         { Abstraction ($2, $4) }
	| app SLASH VARIABLE DOT expr 	{ Application ($1, Abstraction ($3, $5)) }
	| app				{ $1 }
app:
	app atom			{ Application ($1, $2) }
	| atom				{ $1 }
atom:
	LPAREN expr RPAREN		{ $2 }
	| VARIABLE			{ Variable ($1) }
