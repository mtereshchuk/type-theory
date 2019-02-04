{
open Parser
}

let variable = ['a' - 'z'] ['a' - 'z' '0' - '9' '\'']*
let ws = [' ' '\n' '\r' '\t']+

rule main = parse
        | variable as v { VARIABLE(v) }
        | "\\"          { SLASH }
        | "."           { DOT }
        | "("           { LPAREN }
        | ")"           { RPAREN }
        | eof           { EOF }  
	| ws            { main lexbuf }
