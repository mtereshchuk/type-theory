open Tree;;
open Buffer;;

let read_lines = 
  let lines = ref [] in
  try
    while true; do
      lines := (read_line ()) :: !lines
    done; []
  with End_of_file -> List.rev !lines;;

let to_string tree = 
  let buff = create 1000 in
  let rec fill_buff t = match t with
    | Abstraction (s, t) -> add_char buff '('; 
                      add_char buff '\\'; 
                      add_string buff s; 
                      add_string buff "."; 
                      fill_buff t; 
                      add_char buff ')';
    | Application (t1, t2) -> add_char buff '('; 
                       fill_buff t1; 
                       add_char buff ' '; 
                       fill_buff t2; 
                       add_char buff ')'
    | Variable s -> add_string buff s
  in fill_buff tree; 
  contents buff;;

print_string (
  to_string (
    Parser.main Lexer.main (
      Lexing.from_string (
        String.concat "\n" read_lines
      )
    )
  )
);;
print_newline ();;