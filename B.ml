open Tree;;
open Buffer;;

let to_string tree = 
    let buff = create 1000 in
    let rec fill_buff t = match t with
        | Abstraction (s, t) -> add_string buff ("(\\" ^ s ^ "."); fill_buff t; add_char buff ')';
        | Application (t1, t2) -> add_char buff '('; fill_buff t1; add_char buff ' '; fill_buff t2; add_char buff ')';
        | Variable s -> add_string buff s;
    in fill_buff tree; 
    contents buff;
;;

module SS = Set.Make(String);;

let extract_free_vars tree =
    let rec extract_free_vars_rec t busy = match t with
        | Abstraction (s, t) -> extract_free_vars_rec t (SS.add s busy);
        | Application (t1, t2) -> SS.union (extract_free_vars_rec t1 busy) (extract_free_vars_rec t2 busy);
        | Variable s -> if (SS.mem s busy) = false then SS.singleton s else SS.empty;
    in extract_free_vars_rec tree SS.empty;
;;

let rec replace tree x a = match tree with
    | Abstraction (s, t) -> if s = x then tree else Abstraction (s, replace t x a);
    | Application (t1, t2) -> Application (replace t1 x a, replace t2 x a);
    | Variable s -> if s = x then a else tree;
;;

let modify s map = if Hashtbl.mem map s 
    then Hashtbl.find map s 
    else let new_val = s ^ (string_of_int (Random.int 100)) in
        Hashtbl.add map s new_val;
        new_val
;;

let check_modify s not_free_vars vars map = if (SS.mem s vars) && (SS.mem s not_free_vars) then modify s map else s;;

let rec refactor tree not_free_vars vars map = 
    match tree with
    | Abstraction (s, t) -> 
        let new_s = check_modify s (SS.add s not_free_vars) vars map in 
        Abstraction (new_s, refactor t (SS.add s not_free_vars) vars map);
    | Application (t1, t2) -> Application (refactor t1 not_free_vars vars map, refactor t2 not_free_vars vars map);
    | Variable s -> Variable (check_modify s not_free_vars vars map);
;;

let rec reduce_once tree = match tree with
    | Abstraction (s, t) -> Abstraction (s, reduce_once t);
    | Application (t1, t2) -> (match t1 with
        | Abstraction (s_, t_) -> 
            let map = Hashtbl.create 100 in
            let vars = extract_free_vars t2 in
            let x = check_modify s_ (SS.singleton s_) vars map in
            let refactored = refactor t_ (SS.singleton s_) vars map in
            replace refactored x t2;    
        | Application (t1_, t2_) -> Application (reduce_once t1, reduce_once t2);
        | Variable s -> Application (t1, reduce_once t2);
    );
    | Variable s -> tree;
;;

let rec has_redex tree = match tree with
    | Abstraction (s, t) -> has_redex t;
    | Application (t1, t2) -> (match t1 with
        | Abstraction (s_, t_) -> true;
        | Application (t1_, t2_) -> (has_redex t1) || (has_redex t2);
        | Variable s -> has_redex t2;
    );
    | Variable s -> false;
;;

let rec reduce tree = (*print_string ((to_string tree) ^ "\n");*) if has_redex tree then reduce (reduce_once tree) else tree;;

let start = Sys.time () in
print_string ((to_string (reduce (Parser.main Lexer.main (Lexing.from_string (read_line ()))))) ^ "\n");
print_float (Sys.time () -. start); print_char '\n';;