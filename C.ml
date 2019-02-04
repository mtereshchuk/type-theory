open Tree;;

type expression = 
    | Impl of expression * expression
    | Var of string;;
type equation = Eq of expression * expression;;
exception Incompatible;;

let cnt = ref 0;;
let next_var () = cnt := !cnt + 1; Var ("t" ^ string_of_int !cnt);;
let contains map tree = Hashtbl.mem map tree;;
let set map str expr = Hashtbl.add map str expr;;
let get map str = Hashtbl.find map str;;
let set_next_if_not_contains map tree = if not (contains map tree) then set map tree (next_var ());;
let get_and_set_rand_if_not_contains map tree = set_next_if_not_contains map tree; get tree;;

let rec print_expr expr = match expr with
    | Var x -> print_string x;
    | Impl (a, b) -> print_char '('; print_expr a; print_string "->"; print_expr b; print_char ')';
;;

let print_eq (Eq (l, r)) = print_expr l; print_string " = "; print_expr r; print_char '\n'
;;

let print_system system = 
    List.iter print_eq system; print_string "\n";
;;

let build_system tree = 
    let rec build_rec tree map = match tree with
    | Abstraction (x, p) ->
        Hashtbl.remove map x;
        Hashtbl.add map x (next_var ());
        let (p_sys, p_type) = build_rec p map in
        let x_type = Hashtbl.find map x in
        (p_sys, Impl (x_type, p_type));
    | Application (p, q) -> 
        let (p_sys, p_type) = build_rec p map in
        let (q_sys, q_type) = build_rec q map in
        let cur_type = next_var () in
        (Eq (p_type, Impl (q_type, cur_type)) :: (p_sys @ q_sys), cur_type);
    | Variable x -> if not (Hashtbl.mem map x) then Hashtbl.add map x (next_var ()); 
        (*if x = "x" then (print_expr (Hashtbl.find map x); print_char '\n');*)
        ([], Hashtbl.find map x);
    in
    let map = Hashtbl.create 100 
    in let (system, t) = build_rec tree map in
    (*print_expr t; print_char '\n';
    print_expr (Hashtbl.find map "x"); print_char '\n';
    print_expr (Hashtbl.find map "y"); print_char '\n';*)
    system;
;;

let is_incompatible_system system =
    let rec contains_var expr var = match expr with
    | Impl (a, b) -> (contains_var a var) || (contains_var b var);
    | Var x -> expr = var;
    in 
        let is_incompatible_equation (Eq (l, r)) = match l with
        | Impl (a, b) -> false;
        | Var x -> match r with 
            | Var y -> false;
            | Impl (c, d) -> contains_var r l;
        in 
            List.exists (fun eq -> is_incompatible_equation eq) system;
;;

let is_solved_system system = 
    let is_solved_equation (Eq (l, r)) = match l with
    | Impl (a, b) -> false;
    | Var x -> true;
    in 
        if List.for_all is_solved_equation system
        then 
            let left_parts = List.map (fun (Eq (l, r)) -> l) system in
            let map_ = Hashtbl.create 100 in
            if List.for_all (fun var -> if Hashtbl.mem map_ var then false else (Hashtbl.add map_ var true; true)) left_parts 
            then 
                let rec right_part_contains expr = match expr with
                | Var _ -> List.mem expr left_parts;
                | Impl (a, b) -> (right_part_contains a) || (right_part_contains b);
                in List.for_all (fun (Eq (l, r)) -> not (right_part_contains r)) system;
            else false;
        else false;
;;

let can_do_action_1 system = 
    let is_reversed (Eq (l, r)) = match l with
    | Var _ -> false;
    | Impl (_, _) -> match r with
        | Var _ -> true;
        | Impl (_, _) -> false;
    in List.exists is_reversed system
;;

let action_1 system = 
    (*print_string "action 1\n";*)
    let reverse (Eq (l, r)) = match l with
    | Var _ -> Eq (l,r);
    | Impl (_, _) -> match r with
        | Var _ -> Eq (r, l);
        | Impl (_, _) -> Eq (l, r);
    in List.map reverse system
;;

let can_do_action_2 system = List.exists (fun (Eq (l, r)) -> l == r) system;;

let action_2 system = 
    (*print_string "action 2\n";*)
    List.filter (fun (Eq (l, r)) -> l != r) system
;;

let can_do_action_3 system = 
    let can_do_reduction (Eq (l, r)) = match l with
    | Var _ -> false;
    | Impl (_, _) -> match r with
        | Var _ -> false;
        | Impl (_, _) -> true;
    in
    List.exists can_do_reduction system;
;;

let action_3 system =
    (*print_string "action 3\n";*)
    let can_do_reduction (Eq (l, r)) = match l with
    | Var _ -> false;
    | Impl (_, _) -> match r with
        | Var _ -> false;
        | Impl (_, _) -> true;
    in
        let result = ref (List.filter (fun eq -> not (can_do_reduction eq)) system) in
        let reduction_part = List.filter can_do_reduction system in
        let extract_2_eqs (Eq (Impl (a, b), Impl (c, d))) = Eq (a, c) :: [Eq (b, d)] in
        let func eq = result := (extract_2_eqs eq) @ !result in
        List.iter func reduction_part;
        !result;
;;
    
let action_4 system = 
    (*print_string "action 4\n";*)
    let map_ = Hashtbl.create 100 in
    List.iter (fun (Eq (l, r)) -> Hashtbl.add map_ l 0) system;
    let rec walk expr = match expr with
    | Impl (a, b) -> walk a; walk b;
    | Var _ -> if Hashtbl.mem map_ expr
        then (let value = Hashtbl.find map_ expr in
            Hashtbl.remove map_ expr;
            Hashtbl.add map_ expr (value + 1))
    in List.iter (fun (Eq (l, r)) -> walk l; walk r) system;
    (*Hashtbl.iter (fun var cnt -> print_expr var; print_char ' '; print_int cnt; print_char '\n') map_;*)
    let main_var = ref (Var "") in
    Hashtbl.iter (fun var cnt -> if cnt > 1 then main_var := var) map_;
    let Eq(l_, r_) = List.find (fun (Eq (l, r)) -> l = !main_var) system in
    (*print_string "chosed : "; print_eq (Eq(l_, r_));*)
    let rec replace expr x f = match expr with
    | Impl (a, b) -> Impl (replace a x f, replace b x f);
    | Var _ -> if expr = x then f else expr; 
    in
    Eq (l_, r_) :: (List.map (fun (Eq (l, r)) -> Eq (replace l l_ r_, replace r l_ r_)) system);
;;

let rec solve_system system =
    (*print_system system;*)
    if is_incompatible_system system then raise Incompatible;
    if is_solved_system system then system
    else if can_do_action_1 system then solve_system (action_1 system)
    else if can_do_action_2 system then solve_system (action_2 system)
    else if can_do_action_3 system then solve_system (action_3 system)
    else solve_system (action_4 system)
    ;;
;;

open Buffer;;

let to_string tree = 
    let buff = create 1000 in
    let rec fill_buff t = match t with
        | Abstraction (x, p) -> add_string buff ("(\\" ^ x ^ "."); fill_buff p; add_char buff ')';
        | Application (p, q) -> add_char buff '('; fill_buff p; add_char buff ' '; fill_buff q; add_char buff ')';
        | Variable x -> add_string buff x;
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

let main tree = 
    try
        let system = build_system tree in
        (*print_system system;*)
        let solution = solve_system system in
        (*print_system solution;*)
        let solution_map = Hashtbl.create 1000 in
        List.iter (fun (Eq (l, r)) -> Hashtbl.add solution_map l r) solution;

        let rec replace expr = match expr with
        | Impl (a, b) -> Impl (replace a, replace b);
        | Var x -> if Hashtbl.mem solution_map expr then Hashtbl.find solution_map expr else expr;
        in

        let rec string_of_expr expr = match expr with
        | Var x -> x;
        | Impl (a, b) -> "(" ^ string_of_expr a ^ "->" ^ string_of_expr b ^ ")";
        in
        
        let rec print_context list = match list with
        | [x, t] -> print_string (x ^ " : " ^ (string_of_expr (replace t)) ^ " ");
        | (x, t) :: rem -> print_string (x ^ " : " ^ (string_of_expr (replace t)) ^ ", "); print_context rem;
        | [] -> ();
        in

        let rec get_type tree map = match tree with
        | Abstraction (x, p) ->
            Hashtbl.remove map x;
            Hashtbl.add map x (next_var ());
            let p_type = get_type p map in
            let x_type = Hashtbl.find map x in
            Impl (x_type, p_type);
        | Application (p, q) -> 
            get_type p map;
            get_type q map;
            let cur_type = next_var () in
            cur_type;
        | Variable x -> if not (Hashtbl.mem map x) then Hashtbl.add map x (next_var ()); 
            Hashtbl.find map x;
        in

        let rec walk_print tree depth context map = 
            for i = 1 to depth do print_string "*   " done;
            
            let old_cnt = !cnt in
            let tmp_map = Hashtbl.copy map in
            let cur_type = get_type tree tmp_map in
            cnt := old_cnt;

            let free_vars = SS.elements (extract_free_vars tree) in
            let new_context_ref = ref [] in 
            List.iter 
                (
                    fun s -> 
                    if not (List.exists (fun (x, _) -> String.equal x s) context) 
                    then new_context_ref := (s, Hashtbl.find tmp_map s) :: !new_context_ref
                ) 
            free_vars;
            let new_context = context @ !new_context_ref in

            print_context new_context;
            print_string "|- ";
            print_string ((to_string tree) ^ " : ");
            print_string (string_of_expr (replace cur_type));
            (*print_string (" {" ^ (string_of_expr cur_type) ^ "}");*)
            print_string " ";

            match tree with
            | Abstraction (x, p) -> Hashtbl.remove map x; Hashtbl.add map x (next_var ()); 
                if not (List.exists (fun (a, _) -> String.equal a x) new_context) 
                then new_context_ref := (x, Hashtbl.find tmp_map x) :: new_context 
                else new_context_ref := new_context;
                print_string "[rule #3]\n"; walk_print p (depth + 1) !new_context_ref map;
            | Application (p, q) -> print_string "[rule #2]\n"; walk_print p (depth + 1) new_context map; walk_print q (depth + 1) new_context map;
                cnt := !cnt + 1;
            | Variable x -> if not (Hashtbl.mem map x) then Hashtbl.add map x (next_var ()); print_string "[rule #1]\n";
        
        in
        cnt := 0;
        walk_print tree 0 [] (Hashtbl.create 100);
    with Incompatible -> print_string "Expression has no type\n";
;;

(*let start = Sys.time () in*)
main (Parser.main Lexer.main (Lexing.from_string (read_line ())));
(*print_float (Sys.time () -. start); print_char '\n';;*)