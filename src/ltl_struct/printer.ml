open Ltlast


let rec to_string exp =
  let print_paren exp =
    match exp with
      | True | False | Atom(_) | Not(_) | X(_) | F(_) | G(_)
          -> to_string exp
      | _ -> "(" ^ (to_string exp) ^ ")"
  in
  match exp with
    | True           -> "⊤"
    | False          -> "⊥"
    | Atom(p)        -> p
    | Not(exp)       -> "¬" ^ (print_paren exp)
    | And(l, r)      -> (print_paren l) ^ " ∧ " ^ (print_paren r)
    | Or(l, r)       -> (print_paren l) ^ " ∨ " ^ (print_paren r)
    | X(exp)      	 -> "X " ^ (print_paren exp)
    | F(exp)   		 -> "F " ^ (print_paren exp)
    | G(exp)  		 -> "G " ^ (print_paren exp)
    | U(l, r)      	 -> (print_paren l) ^ " U " ^ (print_paren r)

let interpPrint a =
  Printf.printf "--------------------------- LTL Formula  ------------------------------\n\n";
  print_string "====> ";print_string (to_string a);print_string "\n\n"
