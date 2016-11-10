open Graph
open Ast
open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.Goal
open Z3.Probe
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer

  
exception NotInList

(* Section de fonctions utiles *)
let string_of_node = function Nodek (id) -> id
let string_of_prop = function Prop (id) -> id					    
let equals_node (e1:Ast.nodek) (e2:Ast.nodek) = match e1,e2 with 
  | Nodek a , Nodek b -> 
    if String.compare a b = 0 then true 
    else false

let equals_label e1 e2 = match e1,e2 with 
  | (a,q1) , (b,q2)  -> equals_node a b

let rec assoc_in_list (node:Ast.nodek) = function
  | [] -> raise NotInList
  | a::q when (equals_node a node)  = true -> true
  | a::q  -> assoc_in_list node q


(** gestion des variables **)
			   


let fresh_p k = ( "p"^ (string_of_int k)  )
let fresh_t k = ( "z"^ (string_of_int k) ^"t" )
let fresh_zp k = ( "z"^ (string_of_int k) ^"p" )
		      
let fresh_it k = ( "z"^ (string_of_int k) ^"it" )

(** 
We make a set of tuples of variables which contains one element of each set Zt = {zt1...ztn} Zp = {zp1...zpn} Zit = {zit1...zitn}
and i which means a step in formula. 
 *)
let rec mk_varlist acc ctx = function
  | 0 -> 
     let zp = Expr.mk_const ctx (mk_string ctx (fresh_zp 0))  (Integer.mk_sort ctx) in
     let zt = Expr.mk_const ctx (mk_string ctx (fresh_t 0))  (Integer.mk_sort ctx) in
     let zit = Expr.mk_const ctx (mk_string ctx (fresh_it 0))  (Integer.mk_sort ctx) in
     (0,zp,zt,zit):: acc
  | n ->
     let zp = Expr.mk_const ctx (mk_string ctx (fresh_zp n))  (Integer.mk_sort ctx) in
     let zt = Expr.mk_const ctx (mk_string ctx (fresh_t n))  (Integer.mk_sort ctx) in
     let zit = Expr.mk_const ctx (mk_string ctx (fresh_it n))  (Integer.mk_sort ctx) in
     mk_varlist ((n,zp,zt,zit)::acc) ctx (n-1)
		      

let rec mk_varlist_pos acc ctx = function
  | 0 -> 
     let p = Expr.mk_const ctx (mk_string ctx (fresh_p 0))  (Integer.mk_sort ctx) in
     (0,p)::(acc)
  | n ->
     let p = Expr.mk_const ctx (mk_string ctx (fresh_p n))  (Integer.mk_sort ctx) in
     (**Printf.printf "i: %d | p : %s \n "
                   n (Expr.to_string p) ;**)
     mk_varlist_pos ((n,p)::acc) ctx (n-1)

		    
let mk_globals_args ctx n =
  let lz = mk_varlist [] ctx n in
  let lp = mk_varlist_pos [] ctx n in
  List.fold_right2 (
      fun (i',p) (i,zp,zt,zit) l ->
     (*Printf.printf "i: %d | p : %s| zp : %s| zt : %s | zit : %s \n "
                    i' (Expr.to_string p) (Expr.to_string zp) (Expr.to_string zt) (Expr.to_string zit);*)
      (i',p,zp,zt,zit)::l
    ) lp lz []

			   
let getZp l = List.map (fun (i,p,zp,zt,zit) -> zp) l
let getZt l = List.map (fun (i,p,zp,zt,zit) -> zt) l
let getZit l = List.map (fun (i,p,zp,zt,zit) -> zit) l
let get_schema_args l = List.map (fun (i,p,zp,zt,zit) ->  (i,zp,zt,zit)) l
let getP l = List.map (fun (i,p,zp,zt,zit) -> (i,p) ) l


			   
(* extrait le code depuis un fichier *)
let lit name fichier = 
  let buf = open_in name in 
  try
    while true do
      fichier := (!fichier)^ (input_line (buf) )
    done
  with End_of_file -> ()  

exception TestFailedException of string

let saveSmtTest str name =
  let out = open_out name in
  output_string out str;
  close_out out
            
let check_bench ctx l name =
  let smt = Z3.SMT.benchmark_to_smtstring ctx "" "LIA" "" ""  [] (l) in
  saveSmtTest smt name
      
   


				   
let afficher ctx l name =
  Printf.printf "Running Z3 version %s\n" Version.to_string ;
  Printf.printf " Satisfaisabilité  :  " ;
  let g = (Z3.Goal.mk_goal ctx true false false) in
  (Goal.add g [l]) ;
(
  let solver = (mk_solver ctx None) in
  
  (List.iter (fun a -> (Solver.add solver [a])) (get_formulas g) ) ;
  if (check solver []) != SATISFIABLE then
    (
      Printf.printf " -!!- Unsat -!!- \n\n";
      let smt = Z3.SMT.benchmark_to_smtstring ctx "" "LIA" "" ""  [] (l) in
      (**Printf.printf "%s\n" ("Solver: " ^ (Solver.to_string solver)) ;**)
      saveSmtTest smt name;
      
    )
  else(
    let smt = Z3.SMT.benchmark_to_smtstring ctx "" "LIA" "" ""  [] (l) in
    (**Printf.printf "%s\n" ("Solver: " ^ (Solver.to_string solver)) ;**)
    saveSmtTest smt name;
    Printf.printf " -||- Sat -||- \n\n"
  )
) 

let check_not ctx l =
  let g = (Z3.Goal.mk_goal ctx true false false) in
  (Goal.add g [l]) ;
(
  let solver = (mk_solver ctx None) in
  
  (List.iter (fun a -> (Solver.add solver [a])) (get_formulas g) ) ;
  if (check solver []) != SATISFIABLE then
    (
      Boolean.mk_true ctx
      
    )
  else(
    Boolean.mk_false ctx
  )
) 			


