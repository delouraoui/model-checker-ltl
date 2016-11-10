open Ast
open Util
       
(* Permet de verifier si un noeud à préalablement été déclaré *)
let is_declared (node:Ast.nodek) liste = 
  try
    assoc_in_list node liste
  with NotInList
     | _ -> let message = "the node "^(string_of_node node)^" has not declared" in
	    Printf.printf "%s" message;exit(0)

let rec cert_trans nodeListe = function
  | [] -> true 
  | (a,w,b)::q -> 
     (is_declared a nodeListe) && (is_declared b nodeListe) && (cert_trans nodeListe q)

let cert nodeListe = function
  | Transl l -> 
     if cert_trans nodeListe l == true then (**print_string "good\n"**) ()
     else print_string "bad\n"
  | _ -> ()

(*teste qu'il n'éxiste pas deux même noeuds dans la déclaration des noeuds *)
let rec not_two_same nodeListe = 
  (List.for_all (fun a -> not (List.exists (fun b -> a!=b && (equals_label a b) ) nodeListe) ) nodeListe) 

(* Fonction d'evalutation principale d'une structure de kripke *)
let rec is_weel_formed = function
  | KripDef(l) ->
     compute_env l
		 
and compute_env = function   
  | [] -> ()
  | (a,subexp)::q ->
     compute_env' [] subexp;
     compute_env q
		 
and compute_env' env = function   
  | [] -> ()
  | subexp::q ->
     let env'  = (extract_node subexp) in 
     evaluate env subexp;
     compute_env' (env'@env) q
		  
and extract_node = function
  | Nodel l -> 
     if (not_two_same l) =true then 
       liste_Node [] l 
     else
       begin
	 Printf.printf "there are two same node declared";
	 exit(0)
       end
  | _ -> []

and extract_node_label = function
  | Nodel l -> 
     if (not_two_same l) =true then 
       liste_Node_label [] l 
     else
       begin
	 Printf.printf "there are two same node declared";
	 exit(0)
       end
  | _ -> []
	   
and liste_Node acc = function 
  | [] -> acc
  | (e,lp)::q -> liste_Node (e::acc) q
			    
and liste_Node_label acc = function 
  | [] -> acc
  | (e,lp)::q -> liste_Node_label ((e,lp)::acc) q

and evaluate env a = 
  cert env a
