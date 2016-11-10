open Ast

(* Module printer permet d'afficher une structure de kripke déjà parsé *)
let rec interpPrint = function 
  | KripDef(l)->
     itersubexp l

and itersubexp = function
  | [] -> print_string "} \n"
  | ((Id id),e)::q ->
     print_string "kripke ";print_string id;print_string "{\n";
     iterlexp e;itersubexp q
and iterlexp = function
  | [] -> print_string "} \n"
  | e::q ->
     print_string "kripke ";print_string "{\n";
     interpsub e;iterlexp q
			  
and interpsub = function 
  | Nodel l ->  auxNode l;print_string "\n"
  | AtmProp l -> showlistprop l;print_string "\n"
  | Actionl l -> showlistprop l;print_string "\n"
  | InitNode i -> showNode i;print_string "\n"
  | Transl l -> auxTrans l ;print_string "\n"

and auxTrans = function
  | [] -> print_string ""
  | (e1,a,e2)::q -> print_string "<";showNode e1;print_string ", "; showProp a;print_string ","; showNode e2;print_string ">"; auxTrans q

and auxNode = function 
  | [] -> print_string ""
  | (e,[])::q ->
     print_string "<";showNode e;print_string "";print_string "> "; auxNode q                   
  | (e,lp)::q ->
     print_string "< Node : ";showNode e;print_string ", AP : "; showlistprop lp;print_string " > "; auxNode q
                                                                                              

and showlistprop = function 
  | [] -> print_string ""
  | [a] -> showProp a;print_string ""; showlistprop []
  | a::q -> showProp a;print_string ","; showlistprop q

and showProp = function
  | Prop a -> print_string a

and showNode = function
  | Nodek a -> print_string a

