open Ast
open Graphk
open BuildGraph
open Control
open Util
open Enum_circ_HJ
open Solve

   
let kripk ast = 
  is_weel_formed  ast;
  List.map
    (
      fun (a,nodeliste,transitions,labels) ->
      Printf.printf "---------------------------Kripke Model : %s ---------------------------\n\n" a;
      Printf.printf " Nodes Set { ";
      KripkePrinter.auxNode (List.rev labels);
      Printf.printf "}\n";
      let g  = G.create () in
      let g' = GO.create () in
      let _ = create_graphe transitions nodeliste g g' in
      (** convertion des listes de string node en list **)
      let tab = tabassoc g' g in
      (** on construit la fonction d'etiquettage **)
      let labels =
	List.map
	  (fun (a,l') ->
	   let lst = List.map string_of_prop l' in
	   let node = string_of_node a in 
	   let node =  List.assoc node tab in
	   (node,lst)
	  ) labels in
      
  
      let (cycle,lengthloops,set_loop)  = enum_circ g' g (G.nb_vertex g) in
      let (k,beta,gamma,set, sourceOfTrans, sourceOfTransOfLoop) = mk_set_transAndLoop transitions cycle g in
      try
	is_flat g (G.nb_vertex g);
	print_string " la structure est plate  \n";
	(***On construit la liste de transitions de chaque cycle  **)
	let set_loop = buildtrLoop set_loop in 
	let sot = (List.map string_of_node sourceOfTrans) in
	let sot = List.map (fun x -> List.assoc x tab)  sot in
	let sol = List.map (fun x -> List.assoc x tab)  sourceOfTransOfLoop in
        
	(beta,gamma,k-1,set,lengthloops,labels ,sot, sol,set_loop)	     
      with Not_flate ->
	(
	  print_string " la structure n'est pas plate  \n";
	  exit(0)
	)
	  
    ) (extract ast)	      
    
