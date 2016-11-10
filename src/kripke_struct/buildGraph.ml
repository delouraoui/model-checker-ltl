open Ast
open Util
open Control
open Graphk


(** Extraction de la structure de kripke en structure de graphe*)
let get_labels lstNode =
  List.flatten (List.map extract_node_label lstNode)
       
 let rec extract = function 
  | KripDef(l) ->
     compute_expresions l
			
and compute_expresions = function
  | [] -> []
  | ((Id a),subexp)::q ->
     let env  = (compute_expresion subexp) in
     let labels = get_labels subexp in 
     (a,env,extract_trans subexp,labels)::(compute_expresions q) 
				     
and compute_expresion = function   
  | [] -> []
  | subexp::q ->
     (extract_node subexp)@compute_expresion q

and extract_trans = function
  | [] -> []
  | a::q -> 
     match a with
     | Transl l -> l@(extract_trans q)
     | _ -> extract_trans q

       
let labi = ref 1

let rec btl start = function
  | [] -> []
  | a::q ->
     (start,a)::(btl a q)
	       
let buildtrLoop l =
  List.map
    (fun (start,l) -> 
     btl start l
    ) l

	       
let index_cycles cycles =
  List.mapi  (fun i (source,target) ->
	      (1,(i+1),source,target)) cycles
	     
let index_transitions transition =
  List.mapi  (fun i (source,e,target) ->
	       (0,(i+1),source,target)) transition
	       
let trans_set indexedTransitions =
  (List.fold_left
       (fun l (k,i,source,target) ->
	((0,i,source,target),
	 (List.filter (fun (k',j,source',target') ->
		       (string_of_node target) = (string_of_node source') )
		      indexedTransitions))::l    
       )
       [] indexedTransitions )

let trans_in_loops indexedTransitions indexedCycles =
    (List.fold_left
       (fun l (k,i,source,target) ->
	((0,i,source,target),
	 (List.filter ( fun (k',j,source',target') ->
			(string_of_node target) = source'  
		      ) indexedCycles )):: l    
       ) [] indexedTransitions )
      
let loops_in_trans indexedTransitions indexedCycles =
   (List.fold_left
       (fun l (k,i,source,target) ->
	((1,i,source,target),
	 (List.filter ( fun (k',j,source',target') ->
			target = (string_of_node source')  
		      ) indexedTransitions ))::l    
       ) [] indexedCycles )
     
let loops_in_loop indexedCycles =
   (List.fold_left
       (fun l (k,i,source,target) ->
	((1,i,source,target),
	 (List.filter ( fun (k',j,source',target') ->
			(target) = ( source')  
		      ) indexedCycles ))::l    
       ) [] indexedCycles )
     
let source_of_trans indexedTransitions =
  List.map (fun (_,i,source,target) -> source) indexedTransitions 

let source_of_trans_of_loop indexedCycles  =
  List.map (fun (_,i,source,target) -> source) indexedCycles 
	   
let mk_set_transAndLoop transitions cycles g =
  
  let indexedTransitions =
    index_transitions transitions in

  let indexedCycles =
    index_cycles cycles in 
  
  let settrans =
    trans_set indexedTransitions  in
  
  let transInLoop =
    trans_in_loops indexedTransitions indexedCycles in
  
  let loopInTrans =
    loops_in_trans indexedTransitions indexedCycles in

  let loopInLoop =
    loops_in_loop indexedCycles in
  
  let setOfTrans =
    List.flatten
      (List.map  (fun ((a,b,_,_),l) ->
		  (List.map (fun (a',b',_,_) ->
			     (a,b,a',b')) l ) )
		 settrans) in
  let s2 =
    List.flatten
      (List.map  (fun ((a,b,_,_),l) ->
		  (List.map (fun (a',b',_,_) ->
			     (a,b,a',b')) l ) )
		 transInLoop) in
  let s3 =
    List.flatten
      (List.map  (fun ((a,b,_,_),l) ->
		  (List.map (fun (a',b',_,_) ->
			     (a,b,a',b')) l ) )
		 loopInTrans) in
  let s4 =
    List.flatten
      (List.map  (fun ((a,b,_,_),l) ->
		  (List.map (fun (a',b',_,_) ->
			     (a,b,a',b')) l ) )
		 loopInLoop) in
  
  let sourceOfTrans =
    source_of_trans indexedTransitions in 

  let sourceOfTransOfLoop =
    source_of_trans_of_loop indexedCycles in

  let setOfLoop = s2@s3@s4 in

  let s = setOfTrans @ setOfLoop  in
  (**  number of transitions **)
  let beta = List.length indexedTransitions in
  (** number of loops **)
  let gamma = List.length indexedCycles in
  (** size of path schema **)
  let k = beta + gamma + 1 in			   
  (k,beta,gamma, s, sourceOfTrans, sourceOfTransOfLoop)

    
let rec create_graphe transitions nodeliste g g' =
  let dict =
    List.mapi
      (fun i e ->
       let str = (string_of_node e) in
       (e,(G.V.create ((i+1),str) ))
      )
      (List.rev nodeliste) in
  let dict' =
    List.map (fun (e,v) ->	       
	      (e,(GO.V.create (fst (G.V.label v)) ) ))  dict in
  
  let m = List.iter (fun (a,e,b) ->
	     let va' = (List.assoc a dict') in
	     let vb' = (List.assoc b dict') in
	     let va = (List.assoc a dict) in
	     let vb = (List.assoc b dict) in
	     let edgeG = G.E.create va (string_of_int !labi) vb in
	     let edgeGo = GO.E.create va' (!labi) vb' in
	     labi := !labi + 1;
	     G.add_edge_e g edgeG;
	     GO.add_edge_e g' edgeGo;
	    ) transitions    in
  
  m

let coer :(GO.V.label) -> (int ) = fun v -> v 

	    
let tabassoc g g' =
  let num = G.fold_vertex (fun v acc ->
	        	   let couple = (G.V.label v) in
			   (couple::acc) ) g' [] in
  GO.fold_vertex (fun v acc ->
		  let lab = GO.V.label v in
		  let str = (List.assoc lab num) in
		  ((str,coer lab)::acc) ) g []
