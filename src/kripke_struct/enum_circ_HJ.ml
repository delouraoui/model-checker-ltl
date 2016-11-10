open Graphk
open Graph

       
let nVertices = ref 0
let inity n = nVertices := n    		   	      
let nCircuits = ref 0 (** number of circuits *)		    
let lenLongest = ref 0
let blocked = ref (Hashtbl.create (!nVertices+1))
(* graph portions that yield no elementary circuit *)
let b = ref (Hashtbl.create (!nVertices+1))
(* list to accumulate the circuits found  *)
let lengthHistogram = ref (Array.make (!nVertices+1) 0)(*  histogram circuits by length*)
let vertexPopularity = ref (Array.make_matrix (!nVertices+1) !nVertices 0) 
let longestCircuit = ref [] 
let enumeration = ref true
let setL = ref []		      
let setlength = ref []
let setC = ref []
let stack = Stack.create () 

		      
let rec list_of_stack' s l =
  try
    list_of_stack' s ((Stack.pop s)::l)
  with Stack.Empty -> (l)
			
let list_of_stack s =
  list_of_stack' s []
		 
let rec unblock u =
  Hashtbl.replace !blocked u false;
  try
    List.iter (fun w ->
	       let l = (Hashtbl.find !b u) in
	       let lclean = List.filter (fun v' ->not (GO.V.equal v' w)) l in
	       Hashtbl.replace !b u lclean;			     			  
	       if Hashtbl.find !blocked w then 
		 unblock w;
	       
	      ) (Hashtbl.find !b u)
  with Not_found -> ()
let print tab =
  let l = list_of_stack (Stack.copy stack) in
  List.iter (fun v ->
	     Printf.printf "  [%s] " (List.assoc v tab)			   
	    ) (l)
let rec circuit v start g tab =
  begin 
    let f = ref false in 
    Stack.push v stack;
    Hashtbl.replace !blocked v true;
    (** for all neighboor of v we do *)
    GO.iter_succ (
	fun w ->
	if ((GO.V.label w) >= (GO.V.label start) ) then
	  begin
	    if (GO.V.equal w start) then
	      begin
		Printf.printf " Cycle from : ";
		print tab;
		let  lof = (list_of_stack (Stack.copy stack)) in
		setC := (start,lof) :: !setC;
		setL :=(start,List.hd (lof)):: (!setL);
		setlength := (List.length lof):: (!setlength);
		Printf.printf " [%s] \n " (List.assoc start tab);
		let top = Stack.length stack in
		!lengthHistogram.(top) <- !lengthHistogram.(top) + 1;
		nCircuits := !nCircuits + 1;
		if(top > !lenLongest) then
		  begin 
		    lenLongest := top;
		    longestCircuit := list_of_stack (Stack.copy stack)
		  end;
		Stack.iter
		  (fun vx ->
		   !vertexPopularity.(top).(GO.V.label vx) <- (!vertexPopularity.(top).(GO.V.label vx) + 1)) stack;
		f := true;
	      end
	    else if not (Hashtbl.find !blocked w) then
	      if (circuit w start g tab) then
		f := true	 
	  end
      ) g v;
    if !f then
      unblock v
    else
      GO.iter_succ (fun w ->
		    if ((GO.V.label w) >= (GO.V.label start) ) then 
		      begin 
			let ltmpb = Hashtbl.find !b  w in
			try 
			  if (List.mem v ltmpb) then
			    ()
			with Not_found ->
			  Hashtbl.replace !b w (v::ltmpb)
		      end 
		   ) g v;
    let _ = Stack.pop stack in ();
    !f
  end 
let tabofnum g g' =
  let num = G.fold_vertex (fun v acc ->
	        	   let couple = (G.V.label v) in
			   (couple::acc) ) g' [] in
  GO.fold_vertex (fun v acc ->
		  let lab = GO.V.label v in
		  let str = (List.assoc lab num) in
		  ((v,str)::acc) ) g []
		 
let initBase n =
  blocked := (Hashtbl.create (0)) ;
  (* graph portions that yield no elementary circuit *)
  b:= (Hashtbl.create (0));
  (* list to accumulate the circuits found  *)
  lengthHistogram := (Array.make (n+1) 0) ;(*  histogram circuits by length*)
  vertexPopularity := (Array.make_matrix (n+1) n 0) ;
  nVertices := n


		 
		 
let rec init g =
  Hashtbl.reset !blocked;
  Hashtbl.reset !b;
  Stack.clear stack;
  setL := []; 
  GO.iter_vertex ( fun v -> 
		   Hashtbl.add !blocked v false;
		   Hashtbl.add !b v []
		 ) g
let rec clear g =
  GO.iter_vertex ( fun v -> 
		   Hashtbl.replace !blocked v false;
		   Hashtbl.replace !b v []
		 ) g
let getLisOne l tab =
  List.map (fun (source,target) ->
	    ((List.assoc source tab),(List.assoc target tab)) ) l
let coer : (GO.V.label) -> int = fun v -> v
			    

	   
let enum_circ g g' n =
  initBase (n+1) ;
  init g;
  let tab = (tabofnum g g') in
  Printf.printf " Transitions  : \n ";
  GO.iter_edges (fun a b  ->
		  let stra = (List.assoc a tab) in
		  let strb = (List.assoc b tab) in
		  Printf.printf " (%d,%s) --> (%d,%s) \n " (GO.V.label a) stra (GO.V.label b) strb;
		) g;
  Printf.printf " Loop  : \n ";
 
  GO.iter_vertex
    (fun v ->
     clear g;
     let _ = circuit v v g tab in
     ()
    ) g;
  
  let set_loop l =
    let (start,l) = l in 
    coer (GO.V.label start) , (List.map (fun x ->
	       (coer (GO.V.label x)) 
	      ) l) in
  let set_loop = List.map set_loop !setC in
 (getLisOne !setL tab,!setlength,set_loop)

