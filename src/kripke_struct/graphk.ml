open Graph
open Sys
open Format
open Rand
exception Not_flate     

       
module V = struct
    type t =  (int * string)
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
  end
module E = struct
    type t = string
    let compare = Pervasives.compare
    let default = ""
  end
	     
module G = Imperative.Digraph.ConcreteBidirectionalLabeled(V)(E)
module GO = Pack.Digraph
		
let length l = List.length l
let ( -| ) x l = List.mem x l
let  f f l = List.filter f l
let show = Graph.Pack.Digraph.display_with_gv					 

let rec computesucc g t beforeview v =  function
  | 0 -> []
  | n ->
    G.succ g v
     |>  f (fun x -> not ( x -| beforeview) )  
     |> (fun bef -> 
      List.fold_left
        (fun l noeud' ->
	  (*Printf.printf " voisin [%s] atteind en %d coup \n"
			      (snd (G.V.label v)) (t-n);*)
	  noeud'::(computesucc g t (beforeview@bef) noeud' (n-1))@l
        ) [] bef
	)
let is_flat' g taille =
  G.iter_vertex
    (fun v ->
     let pred = (G.pred g v) in
     if  length pred >= 2 then
       begin
	 (*Printf.printf " \n En partant du noeud [%s] : \n" (snd (G.V.label v)) ;*)
	 (computesucc g (taille+1) [v] v taille)
	 |> f (fun x ->  (x -| pred) ) 
	 |> (fun x ->
	     if (length x) >= 2 then
	       raise Not_flate
	    )
       end	 
    ) g


let is_flat g taille =		    
  is_flat' g taille
    
