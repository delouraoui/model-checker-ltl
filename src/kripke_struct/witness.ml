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


          
let in_loop ctx j q0 z2t z2p source_of_k_loop =
 
  (**ztj+1 = 1 *)
  let e1 = Boolean.mk_eq ctx z2t (Integer.mk_numeral_i ctx 1)  in
  (**zpj+1 = j *)
  let e2 = Boolean.mk_eq ctx z2p (Integer.mk_numeral_i ctx j) in
  (**q0 = source(loop(j)) *)
  let e3 = Boolean.mk_eq ctx (Integer.mk_numeral_i ctx source_of_k_loop) q0 in
  
  (** ztj+1 = 0 /\ zpj+1 = j  /\ q0 = source(loop(j)) **)
  Boolean.mk_and ctx [e1;e2;e3] 

                   
let in_trans ctx j q0 z2t z2p source_of_k_trans =
  
  (**ztj+1 = 0 *)
  let e1 = Boolean.mk_eq ctx z2t (Integer.mk_numeral_i ctx 0)  in
  (**zpj+1 = j *)
  let e2 = Boolean.mk_eq ctx z2p (Integer.mk_numeral_i ctx j) in
  (**q0 = source(δ(j)) *)
  let e3 = Boolean.mk_eq ctx (Integer.mk_numeral_i ctx source_of_k_trans) q0 in
  (** ztj+1 = 0 /\ zpj+1 = k  /\ q0 = source(δ(j)) **)
  Boolean.mk_and ctx [e1;e2;e3]
                
let rec loop_state ctx acc q0 z2p z2t loop_set_of_source = function
  | 0 -> acc
  | j ->
     (** source(loop(j)) *)
     let source_of_k_loop = List.nth loop_set_of_source (j-1) in
     (** z1t = 1 /\ z1p = j  /\ q0 = source(loop(j)) **)
     let loop = in_loop ctx j q0 z2t z2p source_of_k_loop in
     (*Printf.printf "j :%d source(δ(j) : %d %s \n" j source_of_k_loop (Expr.to_string loop);*)
     loop_state ctx (loop::acc) q0 z2p z2t loop_set_of_source (j-1)
                
let rec trans_state ctx acc q0 z2p z2t trans_set_of_source = function
  | 0 -> acc
  | j ->
     (** source(δ(j)) *)
     let source_of_k_trans = List.nth trans_set_of_source (j-1) in
     
     (** z1t = 0 /\ z1p = j  /\ q0 = source(δ(j)) **)
     let trans = in_trans ctx j q0 z2t z2p source_of_k_trans in
     
                   
     trans_state ctx (trans::acc) q0 z2p z2t trans_set_of_source (j-1)
                 
let rec loop_pos ctx acc pi pim zp zit zt lengthOfLoop = function
  | 0 -> Boolean.mk_or ctx (acc)
  | j ->
     (**ztj+1 = 1 *)
     let e1 = Boolean.mk_eq ctx zt (Integer.mk_numeral_i ctx 1)  in
     (**zpj+1 = j *)
     let e2 = Boolean.mk_eq ctx zp (Integer.mk_numeral_i ctx j) in
     (**pi = pi-1 + zit * length(loop(j)) *)
     
     let mult =  Arithmetic.mk_mul ctx [(Integer.mk_numeral_i ctx (List.nth lengthOfLoop (j-1) ));zit] in
     let addpim = Arithmetic.mk_add ctx [mult;pim] in
     
     let eq = Boolean.mk_eq ctx pi addpim in
     
     let conj = Boolean.mk_and ctx [e1;e2;eq] in
     
     loop_pos ctx (conj::acc) pi pim zp zit zt lengthOfLoop (j-1)
                
let rec trans_pos ctx acc pi pim zp zt zit  = function
  | 0 -> Boolean.mk_or ctx (acc)
  | j ->
     
     let e1 =  Boolean.mk_eq ctx zt (Integer.mk_numeral_i ctx 0) in
     
     let e2 = Boolean.mk_eq ctx zp (Integer.mk_numeral_i ctx j) in

     let mult =  Arithmetic.mk_mul ctx [(Integer.mk_numeral_i ctx 1);zit] in
     
     let addP1 = Arithmetic.mk_add ctx [mult;pim] in
     
     let e3 = Boolean.mk_eq ctx pi addP1 in

     let conj = Boolean.mk_and ctx [e1;e2;e3] in
     
     trans_pos ctx (conj::acc) pi pim zp zt  zit (j-1)
                 
       	  
(** la list wl correspond aux arguments de Witness Zt Zp Zit p0...pBeta+gamma **)	  
let witness ctx wl pl q0 trans_set_of_source loop_set_of_source lengthOfLoop beta gamma =
  (**Printf.printf " Witness \n " ;**)
  let (_,p0) =  (List.find (fun (i,p) -> i=0 ) pl) in
  let  (_,_,z2p,z2t,_)  =  (List.find (fun (i,p,zp,zt,zit) -> i=2 ) wl) in
  let  (_,_,_,z1t,_)  =  (List.find (fun (i,p,zp,zt,zit) -> i=1 ) wl) in
  
  let beta = beta   in
  let gamma = gamma  in
  let startPos = Boolean.mk_eq ctx p0 (Integer.mk_numeral_i ctx (0)) in 
  let transStates = trans_state ctx [] q0 z2p z2t trans_set_of_source (beta) in
  let loopStates = loop_state ctx [] q0 z2p z2t loop_set_of_source gamma in
  let controlState = Boolean.mk_or ctx (transStates@loopStates) in

  let l =
    (List.fold_left2 (
         fun l (i,_,_,_,_) (i,p) ->
          (*Printf.printf "i: %d | p : %s | zp : %s| zt : %s | zit : %s \n "
                    i (Expr.to_string p) (Expr.to_string zp) (Expr.to_string zt) (Expr.to_string zit);*)
      (match i with
       | 0 -> l
       | n when n < (beta+gamma)->
	  let  (iM1,pM1) = (List.find (fun (i,_) -> i=n-1) pl)  in
          let (j,_,zp,zt,zit) = List.find (fun (jp,_,zpp,ztp,zitp) -> jp = n+1) wl in
          (*Printf.printf "i: %d | p : %s | zp : %s| zt : %s | zit : %s \n "
                    i (Expr.to_string p) (Expr.to_string zp) (Expr.to_string zt) (Expr.to_string zit);*)
          let e1 = Arithmetic.mk_lt ctx (Integer.mk_numeral_i ctx i) z1t in
          
          let loop = loop_pos ctx [] p pM1 zp zit zt lengthOfLoop gamma in
          
          let trans = trans_pos ctx [] p pM1 zp zt zit beta in
          
	  let exp = Boolean.mk_or ctx (loop::trans::[]) in
          
          let e = (Boolean.mk_implies ctx e1 exp) in
          (*Printf.printf "Witeness  %s \n" (Expr.to_string e);*)
          
	  e :: l
       | _ -> l)
        
       ) [] wl pl) in
  ctx ,Boolean.mk_and ctx (startPos::controlState::l)

		 
      
		
	   
		   
