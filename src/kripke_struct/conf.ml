open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer
open Witness
open Schema 

       


let rec conf_trans beta acc ctx q zpj trans_set_of_source =
  match beta with
  | 0 ->

     Boolean.mk_or ctx acc
  | k  ->

     (** zpj+2 = k **)
     let is_k = Boolean.mk_eq ctx zpj (Integer.mk_numeral_i ctx k)  in
     let source_of_k_trans = List.nth trans_set_of_source (k-1) in
     let e5 =
       Boolean.mk_eq ctx (Integer.mk_numeral_i ctx source_of_k_trans) q in
     let e = Boolean.mk_and ctx [e5;is_k] in
     conf_trans (beta-1) (e ::acc) ctx q zpj trans_set_of_source
              
let rec conf_loop gamma acc ctx q zpj p pj set_of_loop =
  match gamma with
  | 0 ->
     (**  
      V k ∈ [0;β-1] (
      zpj+1 = k  /\ ( V l ∈ [0;length(lk)-1] (p-pj = l % length(lk) /\ y = source((δ(l) ∈ loop(k) )) ) ) )
 Boolean.mk_or ctx acc
      **)
     Boolean.mk_or ctx acc
                    
  | k ->
     (** zpj+1 = k **)
     let is_k = Boolean.mk_eq ctx zpj (Integer.mk_numeral_i ctx k)  in
     (** length(loop(k)) longeur de la k-ieme boucle **)
     let length = List.length (List.nth set_of_loop (k-1))   in
     (*Printf.printf " In Loop  : \n " ;*)
     let inLooplist =
       List.fold_right (
	   fun (l,source) acc' ->
           
	   (** p-pj **)
	   let leq = Arithmetic.mk_sub ctx [p;pj] in
	   (** l % length(lk) **)
	   let req = Integer.mk_mod ctx leq (Integer.mk_numeral_i ctx length) in
	   
	   (** //// \\\\ WARNING Il y avait un bug ici on disait  ( p-pj = l  % length(lk))
au lieu de dire  (l = p-pj  % length(lk)) qui est juste  **)
	   let eql = Boolean.mk_eq ctx (Integer.mk_numeral_i ctx (l)) req in
	   (*Printf.printf " %s \n" (Expr.to_string eql);*)
	   (** 
              On check l'égalité entre y et la l-ieme transition de la k-ieme boucle 
              q = source((δ(l) ∈ loop(k) ))
	    **)
           (*Printf.printf "k-iem loop  : %d l-iem transisition : %d  source %d \n" k l source;*)
	   let eqr = Boolean.mk_eq ctx q (Integer.mk_numeral_i ctx source) in
           (*Printf.printf " %s \n" (Expr.to_string eqr);*)
	   (** (p-pj = l % length(lk) /\ q = source((δ(l) ∈ loop(k) )) ) **)
	   let ad = Boolean.mk_and ctx [eql;eqr] in
           ad::acc'
					   
	 ) (List.nth set_of_loop (k-1)) [] in
     (** V l ∈ [0;length(lk)-1] (p-pj = l % length(lk) /\ q = source((δ(l) ∈ loop(k) )) ) **)
     let orInLoop = Boolean.mk_or ctx inLooplist in
     (*Printf.printf " %s \n" (Expr.to_string orInLoop);*)
     (**  zpj+1 = k  /\ ( V l ∈ [0;length(lk)-1] (p-pj = l % length(lk) /\ q = source((δ(l) ∈ loop(k) )) ) ) **)
     let ek = Boolean.mk_and ctx [is_k;orInLoop] in

      (** iteration*)
     conf_loop (gamma-1) (ek::acc) ctx q zpj p pj set_of_loop


let pos_in_trans ctx beta j z1p ztj zpj p pj trans_set_of_source q =
  (** j <= zt0 **)
  let e1 = Arithmetic.mk_le ctx (Integer.mk_numeral_i ctx j) z1p  in
  (** p = pj **)
  let e2 = Boolean.mk_eq ctx p pj in
  
  let e3 = Boolean.mk_eq ctx ztj (Integer.mk_numeral_i ctx 0) in
  
  let e4 = conf_trans beta [] ctx q zpj trans_set_of_source in
  
  
  Boolean.mk_and ctx [e1;e2;e3;e4]
    
let pos_in_loop ctx gamma j z1p ztj zpj set_of_loop loop_set_of_source p pj pSuccj q =

  (**  j <= zt0 **)
  let e1 = Arithmetic.mk_le ctx (Integer.mk_numeral_i ctx j) z1p  in
  (*Printf.printf "%s \n" (Expr.to_string z0);*)
  (** pj < p **)
  let encLft = Arithmetic.mk_le ctx pj p in
  (** p < pj+1 **)
  let encCntr = Arithmetic.mk_lt ctx p pSuccj in

  let e3 = Boolean.mk_eq ctx ztj (Integer.mk_numeral_i ctx 1) in
 
  let confLoop = conf_loop (gamma) [] ctx q zpj p pj set_of_loop in
  
  Boolean.mk_and ctx [e1;encLft;encCntr;e3;confLoop]
                 
let pos_in_last_loop ctx gamma j z1p zpj set_of_loop p pj pSuccj q =

  (**  j <= zt0 **)
  let e1 = Boolean.mk_eq ctx (Integer.mk_numeral_i ctx j) z1p  in

  (** pj < p **)
  let encLft = Arithmetic.mk_le ctx pj p in

  let confLoop = conf_loop (gamma) [] ctx q zpj p pj set_of_loop in
  
  Boolean.mk_and ctx [e1;encLft;confLoop]

                 
(**
wl : Zt, Zp Zit p....pγ+β-1
set : {(a,b,a',b') | )}
trans_set_of_source : {source(δ(0)),..,source(δ(β))}
loop_set_of_source : {source(loop(0)),..,source(loop(γ))}
set_of_loop : { (l0,{source(δ(0)),..,source(δ(l0))}),..,(lγ,{source(δ(0)),..,source(δ(lγ))})}
witn : Witenesse ou les variables ne sont pas encore quantifié 

conf( Zt, Zp , Zit , q0, p ,q) 
 **)

let conf ctx wl beta gamma k set lengthOfLoop
	 trans_set_of_source loop_set_of_source set_of_loop q0 p q =


  (**Warning verifier l'odre des liste de longueurs des loop**)
  let loop_set_of_source = List.rev loop_set_of_source in
  let set_of_loop = List.rev set_of_loop in
 
  let schema_args = Util.get_schema_args wl in
		      
  let ctx , schemaFormula =
    psi ctx schema_args set beta gamma k in
  
  let pl = Util.getP wl in 
  let ctx , witn =
    witness ctx wl pl q0 trans_set_of_source loop_set_of_source lengthOfLoop beta gamma in
 
  (** p0, ... , pβ+γ-1 **)
  let listOfPi = List.filter (fun (i,p,_,_,_) -> i < (beta+gamma)) wl in
  let listOfPi = List.map (fun (i,p,_,_,_) -> p) listOfPi in
  (** zt0 **)
  let (_,_,z1p,_,_) = List.find (fun (i,_,_,_,_) -> i = 1) wl in
  
  (*Printf.printf "nb boucles : %d \n" (gamma);
  Printf.printf "nb transitions : %d \n" (beta);*)
  
  let mainCond =
    
    List.fold_left2 (
	fun acc (j,_,_,_,_) (_,pj) ->
	match j with
        
	| n when n <(beta + gamma -1 )  ->
	   (** On prend j+1 pj+1 zpj+1 ztj+1 et zitj+1 **)
           let (succj',succpj) = List.find (fun (jp,pjp) -> jp = n+1) pl in
	   let (succj,_,succzp,succzt,succzit) = List.find (fun (jp,_,zpp,ztp,zitp) -> jp = n+2) wl in
           (*Printf.printf "%d %s %s %s %s \n" succj (Expr.to_string succzp) (Expr.to_string succzt) (Expr.to_string succzit) (Expr.to_string succpj) ;*)
                         
           let confTrans = pos_in_trans ctx beta j z1p succzt succzp p pj trans_set_of_source q in
           let confLoopB = pos_in_loop ctx gamma j z1p succzt succzp set_of_loop loop_set_of_source p pj succpj q in
           let confLoopL = pos_in_last_loop ctx gamma j z1p succzp set_of_loop p pj succpj q in
           (* Printf.printf "trans : %s \n" (Expr.to_string confTrans);*)
           (**let confloop = Boolean.mk_and ctx [confLoopB;confLoopL] in **)
           (*Printf.printf " conf : %s \n" (Expr.to_string confLoopL);*)
           
	   [confLoopB ;confTrans;confLoopL] @acc
                                                                  
	| n -> acc
      ) [] wl pl in

  let subConf = Boolean.mk_or ctx mainCond in
  (*  Printf.printf "conf :  %s \n" (Expr.to_string subConf);*)
   
  let f = Boolean.mk_and ctx [schemaFormula;witn;subConf] in
  
  (** ∃ p0, ... , pβ+γ-1 Witeness(p0 , ... , pβ+γ-1, Zt, Zp, Zit) /\ schema /\ subconf **)
  let formula =
    (Quantifier.expr_of_quantifier
       (Quantifier.mk_exists_const ctx listOfPi f (None) [] []
				   (None)
				 None)) in
  formula		
    
      
		
	   
		   
 
