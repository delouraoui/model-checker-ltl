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



				  
let rec mk_varslistsp acc = function
  | [] ->
     List.rev acc
  | [(i,zp,zt,zit)] ->
     List.rev (acc)
  | [(i,zp,zt,zit);(i',zp',zt',zit')] ->
     List.rev (acc)
  | (i,zp,zt,zit)::(i',zp',zt',zit')::q when i > 0 ->
      mk_varslistsp ((i,zp,zt,zp',zt')::acc) ((i',zp',zt',zit')::q)
  | a::q  ->
      mk_varslistsp (acc) (q)
    

let psi1' varslist cnst z1p ctx =
    List.fold_left (
	fun l (i,zp,zt,zit) ->
         
	match i with
        | 0 -> []
        | 1 ->
	 
	   let e1 = Boolean.mk_eq ctx zt zp in
	   let e2 = Boolean.mk_eq ctx zit zp in
	   let e3 = Arithmetic.mk_gt ctx zp (Integer.mk_numeral_i ctx 0) in
	   let e4 = Arithmetic.mk_lt ctx zp (Integer.mk_numeral_i ctx cnst) in
	   let conj = Boolean.mk_and ctx [e1;e2;e3;e4] in 
	   [conj]
	| n ->
	  let e1 = Boolean.mk_eq ctx zt zp in
	  let e2 = Boolean.mk_eq ctx zp zit in
	  let e3 = Boolean.mk_eq ctx zit (Integer.mk_numeral_i ctx 0) in
          let zplus = Arithmetic.mk_add ctx [z1p;(Integer.mk_numeral_i ctx 1)] in
	  let e = Arithmetic.mk_lt ctx  zplus (Integer.mk_numeral_i ctx i) in
	  let conj = Boolean.mk_and ctx [e1;e2;e3] in
	  (Boolean.mk_implies ctx e conj)::l
      ) [] varslist
		    
let psi1 varslist cnst z1p ctx =
  let l = psi1' varslist cnst z1p ctx  in 
  Boolean.mk_and ctx l


		   
let psi2' varslist ctx =
   List.fold_left (
       fun l (i,_,zt,_) ->

	match i with
	| 0 -> []
        | 1 -> []
	| n ->
	  let e1 = Arithmetic.mk_le ctx zt (Integer.mk_numeral_i ctx 1) in
	  (*let e2 = Arithmetic.mk_ge ctx zt (Integer.mk_numeral_i ctx 0)  in*)
	  let conj = Boolean.mk_and ctx [e1] in
	  conj::l
      ) [] varslist

let psi2 varslist ctx =
   let l = psi2' varslist ctx  in 
   Boolean.mk_and ctx l

		  
let psi3' varslist beta z1p ctx =
   List.fold_left (
       fun l (i,zp,zt,_) ->
	match i with
	| 0 -> []
        | 1 -> []
	| n ->
           let z1p = Arithmetic.mk_add ctx [z1p;(Integer.mk_numeral_i ctx 1)] in
           
	   let el = Arithmetic.mk_le ctx (Integer.mk_numeral_i ctx i) z1p in
           
	   let ell = Boolean.mk_eq ctx zt (Integer.mk_numeral_i ctx 0) in
           
	   let er1 = Arithmetic.mk_le ctx zp (Integer.mk_numeral_i ctx beta) in
           
	   let er2 = Arithmetic.mk_ge ctx zp (Integer.mk_numeral_i ctx 1) in
           
	   let conj = Boolean.mk_and ctx [er1;er2] in
           
	   let impl1 = Boolean.mk_implies ctx ell conj in
	   let impl = Boolean.mk_implies ctx el impl1 in
	   impl::l
      ) [] varslist

let psi3 varslist beta z1p ctx =
  let l = psi3' varslist beta z1p ctx  in 
  Boolean.mk_and ctx l
		  
let psi4' varslist gamma z1p ctx =
   List.fold_left (
       fun l (i,zp,zt,_) ->
	match i with
	| 0 -> []   
	| n ->
           let z1p = Arithmetic.mk_add ctx [z1p;(Integer.mk_numeral_i ctx 1)] in
           
	   let el = Arithmetic.mk_le ctx (Integer.mk_numeral_i ctx i) z1p in
           
	   let ell = Boolean.mk_eq ctx zt (Integer.mk_numeral_i ctx 1) in
           
	   let er1 = Arithmetic.mk_le ctx zp (Integer.mk_numeral_i ctx gamma) in
           
	   let er2 = Arithmetic.mk_ge ctx zp (Integer.mk_numeral_i ctx 1) in
           
	   let conj = Boolean.mk_and ctx [er1;er2] in
           
	   let impl1 = Boolean.mk_implies ctx ell conj in
           
	   let impl = Boolean.mk_implies ctx el impl1 in
           
	   impl::l
     ) [] varslist

let psi4 varslist gamma z1p ctx =
  let l = psi4' varslist gamma z1p ctx  in 
  Boolean.mk_and ctx l
		  
let psi5' varslist z1p ctx =
   List.fold_left (
       fun l (i,_,zt,_) ->
	match i with
	| 0 -> []
        | 1 -> []
	| n ->
          
           let z1p = Arithmetic.mk_add ctx [z1p;(Integer.mk_numeral_i ctx 1)] in
           
	   let e1 = Boolean.mk_eq ctx (Integer.mk_numeral_i ctx i) z1p in
           
	   let e2 = Boolean.mk_eq ctx zt (Integer.mk_numeral_i ctx 1) in
           
	   let impl = Boolean.mk_implies ctx e1 e2 in
           
	   impl::l
     ) [] varslist

let psi5 varslist z1p ctx =
  let l = psi5' varslist z1p ctx  in 
  Boolean.mk_and ctx l
		  
let psi6' varslist z1p ctx =
   List.fold_left (
       fun l (i,zp,zt,_) ->
	match i with
	| 0 -> []
        | 1 -> []
	| n ->
           
           let z1p = Arithmetic.mk_add ctx [z1p;(Integer.mk_numeral_i ctx 1)] in
           
	   let e1 = Arithmetic.mk_le ctx (Integer.mk_numeral_i ctx i) z1p in
           
	   let e2 = Boolean.mk_and ctx 
	     (List.fold_left (
		 fun l' (j,zp',zt',_) ->
		 match j with
		 | 0 -> []
                 | 1 -> []
		 | n' when j <> i ->
		    let e1' = Arithmetic.mk_le ctx (Integer.mk_numeral_i ctx j) z1p in
                    
		    let e2' = Boolean.mk_not ctx (Boolean.mk_eq ctx zp zp' ) in
                    
		    let e3' = Boolean.mk_not ctx (Boolean.mk_eq ctx zt zt' ) in
                    
		    let eor =  Boolean.mk_or ctx [e2';e3'] in
                    
		    (Boolean.mk_implies ctx e1' eor ):: l'
		 | _ -> l'   
		    
 	       ) [] varslist ) in 
	   let impl = Boolean.mk_implies ctx e1 e2 in
           
	   impl::l
     ) [] varslist

let psi6 varslist z1p ctx =
  let l = psi6' varslist z1p ctx  in 
  Boolean.mk_and ctx l
		  
let psi7' varslist setst z1p lim ctx =

  List.fold_left (
      fun l (i,zp,zt,zit) ->
	match i with
	| 0 -> []
        | 1 -> []
	| n when n < lim ->
           
           let (i',zp',zt',zit') = List.find (fun (k,zp,zt,zit) -> k = n+1 ) varslist in
           
	   let e = Arithmetic.mk_le ctx (Integer.mk_numeral_i ctx i) z1p in
           
	   let exp = Boolean.mk_or ctx
	    (List.fold_left (
		 fun l' (a,b,a',b') ->
		 
		 let eqa  = Boolean.mk_eq ctx (Integer.mk_numeral_i ctx a)  zt in
		 let eqb  = Boolean.mk_eq ctx  (Integer.mk_numeral_i ctx b) zp in
		 let eqa' = Boolean.mk_eq ctx  (Integer.mk_numeral_i ctx a') zt' in
		 let eqb' = Boolean.mk_eq ctx  (Integer.mk_numeral_i ctx b') zp' in
		 let e = (Boolean.mk_and ctx [eqa;eqb;eqa';eqb']) in
                 
                 e::l'
                                                             
	       ) [] setst
	    )
	   in
	   let impl = Boolean.mk_implies ctx e exp in
	   impl::l
        | _ -> l
     ) [] varslist

let psi7 varslist setst z1p k ctx =
  let l = psi7' varslist setst z1p k ctx in
  Boolean.mk_and ctx l
		 
let psi8' varslist z1p ctx =
   List.fold_left (
       fun l (i,zp,zt,zit) ->
        
	match i with
	| 0 -> []
        | 1 -> []
	| n ->
           (**Printf.printf "%d %s %s %s \n" i (Expr.to_string zp) (Expr.to_string zt) (Expr.to_string zit) ;**)
           let z1p = Arithmetic.mk_add ctx [z1p;(Integer.mk_numeral_i ctx 1)] in
	   let el = Arithmetic.mk_le ctx (Integer.mk_numeral_i ctx i) z1p in
           
	   let ell = Arithmetic.mk_gt ctx zit (Integer.mk_numeral_i ctx 0) in
           
	   let er1 = Boolean.mk_eq ctx zt (Integer.mk_numeral_i ctx 0) in
           
	   let er2 = Boolean.mk_eq ctx zit (Integer.mk_numeral_i ctx 1) in
           
	   let impl1 = Boolean.mk_implies ctx er1 er2 in
           
	   let conj = Boolean.mk_and ctx [ell;impl1] in
           
	   let impl = Boolean.mk_implies ctx el conj in
           
	   (*let finalcd1 = Boolean.mk_eq ctx z1p (Integer.mk_numeral_i ctx i) in
	   let finalcd2 = Boolean.mk_eq ctx zit (Integer.mk_numeral_i ctx 0) in
	   let impl2 = Boolean.mk_implies ctx finalcd1 finalcd2 in*)
	   impl::l
                   
     ) [] varslist

let psi8 varslist z1p ctx =
  let l = psi8' varslist z1p ctx  in 
  Boolean.mk_and ctx l
		  
 
let psi ctx varslist set beta gamma k =
  let (i,z1p,z1t,z1it) = List.find (fun (i,z1p,z1t,z1it) -> i = 1 ) varslist in
  let p1 = psi1 varslist k z1p ctx in
  
  (**Util.afficher ctx p1 "ps1" ;**)
  let p2 = psi2 varslist ctx in
  
  (**Util.afficher ctx p2 "ps2" ;**)
  let p3 = psi3 varslist beta z1p ctx in
  (**Util.afficher ctx p3 "ps3" ;**)
  let p4 = psi4 varslist gamma z1p ctx in
  (**Util.afficher ctx p4 "ps4" ;**)
  let p5 = psi5 varslist z1p ctx in
  (**Util.afficher ctx p5 "ps5" ;**)
  let p6 = psi6 varslist z1p ctx in
  (**Util.afficher ctx p6 "ps6" ;**)
  let p7 = psi7 varslist set z1p k ctx in
  (*Util.afficher ctx p7 "ps7" ;*)
  let p8 = psi8 varslist z1p ctx in
  (**Util.afficher ctx p8 "ps8" ;**)
  let psi' =
    Boolean.mk_and ctx [p1;p2;p3;p4;p5;p6;p7;p8] in
  (ctx,psi')
    
