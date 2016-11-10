open Ltlast
open Conf
open Ast 
open Kripke
open Ltl
open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.Solver
open Z3.Arithmetic
open Z3.Arithmetic.Integer
let z3 = ref false
let rec without_not i =
  function 
  | And(a,b) ->
     And(without_not i a,without_not i b)
  | Or(a,b) ->
     Or(without_not i a,without_not i b)
  | U(a,b) ->
     U(without_not i a,without_not i b)
  | Not(a) when i = 0 ->
     a
  | Not(a) ->
     (without_not (i-1) a)
       
  | Atom(a) ->
     Atom(a)
  | F(a) ->
     F(without_not i a)
  | G(a) ->
     G (without_not i a)
  | X a ->
     X (without_not i a)
  | e -> e

let fresh_q =
  let r = ref (-1) in
  fun () -> incr r; "q"^(string_of_int (!r))
let fresh_p =
  let r = ref (-1) in
  fun () -> incr r; "y"^(string_of_int (!r))
                          
let rec  in_label acc p = function 
  | [] -> acc
  | (a,l)::q ->
     if  List.mem p l then
       (
         
         in_label (a::acc) p q
       )
     else
       in_label (acc) p q

let atome conf labels ctx a q =

  let stra = Expr.to_string a in 
  let listOfj = in_label [] stra labels in    

  let listofeq =
    List.fold_left (
	fun acc (j) ->
        
        (Boolean.mk_eq ctx (Integer.mk_numeral_i ctx j) q)::acc
      ) [] listOfj in
  let is_eq  = Boolean.mk_or ctx listofeq in
  (*Printf.printf "eq :  %s \n" (Expr.to_string is_eq);*)
  let f = Boolean.mk_and ctx [is_eq;conf] in
  let existConfFormula =
    (Quantifier.expr_of_quantifier
       (Quantifier.mk_exists_const ctx [q] f (None) [] []
				   (None)
				   (None))) in
  ctx , existConfFormula
    


let gen ctx wl pglobal q0 beta gamma k set lengthOfLoop label
	trans_set_of_source loop_set_of_source set_of_loop e quantifiableVars =
  
  

  let rec check not wl labels ctx p q0 = function 
    | True      ->
       ctx,Boolean.mk_true ctx
    | False     ->
    
       ctx,Boolean.mk_false ctx
                          
    | Atom(p')   ->
       let q = Expr.mk_const ctx (mk_string ctx (fresh_q ()))  (Integer.mk_sort ctx) in
       let conf' =
	 conf ctx wl beta gamma k set lengthOfLoop
	      trans_set_of_source loop_set_of_source set_of_loop q0 p q in
       let vars = Expr.mk_const ctx (mk_string ctx p')  (Integer.mk_sort ctx) in
       atome conf' labels ctx vars q 

    | Not(exp)  ->
       let eqP = Boolean.mk_eq ctx pglobal (Integer.mk_numeral_i ctx 0) in
       let eqQ0 = Boolean.mk_eq ctx q0 (Integer.mk_numeral_i ctx 0) in
       let e = without_not not e in 
       let ctx, e' = check (not+1) wl labels ctx pglobal q0 e in
       let existsFormula = (Quantifier.expr_of_quantifier
		              (Quantifier.mk_exists_const ctx quantifiableVars (Boolean.mk_and ctx [eqP;eqQ0;e']) (None) [] []
						       (None)
						       (None))) in

       ctx, (Util.check_not ctx existsFormula)

    | And(l, r) ->

       let ctx, r' = check not wl labels ctx p q0 r in
       let ctx, l' = check not wl labels ctx p q0 l in
       ctx, Boolean.mk_and ctx [l';r']

    | Or(l, r)  ->
       
       let ctx, r' = check not wl labels ctx p q0 r in
       let ctx, l' = check not wl labels ctx p q0 l in
       
       ctx ,Boolean.mk_or ctx [l';r']

    | X(exp)    ->
       let p' = Expr.mk_const ctx (mk_string ctx (fresh_p ()))  (Integer.mk_sort ctx) in
       let pplus = Arithmetic.mk_add ctx [p;(Integer.mk_numeral_i ctx 1)] in
       let ctx, check' = check not wl labels ctx p' q0 exp in
       
       let eqy = Boolean.mk_eq ctx pplus p' in
       let uu = Boolean.mk_and ctx [eqy;check'] in 
       ctx, (Quantifier.expr_of_quantifier
	  (Quantifier.mk_exists_const ctx [p'] uu (None) [] []
				      (None)
                                      (None))) 
         

    | F(exp) ->
       
       let p' = Expr.mk_const ctx (mk_string ctx (fresh_p ()))  (Integer.mk_sort ctx) in
       let ctx, check' = check not wl labels ctx p' q0 exp in
       let lei = Arithmetic.mk_le ctx p p' in
       let uu = Boolean.mk_and ctx [lei;check'] in
       ctx, (Quantifier.expr_of_quantifier
	  (Quantifier.mk_exists_const ctx [p'] uu (None) [] []
				      (None)
				      (None)))
         
         
    | G(exp) ->
       
       let p' = Expr.mk_const ctx (mk_string ctx (fresh_p ()))  (Integer.mk_sort ctx) in
       let ctx, check' = check not wl labels ctx p' q0 exp in
       let lei = Arithmetic.mk_le ctx p p' in
       let uu = Boolean.mk_implies ctx lei check' in 
       ctx, (Quantifier.expr_of_quantifier
	  (Quantifier.mk_forall_const ctx [p'] uu (None) [] []
				      (None)
                                      (None)))
         
         
    | U(e1,e2)->
       (** mettre les quantificateurs en dernier **)
       let p' = Expr.mk_const ctx (mk_string ctx (fresh_p()))  (Integer.mk_sort ctx) in
       let p'' = Expr.mk_const ctx (mk_string ctx (fresh_p ()))  (Integer.mk_sort ctx) in
       
       let lei = Arithmetic.mk_le ctx p p'' in
       
       let ctx, check'' = check not wl labels ctx p'' q0 e2 in
       
       let left = Boolean.mk_and ctx [lei;check''] in


       let ley = Arithmetic.mk_le ctx p p' in
       let lty = Arithmetic.mk_lt ctx p' p'' in
       let preCond = Boolean.mk_and ctx [lty;ley] in
       
       let ctx, check' = check not wl labels ctx p' q0 e1 in
             
       let right = Boolean.mk_implies ctx preCond check' in 
       
       let f = (Quantifier.expr_of_quantifier
	          (Quantifier.mk_forall_const ctx [p'] right (None) [] []
					      (None)
					      (None))) in
       let f = Boolean.mk_and ctx [left;f] in
       ctx, (Quantifier.expr_of_quantifier
	       (Quantifier.mk_exists_const ctx [p''] f (None) [] []
					   (None)
					   (None))) 
  in
  
  let ctx, check' = check 0 wl label ctx pglobal q0 e in
  let eqP = Boolean.mk_eq ctx pglobal (Integer.mk_numeral_i ctx 0) in
  let eqQ0 = Boolean.mk_eq ctx q0 (Integer.mk_numeral_i ctx 1) in
  ctx , Boolean.mk_and ctx [check';eqP;eqQ0] 

         
let checker formula =
  (fun (beta, gamma, k, set, lengthOfLoop, label
	,trans_set_of_source,loop_set_of_source,set_of_loop) ->
    let ctx = (mk_context [("auto_config","true") ]) in
    let p = Expr.mk_const ctx (mk_string ctx (fresh_p ()) )  (Integer.mk_sort ctx) in
    let q0 = Expr.mk_const ctx (mk_string ctx (fresh_q ()))  (Integer.mk_sort ctx) in
    let wl = Util.mk_globals_args ctx (k) in
    let zp = Util.getZp wl in
    let zt = Util.getZt wl in
    let zit = Util.getZit wl in
    let quantifiableVars = [p]@[q0]@zt@zp@zit in
    let ctx, frm =
      gen ctx wl p q0 beta gamma k set lengthOfLoop label
	  trans_set_of_source loop_set_of_source set_of_loop formula quantifiableVars in
   
    let f = Boolean.mk_and ctx [frm] in  
    let existsFormula = (Quantifier.expr_of_quantifier
		           (Quantifier.mk_exists_const ctx quantifiableVars f (None) [] []
						       (None)
						       (None))) in
    if not !z3 then
      begin
        Util.check_bench ctx existsFormula "Check.smt2";
        let _ = Sys.command ("sed -i 's/implies/=>/g' Check.smt2") in
        let _ = Sys.command ("sed -i -e \"1d\" Check.smt2") in
        Sys.command ("sed -i -e \"1d\" Check.smt2")
      end
    else
      begin
        Util.afficher ctx existsFormula "Check.smt2";
        Sys.command ("sed -i 's/implies/=>/g' Check.smt2")
      end
  )
    

(** --------------------------------------------**)
(** ----------Administative part----------------**)
(** --------------------------------------------**)

    
(* Fonction permetant de récupérer l'ast généré après parsing *)
let parse (s : string) : kripke =
  KripkeParser.main KripkeLexer.token (Lexing.from_string s)
(* corps principal du programme *)
(*type choice = VIZ | GRAPICS | EX of int*)
(*let choice = ref None*)
let fichierltl = ref None
let fichiera = ref None
let arg_spec =
  ["-m", Arg.String (fun i -> fichiera := Some i ),
   " <string>  name of file with kripke model";
   "-ltl", Arg.String (fun i -> fichierltl := Some i ),
   " <string>  name of file with ltl formula";
   "-z3", Arg.Unit (fun () -> z3 := true ),
   "enable the smt-Z3";
(**
   "-gv", Arg.Unit (fun () -> choice := Some VIZ),
   " display with gv";
   "-e", Arg.Int (fun i -> choice := Some (EX i)),
   " display with gv";**)
  ]
let () = Arg.parse arg_spec (fun _ -> ()) "Usage ./check -m <kripke-file-name> -ltl <ltl-file-name>"
		   
let getKripkeFile () =
  match !fichiera with
  | None ->   print_string "Usage ./check -m <kripke-file-name> -ltl <ltl-file-name>\n";exit(0)
  | Some fichier -> fichier
		      
let getLtlFile () =
  match !fichierltl with
  | None ->   print_string "Usage ./check -m <kripke-file-name> -ltl <ltl-file-name>\n";exit(0)
  | Some fichier -> fichier
		      
let get_ast () = 
  begin
    let fichier = getKripkeFile () in 
    if Sys.file_exists fichier then
      let contenu = ref "" in
      let _ = lit fichier contenu in
      let ast = (parse (!contenu)) in
      ast 
    else
      (
	print_string "The file doesn't exist\n";
	exit(0)
      )
  end

let checker =
  Printf.printf "---------------------------Flat Model-Cheking--------------------------\n\n\n" ;
  let ast = get_ast () in
  let ltlFile = getLtlFile () in
  let infos  = kripk ast in
  let formula = get_formula ltlFile in
  Printf.printf "--------------------------- SMT - Solver Part -------------------------\n\n" ;
  List.map (checker formula) infos

	   
let main () =
  checker
;;main()  
