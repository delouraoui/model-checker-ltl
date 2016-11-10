
type kripke = 
  KripDef of (kripkename * kripkexp list) list
   
 and kripkexp =
   Nodel    of (nodek * prop list) list
   | AtmProp  of prop list
   | Actionl  of prop list
   | InitNode of nodek
   | Transl   of (nodek * prop * nodek) list

 and prop = 
   Prop of string

 and nodek =
   Nodek of string 

 and kripkename = 
   Id of string
