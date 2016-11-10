open Ltlast


(* Fonction permetant de récupérer l'ast généré après parsing *)
let parse (s : string) : ltl_formul = LtlParser.main LtlLexer.token (Lexing.from_string s)

let lit name fichier = 
  let buf = open_in name in 
  try
    while true do
      fichier := (!fichier)^ (input_line (buf) )
    done
  with End_of_file -> ()  


let get_formula fichier = 
  if Sys.file_exists fichier then
   begin
     let contenu = ref "" in
     let _ = lit fichier contenu in
     let ast =  (parse (!contenu)) in
     Printer.interpPrint ast;
     ast
   end
  else(
    print_string "le fichier n existe pas\n";
    exit(1)
  )


       
