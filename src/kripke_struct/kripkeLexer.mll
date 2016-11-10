{
  open Lexing
  open KripkeParser
  let line_number = ref 0
   let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    line_number := !line_number + 1;	
    f lexbuf
}



let newline' = ('\010' | '\013')

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

let lowercase_alpha = ['a'-'z']

let uppercase_alpha = ['A'-'Z']

let alpha = lowercase_alpha | uppercase_alpha

let alphanum = alpha | digit | '_'

let basic_identifier = lowercase_alpha alphanum*

let constr_id = (uppercase_alpha | '_') alphanum* 

rule token = parse
  (** Layout *)
  | newline             {  next_line_and token lexbuf }
  | blank+              { token lexbuf               }
  | "//"                { commentLine lexbuf; token lexbuf  }
  | "/*"                { comment lexbuf; token lexbuf}

  (** Literals *)

  (************************* STRING ***************************)
  
  | '"'             { PROP( (stringBuffer (Buffer.create 128) lexbuf) ) }

  (** Keywords *)
  | "Kripke"              { KRIPKE     }
  | "node-list"           { NODEL     }
  | "atomic-prop"         { ATOMP     }
  | "actions"             { ACTION    }
  | "initial-node"        { INITNODE  }
  | "transition-list"     { TRANSL    }

  (** Punctuation *)
  | "::="           { DEQUAL    }
  | ";"             { SEMICOLON }
  | "{"             { LCURLY    }
  | "}"             { RCURLY    }
  | ","             { COMMA     }
  | "<"             { CHL       }
  | ">"             { CHR       }
  | eof             { EOF       }

  (** les identifieurs *)

  | basic_identifier as i        { BID i      }
  | constr_id as i               { ID i      }
  (** Lexing error. *)
  | _               { Printf.printf  "unexpecting lexeme at line %d " !line_number; exit(1) }

and comment = parse
  | "*/" { () }
  | "/*" { comment lexbuf; comment lexbuf }
  | eof  {Printf.printf  "unexpecting lexeme at line %d " !line_number; exit(1) }
  | _   { comment lexbuf }

and commentLine = parse
  | "\n" { () }
  | eof  { ()  }
  | _   { commentLine lexbuf }

and  stringBuffer buffer = parse
  | '"'           { Buffer.contents buffer }
  | newline' as c   { Buffer.add_char buffer c; stringBuffer buffer lexbuf }
  | '\\' '/'      { Buffer.add_char buffer '/'; stringBuffer buffer lexbuf }
  | '\\' '\\'     { Buffer.add_char buffer '\\'; stringBuffer buffer lexbuf }
  | '\\' 'b'      { Buffer.add_char buffer '\b'; stringBuffer buffer lexbuf }
  | '\\' 'n'      { Buffer.add_char buffer '\n'; stringBuffer buffer lexbuf }
  | '\\' 'r'      { Buffer.add_char buffer '\r'; stringBuffer buffer lexbuf }
  | '\\' 't'      { Buffer.add_char buffer '\t'; stringBuffer buffer lexbuf }
  | '\\' '"'      { Buffer.add_char buffer '\"'; stringBuffer buffer lexbuf }
  | '\\' '''      { Buffer.add_char buffer '\''; stringBuffer buffer lexbuf }
  | '\\' _ as char  { Buffer.add_string buffer char; stringBuffer buffer lexbuf }
  | eof  { failwith "Unterminated comment ." }
  | _ as char { Buffer.add_char buffer char; stringBuffer buffer lexbuf }

