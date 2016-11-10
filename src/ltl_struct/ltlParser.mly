%token AND
%token TRUE
%token FALSE
%token NOT
%token OR
%token UNTIL
%token NEXT
%token ALWAYS
%token EVENTUALLY
%token IMPLIES
%token LPAREN RPAREN
%token <string> BID
%token EOF

%left UNTIL
%nonassoc NEXT ALWAYS EVENTUALLY
%left AND OR
%nonassoc NOT

%start main             /* the entry point */
%type <Ltlast.ltl_formul> main
%%
main:
    ltl_formul EOF              { $1 }
;

ltl_formul:
  | FALSE                               { Ltlast.False }
  | TRUE                                { Ltlast.True }
  | BID                                 { Ltlast.Atom ($1) }
  | NOT ltl_formul                      { Ltlast.Not ($2) }
  | ltl_formul AND ltl_formul           { Ltlast.And ($1, $3) }
  | ltl_formul OR ltl_formul            { Ltlast.Or ($1,$3) }

  | NEXT ltl_formul                     { Ltlast.X ($2) }
  | ALWAYS ltl_formul                   { Ltlast.G ($2) }
  | EVENTUALLY ltl_formul               { Ltlast.F ($2) }
 
  | ltl_formul UNTIL ltl_formul         { Ltlast.U ($1,$3) }
  
  | LPAREN ltl_formul RPAREN
    { $2 }
