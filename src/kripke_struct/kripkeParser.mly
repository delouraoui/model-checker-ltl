%token NODEL
%token ATOMP
%token ACTION
%token INITNODE
%token TRANSL
%token DEQUAL
%token SEMICOLON
%token LCURLY
%token RCURLY
%token COMMA 
%token KRIPKE
%token CHL
%token CHR

%token <string>  PROP BID ID
%token EOF

%left SEMICOLON
%start main             /* the entry point */
%type <Ast.kripke> main
%%
main:
    kripke EOF              { $1 }
;

kripke :
 subkripke { Ast.KripDef ($1) }

subkripke:
  | KRIPKE kripkename LCURLY kripkes RCURLY SEMICOLON subkripke  {($2,$4)::$7}
  | KRIPKE kripkename LCURLY kripkes RCURLY                      {($2,$4)::[]}
  |                                                               {[]}

kripkes:
  | kripkexp SEMICOLON kripkes  {$1::$3}
  | kripkexp                    {$1::[]}
  |                             {[]}

  
kripke_listt:
    | triple COMMA kripke_listt {$1::$3}
    | triple                   {$1::[]}
    |                        {[]}
kripke_listb:
    | pair COMMA kripke_listb {$1::$3}
    | pair                   {$1::[]}
    |                        {[]}

kripke_list:
    | prop COMMA kripke_list {$1::$3}
    | prop                   {$1::[]}
    |                        {[]}

kripkexp:
    NODEL DEQUAL kripke_listb  {Ast.Nodel($3)}
  | ATOMP DEQUAL kripke_list  {Ast.AtmProp($3)}
  | ACTION DEQUAL kripke_list  {Ast.Actionl($3)}
  | TRANSL DEQUAL kripke_listt  {Ast.Transl ($3)}
  | INITNODE DEQUAL nodek  {Ast.InitNode ($3)}

triple:
 CHL nodek COMMA prop COMMA nodek CHR {($2,$4,$6)}

pair: 
 CHL nodek COMMA kripke_list CHR {($2,$4)}

prop:
 PROP { Ast.Prop($1) }

kripkename:
| ID  { Ast.Id($1) }

nodek : 
| BID { Ast.Nodek($1) }

