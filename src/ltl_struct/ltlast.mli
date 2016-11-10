
type ltl_formul =
	| False
	| True
	| Atom of string
	| Not of ltl_formul 
	| And of ltl_formul * ltl_formul 
	| Or of ltl_formul * ltl_formul 
	| U of ltl_formul * ltl_formul 
	| X of ltl_formul 
	| G of ltl_formul
	| F of ltl_formul