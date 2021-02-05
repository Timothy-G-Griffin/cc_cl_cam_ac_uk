
type address 


type store = address -> value 

and value = 
     | REF of address 
     | INT of int 
     | BOOL of bool 
     | UNIT
     | INL of value 
     | INR of value 
     | FUN of ((value * store) -> (value * store))
     | TUPLE of (value list)

type env = Ast.var -> value 

val string_of_value : value -> string 

val interpret :  Ast.expr * env * store -> (value * store) 

val interpret_top_level : Ast.expr -> value 




