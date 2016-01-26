val verbose : bool ref 

val heap_max : int ref 

type address = int 

type value = 
     | REF of address 
     | INT of int 
     | BOOL of bool 
     | UNIT
     | PAIR of value * value 
     | INL of value 
     | INR of value 
     (* bool flag = true means closure in recursive environment! *) 
     | CLOSURE of bool * closure  

and closure = Ast.var * Ast.expr * env 

and continuation_action = 
  | UNARY of Ast.unary_oper
  | OPER of Ast.oper * value
  | OPER_FST of Ast.expr * env * Ast.oper 
  | ASSIGN of value
  | ASSIGN_FST of Ast.expr * env
  | TAIL of Ast.expr list * env
  | IF of Ast.expr * Ast.expr * env
  | WHILE of Ast.expr * Ast.expr * env
  | MKPAIR of value 
  | PAIR_FST of Ast.expr * env 
  | FST 
  | SND 
  | MKINL 
  | MKINR 
  | MKREF 
  | DEREF 
  | CASE of Ast.var * Ast.expr * Ast.var * Ast.expr * env 
  | APPLY of value 
  | ARG of Ast.expr * env 

and continuation = continuation_action  list

and binding = Ast.var * value

and env = binding list

type state = 
   | EXAMINE of Ast.expr * env * continuation 
   | COMPUTE of continuation * value 

val step : state -> state 

val driver : int -> state -> value 

val eval : Ast.expr * env -> value 

val interpret : Ast.expr -> value 

val string_of_value : value -> string 

