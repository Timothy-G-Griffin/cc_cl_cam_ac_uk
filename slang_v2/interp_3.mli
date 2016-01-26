
val verbose : bool ref 

val heap_max : int ref 

type address = int 

type label = string 

type location = label * (address option) 

type value = 
  | REF of address 
  | INT of int 
  | BOOL of bool 
  | UNIT
  | PAIR of value * value 
  | INL of value 
  | INR of value 
  | CLOSURE of location * env 

and instruction = 
  | PUSH of value 
  | LOOKUP of Ast.var 
  | UNARY of Ast.unary_oper 
  | OPER of Ast.oper 
  | ASSIGN 
  | SWAP
  | POP 
  | BIND of Ast.var 
  | FST
  | SND
  | DEREF 
  | APPLY
  | RETURN 
  | MK_PAIR 
  | MK_INL
  | MK_INR
  | MK_REF 
  | MK_CLOSURE of location 
  | TEST of location 
  | CASE of location
  | GOTO of location
  | LABEL of label 
  | HALT 

and code = instruction list 

and binding = Ast.var * value

and env = binding list

type env_or_value = 
  | EV of env           (* an environment on the run-time stack *) 
  | V of value          (* a value on the run-time stack *) 
  | RA of address    (* a return address on the run-time stack *) 

type env_value_stack = env_or_value list 

type state = address * env_value_stack 

val installed : (instruction array) ref

val step : state -> state 

val compile : Ast.expr -> code  

val driver : int -> state -> value 

val interpret : Ast.expr -> value 

val string_of_code : code -> string 

val string_of_value : value -> string 

