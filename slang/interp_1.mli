
type address = int

type value =
     | REF of address
     | INT of int
     | BOOL of bool
     | UNIT
     | TUPLE of value list
     | INL of value
     | INR of value
     | REC_CLOSURE of closure
     | CLOSURE of closure

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
  | MK_TUPLE of value list
  | TUPLE_FST of Ast.expr list * value list * env
  | FST
  | SND
  | PROJ of int
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

