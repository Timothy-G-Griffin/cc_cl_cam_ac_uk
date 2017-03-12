(**************************************
Compiler Construction 2016
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 
(* Interpreter 1. 

   Derived from Interpreter 1 via 
   CPS and DFC transformations applied
   to the code of Interp_0.interpret. 

*) 


open Ast 

let complain = Errors.complain

type address = int 

type value = 
     | REF of address 
     | INT of int 
     | BOOL of bool 
     | UNIT
     | PAIR of value * value 
     | INL of value 
     | INR of value 
     | REC_CLOSURE of closure
     | CLOSURE of closure 

and closure = var * expr * env 

and continuation_action = 
  | UNARY of unary_oper 
  | OPER of oper * value 
  | OPER_FST of Ast.expr * env * Ast.oper 
  | ASSIGN of value
  | ASSIGN_FST of Ast.expr * env
  | TAIL of Ast.expr list * env
  | IF of expr * expr * env 
  | WHILE of Ast.expr * Ast.expr * env
  | MKPAIR of value 
  | PAIR_FST of expr * env 
  | FST
  | SND
  | MKINL
  | MKINR
  | MKREF 
  | DEREF 
  | CASE of var * expr * var * expr * env 
  | APPLY of value 
  | ARG of expr * env 

and continuation = continuation_action  list

and binding = var * value

and env = binding list

type state = 
   | EXAMINE of expr * env * continuation 
   | COMPUTE of continuation * value 



(* update : (env * binding) -> env *) 
let update(env, (x, v)) = (x, v) :: env 

(* When making a closure, only include bindings that 
   are needed. 
*) 

let rec inlist x = function 
  | [] -> false 
  | y :: rest -> (x = y) || (inlist x rest) 

let rec filter_env fvars = function 
  | [] -> [] 
  | (x, v) :: rest -> if inlist x fvars then (x, v) :: (filter_env fvars rest) else (filter_env fvars rest)

let mk_fun(x, body, env) = 
    let fvars = Free_vars.free_vars ([x], body) in 
    let smaller_env = filter_env fvars env in 
      CLOSURE(x, body, smaller_env)

let mk_rec_fun(f, x, body, env) = 
    let fvars = Free_vars.free_vars ([f; x], body) in 
    let smaller_env = filter_env fvars env in 
    let f_binding = (f, REC_CLOSURE(x, body, [])) in 
       CLOSURE(x, body, f_binding :: smaller_env)

(* 
      for a recursive function f we want 

      lookup (env, f) = FUN(true, (x, body, env))  
*) 
let lookup (env, x) = 
    let rec aux = function 
      | [] -> complain (x ^ " is not defined!\n")
      | (y, v) :: rest -> 
          if x = y 
          then match v with 
             | REC_CLOSURE(z, body, _) -> 
                 CLOSURE(z, body, (y, REC_CLOSURE(z, body, [])) :: rest)
             | _ -> v 
          else aux rest  
      in aux env 


let readint () = let _ = print_string "input> " in read_int() 

let do_unary = function 
  | (NOT,  BOOL m) -> BOOL (not m)
  | (NEG,  INT m)  -> INT (-m)
  | (READ, UNIT)   -> INT (readint())
  | (op, _) -> complain ("malformed unary operator: " ^ (string_of_unary_oper op))

let do_oper = function 
  | (AND,  BOOL m,  BOOL n) -> BOOL (m && n)
  | (OR,   BOOL m,  BOOL n) -> BOOL (m || n)
  | (EQB,  BOOL m,  BOOL n) -> BOOL (m = n)
  | (LT,   INT m,   INT n)  -> BOOL (m < n)
  | (EQI,  INT m,   INT n)  -> BOOL (m = n)
  | (ADD,  INT m,   INT n)  -> INT (m + n)
  | (SUB,  INT m,   INT n)  -> INT (m - n)
  | (MUL,  INT m,   INT n)  -> INT (m * n)
  | (DIV,  INT m,   INT n)  -> INT (m / n)
  | (op, _, _)  -> complain ("malformed binary operator: " ^ (string_of_oper op))


let string_of_list sep f l = 
   let rec aux f = function 
     | [] -> ""
     | [t] -> (f t)
     | t :: rest -> (f t) ^  sep  ^ (aux f rest)
   in "[" ^ (aux f l) ^ "]"


let rec string_of_value = function 
     | REF a          -> "REF(" ^ (string_of_int a) ^ ")"
     | BOOL b         -> string_of_bool b
     | INT n          -> string_of_int n 
     | UNIT           -> "UNIT"
     | PAIR(v1, v2)   -> "PAIR(" ^ (string_of_value v1) ^ ", " ^ (string_of_value v2) ^ ")"
     | INL v          -> "INL(" ^ (string_of_value v) ^ ")"
     | INR v          -> "INR(" ^ (string_of_value v) ^ ")"
     | CLOSURE cl     -> "CLOSURE(" ^ (string_of_closure cl) ^ ")"
     | REC_CLOSURE cl -> "REC_CLOSURE(" ^ string_of_closure cl ^ ")"

and string_of_closure (x, e, env) = x ^ ", " ^ (Ast.string_of_expr e) ^  ", " ^ (string_of_env env)

and string_of_env env = string_of_list ",\n " string_of_binding env 

and string_of_binding (x, v) =    "(" ^ x ^ ", " ^ (string_of_value v) ^ ")"

let string_of_expr_list = string_of_list "; " Ast.string_of_expr

let string_of_continuation_action = function 
  | UNARY op     -> "UNARY " ^ (string_of_unary_oper op) 
  | MKPAIR v     -> "MKPAIR " ^ (string_of_value v)
  | FST          -> "FST"
  | SND          -> "SND"
  | MKINL        -> "MKINL"
  | MKINR        -> "MKINR"
  | APPLY v     -> "APPLY " ^ (string_of_value v) 
  | ARG (e, env) -> "ARG("  ^ (Ast.string_of_expr e)   ^ ", " ^ (string_of_env env) ^ ")"
  | OPER (op, v) -> "OPER(" ^ (string_of_oper op) ^ ", " ^ (string_of_value v) ^ ")"
  | CASE(x1, e1, x2, e2, env) -> 
      "CASE(" ^ x1 ^ ", " 
                ^ (Ast.string_of_expr e1) ^ ", " 
                ^ x2 ^ ", " 
                ^ (Ast.string_of_expr e2) ^ ", " 
                ^ (string_of_env env) ^ ")"
  | PAIR_FST (e, env)    -> 
      "PAIR_FST(" ^ (Ast.string_of_expr e) ^ ", " ^ (string_of_env env) ^ ")"
  | OPER_FST(e, env, op) ->  
      "OPER_FST(" ^ (Ast.string_of_expr e) ^ ", " ^ (string_of_env env) ^ ", " ^ (string_of_oper op) ^ ")"
  | IF (e1, e2, env) -> 
      "IF(" ^ (Ast.string_of_expr e1) ^ ", " ^ (Ast.string_of_expr e2) ^ ", " ^ (string_of_env env) ^ ")"
  | ASSIGN v -> "MKPAIR " ^ (string_of_value v)
  | ASSIGN_FST (e, env) -> 
     "ASSIGN_FST(" ^ (Ast.string_of_expr e) ^ ", " ^ (string_of_env env) ^ ")"
  | TAIL (el , env) -> "TAIL("  ^ (string_of_expr_list el)   ^ ", " ^ (string_of_env env) ^ ")"
  | WHILE (e1, e2, env) -> 
      "WHILE(" ^ (Ast.string_of_expr e1) ^ ", " ^ (Ast.string_of_expr e2) ^ ", " ^ (string_of_env env) ^ ")"
  | MKREF -> "MKREF" 
  | DEREF -> "DEREF" 

let string_of_continuation = string_of_list ";\n " string_of_continuation_action

let string_of_state = function 
   | EXAMINE(e, env, cnt) -> 
      "EXAMINE(" ^ (Ast.string_of_expr e) ^ ", " 
              ^ (string_of_env env) ^ ", " 
              ^ (string_of_continuation cnt) ^ ")" 
   | COMPUTE(cnt, v)     -> 
      "COMPUTE(" ^ (string_of_continuation cnt) ^ ", " 
               ^ (string_of_value v) ^ ")"


let heap  = Array.make Option.heap_max (INT 0)

let next_address = ref 0 

let new_address () = let a = !next_address in (next_address := a + 1; a) 

let mk_ref v = let a = new_address () in let _ = heap.(a) <- v in REF a 

let do_assign a v = (heap.(a) <- v)

 
let step = function 
 (* EXAMINE --> EXAMINE *) 
 | EXAMINE(UnaryOp(op, e),              env, k) -> EXAMINE(e,  env, (UNARY op) :: k)
 | EXAMINE(Op(e1, op, e2),              env, k) -> EXAMINE(e1, env, OPER_FST(e2, env, op) :: k)
 | EXAMINE(If(e1, e2, e3),              env, k) -> EXAMINE(e1, env, IF(e2, e3, env) :: k)
 | EXAMINE(Pair(e1, e2),                env, k) -> EXAMINE(e1, env, PAIR_FST(e2, env) :: k)
 | EXAMINE(Fst e,                       env, k) -> EXAMINE(e,  env, FST :: k)
 | EXAMINE(Snd e,                       env, k) -> EXAMINE(e,  env, SND :: k) 
 | EXAMINE(Inl e,                       env, k) -> EXAMINE(e,  env, MKINL :: k) 
 | EXAMINE(Inr e,                       env, k) -> EXAMINE(e,  env, MKINR :: k) 
 | EXAMINE(Case(e, (x1, e1), (x2, e2)), env, k) -> EXAMINE(e,  env, CASE(x1, e1, x2, e2, env) :: k)
 | EXAMINE(App(e1, e2),                 env, k) -> EXAMINE(e2, env, ARG(e1, env) :: k)
 | EXAMINE(LetFun(f, (x, body), e),     env, k) -> EXAMINE(e, update(env, (f, mk_fun(x, body, env))) , k) 
 | EXAMINE(LetRecFun(f, (x, body), e),  env, k) -> EXAMINE(e, update(env, (f, mk_rec_fun(f, x, body, env))) , k) 
 | EXAMINE(Ref e,                       env, k) -> EXAMINE(e,  env, MKREF :: k)
 | EXAMINE(Deref e,                     env, k) -> EXAMINE(e,  env, DEREF :: k)
 | EXAMINE(Assign(e1, e2),              env, k) -> EXAMINE(e1, env, ASSIGN_FST(e2, env) :: k)
 | EXAMINE(Seq [e],                     env, k) -> EXAMINE(e, env, k) 
 | EXAMINE(Seq (e :: rest),             env, k) -> EXAMINE(e, env, TAIL (rest, env) :: k) 
 | EXAMINE(While(e1, e2),               env, k) -> EXAMINE(e1, env, WHILE(e1, e2, env) :: k)
 (* EXAMINE --> COMPUTE *) 
 | EXAMINE(Unit,              _, k) -> COMPUTE(k, UNIT) 
 | EXAMINE(Var x,           env, k) -> COMPUTE(k, lookup (env, x))
 | EXAMINE(Integer n,         _, k) -> COMPUTE(k, INT n)
 | EXAMINE(Boolean b,         _, k) -> COMPUTE(k, BOOL b) 
 | EXAMINE(Lambda(x, body), env, k) -> COMPUTE(k, mk_fun(x, body, env))
 (* COMPUTE --> COMPUTE *) 
 | COMPUTE((UNARY op) :: k,    v) -> COMPUTE(k ,(do_unary(op, v)))
 | COMPUTE(OPER(op, v1) :: k, v2) -> COMPUTE(k, do_oper(op, v1, v2))
 | COMPUTE((MKPAIR v1) :: k,  v2) -> COMPUTE(k, PAIR(v1, v2))
 | COMPUTE(FST :: k,  PAIR(v, _)) -> COMPUTE(k, v)
 | COMPUTE(SND :: k,  PAIR(_, v)) -> COMPUTE(k, v)
 | COMPUTE(MKINL :: k,         v) -> COMPUTE(k, (INL v))
 | COMPUTE(MKINR :: k,         v) -> COMPUTE(k, (INR v))
 | COMPUTE(MKREF :: k,         v) -> COMPUTE(k , mk_ref v)
 | COMPUTE(DEREF :: k,     REF a) -> COMPUTE(k , heap.(a))
 | COMPUTE(ASSIGN (REF a) :: k, v) -> let _ = do_assign a v in COMPUTE(k , UNIT) 
 | COMPUTE(WHILE (_, _, _) :: k,         BOOL false) -> COMPUTE(k, UNIT) 
 (* COMPUTE --> EXAMINE *) 
 | COMPUTE(OPER_FST (e2, env, op) :: k,         v1)  -> EXAMINE(e2, env, OPER (op, v1) :: k)
 | COMPUTE((APPLY v2) :: k, CLOSURE(x, body, env))  -> EXAMINE(body, update(env, (x, v2)), k)
 | COMPUTE((APPLY v2) :: k, REC_CLOSURE(x, body, env)) -> EXAMINE(body, update(env, (x, v2)), k)
 | COMPUTE(ARG (e2, env) :: k,                   v)  -> EXAMINE(e2, env, (APPLY v) :: k)
 | COMPUTE(PAIR_FST (e2, env) :: k,             v1)  -> EXAMINE(e2, env, (MKPAIR v1) :: k)
 | COMPUTE(CASE (x1, e1, x2, e2, env) :: k,  INL v)  -> EXAMINE(e1, update(env, (x1, v)), k) 
 | COMPUTE(CASE (x1, e1, x2, e2, env) :: k,  INR v)  -> EXAMINE(e2, update(env, (x2, v)), k)
 | COMPUTE(IF (e2, e3, env) :: k,        BOOL true)  -> EXAMINE(e2, env, k)
 | COMPUTE(IF (e2, e3, env) :: k,       BOOL false)  -> EXAMINE(e3, env, k)

 | COMPUTE(ASSIGN_FST (e2, env) :: k,            v)  -> EXAMINE(e2, env, ASSIGN v :: k)
 | COMPUTE(WHILE (e1, e2, env) :: k,     BOOL true)  -> EXAMINE(Seq [e2; e1], env, WHILE(e1, e2, env)::k)
 | COMPUTE((TAIL (el, env)) :: k,     _)  ->  EXAMINE(Seq el, env, k)
 | state -> complain ("step : malformed state = " ^ (string_of_state state) ^ "\n")

let rec driver n state = 
  let _ = if Option.verbose 
          then print_string ("\nstate " ^ (string_of_int n) ^ " = \n" ^ (string_of_state state) ^ "\n")
          else () 
  in match state with 
     | COMPUTE([], v) -> v 
     | _              -> driver (n + 1) (step state) 

let eval(e, env) = driver 1 (EXAMINE(e, env, []))

(* env_empty : env *) 
let env_empty = [] 

(* interpret : expr -> value *) 
let interpret e = eval(e, env_empty)

    

      
    
    
