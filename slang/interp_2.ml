(**************************************
Compiler Construction 2016
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 
(* Interpreter 2. 

A high-level stack-oriented abstract machine with compiler. 
What do I mean by "high-level"? 
---Code is still tree-structured. 
---Complex values are pushed onto value stack.  
---Slang state (heap) used only for references. 
---Code is maintained on a code stack. 
---Program variables contained in code.
*) 


open Ast 

let complain = Errors.complain

type address = int 

type var = string 

type value = 
     | REF of address 
     | INT of int 
     | BOOL of bool 
     | UNIT
     | INL of value 
     | INR of value 
     | CLOSURE of closure    
     | REC_CLOSURE of code
     | TUPLE of (value list)

and closure = code * env 

and instruction = 
  | PUSH of value 
  | LOOKUP of var 
  | UNARY of unary_oper 
  | OPER of oper 
  | ASSIGN 
  | SWAP
  | POP 
  | BIND of var
  | DEREF 
  | APPLY
  | MK_TUPLE of int
  | MK_INL
  | MK_INR
  | MK_REF 
  | MK_CLOSURE of code 
  | MK_REC of var * code 
  | TEST of code * code
  | CASE of code * code
  | WHILE of code * code
  | INDEX of int

and code = instruction list 

and binding = var * value

and env = binding list

type env_or_value = EV of env | V of value 

type env_value_stack = env_or_value list

(* This is the the slang program state --- that is, values for references *) 
(* It is an array of referenced values together with next unallocated address *)
type state = (value array) * int 

type interp_state = code * env_value_stack * state 

(* Printing *) 

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
     | TUPLE vl       -> "(" ^ (string_of_value_list vl)
     | INL v           -> "inl(" ^ (string_of_value v) ^ ")"
     | INR  v          -> "inr(" ^ (string_of_value v) ^ ")"
     | CLOSURE(cl) -> "CLOSURE(" ^ (string_of_closure cl) ^ ")"
     | REC_CLOSURE(c) -> "REC_CLOSURE(" ^ (string_of_code c) ^ ")"

and string_of_value_list = function
  | [] -> "" (* error if this happens *)
  | [v] -> (string_of_value v) ^ ")"
  | (v::rest) -> (string_of_value v) ^ ", " ^ string_of_value_list rest

and string_of_closure (c, env) = 
   "(" ^ (string_of_code c) ^ ", " ^ (string_of_env env) ^ ")"

and string_of_env env = string_of_list ",\n " string_of_binding env 

and string_of_binding (x, v) =    "(" ^ x ^ ", " ^ (string_of_value v) ^ ")"

and string_of_instruction = function 
 | UNARY op     -> "UNARY " ^ (string_of_uop op) 
 | OPER op      -> "OPER " ^ (string_of_bop op)
 | MK_TUPLE n   -> "MK_TUPLE " ^ string_of_int n
 | MK_INL       -> "MK_INL"
 | MK_INR       -> "MK_INR"
 | MK_REF       -> "MK_REF"
 | PUSH v       -> "PUSH " ^ (string_of_value v) 
 | LOOKUP x     -> "LOOKUP " ^ x
 | TEST(c1, c2) -> "TEST(" ^ (string_of_code c1) ^ ", " ^ (string_of_code c2) ^ ")"
 | CASE(c1, c2) -> "CASE(" ^ (string_of_code c1) ^ ", " ^ (string_of_code c2) ^ ")"
 | WHILE(c1, c2) -> "WHILE(" ^ (string_of_code c1) ^ ", " ^ (string_of_code c2) ^ ")"
 | APPLY        -> "APPLY"
 | BIND x       -> "BIND " ^ x
 | SWAP         -> "SWAP"
 | POP          -> "POP"
 | DEREF        -> "DEREF"
 | ASSIGN       -> "ASSIGN"
 | MK_CLOSURE c -> "MK_CLOSURE(" ^ (string_of_code c) ^ ")" 
 | MK_REC(f, c) -> "MK_REC(" ^ f ^ ", " ^ (string_of_code c) ^ ")"
 | INDEX i -> "INDEX " ^ (string_of_int i)

and string_of_code c = string_of_list ";\n " string_of_instruction c 

let string_of_env_or_value = function 
  | EV env -> "EV " ^ (string_of_env env)
  | V v -> "V " ^ (string_of_value v)

let string_of_env_value_stack = string_of_list ";\n " string_of_env_or_value 

let string_of_state (heap, i)  = 
    let rec aux k = 
            if i < k 
	    then "" 
	    else (string_of_int k) ^ " -> " ^ (string_of_value (heap.(k))) ^ "\n" ^ (aux (k+1)) 
    in if i = 0
       then ""
       else "\nHeap = \n" ^ (aux 0) 

let string_of_interp_state (c, evs, s) = 
     "\nCode Stack = \n" ^ (string_of_code c) 
     ^ "\nEnv/Value Stack = \n" ^ (string_of_env_value_stack evs) 
     ^ (string_of_state(s)) 

(* The "MACHINE" *) 

(* allocate a new location in the heap
   and give it value v
*) 
let allocate (heap, i) v = 
    if i < Option.heap_max 
    then let _ = heap.(i) <- v
         in (i, (heap, i+1))
    else complain "runtime error: heap kaput"

let deref (heap, _) a = heap.(a)

let assign (heap, i) a v =
    let _ = heap.(a) <- v
    in (heap, i) 


(* update : (env * binding) -> env *) 
let update(env, (x, v)) = (x, v) :: env 

let mk_fun(c, env) = CLOSURE(c, env) 
let mk_rec(f, c, env) = CLOSURE(c, (f, REC_CLOSURE(c))::env)

(* 
   in interp_0: 

   interpret(LetRecFun(f, (x, body), e), env) = 
       let rec new_env g = 
           if g = f then FUN (fun v -> interpret(body, update(new_env, (x, v)))) else env g
       in interpret(e, new_env, store) 

      new_env x = env x 
      new_env f = FUN (fun v -> interpret(body, update(new_env, (x, v))))

      lookup (env1 @ [(f, cl1)] @ evn2, f) = 
        CLOSURE (false, (x, body, (f, cl2) :: env2))  
*) 
let lookup_opt (env, x) = 
    let rec aux = function 
      | [] -> None 
      | (y, v) :: rest -> 
          if x = y 
          then Some(match v with 
               | REC_CLOSURE(body) -> mk_rec(x, body, rest)
               | _ -> v)
          else aux rest  
      in aux env 

let rec search (evs, x) = 
  match evs with 
  | [] -> complain (x ^ " is not defined!\n")
  | (V _) :: rest -> search (rest, x) 
  | (EV env) :: rest -> 
    (match lookup_opt(env, x) with 
    | None -> search (rest, x) 
    | Some v -> v 
    ) 

 let rec evs_to_env = function 
  | [] -> []
  | (V _) :: rest -> evs_to_env rest 
  | (EV env) :: rest -> env @ (evs_to_env rest) 
    
    
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

(*
    val step : interp_state -> interp_state 
             = (code * env_value_stack * state) -> (code * env_value_stack * state) 
*)

let rec take = function
  | (0, ls) -> ([], ls)
  | (n, V(v)::vs) -> let (front, rest) = take (n-1, vs) in (v::front, rest)
  | _ -> complain "Stack underflow on take"

let rec nth = function
    | (1, t::tl) -> t
    | (n, t::tl) -> if n > 0 then nth (n-1,tl) else complain "Runtime indexing error - should never happen"
    | _ -> complain "Runtime indexing error - should never happen"

let step = function 

(* (code stack,         value/env stack, state) -> (code stack,  value/env stack, state) *) 
 | ((PUSH v) :: ds,                        evs, s) -> (ds, (V v) :: evs, s)
 | (POP :: ds,                        e :: evs, s) -> (ds, evs, s) 
 | (SWAP :: ds,                e1 :: e2 :: evs, s) -> (ds, e2 :: e1 :: evs, s) 
 | ((BIND x) :: ds,               (V v) :: evs, s) -> (ds, EV([(x, v)]) :: evs, s) 
 | ((LOOKUP x) :: ds,                      evs, s) -> (ds, V(search(evs, x)) :: evs, s)
 | ((UNARY op) :: ds,             (V v) :: evs, s) -> (ds, V(do_unary(op, v)) :: evs, s) 
 | ((OPER op) :: ds,   (V v2) :: (V v1) :: evs, s) -> (ds, V(do_oper(op, v1, v2)) :: evs, s)

 | ((MK_TUPLE n) :: ds,                    evs, s) -> let (vs, rest) = take (n, evs) in
                                                           (ds, V(TUPLE(List.rev vs))::rest, s)
 | (INDEX (i) :: ds,       (V (TUPLE vl))::evs, s) -> (ds, (V (nth (i, vl))) :: evs, s)

 | (MK_INL :: ds,                 (V v) :: evs, s) -> (ds, V(INL v) :: evs, s)
 | (MK_INR :: ds,                 (V v) :: evs, s) -> (ds, V(INR v) :: evs, s)
 | (CASE (c1,  _) :: ds,         V(INL v)::evs, s) -> (c1 @ ds, (V v) :: evs, s) 
 | (CASE ( _, c2) :: ds,         V(INR v)::evs, s) -> (c2 @ ds, (V v) :: evs, s) 
 | ((TEST(c1, c2)) :: ds,  V(BOOL true) :: evs, s) -> (c1 @ ds, evs, s) 
 | ((TEST(c1, c2)) :: ds, V(BOOL false) :: evs, s) -> (c2 @ ds, evs, s)
 | (ASSIGN :: ds,  (V v) :: (V (REF a)) :: evs, s) -> (ds, V(UNIT) :: evs, assign s a v)
 | (DEREF :: ds,            (V (REF a)) :: evs, s) -> (ds, V(deref s a) :: evs, s)
 | (MK_REF :: ds,                 (V v) :: evs, s) -> let (a, s') = allocate s v in (ds, V(REF a) :: evs, s')
 | ((WHILE(c1, c2)) :: ds,V(BOOL false) :: evs, s) -> (ds, V(UNIT) :: evs, s) 
 | ((WHILE(c1, c2)) :: ds, V(BOOL true) :: evs, s) -> (c2 @ [POP] @ c1 @ [WHILE(c1, c2)] @ ds, evs, s)
 | ((MK_CLOSURE c) :: ds,                  evs, s) -> (ds,  V(mk_fun(c, evs_to_env evs)) :: evs, s)
 | (MK_REC(f, c) :: ds,                    evs, s) -> (ds,  V(mk_rec(f, c, evs_to_env evs)) :: evs, s)
 | (APPLY :: ds,  V(CLOSURE (c, env)) :: (V v) :: evs, s) 
                                                   -> (c @ ds, (V v) :: (EV env) :: evs, s)
 | state -> complain ("step : bad state = " ^ (string_of_interp_state state) ^ "\n")

let rec driver n state = 
  let _ = if Option.verbose 
          then print_string ("\nState " ^ (string_of_int n) 
                             ^ " : " ^ (string_of_interp_state state) ^ "\n")
          else () 
  in match state with 
     | ([], [V v], s) -> (v, s)  
     | _ -> driver (n + 1) (step state) 


(* A BIND will leave an env on stack. 
   This gets rid of it.  *) 
let leave_scope = [SWAP; POP] 

(*
   val compile : expr -> code 
*) 
let rec compile = function 
 | Unit           -> [PUSH UNIT] 
 | Integer n      -> [PUSH (INT n)] 
 | Boolean b      -> [PUSH (BOOL b)] 
 | Var x          -> [LOOKUP x] 
 | UnaryOp(op, e) -> (compile e) @ [UNARY op]
 | Op(e1, op, e2) -> (compile e1) @ (compile e2) @ [OPER op] 

 | Tuple(el)      -> List.fold_right (fun e -> fun c -> (compile e) @ c) el [MK_TUPLE(List.length el)]
 | Index(i, e)    -> (compile e) @ [INDEX i]
 | Inl e          -> (compile e) @ [MK_INL]
 | Inr e          -> (compile e) @ [MK_INR] 
 | Case(e, (x1, e1), (x2, e2)) -> 
       (compile e)
       @ [CASE((BIND x1) :: (compile e1) @ leave_scope, 
               (BIND x2) :: (compile e2) @ leave_scope)]
 | If(e1, e2, e3) -> (compile e1) @ [TEST(compile e2, compile e3)]
 | Seq []         -> [] 
 | Seq [e]        -> compile e
 | Seq (e ::rest) -> (compile e) @ [POP] @ (compile (Seq rest))
 | Ref e          -> (compile e) @ [MK_REF] 
 | Deref e        -> (compile e) @ [DEREF] 
 | While(e1, e2)  -> let cl = compile e1 in cl @ [WHILE(cl, compile e2)]
 | Assign(e1, e2) -> (compile e1) @ (compile e2) @ [ASSIGN] 
 | App(e1, e2)    -> (compile e2)   (* I chose to evaluate arg first *) 
                     @ (compile e1) 
                     @ [APPLY; 
                        SWAP; POP]  (* get rid of env left on stack *) 
 | Lambda(x, e)   -> [MK_CLOSURE((BIND x) :: (compile e) @ leave_scope)]
 | LetFun(f, (x, body), e)    -> 
       (MK_CLOSURE((BIND x) :: (compile body) @ leave_scope)) :: 
       (BIND f) :: 
       (compile e) @ leave_scope
 | LetRecFun(f, (x, body), e) -> 
       (MK_REC(f, (BIND x) :: (compile body) @ leave_scope)) ::  
       (BIND f) :: 
       (compile e) @ leave_scope


(* The initial Slang state is the Slang state : all locations contain 0 *) 

let initial_state  = (Array.make Option.heap_max (INT 0), 0)

let initial_env = [] 

(* interpret : expr -> (value * state) *) 
let interpret e = 
    let c = compile e in 
    let _ = if Option.verbose 
            then print_string("Compile code =\n" ^ (string_of_code c) ^ "\n")
            else () 
    in driver 1 (c, initial_env, initial_state)




    

      
    
    
