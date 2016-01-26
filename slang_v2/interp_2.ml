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
---Heap used only for references. 
---Code is maintained on a code stack. 
---Program variables contained in code.
*) 


open Ast 

let complain = Errors.complain

let heap_max = ref 1000 

let verbose = ref false 

type address = int 

type var = string 

type value = 
     | REF of address 
     | INT of int 
     | BOOL of bool 
     | UNIT
     | PAIR of value * value 
     | INL of value 
     | INR of value 
     | CLOSURE of bool * closure    

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
  | FST
  | SND
  | DEREF 
  | APPLY
  | MK_PAIR 
  | MK_INL
  | MK_INR
  | MK_REF 
  | MK_CLOSURE of code 
  | MK_REC of var * code 
  | TEST of code * code
  | CASE of code * code
  | WHILE of code * code

and code = instruction list 

and binding = var * value

and env = binding list

type env_or_value = EV of env | V of value 

type env_value_stack = env_or_value list 

type state = code * env_value_stack 

(* The heap *) 

let heap  = Array.make !heap_max (INT 0)

let next_address = ref 0 

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
     | PAIR(v1, v2)    -> "(" ^ (string_of_value v1) ^ ", " ^ (string_of_value v2) ^ ")"
     | INL v           -> "inl(" ^ (string_of_value v) ^ ")"
     | INR  v          -> "inr(" ^ (string_of_value v) ^ ")"
     | CLOSURE(b, cl) -> "CLOSURE(" ^ (string_of_bool b) ^ ", " ^ (string_of_closure cl) ^ ")"

and string_of_closure (c, env) = 
   "(" ^ (string_of_code c) ^ ", " ^ (string_of_env env) ^ ")"

and string_of_env env = string_of_list ",\n " string_of_binding env 

and string_of_binding (x, v) =    "(" ^ x ^ ", " ^ (string_of_value v) ^ ")"

and string_of_instruction = function 
 | UNARY op     -> "UNARY " ^ (string_of_uop op) 
 | OPER op      -> "OPER " ^ (string_of_bop op) 
 | MK_PAIR      -> "MK_PAIR"
 | FST          -> "FST"
 | SND          -> "SND"
 | MK_INL       -> "MK_INL"
 | MK_INR       -> "MK_INR"
 | MK_REF       -> "MK_REF"
 | PUSH v       -> "PUSH " ^ (string_of_value v) 
 | LOOKUP x     -> "LOOKUP " ^ x
 | TEST(c1, c2) -> "TEST(" ^ (string_of_code c1) ^ ", " ^ (string_of_code c1) ^ ")"
 | CASE(c1, c2) -> "CASE(" ^ (string_of_code c1) ^ ", " ^ (string_of_code c1) ^ ")"
 | WHILE(c1, c2) -> "WHILE(" ^ (string_of_code c1) ^ ", " ^ (string_of_code c1) ^ ")"
 | APPLY        -> "APPLY"
 | BIND x       -> "BIND " ^ x
 | SWAP         -> "SWAP"
 | POP          -> "POP"
 | DEREF        -> "DEREF"
 | ASSIGN       -> "ASSIGN"
 | MK_CLOSURE c -> "MK_CLOSURE(" ^ (string_of_code c) ^ ")" 
 | MK_REC(f, c) -> "MK_REC(" ^ f ^ ", " ^ (string_of_code c) ^ ")"

and string_of_code c = string_of_list ";\n " string_of_instruction c 

let string_of_env_or_value = function 
  | EV env -> "EV " ^ (string_of_env env)
  | V v -> "V " ^ (string_of_value v)

let string_of_env_value_stack = string_of_list ";\n " string_of_env_or_value 

let string_of_heap ()  = 
    let rec aux k = 
            if !next_address < k 
	    then "" 
	    else (string_of_int k) ^ " -> " ^ (string_of_value (heap.(k))) ^ "\n" ^ (aux (k+1)) 
    in "\nHeap = \n" ^ (aux 0) 

let string_of_state (c, evs) = 
     "\nCode Stack = \n" ^ (string_of_code c) 
     ^ "\nEnv/Value Stack = \n" ^ (string_of_env_value_stack evs) 
     ^ (if !next_address = 0 then "" else string_of_heap()) 

(* The "MACHINE" *) 

(* allocate a new location in the heap *) 
let allocate () = 
    if !next_address < !heap_max 
    then let a = !next_address in (next_address := a + 1; a) 
    else complain "runtime error: heap kaput"

(* update : (env * binding) -> env *) 
let update(env, (x, v)) = (x, v) :: env 

let mk_fun(c, env) = CLOSURE(false, (c, env)) 
let mk_rec(f, c, env) = CLOSURE(false, (c, (f, CLOSURE(true, (c, []))) :: env))

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
               | CLOSURE(true, (body, _)) -> 
                   CLOSURE(false, (body, (x, CLOSURE(true, (body, []))) :: rest))
               | _ -> v)
          else aux rest  
      in aux env 

let lookup (env, x) = 
    match lookup_opt (env, x) with 
    | None -> complain (x ^ " is not defined!\n")
    | Some v -> v 

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
  | (op, _, _)  -> complain ("malformed binary operator: " ^ (string_of_oper op))

(*
    val step : state -> state 
             = (code * env_value_stack) -> (code * env_value_stack) 
*) 
let step = function 
(* (code stack,                value/env stack) -> (code stack,  value/env stack) *) 
 | ((PUSH v) :: ds,                        evs) -> (ds, (V v) :: evs)
 | (POP :: ds,                        s :: evs) -> (ds, evs) 
 | (SWAP :: ds,                s1 :: s2 :: evs) -> (ds, s2 :: s1 :: evs) 
 | ((BIND x) :: ds,               (V v) :: evs) -> (ds, EV([(x, v)]) :: evs) 
 | ((LOOKUP x) :: ds,                      evs) -> (ds, V(search(evs, x)) :: evs)
 | ((UNARY op) :: ds,             (V v) :: evs) -> (ds, V(do_unary(op, v)) :: evs) 
 | ((OPER op) :: ds,   (V v2) :: (V v1) :: evs) -> (ds, V(do_oper(op, v1, v2)) :: evs)
 | (MK_PAIR :: ds,     (V v2) :: (V v1) :: evs) -> (ds, V(PAIR(v1, v2)) :: evs)
 | (FST :: ds,           V(PAIR (v, _)) :: evs) -> (ds, (V v) :: evs)
 | (SND :: ds,           V(PAIR (_, v)) :: evs) -> (ds, (V v) :: evs)
 | (MK_INL :: ds,                 (V v) :: evs) -> (ds, V(INL v) :: evs)
 | (MK_INR :: ds,                 (V v) :: evs) -> (ds, V(INR v) :: evs)
 | (CASE (c1,  _) :: ds,         V(INL v)::evs) -> (c1 @ ds, (V v) :: evs) 
 | (CASE ( _, c2) :: ds,         V(INR v)::evs) -> (c2 @ ds, (V v) :: evs) 
 | ((TEST(c1, c2)) :: ds,  V(BOOL true) :: evs) -> (c1 @ ds, evs) 
 | ((TEST(c1, c2)) :: ds, V(BOOL false) :: evs) -> (c2 @ ds, evs) 
 | (ASSIGN :: ds,  (V v) :: (V (REF a)) :: evs) -> (heap.(a) <- v; (ds, V(UNIT) :: evs))
 | (DEREF :: ds,            (V (REF a)) :: evs) -> (ds, V(heap.(a)) :: evs)
 | (MK_REF :: ds,                 (V v) :: evs) -> let a = allocate () in (heap.(a) <- v; 
                                                   (ds, V(REF a) :: evs))
 | ((WHILE(c1, c2)) :: ds,V(BOOL false) :: evs) -> (ds, evs) 
 | ((WHILE(c1, c2)) :: ds, V(BOOL true) :: evs) -> (c1 @ [WHILE(c1, c2)] @ ds, evs) 
 | ((MK_CLOSURE c) :: ds,                  evs) -> (ds,  V(mk_fun(c, evs_to_env evs)) :: evs)
 | (MK_REC(f, c) :: ds,                    evs) -> (ds,  V(mk_rec(f, c, evs_to_env evs)) :: evs)
 | (APPLY :: ds,  V(CLOSURE (_, (c, env))) :: (V v) :: evs) 
                                                -> (c @ ds, (V v) :: (EV env) :: evs)
 | state -> complain ("step : bad state = " ^ (string_of_state state) ^ "\n")

let rec driver n state = 
  let _ = if !verbose 
          then print_string ("\nState " ^ (string_of_int n) 
                             ^ " : " ^ (string_of_state state) ^ "\n")
          else () 
  in match state with 
     | ([], [V v]) -> v 
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
 | Pair(e1, e2)   -> (compile e1) @ (compile e2) @ [MK_PAIR] 
 | Fst e          -> (compile e) @ [FST] 
 | Snd e          -> (compile e) @ [SND] 
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


(* interpret : expr -> value *) 
let interpret e = 
    let c = compile e in 
    let _ = if !verbose 
            then print_string("Compile code =\n" ^ (string_of_code c) ^ "\n")
            else () 
    in driver 1 (c, [])




    

      
    
    
