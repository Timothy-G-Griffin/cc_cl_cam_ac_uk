(**************************************
Compiler Construction 2016
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 



(* This file derives a simple expression compiler from 
   a recursive evaluation function.  This is done in 
   small steps where each step is a simple transformation
   easily seen (or proved) to be correct. 

   In the OCaml read-eval-print loop (repl) do this to 
   load the file: 

     #use "expr_machine.ml";; 

   ROAD MAP: 

   1) eval   : a simple recursive evaluator 
   2) eval_2 : a cps-transformed version of eval (now tail-recursive) 
   3) eval_3 : a defunctionalized version of eval_2 (introduces apply_2) 
   4) eval_4 : EUREKA : a continuation can be represented as a list (used like a stack) 
   5) eval_5 : combine apply_4 and eval_aux_4 into a two-state machine with accumulator 
   6) eval_6 : split stack into two : directive stack and value stack 
   6) eval_7 : refactor!  eval_6 e = eval_7 (compile e) 
*) 



(* Datatype for expression 
 
  For example, "2 + ((5 -1) * 4)"  is represented by 

  PLUS(INT 2, MULT(SUBT(INT 5, INT 1), INT 4))

*) 
type expr = 
   | INT of int 
   | PLUS of expr * expr
   | SUBT of expr * expr
   | MULT of expr * expr

(* eval : expr -> int 
   a simple recusive evaluator for expressions
*) 
let rec eval = function 
   | INT a             -> a 
   | PLUS(e1, e2)   -> (eval e1) + (eval e2) 
   | SUBT(e1, e2)   -> (eval e1) - (eval e2) 
   | MULT(e1, e2)   -> (eval e1) * (eval e2) 

(* apply cps transform to eval *) 

type cnt_2  = int -> int 

type state_2 = expr * cnt_2 

(* eval_aux_2 : state_2 -> int *) 
let rec eval_aux_2 (e, cnt) = 
   match e with 
   | INT a        -> cnt a 
   | PLUS(e1, e2) -> eval_aux_2(e1, fun v1 -> eval_aux_2(e2, fun v2 -> cnt(v1 + v2)))
   | SUBT(e1, e2) -> eval_aux_2(e1, fun v1 -> eval_aux_2(e2, fun v2 -> cnt(v1 - v2)))
   | MULT(e1, e2) -> eval_aux_2(e1, fun v1 -> eval_aux_2(e2, fun v2 -> cnt(v1 * v2)))

(* 
    Claim 1 : c (eval e) = eval_aux_2(e, c) 
    Prove this by induction on the structire of e.  
*) 
      
(* id_cnt : cnt_2 *) 
let id_cnt (x : int) = x 

(*  eval_2 : expr -> int *) 
let eval_2 e = eval_aux_2(e, id_cnt) 




(* Now apply dfc transform to eval_cps 

*) 
type cnt_3 = 
  | ID 
  | OUTER_PLUS of expr * cnt_3
  | OUTER_SUBT of expr * cnt_3
  | OUTER_MULT of expr * cnt_3
  | INNER_PLUS of int * cnt_3
  | INNER_SUBT of int * cnt_3
  | INNER_MULT of int * cnt_3

type state_3 = expr * cnt_3 

(* apply_3 : cnt_3 * int -> int *) 
let rec apply_3 = function 
   | (ID,                   v) -> v 
   | (OUTER_PLUS(e2, cnt), v1) -> eval_aux_3(e2, INNER_PLUS(v1, cnt))
   | (OUTER_SUBT(e2, cnt), v1) -> eval_aux_3(e2, INNER_SUBT(v1, cnt))
   | (OUTER_MULT(e2, cnt), v1) -> eval_aux_3(e2, INNER_MULT(v1, cnt))
   | (INNER_PLUS(v1, cnt), v2) -> apply_3(cnt, v1 + v2) 
   | (INNER_SUBT(v1, cnt), v2) -> apply_3(cnt, v1 - v2) 
   | (INNER_MULT(v1, cnt), v2) -> apply_3(cnt, v1 * v2) 

(* eval_aux_2 : state_3 -> int *) 
and eval_aux_3 (e, cnt) = 
   match e with 
   | INT a        -> apply_3(cnt, a) 
   | PLUS(e1, e2) -> eval_aux_3(e1, OUTER_PLUS(e2, cnt)) 
   | SUBT(e1, e2) -> eval_aux_3(e1, OUTER_SUBT(e2, cnt)) 
   | MULT(e1, e2) -> eval_aux_3(e1, OUTER_MULT(e2, cnt)) 

(* eval_3 : expr -> int *) 
let eval_3 e = eval_aux_3(e, ID) 


(* Of course the cnt_dfc type is really a list containing six 
   kinds of elements (being used by the code like a push/pop stack). 
   So let's make that explicit.
*)
  
type tag = 
  | O_PLUS of expr
  | I_PLUS of int 
  | O_SUBT of expr
  | I_SUBT of int 
  | O_MULT of expr
  | I_MULT of int 

type cnt_4 = tag list 

type state_4 = expr * cnt_4

(* apply_4 : cnt_4 * int -> int *) 
let rec apply_4 = function 
   | ([],              v)  -> v 
   | ((O_PLUS e2) :: cnt, v1) -> eval_aux_4(e2, (I_PLUS v1) :: cnt)
   | ((O_SUBT e2) :: cnt, v1) -> eval_aux_4(e2, (I_SUBT v1) :: cnt)
   | ((O_MULT e2) :: cnt, v1) -> eval_aux_4(e2, (I_MULT v1) :: cnt)
   | ((I_PLUS v1) :: cnt, v2) -> apply_4(cnt, v1 + v2)
   | ((I_SUBT v1) :: cnt, v2) -> apply_4(cnt, v1 - v2)
   | ((I_MULT v1) :: cnt, v2) -> apply_4(cnt, v1 * v2)

(* eval_aux_4 : state_4 -> int *) 
and eval_aux_4 (e, cnt) = 
   match e with 
   | INT a        -> apply_4(cnt, a) 
   | PLUS(e1, e2) -> eval_aux_4(e1, O_PLUS(e2) :: cnt) 
   | SUBT(e1, e2) -> eval_aux_4(e1, O_SUBT(e2) :: cnt) 
   | MULT(e1, e2) -> eval_aux_4(e1, O_MULT(e2) :: cnt) 

(* eval_4 : expr -> int *) 
let eval_4 e = eval_aux_4(e, []) 



(* two state accumulator *) 
type acc = 
  | A_INT of int 
  | A_EXP of expr 

type cnt_5 = cnt_4

type state_5 = cnt_5 * acc 

(* step : state_5 -> state_5 *) 
let step_5 = function 
   | (cnt,           A_EXP (INT a)) -> (cnt, A_INT a)
   | (cnt,    A_EXP (PLUS(e1, e2))) -> (O_PLUS(e2) :: cnt, A_EXP e1) 
   | (cnt,    A_EXP (SUBT(e1, e2))) -> (O_SUBT(e2) :: cnt, A_EXP e1) 
   | (cnt,    A_EXP (MULT(e1, e2))) -> (O_MULT(e2) :: cnt, A_EXP e1) 
   | ((O_PLUS e2) :: cnt, A_INT v1) -> ((I_PLUS v1) :: cnt, A_EXP e2)
   | ((O_SUBT e2) :: cnt, A_INT v1) -> ((I_SUBT v1) :: cnt, A_EXP e2)
   | ((O_MULT e2) :: cnt, A_INT v1) -> ((I_MULT v1) :: cnt, A_EXP e2)
   | ((I_PLUS v1) :: cnt, A_INT v2) -> (cnt, A_INT (v1 + v2))
   | ((I_SUBT v1) :: cnt, A_INT v2) -> (cnt, A_INT (v1 - v2))
   | ((I_MULT v1) :: cnt, A_INT v2) -> (cnt, A_INT (v1 * v2))
   | ([],                  A_INT v) -> ([], A_INT v) 

(* driver : state_5 -> int 

   This is clearly TAIL RECURSIVE! 
*) 
let rec driver_5 = function 
    | ([], A_INT v) -> v
    | state         -> driver_5 (step_5 state) 

(* eval_5 : expr -> int *) 
let eval_5 e = driver_5([], A_EXP e) 



(* 

Split stack into directive stack and value stack 

(A_INT v) is now the top of the value stack v :: vs 

(A_EXP e) is now the top of the directive stack  (E e) :: ds 

*) 

let verbose = ref true 

type directive = 
  | E of expr 
  | DO_PLUS 
  | DO_SUBT
  | DO_MULT 

type directive_stack = directive list 

type value_stack = int list 

type state_6 = directive_stack * value_stack 

(* step_6 : state_6 -> state_6 *) 
let step_6 = function 
   | (E(INT v) :: ds,            vs) -> (ds, v :: vs)
   | (E(PLUS(e1, e2)) :: ds,     vs) -> ((E e1) :: (E e2) :: DO_PLUS :: ds, vs)
   | (E(SUBT(e1, e2)) :: ds,     vs) -> ((E e1) :: (E e2) :: DO_SUBT :: ds, vs) 
   | (E(MULT(e1, e2)) :: ds,     vs) -> ((E e1) :: (E e2) :: DO_MULT :: ds, vs) 
   | (DO_PLUS :: ds, v2 :: v1 :: vs) -> (ds, (v1 + v2) :: vs) 
   | (DO_SUBT :: ds, v2 :: v1 :: vs) -> (ds, (v1 - v2) :: vs) 
   | (DO_MULT :: ds, v2 :: v1 :: vs) -> (ds, (v1 * v2) :: vs) 
   | _ -> failwith "eval : runtime error!"        

let string_of_list f l = 
   let rec aux f = function 
     | [] -> ""
     | [t] -> (f t)
     | t :: rest -> (f t) ^ "; " ^ (aux f rest)
   in "[" ^ (aux f l) ^ "]"


let rec string_of_expr = function 
   | INT a          -> "INT(" ^ (string_of_int a) ^ ")"
   | PLUS(e1, e2)   -> "PLUS(" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
   | SUBT(e1, e2)   -> "SUBT(" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
   | MULT(e1, e2)   -> "MULT(" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"

let string_of_directive = function 
  | E e     -> "E(" ^ (string_of_expr e) ^ ")"
  | DO_PLUS -> "DO_PLUS" 
  | DO_SUBT -> "DO_SUBT" 
  | DO_MULT -> "DO_MULT" 


let string_of_state6 n = function 
  | (ds, vs) -> "\nstate " 
                 ^ (string_of_int n) 
                 ^ "\nDS = " ^ (string_of_list string_of_directive (List.rev ds)) 
                 ^ "\nVS = " ^ (string_of_list string_of_int (List.rev vs))



(* driver_6 : state_6 -> int *) 
let rec driver_6 n state = 
    let _ = if !verbose then print_string(string_of_state6 n state) else () in 
    match state with 
    | ([], [v]) -> v
    | _         -> driver_6 (n+1) (step_6 state) 

let eval_6 e = driver_6 1 ([E e], []) 


(* Now refactor this evaluator so that 
   the decomposition of the expression is done first. 
   
*) 
type instr = 
  | Ipush of int 
  | Iplus  
  | Isubt  
  | Imult  

type code = instr list 

type state_7 = code * value_stack 

(* compile : expr -> code *) 
let rec compile = function 
   | INT a          -> [Ipush a] 
   | PLUS(e1, e2)   -> (compile e1) @ (compile e2) @ [Iplus] 
   | SUBT(e1, e2)   -> (compile e1) @ (compile e2) @ [Isubt] 
   | MULT(e1, e2)   -> (compile e1) @ (compile e2) @ [Imult] 

(* step_7 : state_7 -> state_7 *) 
let step_7 = function 
   | (Ipush v :: is,      vs) ->  (is, v :: vs)
   | (Iplus :: is, v2::v1::vs) -> (is, (v1 + v2) :: vs)
   | (Isubt :: is, v2::v1::vs) -> (is, (v1 - v2) :: vs)
   | (Imult :: is, v2::v1::vs) -> (is, (v1 * v2) :: vs)
   | _ -> failwith "eval : runtime error!"        

let rec string_of_instr = function 
  | Ipush a -> "push " ^ (string_of_int a)
  | Iplus -> "add"
  | Isubt -> "sub"
  | Imult -> "mul"


let string_of_state7 n = function 
  | (is, vs) -> "\nstate " 
                 ^ (string_of_int n) 
                 ^ "\nIS = " ^ (string_of_list string_of_instr (List.rev is)) 
                 ^ "\nVS = " ^ (string_of_list string_of_int (List.rev vs))


(* driver_7 : state_7 -> int *) 
let rec driver_7 n state =
    let _ = if !verbose then print_string(string_of_state7 n state) else () in 
    match state with 
    | ([], [v]) -> v
    | _ -> driver_7 (n+1) (step_7 state)

let eval_7 e = driver_7 1 (compile e, []) 

(* testing *) 

let test_all e = [eval e; eval_2 e; eval_3 e; eval_4 e; eval_5 e; eval_6 e; eval_7 e] 

let v101 = PLUS(INT 89, MULT(INT 2, SUBT(INT 10, INT 4))) 

let v = PLUS(MULT(INT 89, INT 2), SUBT(INT 10, INT 4)) 

(* for testing ... *) 

let one = INT 1
let two = INT 2 

(* 2^n *)
let rec two_to_n n = 
    if n < 0 
    then failwith "two_to_n : expects non-negative int!"
    else if n = 0
         then one 
         else MULT(two, two_to_n (n-1))


(* 2^0 + 2^1 + 2^2 + ... + 2^n *) 

let rec two_sum n = 
    if n < 0 
    then failwith "two_sim : expects non-negative int!"
    else if n = 0 
         then one 
         else PLUS(two_to_n n, two_sum (n -1))

(*  Note : looks like OCaml give me 62 bit ints on my macbook : 

> eval (two_sum 61);;
- : int =
4611686018427387903

>  4611686018427387903 +1;;
- : int = -4611686018427387904 
*) 
