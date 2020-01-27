(**************************************
Compiler Construction 2020 
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
   5) eval_5 : combine apply_4 and eval_aux_4 into a two-state machine 
   6) eval_6 : a tricky part : split stack into two : directive stack and value stack 
   7) eval_7 : another tricky part: re-factor. instead of inspection of expressions on the 
               stack, do the inspection before running. That is, *compile* the expression into 
               instructions so that eval_6 e = eval_7 (compile e). 

   Note: this improves on an earlier version by including a 
   more detailed description of the transformations eval_5 --> eval_6 --> eval_7. 
*) 


(* for use below *)

let verbose = ref true 
			 
let string_of_list f l = 
   let rec aux f = function 
     | [] -> ""
     | [t] -> (f t)
     | t :: rest -> (f t) ^ "; " ^ (aux f rest)
   in "[" ^ (aux f l) ^ "]"

(* Datatype for expression 
 
  For example, "2 + ((5 -1) * 4)"  is represented by 

  PLUS(INT 2, MULT(SUBT(INT 5, INT 1), INT 4))

*) 
type expr = 
   | INT of int 
   | PLUS of expr * expr
   | SUBT of expr * expr
   | MULT of expr * expr

(* a few expressions for testing *)
(* 1 + (2 + (3 + (4 + 5))) *) 		      
let test1 = PLUS(INT 1, PLUS(INT 2, PLUS(INT 3, PLUS(INT 4, INT 5))))
(* (((1 + 2) + 3) + 4) + 5 *) 		
let test2 = PLUS(PLUS(PLUS(PLUS(INT 1, INT 2), INT 3), INT 4), INT 5) 

(* string_of_expr : expr -> string *) 		      
let rec string_of_expr = function 
   | INT a          -> string_of_int a
   | PLUS(e1, e2)   -> "(" ^ (string_of_expr e1) ^ " + " ^ (string_of_expr e2) ^ ")"
   | SUBT(e1, e2)   -> "(" ^ (string_of_expr e1) ^ " - " ^ (string_of_expr e2) ^ ")"
   | MULT(e1, e2)   -> "(" ^ (string_of_expr e1) ^ " * " ^ (string_of_expr e2) ^ ")"


(******************************* eval **************************************) 
		      
(* eval : expr -> int 
   a simple recursive evaluator for expressions. 

   This is a very simple version of our interpreter_0. 

   Claim 1 : This interpreter (eval) is correct! 
   Proof: by inspection ;-) 
*) 
let rec eval = function 
   | INT a             -> a 
   | PLUS(e1, e2)   -> (eval e1) + (eval e2) 
   | SUBT(e1, e2)   -> (eval e1) - (eval e2) 
   | MULT(e1, e2)   -> (eval e1) * (eval e2) 


(******************************* eval_2 **************************************)  

(* apply cps transform to the interpreter eval *) 

type cnt_2   = int -> int 
type state_2 = expr * cnt_2 

(* eval_aux_2 : state_2 -> int *) 
let rec eval_aux_2 (e, cnt) = 
   match e with 
   | INT a        -> cnt a 
   | PLUS(e1, e2) -> eval_aux_2(e1, fun v1 -> eval_aux_2(e2, fun v2 -> cnt(v1 + v2)))
   | SUBT(e1, e2) -> eval_aux_2(e1, fun v1 -> eval_aux_2(e2, fun v2 -> cnt(v1 - v2)))
   | MULT(e1, e2) -> eval_aux_2(e1, fun v1 -> eval_aux_2(e2, fun v2 -> cnt(v1 * v2)))

(* 
    Claim 2 : c (eval e) = eval_aux_2(e, c) 
    Proof: by induction on the structure of e.  
*) 
      
(* id_cnt : cnt_2 *) 
let id_cnt (x : int) = x 

(*  eval_2 : expr -> int *) 
let eval_2 e = eval_aux_2(e, id_cnt) 


(******************************* eval_3 **************************************) 			 

(* Now apply "defunctionalisation" to eval_cps. 

  Represent each c : cnt_2 as < c > : cnt_3 

  For example 

  < fun v1 -> eval_aux_2(e2, fun v2 -> cnt(v1 + v2)) >  
      = OUTER_PLUS(e2, < fun v2 -> cnt(v1 + v2) >)

  < fun v2 -> cnt(v1 + v2) > 
       = INNER_PLUS(v1, < cnt >)

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

(* apply_3 : cnt_3 * int -> int 

   Claim 3 : For c : cnt_2 and v : int,  c(v) = apply_3(< c >, v) AND 
             for all e, c, eval_aux_2 (e, c) =  eval_aux_3 (e, < c >). 
   Proof : by induction on the structure of < c >. 
*) 
let rec apply_3 = function 
   | (ID,                   v) -> v 
   | (OUTER_PLUS(e2, cnt), v1) -> eval_aux_3(e2, INNER_PLUS(v1, cnt))
   | (OUTER_SUBT(e2, cnt), v1) -> eval_aux_3(e2, INNER_SUBT(v1, cnt))
   | (OUTER_MULT(e2, cnt), v1) -> eval_aux_3(e2, INNER_MULT(v1, cnt))
   | (INNER_PLUS(v1, cnt), v2) -> apply_3(cnt, v1 + v2) 
   | (INNER_SUBT(v1, cnt), v2) -> apply_3(cnt, v1 - v2) 
   | (INNER_MULT(v1, cnt), v2) -> apply_3(cnt, v1 * v2) 

(* eval_aux_2 : state_3 -> int 
*) 
and eval_aux_3 (e, cnt) = 
   match e with 
   | INT a        -> apply_3(cnt, a) 
   | PLUS(e1, e2) -> eval_aux_3(e1, OUTER_PLUS(e2, cnt)) 
   | SUBT(e1, e2) -> eval_aux_3(e1, OUTER_SUBT(e2, cnt)) 
   | MULT(e1, e2) -> eval_aux_3(e1, OUTER_MULT(e2, cnt)) 

(* eval_3 : expr -> int *) 
let eval_3 e = eval_aux_3(e, ID) 



(******************************* eval_4 **************************************) 			 

(* The cnt_3 type can be thought of as a list containing six 
   kinds of elements (being used by the code like a push/pop stack). 
   So let's make that explicit.

   If c : cnt_3, then we represent it as < c > : cnt_4 as
 
   < ID >  = [] 
   < OUTER_OP(e, cnt) > = O_OP(e) :: < cnt > 
   < INNER_OP(e, cnt) > = I_OP(e) :: < cnt > 

   for OP in {PLUS, SUBT, MULT} 
*)
			 
type tag = 
  | O_PLUS of expr
  | I_PLUS of int 
  | O_SUBT of expr
  | I_SUBT of int 
  | O_MULT of expr
  | I_MULT of int

let string_of_tag = function 
  | O_PLUS e -> "O+(" ^ (string_of_expr e) ^ ")"
  | I_PLUS n -> "I+(" ^ (string_of_int n)  ^ ")"
  | O_SUBT e -> "O-(" ^ (string_of_expr e) ^ ")"		    
  | I_SUBT n -> "I-(" ^ (string_of_int n)  ^ ")"
  | O_MULT e -> "O*(" ^ (string_of_expr e) ^ ")"		    
  | I_MULT n -> "I*(" ^ (string_of_int n)  ^ ")"
					
type cnt_4   = tag list 
type state_4 = expr * cnt_4

(* apply_4 : cnt_4 * int -> int 

   Claim 4 : For all e, c 
             (1) for all v, apply_3(c, v)  = apply_4(< c >, v) and 
             (2) eval_aux_3 (e, c) =  eval_aux_4 (e, < c >). 
   Proof : by induction on the structure of e and c. 
*) 
let rec apply_4 = function 
   | ([],              v)  -> v 
   | ((O_PLUS e2) :: cnt, v1) -> eval_aux_4(e2, (I_PLUS v1) :: cnt)
   | ((O_SUBT e2) :: cnt, v1) -> eval_aux_4(e2, (I_SUBT v1) :: cnt)
   | ((O_MULT e2) :: cnt, v1) -> eval_aux_4(e2, (I_MULT v1) :: cnt)
   | ((I_PLUS v1) :: cnt, v2) -> apply_4(cnt, v1 + v2)
   | ((I_SUBT v1) :: cnt, v2) -> apply_4(cnt, v1 - v2)
   | ((I_MULT v1) :: cnt, v2) -> apply_4(cnt, v1 * v2)

(* eval_aux_4 : state_4 -> int 
*) 
and eval_aux_4 (e, cnt) = 
   match e with 
   | INT a        -> apply_4(cnt, a) 
   | PLUS(e1, e2) -> eval_aux_4(e1, O_PLUS(e2) :: cnt) 
   | SUBT(e1, e2) -> eval_aux_4(e1, O_SUBT(e2) :: cnt) 
   | MULT(e1, e2) -> eval_aux_4(e1, O_MULT(e2) :: cnt) 

(* eval_4 : expr -> int *) 
let eval_4 e = eval_aux_4(e, []) 



(******************************* eval_5 **************************************) 			 

(* Eliminate mutual tail recursion 

   Note that apply_4 and are mutually recursive with types 

   apply_4    : cnt_4 * int -> int
   eval_aux_4 : expr * cnt_4 -> int  

   We can always combine such functions int a single recursive function 
   by "tagging" the two inputs. 

*) 
type state_5 = APPLY of cnt_4 * int  (* APPLY(c, m) represents a call apply_4(c, m) *) 
 	     |  EVAL of cnt_4 * expr (* EVAL(c, e) represents a call eval_aux_4(e, c) *)

let string_of_state5 n = function 
  | EVAL(cnt, e) -> "step " 
                    ^ (string_of_int n)
		    ^ " (EVAL)"
                    ^ "\ncnt = " ^ (string_of_list string_of_tag (List.rev cnt)) 
                    ^ "\nexpr = " ^ (string_of_expr e)
                    ^ "\n" 				      
  | APPLY(cnt, m) -> "step " 
                    ^ (string_of_int n)
		    ^ " (APPLY)"
                    ^ "\ncnt = " ^ (string_of_list string_of_tag (List.rev cnt)) 
                    ^ "\nint = " ^ (string_of_int m)
                    ^ "\n" 				     


(* step : state_5 -> state_5 *) 

let step_5 = function
   | EVAL(cnt,              INT m) -> APPLY(cnt, m)
   | EVAL(cnt,       PLUS(e1, e2)) -> EVAL((O_PLUS e2) :: cnt, e1)
   | EVAL(cnt,       SUBT(e1, e2)) -> EVAL((O_SUBT e2) :: cnt, e1)
   | EVAL(cnt,       MULT(e1, e2)) -> EVAL((O_MULT e2) :: cnt, e1)
   | APPLY((O_PLUS e2) :: cnt, v1) -> EVAL((I_PLUS v1) :: cnt, e2)
   | APPLY((O_SUBT e2) :: cnt, v1) -> EVAL((I_SUBT v1) :: cnt, e2)
   | APPLY((O_MULT e2) :: cnt, v1) -> EVAL((I_MULT v1) :: cnt, e2)
   | APPLY((I_PLUS v1) :: cnt, v2) -> APPLY(cnt, v1 + v2)
   | APPLY((I_SUBT v1) :: cnt, v2) -> APPLY(cnt, v1 - v2)
   | APPLY((I_MULT v1) :: cnt, v2) -> APPLY(cnt, v1 * v2)
   | APPLY([],                  v) -> APPLY([],        v)
				      
(* driver : state_5 -> int 
   This is clearly TAIL RECURSIVE! 

   Claim.5 : (ignoring first argument that counts steps) 
      driver_5 (APPLY(c, m)) = apply_4(c, m) 
      driver_5 (EVAL(c, e))  = eval_aux_4 (e, c)
   Proof : induction on e and c. 

*) 
let rec driver_5 n state = 
    let _ = if !verbose then print_string(string_of_state5 n state) else () in
    match state with   
    | APPLY([], v) -> v
    | _            -> driver_5 (n+1) (step_5 state) 

(* eval_5 : expr -> int *) 
let eval_5 e = driver_5 1 (EVAL([], e)) 


(******************************* eval_6 **************************************)  

(* 
  Now for a tricky part! 

  On close inspection, the continuations in state_5 actually interleave 
  two distinct stacks --- one for "directives" (expressions and instructions) 
  and the other for integer values.  Call the first the "directive stack" 
  and the second the "value stack". 
*) 

type directive = 
  | E of expr 
  | DO_PLUS 
  | DO_SUBT
  | DO_MULT 

type directive_stack = directive list 
type value_stack     = int list 
type state_6         = directive_stack * value_stack

(* 
   state_6 states have the form (DS, VS), where DS is a directive stack 
   and VS is a value stack 

   How are we going to state correctness? We need a translation 
   T :state_5 -> state_6 so that 
   
   Claim 6.1 : Suppose S1, S2 :state_5, then 
      if S1 --step_5-->+ S2, then  T(S1) --step_6-->* T(S2). 

      Here "S1 --step_5-->+ S2" means one or more step_5 steps from S1 to S2, 
      and "T(S1) --step_6-->* T(S2)" means zero or more step_6 steps from T(S1) to T(S2).
  
   On inspection of the traces (see below) we can derive the T as follows. 


   T(EVAL(c, e))              = ((E e) :: DS(c),  VS(c)) 

   T(APPLY((I_OP n) :: c, m)) = (DO_OP :: DS(c),  m :: n :: VS(c)) 
   T(APPLY(c, m))             = (DS(c),  [m] )                   (if no I_OP in c) 
                              = ((E (INT m)) :: DS(c),  VS(c))   (otherwise) 
 
   DS([]) = [] 
   DS((O_OP e) :: c) = [E(e); DO_OP] @ DS(c) 
   DS((I_OP n) :: c) = DO_OP :: DS(c) 

   VS([]) = [] 
   VS((I_OP n) :: c) = n :: VS(c) 
   VS((O_OP e) :: c) = VS(c) 

   Proof of Claim 6.1. Look at each step_5 step and do a case analysis ... 
*) 
					   

let string_of_directive = function 
  | E e     -> string_of_expr e
  | DO_PLUS -> "DO+" 
  | DO_SUBT -> "DO-" 
  | DO_MULT -> "DO*" 

let string_of_state6 n = function 
  | (ds, vs) -> "state " 
                 ^ (string_of_int n) 
                 ^ "\nDS = " ^ (string_of_list string_of_directive (List.rev ds)) 
                 ^ "\nVS = " ^ (string_of_list string_of_int (List.rev vs))
                 ^ "\n" 				 

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

(* driver_6 : state_6 -> int *) 
let rec driver_6 n state = 
    let _ = if !verbose then print_string(string_of_state6 n state) else () in 
    match state with 
    | ([], [v]) -> v
    | _         -> driver_6 (n+1) (step_6 state) 

let eval_6 e = driver_6 1 ([E e], []) 

(*  side-by-side traces 

eval_5 test1;;                       | eval_6 test1;
---------------------------------------------------------------------------
step 1 (EVAL)                        | state 1
cnt = []                             | DS = [(1 + (2 + (3 + (4 + 5))))]
expr = (1 + (2 + (3 + (4 + 5))))     | VS = []
---------------------------------------------------------------------------
step 2 (EVAL)                        | state 2
cnt = [O+((2 + (3 + (4 + 5))))]      | DS = [DO+; (2 + (3 + (4 + 5))); 1]
expr = 1                             | VS = []
---------------------------------------------------------------------------
step 3 (APPLY)                       | 
cnt = [O+((2 + (3 + (4 + 5))))]      | 
int = 1                              | 
---------------------------------------------------------------------------
step 4 (EVAL)                        | state 3
cnt = [I+(1)]                        | DS = [DO+; (2 + (3 + (4 + 5)))]
expr = (2 + (3 + (4 + 5)))           | VS = [1]
---------------------------------------------------------------------------
step 5 (EVAL)                        | state 4
cnt = [I+(1); O+((3 + (4 + 5)))]     | DS = [DO+; DO+; (3 + (4 + 5)); 2]
expr = 2                             | VS = [1]
---------------------------------------------------------------------------
step 6 (APPLY)                       | 
cnt = [I+(1); O+((3 + (4 + 5)))]     | 
int = 2                              | 
---------------------------------------------------------------------------
step 7 (EVAL)                        | state 5
cnt = [I+(1); I+(2)]                 | DS = [DO+; DO+; (3 + (4 + 5))]
expr = (3 + (4 + 5))                 | VS = [1; 2]
---------------------------------------------------------------------------
step 8 (EVAL)                        | state 6
cnt = [I+(1); I+(2); O+((4 + 5))]    | DS = [DO+; DO+; DO+; (4 + 5); 3]
expr = 3                             | VS = [1; 2]
---------------------------------------------------------------------------
step 9 (APPLY)                       | 
cnt = [I+(1); I+(2); O+((4 + 5))]    | 
int = 3                              | 
---------------------------------------------------------------------------
step 10 (EVAL)                       | state 7
cnt = [I+(1); I+(2); I+(3)]          | DS = [DO+; DO+; DO+; (4 + 5)]
expr = (4 + 5)                       | VS = [1; 2; 3]
---------------------------------------------------------------------------
step 11 (APPLY)                      | state 8
cnt = [I+(1); I+(2); I+(3); O+(5)]   | DS = [DO+; DO+; DO+; DO+; 5; 4]
int = 4                              | VS = [1; 2; 3]
---------------------------------------------------------------------------
step 12 (EVAL)                       | 
cnt = [I+(1); I+(2); I+(3); O+(5)]   | 
expr = 4                             | 
---------------------------------------------------------------------------
step 13 (EVAL)                       | state 9
cnt = [I+(1); I+(2); I+(3); I+(4)]   | DS = [DO+; DO+; DO+; DO+; 5]
expr = 5                             | VS = [1; 2; 3; 4]
---------------------------------------------------------------------------
step 14 (APPLY)                      | state 10
cnt = [I+(1); I+(2); I+(3); I+(4)]   | DS = [DO+; DO+; DO+; DO+]
int = 5                              | VS = [1; 2; 3; 4; 5]
---------------------------------------------------------------------------
step 15 (APPLY)                      | state 11
cnt = [I+(1); I+(2); I+(3)]          | DS = [DO+; DO+; DO+]
int = 9                              | VS = [1; 2; 3; 9]
---------------------------------------------------------------------------
step 16 (APPLY)                      | state 12
cnt = [I+(1); I+(2)]                 | DS = [DO+; DO+]
int = 12                             | VS = [1; 2; 12]
---------------------------------------------------------------------------
step 17 (APPLY)                      | state 13
cnt = [I+(1)]                        | DS = [DO+]
int = 14                             | VS = [1; 14]
---------------------------------------------------------------------------
step 18 (APPLY)                      | state 14
cnt = []                             | DS = []
int = 15                             | VS = [15]
---------------------------------------------------------------------------



eval_5 test2;;                       | eval_6 test
---------------------------------------------------------------------------
step 1 (EVAL)                        | state 1
cnt = []                             | DS = [((((1 + 2) + 3) + 4) + 5)]
expr = ((((1 + 2) + 3) + 4) + 5)     | VS = []
---------------------------------------------------------------------------
step 2 (EVAL)                        | state 2
cnt = [O+(5)]                        | DS = [DO+; 5; (((1 + 2) + 3) + 4)]
expr = (((1 + 2) + 3) + 4)           | VS = []
---------------------------------------------------------------------------
step 3 (EVAL)                        | state 3
cnt = [O+(5); O+(4)]                 | DS = [DO+; 5; DO+; 4; ((1 + 2) + 3)]
expr = ((1 + 2) + 3)                 | VS = []
---------------------------------------------------------------------------
step 4 (EVAL)                        | state 4
cnt = [O+(5); O+(4); O+(3)]          | DS = [DO+; 5; DO+; 4; DO+; 3; (1 + 2)]
expr = (1 + 2)                       | VS = []
---------------------------------------------------------------------------
step 5 (EVAL)                        | state 5
cnt = [O+(5); O+(4); O+(3); O+(2)]   | DS = [DO+; 5; DO+; 4; DO+; 3; DO+; 2; 1]
expr = 1                             | VS = []
---------------------------------------------------------------------------
step 6 (APPLY)                       | 
cnt = [O+(5); O+(4); O+(3); O+(2)]   | 
int = 1                              | 
---------------------------------------------------------------------------
step 7 (EVAL)                        | state 6
cnt = [O+(5); O+(4); O+(3); I+(1)]   | DS = [DO+; 5; DO+; 4; DO+; 3; DO+; 2]
expr = 2                             | VS = [1]
---------------------------------------------------------------------------
step 8 (APPLY)                       | state 7
cnt = [O+(5); O+(4); O+(3); I+(1)]   | DS = [DO+; 5; DO+; 4; DO+; 3; DO+]
int = 2                              | VS = [1; 2]
---------------------------------------------------------------------------
step 9 (APPLY)                       | state 8
cnt = [O+(5); O+(4); O+(3)]          | DS = [DO+; 5; DO+; 4; DO+; 3]
int = 3                              | VS = [3]   
---------------------------------------------------------------------------
step 10 (EVAL)                       | 
cnt = [O+(5); O+(4); I+(3)]          | 
expr = 3                             | 
---------------------------------------------------------------------------
step 11 (APPLY)                      | state 9
cnt = [O+(5); O+(4); I+(3)]          | DS = [DO+; 5; DO+; 4; DO+]
int = 3                              | VS = [3; 3]
---------------------------------------------------------------------------
step 12 (APPLY)                      | state 10
cnt = [O+(5); O+(4)]                 | DS = [DO+; 5; DO+; 4]
int = 6                              | VS = [6]   
---------------------------------------------------------------------------
step 13 (EVAL)                       | 
cnt = [O+(5); I+(6)]                 | 
expr = 4                             | 
---------------------------------------------------------------------------
step 14 (APPLY)                      | state 11
cnt = [O+(5); I+(6)]                 | DS = [DO+; 5; DO+]
int = 4                              | VS = [6; 4]
---------------------------------------------------------------------------
step 15 (APPLY)                      | state 12
cnt = [O+(5)]                        | DS = [DO+; 5]
int = 10                             | VS = [10]
---------------------------------------------------------------------------
step 16 (EVAL)                       | 
cnt = [I+(10)]                       | 
expr = 5                             | 
---------------------------------------------------------------------------
step 17 (APPLY)                      | state 13
cnt = [I+(10)]                       | DS = [DO+]
int = 5                              | VS = [10; 5]
---------------------------------------------------------------------------
step 18 (APPLY)                      | state 14
cnt = []                             | DS = []
int = 15                             | VS = [15]
---------------------------------------------------------------------------

*) 
			
(******************************* eval_7 **************************************) 			 			
(* 
   Another tricky part!  

   If we look at the these rules of step_6 

   | (E(INT v) :: ds,            vs) -> (ds, v :: vs)
   | (E(PLUS(e1, e2)) :: ds,     vs) -> ((E e1) :: (E e2) :: DO_PLUS :: ds, vs)
   | (E(SUBT(e1, e2)) :: ds,     vs) -> ((E e1) :: (E e2) :: DO_SUBT :: ds, vs) 
   | (E(MULT(e1, e2)) :: ds,     vs) -> ((E e1) :: (E e2) :: DO_MULT :: ds, vs) 

   we see that they only "take apart" an expression. Why don't 
   we do this instead at "compile time" rather than at "runtime"? 
   In other words, let's re-factor eval_6 so and define a function 
   "compile" and eval_7 so that  eval_6(e) = eval_7(compile(e)). 
*) 
type instr = 
  | Ipush of int 
  | Iplus  
  | Isubt  
  | Imult  

type code = instr list 

type state_7 = code * value_stack 

(* compile : expr -> code 
 
   Note similarity with the lines of step_6 mentioned above! 
*) 
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
  | (is, vs) -> "state " 
                 ^ (string_of_int n) 
                 ^ "\nIS = " ^ (string_of_list string_of_instr (List.rev is)) 
                 ^ "\nVS = " ^ (string_of_list string_of_int (List.rev vs))
                 ^ "\n" 


(* driver_7 : state_7 -> int *) 
let rec driver_7 n state =
    let _ = if !verbose then print_string(string_of_state7 n state) else () in 
    match state with 
    | ([], [v]) -> v
    | _ -> driver_7 (n+1) (step_7 state)

let eval_7 e = driver_7 1 (compile e, []) 


(* 			

    Claim 7: For all e, eval_6(e) = eval_7(compile(e)). 

    How to prove? Again we need a translation from T : state_6 -> state_6. 

    T(DS, VS) = (IS(DS), VS) 

    IS([]) = [] 
    IS(E(INT m) :: DS) = (Ipush m) :: IS(DS)
    IS(E(e) :: DS) = (compile e) @ IS(DS)
    IS(DO_PLUS :: DS) = Iplus :: IS(DS) 
    IS(DO_SUBT :: DS) = Isubt :: IS(DS) 
    IS(DO_MULT :: DS) = Imult :: IS(DS) 


eval_6 test1;;                       | eval_7 test1;
---------------------------------------------------------------------------
state 1                              | state 1
DS = [(1 + (2 + (3 + (4 + 5))))]     | [add; add; add; add; push 5; push 4; push 3; push 2; push 1]
VS = []                              | VS = []
---------------------------------------------------------------------------
state 2                              | 
DS = [DO+; (2 + (3 + (4 + 5))); 1]   | 
VS = []                              | 
---------------------------------------------------------------------------
state 3                              | state 2
DS = [DO+; (2 + (3 + (4 + 5)))]      | IS = [add; add; add; add; push 5; push 4; push 3; push 2]
VS = [1]                             | VS = [1]
---------------------------------------------------------------------------
state 4                              | 
DS = [DO+; DO+; (3 + (4 + 5)); 2]    | 
VS = [1]                             | 
---------------------------------------------------------------------------
state 5                              | state 3
DS = [DO+; DO+; (3 + (4 + 5))]       | IS = [add; add; add; add; push 5; push 4; push 3]
VS = [1; 2]                          | VS = [1; 2]
---------------------------------------------------------------------------
state 6                              | 
DS = [DO+; DO+; DO+; (4 + 5); 3]     | 
VS = [1; 2]                          | 
---------------------------------------------------------------------------
state 7                              | state 4
DS = [DO+; DO+; DO+; (4 + 5)]        | IS = [add; add; add; add; push 5; push 4]
VS = [1; 2; 3]                       | VS = [1; 2; 3]
---------------------------------------------------------------------------
state 8                              | 
DS = [DO+; DO+; DO+; DO+; 5; 4]      | 
VS = [1; 2; 3]                       | 
---------------------------------------------------------------------------
state 9                              | state 5
DS = [DO+; DO+; DO+; DO+; 5]         | IS = [add; add; add; add; push 5]
VS = [1; 2; 3; 4]                    | VS = [1; 2; 3; 4]
---------------------------------------------------------------------------
state 10                             | state 6
DS = [DO+; DO+; DO+; DO+]            | IS = [add; add; add; add]
VS = [1; 2; 3; 4; 5]                 | VS = [1; 2; 3; 4; 5]
---------------------------------------------------------------------------
state 11                             | state 7
DS = [DO+; DO+; DO+]                 | IS = [add; add; add]
VS = [1; 2; 3; 9]                    | VS = [1; 2; 3; 9]
---------------------------------------------------------------------------
state 12                             | state 8
DS = [DO+; DO+]                      | IS = [add; add]
VS = [1; 2; 12]                      | VS = [1; 2; 12]
---------------------------------------------------------------------------
state 13                             | state 9
DS = [DO+]                           | IS = [add]
VS = [1; 14]                         | VS = [1; 14]
---------------------------------------------------------------------------
state 14                             | state 10
DS = []                              | IS = []
VS = [15]                            | VS = [15]
---------------------------------------------------------------------------


eval_6 test2;;                           | eval_7 test2;; 
---------------------------------------------------------------------------
state 1                                  | state 1
DS = [((((1 + 2) + 3) + 4) + 5)]         | IS = [add; push 5; add; push 4; add; push 3; add; push 2; push 1]
VS = []                                  | VS = []
---------------------------------------------------------------------------
state 2                                  | 
DS = [DO+; 5; (((1 + 2) + 3) + 4)]       | 
VS = []                                  |
---------------------------------------------------------------------------
state 3                                  |
DS = [DO+; 5; DO+; 4; ((1 + 2) + 3)]     |
VS = []                                  | 
---------------------------------------------------------------------------
state 4                                  |
DS = [DO+; 5; DO+; 4; DO+; 3; (1 + 2)]   | 
VS = []                                  | 
---------------------------------------------------------------------------
state 5                                  | 
DS = [DO+; 5; DO+; 4; DO+; 3; DO+; 2; 1] | 
VS = []                                  | 
---------------------------------------------------------------------------
state 6                                  | state 2
DS = [DO+; 5; DO+; 4; DO+; 3; DO+; 2]    | IS = [add; push 5; add; push 4; add; push 3; add; push 2]
VS = [1]                                 | VS = [1]
---------------------------------------------------------------------------
state 7                                  | state 3
DS = [DO+; 5; DO+; 4; DO+; 3; DO+]       | IS = [add; push 5; add; push 4; add; push 3; add]
VS = [1; 2]                              | VS = [1; 2]
---------------------------------------------------------------------------
state 8                                  | state 4
DS = [DO+; 5; DO+; 4; DO+; 3]            | IS = [add; push 5; add; push 4; add; push 3]
VS = [3]                                 | VS = [3]
---------------------------------------------------------------------------
state 9                                  | state 5
DS = [DO+; 5; DO+; 4; DO+]               | IS = [add; push 5; add; push 4; add]
VS = [3; 3]                              | VS = [3; 3]
---------------------------------------------------------------------------
state 10                                 | state 6
DS = [DO+; 5; DO+; 4]                    | IS = [add; push 5; add; push 4]
VS = [6]                                 | VS = [6]
---------------------------------------------------------------------------
state 11                                 | state 7
DS = [DO+; 5; DO+]                       | IS = [add; push 5; add]
VS = [6; 4]                              | VS = [6; 4]
---------------------------------------------------------------------------
state 12                                 | state 8
DS = [DO+; 5]                            | IS = [add; push 5]
VS = [10]                                | VS = [10]
---------------------------------------------------------------------------
state 13                                 | state 9
DS = [DO+]                               | IS = [add]
VS = [10; 5]                             | VS = [10; 5]
---------------------------------------------------------------------------
state 14                                 | state 10
DS = []                                  | IS = []
VS = [15]                                | VS = [15]
---------------------------------------------------------------------------
 *) 


(* 
   What can we conclude?

   Claim: For all e, eval(e) = eval_7(e) = driver_7 (compile e, []). 

   We have transformed the recursive "semantics" of expressions
   into a compiler + a stack machine. 
  

*) 			
