(**************************************
Compiler Construction 2016
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 

(* The "defunctionalisation" (dfc) transformation 
   replaces functions with elements in a data structure. 
   It also defines an "apply" function that simulates
   function application on this representation. 


   Suppose we have a (fun x -> e) in our code.  Let 
   y1 :t1, .... yn : tn be the free variables of e, not 
   inluding x.  This "lambda" is replaced by C(y1, y2, ..., yn) 
   where C is a constructor of a new datatype 

   type funs = 
     ...
     | C of t1 * t2 * ... tn 
     ... 

   We have an associated clause in our new apply function 

   let apply funs = functon 
     ...
     | (C(y1, y2, ..., yn), x) -> e' 
     ... 

   where e' is the dfc(e). Finally, we replace every 

       f(e1, e2, ..., en) 

   where this lambda could be applied is replaced by 


      apply(f, e1', e2', ..., en') 

   where each ei' = dfc(ei). 

*) 

(* First a very simple example *) 


(* sum : 'a * 'a * ('a -> int) -> int *) 
let sum (a, b, f) = (f a) + (f b) 

(* test : int * int -> int *) 
let test (x, y) = 
      (sum (x, y, fun z -> z * y))
    * (sum (x, y, fun w -> (w * x) + y))

(* after the dfc transformation : *) 

type funs = FUN1 of int | FUN2 of int * int

(* apply_funs : funs * int -> int *) 
let apply_funs = function 
  | (FUN1 y,     z) -> z * y
  | (FUN2(x, y), w) -> (w * x) + y

(* sum_dfc :  int * int * funs -> int *) 
let sum_dfc (a, b, f) = apply_funs(f, a) + apply_funs(f, b) 

(* test_dfc : int * int -> int *) 
let test_dfc (x, y) = (sum_dfc (x, y, FUN1 y)) * (sum_dfc (x, y, FUN2(x,y)))
      
(* Observation. 
   We have specialized sum_dfc to a version that only calls 
   the functions used by test, represented now as elements of 
   the datastructure funs.  Thus the type of sum_dfc is not 
   as general as that of sum. 
*)     






