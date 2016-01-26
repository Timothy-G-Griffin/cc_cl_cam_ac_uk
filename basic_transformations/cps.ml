(**************************************
Compiler Construction 2016
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 

(* fib : int -> int *) 
let rec fib m =
    if m = 0 
    then 1 
    else if m = 1 
         then 1 
         else fib(m - 1) + fib (m - 2) 


(* fib_cps : ((int -> int) * int) -> int  

   note : fib_cps(f, m) = f(fib m) 

*) 
let rec fib_cps (cnt, m) =
    if m = 0 
    then cnt 1 
    else if m = 1 
         then cnt 1 
         else fib_cps((fun a -> fib_cps((fun b -> cnt (a + b)), m - 2)),  
                      m - 1)

(* the initial continuation *) 
let id x = x 

(* 
   fib_1 : int -> int 

   note : fib_1 m = fib_cps (id, m) = id(fib m) = fib m. 
*) 
let fib_1 m = fib_cps(id, m)


