(**************************************
Compiler Construction 2016
Computer Laboratory 
University of Cambridge 
Timothy G. Griffin (tgg22@cam.ac.uk) 
*****************************************) 


(* gcd : int * int -> int 

   This is a tail-recursive function. 
*) 
let rec gcd(m, n) = 
    if m = n 
    then m 
    else if m < n 
         then gcd(m, n - m)
         else gcd(m - n, n)

let l1 = List.map gcd [(24, 638); (17, 289); (31, 1889)] 



(* gcd_iter : int * int -> int 

   We can replace recursion with simple iteration. 
   The OCaml compiler will do something similar 
   to gcd (above), but on a lower-level intermediate 
   representation. 
*) 

let gcd_iter (m, n) = 
    let rm = ref m 
    in let rn = ref n 
    in let result = ref 0 
    in let not_done = ref true 
    in let _ = while !not_done 
               do 
                 if !rm = !rn 
                 then (not_done := false; result := !rm) 
                 else if !rm < !rn 
                      then rn := !rn - !rm 
                      else rm := !rm - !rn
               done 
    in !result           
    
let l2 = List.map gcd_iter [(24, 638); (17, 289); (31, 1889)] 


(* Here is the mother of all tail-recurive functions *) 
let rec my_while (test, body) () = 
        if test() then my_while(test, body) (body())

(* we can eliminate recursion with iteration! *) 
let my_while_iter (test, body) () = while test() do body() done 
        
        




