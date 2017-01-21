
(* test store *) 

let x : int ref = ref 100 
in let y : int ref = ref 1 
   in 
       !x + !y 
   end 
end 

