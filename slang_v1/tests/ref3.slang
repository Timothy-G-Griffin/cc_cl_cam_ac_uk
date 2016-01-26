
(* test store, sequence, assignment *) 

let x : (int ref) ref = ref (ref 777)  
in begin 
    (!x) := 101; 
    !!x 
   end 
end 

