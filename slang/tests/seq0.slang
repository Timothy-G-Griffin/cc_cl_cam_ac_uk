
(* test store, sequence, assignment *) 

let x : int ref = ref 100
in let y : int ref = ref 1 
   in begin 
        !x + !y
      end   
   end 
end 

