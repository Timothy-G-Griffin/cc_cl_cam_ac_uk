
(* test store, sequence, assignment *) 

let x : int ref = ref 0 
in let y : int ref = ref 0 
   in begin 
        x := 10; 
        y := 1; 
        x := 100; 
        !x + !y
      end   
   end 
end 

