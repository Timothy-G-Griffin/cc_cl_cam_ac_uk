
let x : (int -> int) ref = ref (fun (z : int) -> z + 17 end)
in let y : int ref = ref 0 
   in begin 
        y := !x 4; 
        x := (fun (w : int) -> w * 10 end); 
        ((!x) 8) + !y
      end 
   end 
end 

