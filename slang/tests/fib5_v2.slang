
let h(p : int * int) : int  = 
  let fib( m : int) : int = 
    if m = 0
    then snd p  
    else if m = 1
         then snd p 
         else fib (m - 1) + fib (m -2) 
         end 
    end 
   in 
      fib(fst p) 
   end 
in 
    h(5, 1) 
end 
