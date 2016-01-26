let fib( m : int) : int = 
    if m = 0
    then 1 
    else if m = 1 
         then 1 
         else fib (m - 1) + fib (m -2) 
         end 
    end 
in 
    fib(5) 
end 
