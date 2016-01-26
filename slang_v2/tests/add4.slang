
let add(p : int * int) : int = 
    let h(z : int) : int = z + (snd p)
    in 
       h(fst p)
    end    
in 
  add(1, add(88, 12))
end 
