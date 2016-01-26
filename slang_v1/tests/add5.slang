
let add(p : int * int) : int = 
    let g(w : int) : int = w 
    in 
       let h(z : int) : int = z + g(snd p) 
       in 
          h(fst p)
       end    
   end 
    
in 
  add(1, add(88, 12))
end 
 