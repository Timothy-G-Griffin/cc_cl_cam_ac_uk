let minus(p : int * int) : int = (fst p) - (snd p) in 
let plus(p : int * int) : int = (fst p) + (snd p) in 
let mult(p : int * int) : int = (fst p) * (snd p) in 
   minus(200, mult(9, plus(-3, 14)))
end end end 