
let add(p : int * int) : int = (fst p) + (snd p)
in 
  add (1, add (88, 12))
end 
