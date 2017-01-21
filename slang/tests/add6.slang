
let add(p : int * (int * (int * int))) : int = (fst p) + ((fst (snd p)) + (fst (snd (snd p))))
in 
  add(1, (88, (12, 10000)))
end 
