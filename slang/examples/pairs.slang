(*let (x : int, y : int) = (2,3) in x end*)
(*let f (x : int, y : int * int) : int = #1 y in f (3,(4,5)) end*)
let f (a : int, _ : int) : int = 2 in f (0,0) end
