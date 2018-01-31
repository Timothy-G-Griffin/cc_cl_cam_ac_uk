let f (p: int * int * int * int) : int =
  let x: int = fst p in
    let y: int = snd p in
    x + y
  end
end in
  f (1,5,-9,1)
end