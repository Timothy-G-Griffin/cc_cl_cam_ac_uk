let f(x : unit) : int =
  let f (y : int) : int =
    if y = 0 then 1 else f (y-1) end
  in f 5 end
in f () end