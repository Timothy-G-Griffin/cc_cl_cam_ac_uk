let z1(x1: int) : int = x1 + 0 in 
 let z2(x2: int) : int = z1(x2) + 1 in
    z2(1)
 end 
end
