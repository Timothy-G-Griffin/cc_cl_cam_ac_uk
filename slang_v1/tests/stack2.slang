let z1(x1: int) : int = x1 - 1 in 
 let z2(x2: int) : int = z1(x2) - 2  in 
  let z3(x3: int) : int = z2(x3) - 3 in  
    let z4(x4: int) : int = z3(x4) - 4 in  
      let z5(x5: int) : int = z4(x5) - 5 in  
         15 + z5(10 + z4(6 + z3(3 + z2(1 + z1(17)))))
      end 
    end 
  end 
 end 
end
