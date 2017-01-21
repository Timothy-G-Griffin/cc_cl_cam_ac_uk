let z1(x1: int) : int = x1 + 0 in 
 let z2(x2: int) : int = z1(x2) + 1 in 
  let z3(x3: int) : int = z2(x3) + 2 in  
    let z4(x4: int) : int = z3(x4) + 3 in  
      let z5(x5: int) : int = z4(x5) + 4 in  
         z5(17) + 5
      end 
    end 
  end 
 end 
end
