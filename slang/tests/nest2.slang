
let x1 : int = 1 
in let x2 : int = 2 + x1 
   in let x3 : int = 3 + x1 + x2 
      in let g(y : int) : int = y + x3 
         in let h(z : int) : int = z + g(z) 
            in g(h(g(x3)))
            end 
         end 
      end 
   end 
end 
