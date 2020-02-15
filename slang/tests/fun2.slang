let f y =
  let g x = y + x in g end
in
  let add21 = f(21) in
      let add17 = f(17) in
         add17 3 + add21 60
       end
  end
end