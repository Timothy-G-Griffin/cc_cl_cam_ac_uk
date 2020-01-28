let x : (int + bool) = inl (bool) 4
in
case x of inl (a : int) -> a | inr (b : bool) -> 2 end end