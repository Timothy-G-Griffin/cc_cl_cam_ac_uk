(* (fun (x) -> x + 1 end) 3 *)
(* let a  = 5 in a + 3 end *)
(* (fun (x) -> x + 1 end) true *)

(* let f (x) = 1 in f true + 1 end *)
(* let f (x) = x + 1 in f true + 1 end *)
(* let f (x) = true in f 1 + 1 end *)

(* let (a, b) = 4 in a + 2 end *)
(* let (a, b) = (2, true) in b + 2 end *)
(* let (a, b) = (2, true) in a + 2 end *)

(* let f (a, b) = 5 in f (1, true) + 3 end *)
(* let f (a, b) = 5 in f 1 + 3 end *)
(* let f (a, b) = true in f (1,2) + 3 end *)
(* let f (a, b) = a in f (1,true) + 3 end *)

(* Causes an issue currently, not correctly copying output type *)
(* let f (a, b) = a in f (true,true) + 3 end *)
 (* let f (x) = x in f 1 + 3 end *)
 (* let f (x) = x in (f true) + 3 end *)
(* let f (a, b) = true in f (true,true) + 3 end *)

(*let f (x) = x in f 1 + fst (f (1,2)) end*)
(*let f (x) = x in f 1 + fst (f 5) end*)
(*let f (x) = if x = 1 then true else f (x-1) end in 1 + f 3 end *)
(*let f (x) = if x = 1 then 1 else f (x-1) end in f true end *)
(* let f (x) = if x = 1 then 1 else f (x-1) end in f 3 end *)

(* (fun (x) -> x x end) *)
(* (fun (x) -> x end)(fun (x) -> x end) 5 *)

(*
let f (x) = #1 x in f (1,2) end

poly: : error: Can't find a fixed record type. Found near #1

TODO: Poly doesn't allow it, but could possibly add as feature to ensure only certain parts of a tuple
*)