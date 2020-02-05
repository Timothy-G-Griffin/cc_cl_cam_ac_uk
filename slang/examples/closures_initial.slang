(* fails in i3 and i4: i3 uses function name for label,
    i4 uses function name for label but only if recursive *)
(*
let L0 (x : int) : int =
  if x = 0 then 1 else x * L0 (x-1) end
  in
  let a : int = 3 in L0 a end end
*)

(* only fails in i3 as i4 doesn't use name as label unless recursive,
  L1 is label assigned to end of if statement *)
(*
let a : int = 3 in
let L1 (x : int) : int =
  if x = 0 then 1 else 2 end
  in L1 a end end
*)

(* Fails in i4 only, due to recursive functions using own name as label *)
(*
let f (x : int) : int =
  if x = 0 then 1 else x * f (x-1) end
  in
  let f (y : int * int) : int =
    if (fst y) = 0 then 1 else (fst y) * f ((fst y) - 1, 0) end
    in f (3,0) end end
*)

(* Fails in i3 only due to using 'f' as label name *)
(*
let f (x : int) : int =
  let f (y : int) : int = x + y in
   let a : int = 9 in
   f a end end
in f 5 end
*)

(* error in i3 as recursive call gets wrong f from environment.
 For now only gives an error due to using the same label though. Will see second issue once this fixed *)
(*
let f(x : unit) : int =
    let f (y : int) : int =
    if y = 0 then 1 else f (y-1) end
    in f 5 end
in f () end
*)