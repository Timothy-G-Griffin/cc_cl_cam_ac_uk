open Past 

let complain = Errors.complain

let internal_error msg = complain ("INTERNAL ERROR: " ^ msg)

let report_expecting e msg t = 
    let loc = loc_of_expr e in 
    let loc_str = string_of_loc loc in 
    let e_str = string_of_expr e in 
    let t_str = string_of_type t in
    complain ("ERROR at location " ^ 
	      loc_str ^ "\nExpression " ^ e_str ^ 
	      "\nhas type " ^ t_str ^ ", but expecting " ^ msg) 

let report_types_not_equal loc t1 t2 = 
    let loc_str = string_of_loc loc in 
    let t1_str = string_of_type t1 in
    let t2_str = string_of_type t2 in 
    complain ("Error near location " ^ loc_str ^ 
              "\nExpecting type " ^ t1_str ^ " to be equal to type " ^ t2_str)

let report_type_mismatch (e1, t1) (e2, t2) = 
    let loc1 = loc_of_expr e1 in 
    let loc2 = loc_of_expr e2 in 
    let loc1_str = string_of_loc loc1 in 
    let loc2_str = string_of_loc loc2 in 
    let e1_str = string_of_expr e1 in 
    let e2_str = string_of_expr e2 in 
    let t1_str = string_of_type t1 in
    let t2_str = string_of_type t2 in
    complain ("ERROR, Type Mismatch: expecting equal types, however\n" ^ 
	      "at location " ^ loc1_str ^ "\nexpression " ^ e1_str ^ "\nhas type " ^ t1_str ^ 
	      " and at location " ^ loc2_str ^ "\nexpression " ^ e2_str ^ "\nhas type " ^ t2_str)

(* Used purely for printing *)
let nextFree = ref 0
let newTEany () = (nextFree := !nextFree + 1; TEany (ref (Free ("_" ^ (string_of_int (!nextFree))))))

(* require r1 = r2 or not within r2 at all *)
(* TODO: Probably neater/more efficient way to do *)
(* Could do check before unifying?... *)
let rec occurs_check tr t2 =
   let rec aux = function
      | TEany b -> if b == tr then complain "Failed occurs check" else (match !b with
                    | Bound t -> aux t
                    | Free _ -> true)
      | TEref t -> aux t
      | TEarrow(t1, t2) -> aux t1 && aux t2
      | TEunion(t1, t2) -> aux t1 && aux t2
      | TEprod tl -> List.for_all aux tl
      | _ -> true
   in match t2 with
     | TEany b -> if b == tr then true else (match !b with
                    | Bound t -> occurs_check tr t
                    | Free _ -> true)
     | TEref t ->  aux t
     | TEarrow(t1, t2) ->  aux t1 && aux t2
     | TEunion(t1, t2) -> aux t1 && aux t2
     | TEprod tl -> List.for_all aux tl
     | _ -> true

(* may want to make this more interesting someday ... *)
(* TODO: Add subtyping - requires records i.e. how { vm } done in OCaml *)
(* TODO: Have match_types call type-mismatch or let functions do it to be more specific? *)
let rec match_types (t1, t2) =
match (t1, t2) with
  | (TEany tr, t2) -> (match !tr with
                      | Bound t -> match_types(t, t2)
                      | Free _ -> (match t2 with (* Uniqueness preserved by ptrs/copying and variable names so can use = or == *)
                        | TEany tr2 -> if tr = tr2 then true else (tr := Bound(t2); occurs_check tr t2)
                        | _ -> (tr := Bound(t2); occurs_check tr t2)))
  | (t1, TEany tr) -> (match !tr with
                      | Bound t -> match_types(t1, t)
                      | Free _ -> (match t1 with
                        | TEany tr2 -> if tr2 = tr then true else (tr := Bound(t1); occurs_check tr t1)
                        | _ -> (tr := Bound(t1); occurs_check tr t1)))
  | (TEref t1, TEref t2) -> match_types (t1, t2)
  | (TEarrow(t1, t2), TEarrow(t3, t4)) -> match_types (t1, t3) && match_types (t2, t4)
  | (TEunion(t1, t2), TEunion(t3, t4)) -> match_types (t1, t3) && match_types (t2, t4)
  (* [] represents tuple of unknown size/types *)
  (* TODO: Put back in or find way to defer test
  | (TEprod l1, TEprod l2) -> if l1 = [] then (r1:=!r2; true) else if rl2 = [] then (r2:=!r1; true)
      else List.for_all  match_types  (List.combine rl1 rl2) *)
  | (TEprod l1, TEprod l2) -> List.for_all match_types (List.combine l1 l2)
  | (t1, t2) ->  t1 = t2 (* Could add subtyping here *)

(* follow chains *)
(* TODO: Rewrite other functions to use this for copy/checking *)
let rec simple_type t = match t with
  | TEany b -> (match !b with
      | Bound t1 -> simple_type t1
      | Free _ -> t)
  | _ -> t

let rec lookup x = function
  | [] -> None
  | (y, v) :: rest -> if y == x then Some v else lookup x rest

(* List of bindings needed to preserve pointer graph *)
let rec copy_type t =
  let rec aux bound t = match t with
    | TEany b -> (match lookup b bound with
      | Some v -> (v, bound)
      | None -> (match !b with
        | Bound t -> let (t', bound') = aux bound t in let v = ref (Bound t') in (TEany v, (b, TEany v)::bound')
        | Free s -> let v = newTEany () in (v, (b, v)::bound)))
    | TEref t -> let (t',b') = aux bound t in (TEref t', b')
    | TEarrow (t1,t2) -> let (t1', b1) = aux bound t1 in let (t2', b2) = aux b1 t2 in (TEarrow(t1',t2'), b2)
    | TEunion (t1, t2) -> let (t1', b1) = aux bound t1 in let (t2', b2) = aux b1 t2 in (TEunion(t1',t2'), b2)
    | TEprod tl -> let (vl, bound') =
             List.fold_right (fun t -> fun (tl, bound) -> let (t', bound') = aux bound t in (t'::tl, bound')) tl ([], bound)
                  in (TEprod vl, bound')
    | t -> (t, bound)
  in let (t', _) = aux [] t in t'

(* Augmented to say if type should be copied or not.
Reason - don't copy type of f within recursive definition else argument won't be constrained.
Do copy outside of definition as application shouldn't remove polymorphism *)
let rec find loc x = function
  | [] -> if x = "_" then complain "Values cannot be bound to wildcard variable"
          else complain (x ^ " is not defined at " ^ (string_of_loc loc))
  | (y, v, true) :: rest -> if x = y then copy_type v else find loc x rest
  | (y, v, false) :: rest -> if x = y then v else find loc x rest

let make_inl loc t2 (e, t1)          = (Inl(loc, t2, e),  (TEunion(t1, t2)))
let make_inr loc t1 (e, t2)          = (Inr(loc, t1, e),  (TEunion(t1, t2)))
let make_lambda loc x t1 (e, t2)     = (Lambda(loc, (x, t1, e)),  (TEarrow(t1, t2)))
let make_ref loc (e, t)              = (Ref(loc, e),  (TEref t))
let make_letfun loc f x t1 (body, t2) (e, t)    = (LetFun(loc, f, (x, t1, body), t2, e), t)
let make_letrecfun loc f x t1 (body, t2) (e, t) = (LetRecFun(loc, f, (x, t1, body), t2, e), t)
let make_lettuplefun loc f bl (e1, t2) (e2, t) = (LetTupleFun(loc, f, bl, e1, t2, e2), t)
let make_letrectuplefun loc f bl (e1, t2) (e2, t) = (LetRecTupleFun(loc, f, bl, e1, t2, e2), t)

let make_let loc x t (e1, t1) (e2, t2)  = 
    if match_types (t, t1)
    then (Let(loc, x, t, e1, e2), t2)
    else report_types_not_equal loc t t1

let make_tuple_let loc binds (e1, t1) (e2, t2) =
    let t = (TEprod (List.map snd binds)) in
   if match_types (t, t1) then (LetTuple(loc, binds, e1, e2), t2)
   else report_types_not_equal loc t  t1

let make_if loc (e1, t1) (e2, t2) (e3, t3) =
     if match_types(t1, TEbool) then if match_types (t2, t3)
          then (If(loc, e1, e2, e3), t2)
          else report_type_mismatch (e2, t2) (e3, t3) 
      else report_expecting e1 "boolean"  t1

let make_app loc (e1, t1) (e2, t2) =
    if match_types (TEarrow (t2, newTEany ()), t1) then
    match simple_type t1 with
    | TEarrow(t3, t4) -> (App(loc, e1, e2), t4)
    | _ -> complain "Internal error - Should never happen due to previous type unification"
    else match simple_type t1 with
    | TEarrow(t3, t4) -> report_expecting e2 (string_of_type t3)  t2
    | _ -> report_expecting e1 "function type"  t1

let make_deref loc (e, t) =
    if match_types (TEref (newTEany ()), t) then
    match simple_type t with
    | TEref t' -> (Deref(loc, e), t')
    | _ -> complain "Internal error - Should never happen due to previous type unification"
    else report_expecting e "ref type"  t

let make_uop loc uop (e, t) =
    if uop = NEG then if match_types(t, TEint) then (UnaryOp(loc, uop, e), t) else report_expecting e "integer"  t
                 else if match_types(t, TEbool) then (UnaryOp(loc, uop, e), t) else report_expecting e "boolean"  t

let make_bop loc bop (e1, t1) (e2, t2) =
  match bop with
  | OR | AND -> if match_types (t1, TEbool) then
            if match_types (t2, TEbool) then (Op(loc, e1, bop, e2), TEbool)
            else report_expecting e2 "boolean" t2
          else report_expecting e1 "boolean" t1
  | EQ -> if match_types(t1,t2) then (match simple_type t1 with
             | TEint -> (Op(loc, e1, EQI, e2), TEbool)
             | TEbool -> (Op(loc, e1, EQB, e2), TEbool)
             | TEany _ -> if match_types(t1, TEint) then (Op(loc, e1, EQI, e2), TEbool)
                        else complain "Internal error - Should never happen due to unification"
             | _ -> report_expecting e1 "integer or boolean" t1)
           else report_type_mismatch (e1, t1) (e2, t2)
  | _ -> if match_types(t1, TEint) then
           if match_types(t2, TEint) then
             if bop = LT then (Op(loc, e1, bop, e2), TEbool) else (Op(loc, e1, bop, e2), TEint)
           else report_expecting e2 "integer" t2
         else report_expecting e1 "integer" t1

let make_while loc (e1, t1) (e2, t2)    = 
    if match_types(t1, TEbool)
    then if match_types(t2, TEunit)
         then (While(loc, e1, e2), t2)
         else report_expecting e2 "unit type"  t2
    else report_expecting e1 "boolean"  t1

let make_assign loc (e1, t1) (e2, t2) =
    if match_types(t1, (TEref (newTEany ()))) then
    match simple_type t1 with
    | TEref t -> if match_types(t, t2)
                 then (Assign(loc, e1, e2), TEunit)
                 else report_type_mismatch (e1, t) (e2, t2)
    | _ -> complain "Internal error: Should never happen due to unification"
    else report_expecting e1 "ref type"  t1

let make_case loc left right x1 x2 (e1, t1) (e2, t2) (e3, t3) =
    if match_types(t1, TEunion (newTEany (), newTEany ())) then
    match simple_type t1 with
    | TEunion(left', right') ->
      if match_types(left, left') then
        if match_types(right, right') then
          if match_types(t3, t2) then (Case(loc, e1, (x1, left, e2), (x2, right, e3)), t2)
          else report_type_mismatch (e2, t2) (e3, t3)
        else report_types_not_equal loc  right  right'
      else report_types_not_equal loc  left  left'
    | _ -> complain "Internal error: Should never happen due to unification"
    else  report_expecting e1 "disjoint union"  t1

let rec nth = function
    | (1, t::tl) -> t
    | (n, t::tl) -> if n > 0 then nth (n-1,tl) else complain "Index must be greater than 0"
    | _ -> complain "Index greater than size of tuple"

(* TODO: Work out how to defer type decision of product. Maybe add an option to Bound? *)
(* Could then learn just a subset of features at a time, still allows strong typing
e.g. fn y -> ((fn x -> #2 x) y + 5)  then x must be (TEany(BoundTuple [any, int, ...]*)
let make_index loc i (e, t) =
    match simple_type t with
      | TEprod tl -> (Index(loc, i, e), nth (i, tl))
 (*   if match_types(t,  (TEprod [])) then match !t with
    | TEprod tl ->  (Index(loc, i, e), nth (i, tl))
    | _ -> report_expecting e "tuple type"  t *)
      | TEany _ -> complain "Can't find a fixed record type."
      | _ -> report_expecting e "tuple" t

let rec mem x = function
  | [] -> false
  | (y::rest) -> x = y || mem x rest

let rec check_tuple_bind bound = function
  | [] -> true
  | ((x,_)::rest) -> if mem x bound then false else check_tuple_bind (x::bound) rest

let rec  infer env e =
    match e with 
    | Unit _               -> (e, TEunit)
    | What _               -> (e, TEint)
    | Integer _            -> (e, TEint)
    | Boolean _            -> (e, TEbool)
    | Var (loc, x)         -> (e, find loc x env)
    | Seq(loc, el)         -> infer_seq loc env el 
    | While(loc, e1, e2)   -> make_while loc (infer env e1) (infer env e2) 
    | Ref(loc, e)          -> make_ref loc (infer env e) 
    | Deref(loc, e)        -> make_deref loc (infer env e) 
    | Assign(loc, e1, e2)  -> make_assign loc (infer env e1) (infer env e2) 
    | UnaryOp(loc, uop, e) -> make_uop loc uop (infer env e) 
    | Op(loc, e1, bop, e2) -> make_bop loc bop (infer env e1) (infer env e2) 
    | If(loc, e1, e2, e3)  -> make_if loc (infer env e1) (infer env e2) (infer env e3)
    | Inl (loc, t, e)      -> make_inl loc t (infer env e)
    | Inr (loc, t, e)      -> make_inr loc t (infer env e)
    | Tuple(loc, el)       -> infer_tuple loc env el
    | Case(loc, e, (x1, t1, e1), (x2, t2, e2)) ->
        make_case loc t1 t2 x1 x2 (infer env e) (infer ((x1, t1, false) :: env) e1) (infer ((x2, t2, false) :: env) e2)
    | Lambda (loc, (x, t, e)) ->
        let new_env = if x <> "_" then (x, t, false)::env else env in
        make_lambda loc x t (infer new_env e)
    | App(loc, e1, e2)        -> make_app loc (infer env e1) (infer env e2)
    | Let(loc, x, t, e1, e2)  -> let (e1', t1) = (infer env e1) in
        if match_types (t, t1) then let new_env = if x <> "_" then (x, t1, false)::env else env in
          make_let loc x t (infer env e1) (infer new_env e2)
        else report_types_not_equal loc t t1
    | LetFun(loc, f, (x, t1, body), t2, e) -> if f = "_" then complain "Functions must have names" else
      let ftype = TEarrow(t1, t2) in let env1 = (f, ftype, true) :: env in
      let env2 = if x <> "_" then (x, t1, false) :: env else env in
         (try let (body', t) = infer env2 body in
            if match_types(t, t2) then let p = infer env1 e  in make_letfun loc f x t1 (body', t) p
            else report_expecting body (string_of_type t2) t
          with _ -> let env3 = (f, ftype, false) :: env2 in
                       let (body', t) = infer env3 body in
                       if match_types(t, t2) then
                         let p = infer env1 e  in make_letrecfun loc f x t1 (body', t) p
                       else report_expecting  body (string_of_type  t2)  t)
    | Index(loc, i, e)          -> make_index loc i (infer env e)
    | LetTuple(loc, binds, e1, e2) -> if check_tuple_bind [] binds then
    let bindsType = TEprod (List.map snd binds) in
    let (e1', t1) = (infer env e1) in if match_types (bindsType, t1) then
    make_tuple_let loc binds (e1', t1) (infer ( (List.filter (fun (x,_, _) -> x <> "_") (List.map (fun (x,t) -> (x,t,false)) binds)) @ env) e2)
    else report_expecting e1 (string_of_type  bindsType)  t1
    else complain "Variable already bound in tuple"
    | LetTupleFun (loc, f, bl, e1, t, e2) -> if f = "_" then complain "Functions must have names" else
    if check_tuple_bind [] bl then
    let bindsType = TEprod (List.map snd bl) in
    let ftype = TEarrow (bindsType, t) in
    let env1 = (f, ftype, true) :: env in
            let env2 = (List.filter (fun (x,_, _) -> x <> "_")  (List.map (fun (x,t) -> (x,t,false)) bl)) @ env in
             (try let (e1', t') = infer env2 e1 in if match_types(t', t) then let p = infer env1 e2 in make_lettuplefun loc f bl (e1', t') p
                                                   else report_expecting  e1 (string_of_type  t)  t'
                       with _ -> let env3 = (f, ftype, false) :: env2 in
                                     let (e1', t') = infer env3 e1 in if match_types(t', t) then let p = infer env1 e2 in make_letrectuplefun loc f bl (e1', t') p
                                     else report_expecting  e1 (string_of_type  t)  t'
                      ) else complain "Variable already bound in tuple"
    | LetRecTupleFun (_, _, _, _, _, _) -> internal_error "LetRecTupleFun found in parsed AST"
    | LetRecFun(_, _, _, _, _)  -> internal_error "LetRecFun found in parsed AST"


and infer_seq loc env el = 
    let rec aux carry = function
      | []        -> internal_error "empty sequence found in parsed AST" 
      | [e]       -> let (e', t) = infer env e in (Seq(loc, List.rev (e' :: carry )), t)
      | e :: rest -> let (e', _) = infer env e in aux (e' :: carry) rest
    in aux [] el

and infer_tuple loc env el =
    let rec aux carry carryType = function
      | []      -> internal_error "Empty tuple in parsed AST"
      | [e]     -> let (e', t) = infer env e in (Tuple(loc, List.rev (e' :: carry)), TEprod (List.rev (t::carryType)))
      | e::rest -> let (e', t) = infer env e in aux (e' :: carry) (t::carryType) rest
    in aux [] [] el
       
let env_init = [] 

let check e = 
    let (e', _) = infer env_init e 
    in e' 

