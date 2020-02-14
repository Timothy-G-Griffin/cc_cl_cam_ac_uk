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

let rec find loc x = function 
  | [] -> if x = "_" then complain "Values cannot be bound to wildcard variable"
          else complain (x ^ " is not defined at " ^ (string_of_loc loc))
  | (y, v) :: rest -> if x = y then v else find loc x rest


(* may want to make this more interesting someday ... *)
(* TODO: Add subtyping - requires records i.e. how { vm } done in OCaml *)
let rec match_types (t1, t2) = (t1 = t2)

let make_inl loc t2 (e, t1)          = (Inl(loc, t2, e), TEunion(t1, t2))
let make_inr loc t1 (e, t2)          = (Inr(loc, t1, e), TEunion(t1, t2))
let make_lambda loc x t1 (e, t2)     = (Lambda(loc, (x, t1, e)), TEarrow(t1, t2))
let make_ref loc (e, t)              = (Ref(loc, e), TEref t)
let make_letfun loc f x t1 (body, t2) (e, t)    = (LetFun(loc, f, (x, t1, body), t2, e), t)
let make_letrecfun loc f x t1 (body, t2) (e, t) = (LetRecFun(loc, f, (x, t1, body), t2, e), t)
let make_lettuplefun loc f bl (e1, t2) (e2, t) = (LetTupleFun(loc, f, bl, e1, t2, e2), t)
let make_letrectuplefun loc f bl (e1, t2) (e2, t) = (LetRecTupleFun(loc, f, bl, e1, t2, e2), t)

let make_let loc x t (e1, t1) (e2, t2)  = 
    if match_types (t, t1) 
    then (Let(loc, x, t, e1, e2), t2)
    else report_types_not_equal loc t t1


let make_tuple_let loc binds (e1, t1) (e2, t2) =
    let t = TEprod (List.map snd binds) in
   if match_types (t, t1) then (LetTuple(loc, binds, e1, e2), t2)
   else report_types_not_equal loc t t1

let make_if loc (e1, t1) (e2, t2) (e3, t3) = 
     match t1 with 
     | TEbool -> 
          if match_types (t2, t3) 
          then (If(loc, e1, e2, e3), t2) 
          else report_type_mismatch (e2, t2) (e3, t3) 
      | ty -> report_expecting e1 "boolean" ty 

let make_app loc (e1, t1) (e2, t2) = 
    match t1 with 
    | TEarrow(t3, t4) -> 
         if match_types(t2, t3) 
         then (App(loc, e1, e2), t4)
         else report_expecting e2 (string_of_type t3) t2
    | _ -> report_expecting e1 "function type" t1

let make_deref loc (e, t) = 
    match t with 
    | TEref t' -> (Deref(loc, e), t') 
    | _ -> report_expecting e "ref type" t

let make_uop loc uop (e, t) = 
    match uop, t with 
    | NEG, TEint  -> (UnaryOp(loc, uop, e), t) 
    | NEG, t'     -> report_expecting e "integer" t
    | NOT, TEbool -> (UnaryOp(loc, uop, e), t) 
    | NOT, t'     -> report_expecting e "boolean" t

let make_bop loc bop (e1, t1) (e2, t2) = 
    match bop, t1, t2 with 
    | LT,  TEint,  TEint  -> (Op(loc, e1, bop, e2), TEbool)
    | LT,  TEint,  t      -> report_expecting e2 "integer" t
    | LT,  t,      _      -> report_expecting e1 "integer" t
    | ADD, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | ADD, TEint,  t      -> report_expecting e2 "integer" t
    | ADD, t,      _      -> report_expecting e1 "integer" t
    | SUB, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | SUB, TEint,  t      -> report_expecting e2 "integer" t
    | SUB, t,      _      -> report_expecting e1 "integer" t
    | MUL, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | MUL, TEint,  t      -> report_expecting e2 "integer" t
    | MUL, t,      _      -> report_expecting e1 "integer" t
    | DIV, TEint,  TEint  -> (Op(loc, e1, bop, e2), t1) 
    | DIV, TEint,  t      -> report_expecting e2 "integer" t
    | DIV, t,      _      -> report_expecting e1 "integer" t
    | OR,  TEbool, TEbool -> (Op(loc, e1, bop, e2), t1) 
    | OR,  TEbool,  t     -> report_expecting e2 "boolean" t
    | OR,  t,      _      -> report_expecting e1 "boolean" t
    | AND, TEbool, TEbool -> (Op(loc, e1, bop, e2), t1) 
    | AND, TEbool,  t     -> report_expecting e2 "boolean" t
    | AND, t,      _      -> report_expecting e1 "boolean" t
    | EQ,  TEbool, TEbool -> (Op(loc, e1, EQB, e2), t1) 
    | EQ,  TEint,  TEint  -> (Op(loc, e1, EQI, e2), TEbool)  
    | EQ,  _,      _      -> report_type_mismatch (e1, t1) (e2, t2) 
    | EQI, _, _           -> internal_error "EQI found in parsed AST"
    | EQB, _, _           -> internal_error "EQB found in parsed AST"

let make_while loc (e1, t1) (e2, t2)    = 
    if t1 = TEbool 
    then if t2 = TEunit 
         then (While(loc, e1, e2), TEunit)
         else report_expecting e2 "unit type" t2
    else report_expecting e1 "boolean" t1

let make_assign loc (e1, t1) (e2, t2) = 
    match t1 with 
    | TEref t -> if match_types(t, t2) 
                 then (Assign(loc, e1, e2), TEunit) 
                 else report_type_mismatch (e1, t) (e2, t2)
    | t -> report_expecting e1 "ref type" t 

let make_case loc left right x1 x2 (e1, t1) (e2, t2) (e3, t3) = 
    match t1 with 
    | TEunion(left', right') -> 
      if match_types(left, left') 
      then if match_types(right, right')
           then if match_types(t3, t2)
                then (Case(loc, e1, (x1, left, e2), (x2, right, e3)), t2)
                else report_type_mismatch (e2, t2) (e3, t3)
           else report_types_not_equal loc right right'
      else report_types_not_equal loc left left' 
    | t -> report_expecting e1 "disjoint union" t

let rec nth = function
    | (1, t::tl) -> t
    | (n, t::tl) -> if n > 0 then nth (n-1,tl) else complain "Index must be greater than 0"
    | _ -> complain "Index greater than size of tuple"

let make_index loc i (e, t) =
    match t with
    | TEprod tl -> (Index(loc, i, e), nth (i, tl))
    | _ -> report_expecting e "tuple type" t

let rec mem x = function
  | [] -> false
  | (y::rest) -> x = y || mem x rest

(* TODO: Allow _ variable, but never add to env *)
let rec check_tuple_bind bound = function
  | [] -> true
  | ((x,t)::rest) -> if mem x bound then false else check_tuple_bind (x::bound) rest

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
            make_case loc t1 t2 x1 x2 (infer env e) (infer ((x1, t1) :: env) e1) (infer ((x2, t2) :: env) e2)
    | Lambda (loc, (x, t, e)) -> let new_env = if x <> "_" then (x,t)::env else env in
        make_lambda loc x t (infer new_env e)
    | App(loc, e1, e2)        -> make_app loc (infer env e1) (infer env e2)
    | Let(loc, x, t, e1, e2)  -> let new_env = if x <> "_" then (x,t)::env else env in
        make_let loc x t (infer env e1) (infer new_env e2)
    | LetFun(loc, f, (x, t1, body), t2, e) -> if f = "_" then complain "Functions must have names" else
      let env1 = (f, TEarrow(t1, t2)) :: env in 
      let p = infer env1 e  in
      let env2 = if x <> "_" then (x, t1) :: env else env in
         (try let (body', t') = infer env2 body in if match_types(t2, t') then
              make_letfun loc f x t1 (body', t') p
              else report_types_not_equal loc t' t2
          with _ -> let env3 = (f, TEarrow(t1, t2)) :: env2 in
              let (body', t') = infer env3 body in if match_types(t2, t') then
                   make_letrecfun loc f x t1 (body', t') p
                   else report_types_not_equal loc t' t2
         )
    | Index(loc, i, e)          -> make_index loc i (infer env e)
    | LetTuple(loc, binds, e1, e2) -> if check_tuple_bind [] binds then
      make_tuple_let loc binds (infer env e1) (infer ((List.filter (fun (x,_) -> x <> "_") binds) @ env) e2)
      else complain "Variable already bound in tuple"
    | LetTupleFun (loc, f, bl, e1, t, e2) -> if f = "_" then complain "Functions must have names" else
      if check_tuple_bind [] bl then
      let ftype = TEarrow(TEprod(List.map snd bl), t) in let env1 = (f, ftype) :: env in
      let p = infer env1 e2 in let env2 = (List.filter (fun (x,_) -> x <> "_") bl) @ env in
             (try let (body', t') = infer env2 e1 in if match_types(t, t') then
                  make_lettuplefun loc f bl (body', t') p
                  else report_types_not_equal loc t' t
              with _ -> let env3 = (f, ftype) :: env2 in
                  let (body', t') = infer env3 e1 in if match_types(t, t') then
                  make_letrectuplefun loc f bl (body', t') p
                  else report_types_not_equal loc t' t
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

