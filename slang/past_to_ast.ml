

(*   translate_expr : Past.expr -> Ast.expr 
     Translates parsed AST to internal AST : 
     1) drop file locations 
     2) drop types 
     3) remove let 
     ) replace "?" (What) with unary function call 

  Note : our front-end drops type information.  Is this really a good idea? 
  Could types be useful in later phases of the compiler? 

*) 

let translate_uop = function 
  | Past.NEG -> Ast.NEG 
  | Past.NOT -> Ast.NOT 

let translate_bop = function 
  | Past.ADD -> Ast.ADD 
  | Past.MUL -> Ast.MUL
  | Past.DIV -> Ast.DIV
  | Past.SUB -> Ast.SUB
  | Past.LT -> Ast.LT
  | Past.AND -> Ast.AND
  | Past.OR -> Ast.OR
  | Past.EQI -> Ast.EQI
  | Past.EQB -> Ast.EQB
  | Past.EQ  -> Errors.complain "internal error, translate found a EQ that should have been resolved to EQI or EQB"

let rec tagged i = function
  | [] -> []
  | (x::rest) -> (i, x)::(tagged (i+1) rest)
let rec last = function
  | [x] -> x
  | (x::rest) -> let l = last rest in if l = "_" then x else l
  | [] -> Errors.complain "Error - cannot have empty tuple"

let rec translate_expr = function 
    | Past.Unit _            -> Ast.Unit
    | Past.What _            -> Ast.UnaryOp(Ast.READ, Ast.Unit)
    | Past.Var(_, x)         -> Ast.Var x 
    | Past.Integer(_, n)     -> Ast.Integer n
    | Past.Boolean(_, b)     -> Ast.Boolean b
    | Past.UnaryOp(_, op, e) -> Ast.UnaryOp(translate_uop op, translate_expr e)
    | Past.Op(_, e1, op, e2) -> Ast.Op(translate_expr e1, translate_bop op, translate_expr e2)
    | Past.If(_, e1, e2, e3) -> Ast.If(translate_expr e1, translate_expr e2, translate_expr e3)
    | Past.Inl(_, _, e)       -> Ast.Inl(translate_expr e)
    | Past.Inr(_, _, e)       -> Ast.Inr(translate_expr e)
    | Past.Tuple(_, el)       -> Ast.Tuple(List.map translate_expr el)
    | Past.Case(_, e, l1, l2) -> 
         Ast.Case(translate_expr e, translate_lambda l1, translate_lambda l2) 
    | Past.Lambda(_, l)      -> Ast.Lambda (translate_lambda l)
    | Past.App(_, e1, e2)    -> Ast.App(translate_expr e1, translate_expr e2)
    (* Simple piece of syntactic sugar for case when x is '_' *)
    | Past.Let(_, x, _, e1, e2) -> if x = "_" then Ast.Seq([translate_expr e1; translate_expr e2])
         else Ast.App(Ast.Lambda(x, translate_expr e2), translate_expr e1)
    | Past.LetFun(_, f, l, _, e)     -> 
         Ast.LetFun(f, translate_lambda l, translate_expr e)
    | Past.LetRecFun(_, f, l, _, e)     -> 
         Ast.LetRecFun(f, translate_lambda l, translate_expr e)

    | Past.Seq(_, el) -> Ast.Seq(List.map translate_expr el)
    | Past.While(_, e1, e2) -> Ast.While(translate_expr e1, translate_expr e2)
    | Past.Ref(_, e) -> Ast.Ref(translate_expr e)
    | Past.Deref(_, e) -> Ast.Deref(translate_expr e)
    | Past.Assign(_, e1, e2) -> Ast.Assign(translate_expr e1, translate_expr e2)
    | Past.Index(_, i, e) -> Ast.Index(i, translate_expr e)
    (* let (a,b) = e1 in e2
        Becomes:
       let b = e1 in
           let a = #1 b in
               let b = #2 b in
                   e2
    *)
    | Past.LetTuple(_, binds, e1, e2) -> let xl = List.map fst binds in
        let last_var = last xl in
        let nested_lets = List.fold_right (fun (i,v) -> fun e ->
        Ast.App(Ast.Lambda(v, e), Ast.Index(i, Ast.Var last_var)))
        (List.filter (fun (_,x) -> x <> "_") (tagged 1 xl))
        (translate_expr e2) in
        Ast.App(Ast.Lambda(last_var, nested_lets), translate_expr e1)
    (* let f (a,b) = e1 in e2
            Becomes:
           let f b =
               let a = #1 b in
                   let b = #2 b in
                       e1
           in e2
        *)
    | Past.LetTupleFun(_, f, binds, e1, _, e2) -> let xl = List.map fst binds in
        let last_var = last xl in
          let nested_lets = List.fold_right (fun (i,v) -> fun e ->
          Ast.App(Ast.Lambda(v, e), Ast.Index(i, Ast.Var last_var)))
           (List.filter (fun (_,x) -> x <> "_") (tagged 1 xl))
           (translate_expr e1) in
          Ast.LetFun(f, (last_var, nested_lets), translate_expr e2)
    | Past.LetRecTupleFun(_, f, binds, e1, _, e2) -> let xl = List.map fst binds in
        let last_var = last xl in
          let nested_lets = List.fold_right (fun (i,v) -> fun e ->
          Ast.App(Ast.Lambda(v, e), Ast.Index(i, Ast.Var last_var)))
           (List.filter (fun (_,x) -> x <> "_") (tagged 1 xl))
           (translate_expr e1) in
          Ast.LetRecFun(f, (last_var, nested_lets), translate_expr e2)

and translate_lambda (x, _, body) = (x, translate_expr body) 

