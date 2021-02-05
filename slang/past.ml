(* 

   The Parsed AST 

*) 
type var = string 

type loc = Lexing.position 

type type_expr = 
   | TEint
   | TEbool
   | TEunit
   | TEref of type_expr
   | TEarrow of type_expr * type_expr
   | TEunion of type_expr * type_expr
   | TEprod of type_expr list
   | TEany of type_bind ref

and type_bind =
   | Free of string
   | Bound of type_expr

type formals = (var * type_expr) list

type oper = ADD | MUL | DIV | SUB | LT | AND | OR | EQ | EQB | EQI

type unary_oper = NEG | NOT 

type expr = 
       | Unit of loc  
       | What of loc 
       | Var of loc * var
       | Integer of loc * int
       | Boolean of loc * bool
       | UnaryOp of loc * unary_oper * expr
       | Op of loc * expr * oper * expr
       | If of loc * expr * expr * expr
       | Inl of loc * type_expr * expr
       | Inr of loc * type_expr * expr
       | Tuple of loc * (expr list)
       | Case of loc * expr * lambda * lambda 
       | While of loc * expr * expr
       | Seq of loc * (expr list)
       | Ref of loc * expr 
       | Deref of loc * expr 
       | Assign of loc * expr * expr
       | Lambda of loc * lambda 
       | App of loc * expr * expr
       | Let of loc * var * type_expr * expr * expr
       | LetFun of loc * var * lambda * type_expr * expr
       | LetRecFun of loc * var * lambda * type_expr * expr
       | Index of loc * int * expr
       | LetTuple of loc * ((var * type_expr) list) * expr * expr
       | LetTupleFun of loc * var * ((var * type_expr) list) * expr * type_expr * expr
       | LetRecTupleFun of loc * var * ((var * type_expr) list) * expr * type_expr * expr

and lambda = var * type_expr * expr

let  loc_of_expr = function 
    | Unit loc                      -> loc 
    | What loc                      -> loc 
    | Var (loc, _)                  -> loc 
    | Integer (loc, _)              -> loc 
    | Boolean (loc, _)              -> loc 
    | UnaryOp(loc, _, _)            -> loc 
    | Op(loc, _, _, _)              -> loc 
    | If(loc, _, _, _)              -> loc
    | Inr(loc, _, _)                -> loc 
    | Inl(loc, _, _)                -> loc
    | Tuple(loc, _)                 -> loc
    | Case(loc, _, _, _)            -> loc 
    | Seq(loc, _)                   -> loc 
    | Ref(loc, _)                   -> loc 
    | Deref(loc, _)                 -> loc 
    | Assign(loc, _, _)             -> loc 
    | While(loc, _, _)              -> loc 
    | Lambda(loc, _)                -> loc 
    | App(loc, _, _)                -> loc 
    | Let(loc, _, _, _, _)          -> loc
    | LetFun(loc, _, _, _, _)       -> loc 
    | LetRecFun(loc, _, _, _, _)    -> loc
    | Index(loc, _, _)              -> loc
    | LetTuple (loc,_,_,_)        -> loc
    | LetTupleFun (loc, _, _, _, _, _) -> loc
    | LetRecTupleFun (loc, _, _, _, _, _) -> loc

let string_of_loc loc = 
    "line " ^ (string_of_int (loc.Lexing.pos_lnum)) ^ ", " ^ 
    "position " ^ (string_of_int ((loc.Lexing.pos_cnum - loc.Lexing.pos_bol) + 1))

open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*) 

let rec pp_type = function
  | TEint -> "int"
  | TEbool -> "bool"
  | TEunit -> "unit"
  | TEref t           -> "(" ^ (pp_type t) ^ " ref)"
  | TEarrow(t1, t2)   -> "(" ^ (pp_type t1) ^ " -> " ^ (pp_type t2) ^ ")"
  | TEunion(t1, t2)   -> "(" ^ (pp_type t1) ^ " + " ^ (pp_type t2) ^ ")"
  | TEprod tl -> "(" ^ List.fold_right (fun v -> fun s -> (pp_type v) ^ s) tl ")"
  | TEany b -> match !b with
                | Free s -> "T" ^ s
                | Bound t -> pp_type t

let pp_uop = function 
  | NEG -> "-" 
  | NOT -> "~" 

let pp_bop = function 
  | ADD -> "+" 
  | MUL  -> "*" 
  | DIV  -> "/" 
  | SUB -> "-" 
  | LT   -> "<" 
  | EQ   -> "=" 
  | EQI   -> "eqi" 
  | EQB   -> "eqb" 
  | AND   -> "&&" 
  | OR   -> "||" 

let string_of_oper = pp_bop 
let string_of_unary_oper = pp_uop 

let fstring ppf s = fprintf ppf "%s" s
let pp_type ppf t = fstring ppf (pp_type t) 
let pp_unary ppf op = fstring ppf (pp_uop op) 
let pp_binary ppf op = fstring ppf (pp_bop op)
let pp_int ppf i = fstring ppf (string_of_int i)

(* ignore locations *) 
let rec pp_expr ppf = function 
    | Unit _              -> fstring ppf "()" 
    | What _              -> fstring ppf "?" 
    | Var (_, x)          -> fstring ppf x 
    | Integer (_, n)      -> fstring ppf (string_of_int n)
    | Boolean (_, b)      -> fstring ppf (string_of_bool b)
    | UnaryOp(_, op, e)   -> fprintf ppf "%a(%a)" pp_unary op pp_expr e 
    | Op(_, e1, op, e2)   -> fprintf ppf "(%a %a %a)" pp_expr e1  pp_binary op pp_expr e2 
    | If(_, e1, e2, e3)   -> fprintf ppf "@[if %a then %a else %a @]" 
                                      pp_expr e1 pp_expr e2 pp_expr e3
    | Inl(_, t, e)        -> fprintf ppf "(inl %a %a)" pp_type t pp_expr e
    | Inr(_, t, e)        -> fprintf ppf "(inr %a %a)" pp_type t pp_expr e
    | Tuple (loc, l)       -> fprintf ppf "(%a)" pp_expr (Seq(loc, l))
    | Case(_, e, (x1, t1, e1), (x2, t2, e2)) -> 
        fprintf ppf "@[<2>case %a of@ | inl(%a : %a) -> %a @ | inr(%a : %a) -> %a end@]" 
                     pp_expr e fstring x1 pp_type t1 pp_expr e1 fstring x2 pp_type t2 pp_expr e2
    | Seq (_, [])         -> () 
    | Seq (_, [e])        -> pp_expr ppf e 
    | Seq (l, e :: rest)  -> fprintf ppf "%a; %a" pp_expr e pp_expr (Seq(l, rest))
    | While (_, e1, e2)   -> fprintf ppf "while %a do %a end" pp_expr e1 pp_expr e2 
    | Ref(_, e)           -> fprintf ppf "ref %a" pp_expr e
    | Deref(_, e)         -> fprintf ppf "!%a" pp_expr e
    | Assign(_, e1, e2)   -> fprintf ppf "(%a := %a)" pp_expr e1 pp_expr e2 
    | Lambda(_, (x, t, e)) -> 
         fprintf ppf "(fun %a : %a -> %a)" fstring x pp_type t  pp_expr e
    | App(_, e1, e2)      -> fprintf ppf "%a %a" pp_expr e1 pp_expr e2
    | Let(_, x, t, e1, e2) -> 
         fprintf ppf "@[<2>let %a : %a = %a in %a end@]" fstring x pp_type t pp_expr e1 pp_expr e2
    | LetFun(_, f, (x, t1, e1), t2, e2)     ->
         fprintf ppf "@[let %a(%a : %a) : %a =@ %a @ in %a @ end@]" 
                     fstring f fstring x  pp_type t1 pp_type t2 pp_expr e1 pp_expr e2
    | LetRecFun(_, f, (x, t1, e1), t2, e2)     -> 
         fprintf ppf "@[letrec %a(%a : %a) : %a =@ %a @ in %a @ end@]" 
                     fstring f fstring x  pp_type t1 pp_type t2 pp_expr e1 pp_expr e2
    | Index(_, i, e) -> fprintf ppf "#%a (%a)" pp_int i pp_expr e
    | LetTuple(_, bl, e1, e2) -> fprintf ppf "@[<2>let (%a) = %a in %a end@]"
                pp_bind_list bl  pp_expr e1 pp_expr e2
    | LetTupleFun (_, f, bl, e1, t, e2) -> fprintf ppf "@[<2>let %a(%a) : %a =@ %a @ in %a @ end@]"
                    fstring f pp_bind_list bl   pp_type t  pp_expr e1 pp_expr e2
    | LetRecTupleFun (_, f, bl, e1, t, e2) -> fprintf ppf "@[<2>letrec %a(%a) : %a =@ %a @ in %a @ end@]"
                         fstring f pp_bind_list bl   pp_type t  pp_expr e1 pp_expr e2

and pp_bind_list ppf = function
  | [] -> ()
  | [(x,t)] -> fprintf ppf "%a : %a" fstring x pp_type t
  | (x,t)::rest -> fprintf ppf "%a : %a, %a" fstring x pp_type t pp_bind_list rest

let print_expr e = 
    let _ = pp_expr std_formatter e
    in print_flush () 

let eprint_expr e = 
    let _ = pp_expr err_formatter e
    in print_flush () 

(* useful for degugging *) 

let string_of_uop = function 
  | NEG -> "NEG" 
  | NOT -> "NOT" 

let string_of_bop = function 
  | ADD -> "ADD" 
  | MUL  -> "MUL" 
  | DIV  -> "DIV" 
  | SUB -> "SUB" 
  | LT   -> "LT" 
  | EQ   -> "EQ" 
  | EQI   -> "EQI" 
  | EQB   -> "EQB" 
  | AND   -> "AND" 
  | OR   -> "OR" 

let mk_con con l = 
    let rec aux carry = function 
      | [] -> carry ^ ")"
      | [s] -> carry ^ s ^ ")"
      | s::rest -> aux (carry ^ s ^ ", ") rest 
    in aux (con ^ "(") l 

let rec string_of_type = function
  | TEint             -> "TEint"
  | TEbool            -> "TEbool"
  | TEunit            -> "TEunit"
  | TEref t           -> mk_con "TEref" [string_of_type t]
  | TEarrow(t1, t2)   -> mk_con "TEarrow" [string_of_type t1; string_of_type t2]
  | TEunion(t1, t2)   -> mk_con "TEunion" [string_of_type t1; string_of_type t2]
  | TEprod tl         -> mk_con "TEprod" [string_of_type_list tl]
  | TEany b           -> match !b with
                 | Free s -> "T" ^ s
                 | Bound t -> string_of_type t

and string_of_type_list = function
  | [] -> ""
  | [e] -> string_of_type e
  |  e:: rest -> (string_of_type e ) ^ "; " ^ (string_of_type_list rest)

let rec string_of_expr = function 
    | Unit _              -> "Unit" 
    | What _              -> "What" 
    | Var (_, x)          -> mk_con "Var" [x] 
    | Integer (_, n)      -> mk_con "Integer" [string_of_int n] 
    | Boolean (_, b)      -> mk_con "Boolean" [string_of_bool b] 
    | UnaryOp(_, op, e)   -> mk_con "UnaryOp" [string_of_uop op; string_of_expr e]
    | Op(_, e1, op, e2)   -> mk_con "Op" [string_of_expr e1; string_of_bop op; string_of_expr e2]
    | If(_, e1, e2, e3)   -> mk_con "If" [string_of_expr e1; string_of_expr e2; string_of_expr e3]
    | Tuple(_, el)        -> mk_con "Tuple" [string_of_expr_list el]
    | Inl(_, t, e)        -> mk_con "Inl" [string_of_expr e] 
    | Inr(_, t, e)        -> mk_con "Inr" [string_of_expr e] 
    | Seq (_, el)         -> mk_con "Seq" [string_of_expr_list el] 
    | While (_, e1, e2)   -> mk_con "While" [string_of_expr e1; string_of_expr e2]
    | Ref(_, e)           -> mk_con "Ref" [string_of_expr e] 
    | Deref(_, e)         -> mk_con "Deref" [string_of_expr e] 
    | Assign(_, e1, e2)   -> mk_con "Assign" [string_of_expr e1; string_of_expr e2]
    | Lambda(_, (x, t, e)) -> mk_con "Lambda" [x; string_of_type t; string_of_expr e]
    | App(_, e1, e2)      -> mk_con "App" [string_of_expr e1; string_of_expr e2]
    | Let(_, x, t, e1, e2) -> mk_con "Let" [x; string_of_type t; string_of_expr e1; string_of_expr e2]
    | LetFun(_, f, (x, t1, e1), t2, e2)      ->
          mk_con "LetFun" [
             f; 
             mk_con "" [x; string_of_type t1; string_of_expr e1]; 
             string_of_type t2; 
             string_of_expr e2]
    | LetRecFun(_, f, (x, t1, e1), t2, e2)   -> 
          mk_con "LetRecFun" [
             f; 
             mk_con "" [x; string_of_type t1; string_of_expr e1]; 
             string_of_type t2; 
             string_of_expr e2]
    | Case(_, e, (x1, t1, e1), (x2, t2, e2)) -> 
          mk_con "Case" [
	     string_of_expr e; 
	     mk_con "" [x1; string_of_type t1; string_of_expr e1]; 
	     mk_con "" [x2; string_of_type t1; string_of_expr e2]]
	 | Index(_, i, e) -> mk_con "Index" [string_of_int i; string_of_expr e]
	 | LetTuple(_, bl, e1, e2) -> mk_con "LetTuple" (
	 (List.map (fun (x,t) -> x ^ " : " ^ (string_of_type t)) bl) @ [string_of_expr e1; string_of_expr e2])
	 | LetTupleFun (_, f, bl, e1, t, e2) ->
	        mk_con "LetFun" (f::(List.map (fun (x,t) -> x ^ " : " ^ (string_of_type t)) bl)@
                      [string_of_expr e1; string_of_type t; string_of_expr e2])
     | LetRecTupleFun (_, f, bl, e1, t, e2) ->
     	    mk_con "LetRecFun" (f::(List.map (fun (x,t) -> x ^ " : " ^ (string_of_type t)) bl)@
                      [string_of_expr e1; string_of_type t; string_of_expr e2])

and string_of_expr_list = function 
  | [] -> "" 
  | [e] -> string_of_expr e 
  |  e:: rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)

