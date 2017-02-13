
type var = string 

type oper = ADD | MUL | DIV | SUB | LT | AND | OR | EQB | EQI

type unary_oper = NEG | NOT | READ 

type expr = 
       | Unit  
       | Var of var
       | Integer of int
       | Boolean of bool
       | UnaryOp of unary_oper * expr
       | Op of expr * oper * expr
       | If of expr * expr * expr
       | Pair of expr * expr
       | Fst of expr 
       | Snd of expr 
       | Inl of expr 
       | Inr of expr 
       | Case of expr * lambda * lambda 
       | While of expr * expr 
       | Seq of (expr list)
       | Ref of expr 
       | Deref of expr 
       | Assign of expr * expr 
       | Lambda of lambda 
       | App of expr * expr
       | LetFun of var * lambda * expr
       | LetRecFun of var * lambda * expr

and lambda = var * expr 


open Format

(*
   Documentation of Format can be found here: 
   http://caml.inria.fr/resources/doc/guides/format.en.html
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
*) 

let pp_uop = function 
  | NEG -> "-" 
  | NOT -> "~" 
  | READ -> "read" 


let pp_bop = function 
  | ADD -> "+" 
  | MUL  -> "*" 
  | DIV  -> "/" 
  | SUB -> "-" 
  | LT   -> "<" 
  | EQI   -> "eqi" 
  | EQB   -> "eqb" 
  | AND   -> "&&" 
  | OR   -> "||" 


let string_of_oper = pp_bop 
let string_of_unary_oper = pp_uop 

let fstring ppf s = fprintf ppf "%s" s

let pp_unary ppf t = fstring ppf (pp_uop t) 

let pp_binary ppf t = fstring ppf (pp_bop t) 

let rec pp_expr ppf = function 
    | Unit             -> fstring ppf "()" 
    | Var x            -> fstring ppf x 
    | Integer n        -> fstring ppf (string_of_int n)
    | Boolean b        -> fstring ppf (string_of_bool b)
    | UnaryOp(op, e)   -> fprintf ppf "%a(%a)" pp_unary op pp_expr e 
    | Op(e1, op, e2)   -> fprintf ppf "(%a %a %a)" pp_expr e1  pp_binary op pp_expr e2 
    | If(e1, e2, e3)   -> fprintf ppf "@[if %a then %a else %a @]" 
                                      pp_expr e1 pp_expr e2 pp_expr e3
    | Pair(e1, e2)     -> fprintf ppf "(%a, %a)" pp_expr e1 pp_expr e2
    | Fst e            -> fprintf ppf "fst(%a)" pp_expr e
    | Snd e            -> fprintf ppf "snd(%a)" pp_expr e
    | Inl e            -> fprintf ppf "inl(%a)" pp_expr e
    | Inr e            -> fprintf ppf "inr(%a)" pp_expr e
    | Case(e, (x1, e1), (x2, e2)) -> 
        fprintf ppf "@[<2>case %a of@ | inl %a -> %a @ | inr %a -> %a end@]" 
                     pp_expr e fstring x1 pp_expr e1 fstring x2 pp_expr e2 
    | Lambda(x, e) -> 
         fprintf ppf "(fun %a -> %a)" fstring x pp_expr e
    | App(e1, e2)      -> fprintf ppf "%a %a" pp_expr e1 pp_expr e2

    | Seq el           -> fprintf ppf "begin %a end" pp_expr_list el 
    | While (e1, e2)   -> fprintf ppf "while %a do %a end" pp_expr e1 pp_expr e2 
    | Ref e            -> fprintf ppf "ref(%a)" pp_expr e 
    | Deref e          -> fprintf ppf "!(%a)" pp_expr e 
    | Assign (e1, e2)  -> fprintf ppf "(%a := %a)" pp_expr e1 pp_expr e2 
    | LetFun(f, (x, e1), e2)     -> 
         fprintf ppf "@[let %a(%a) =@ %a @ in %a @ end@]" 
                     fstring f fstring x  pp_expr e1 pp_expr e2
    | LetRecFun(f, (x, e1), e2)  -> 
         fprintf ppf "@[letrec %a(%a) =@ %a @ in %a @ end@]" 
                     fstring f fstring x  pp_expr e1 pp_expr e2
and pp_expr_list ppf = function 
  | [] -> () 
  | [e] -> pp_expr ppf e 
  |  e:: rest -> fprintf ppf "%a; %a" pp_expr e pp_expr_list rest 


let print_expr e = 
    let _ = pp_expr std_formatter e
    in print_flush () 

let eprint_expr e = 
    let _ = pp_expr err_formatter e
    in pp_print_flush err_formatter () 



(* useful for debugging *) 

let string_of_uop = function 
  | NEG -> "NEG" 
  | NOT -> "NOT" 
  | READ -> "READ" 

let string_of_bop = function 
  | ADD -> "ADD" 
  | MUL  -> "MUL" 
  | DIV  -> "DIV" 
  | SUB -> "SUB" 
  | LT   -> "LT" 
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

let rec string_of_expr = function 
    | Unit             -> "Unit" 
    | Var x            -> mk_con "Var" [x] 
    | Integer n        -> mk_con "Integer" [string_of_int n] 
    | Boolean b        -> mk_con "Boolean" [string_of_bool b] 
    | UnaryOp(op, e)   -> mk_con "UnaryOp" [string_of_uop op; string_of_expr e]
    | Op(e1, op, e2)   -> mk_con "Op" [string_of_expr e1; string_of_bop op; string_of_expr e2]
    | If(e1, e2, e3)   -> mk_con "If" [string_of_expr e1; string_of_expr e2; string_of_expr e3]
    | Pair(e1, e2)     -> mk_con "Pair" [string_of_expr e1; string_of_expr e2]
    | Fst e            -> mk_con "Fst" [string_of_expr e] 
    | Snd e            -> mk_con "Snd" [string_of_expr e] 
    | Inl e            -> mk_con "Inl" [string_of_expr e] 
    | Inr e            -> mk_con "Inr" [string_of_expr e] 
    | Lambda(x, e)     -> mk_con "Lambda" [x; string_of_expr e]
    | App(e1, e2)      -> mk_con "App" [string_of_expr e1; string_of_expr e2]
    | Seq el           -> mk_con "Seq" [string_of_expr_list el] 
    | While (e1, e2)   -> mk_con "While" [string_of_expr e1; string_of_expr e2]
    | Ref e            -> mk_con "Ref" [string_of_expr e] 
    | Deref e          -> mk_con "Deref" [string_of_expr e] 
    | Assign (e1, e2)  -> mk_con "Assign" [string_of_expr e1; string_of_expr e2]
    | LetFun(f, (x, e1), e2)      -> 
          mk_con "LetFun" [f; mk_con "" [x; string_of_expr e1]; string_of_expr e2]
    | LetRecFun(f, (x, e1), e2)   -> 
          mk_con "LetRecFun" [f; mk_con "" [x; string_of_expr e1]; string_of_expr e2]
    | Case(e, (x1, e1), (x2, e2)) -> 
          mk_con "Case" [
              string_of_expr e; 
	      mk_con "" [x1; string_of_expr e1]; 
	      mk_con "" [x2; string_of_expr e2]]

and string_of_expr_list = function 
  | [] -> "" 
  | [e] -> string_of_expr e 
  |  e:: rest -> (string_of_expr e ) ^ "; " ^ (string_of_expr_list rest)

