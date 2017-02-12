(* Pretty Printing without brackets*)
let string_replace_char s c_from c_to =
  let char_map i =
    if s.[i] = c_from
    then c_to
    else s.[i] in
  String.init (String.length s) char_map;;
let tail s = (String.sub s 1 ( (String.length s) - 1))
let pp_no_bracket str =
  let rec aux output indent_list left =
    match left with
    | "" -> output
    | x -> match x.[0] with
      | '(' ->
        let old_indent = List.hd indent_list in
        let new_indent = (string_replace_char old_indent '-' ' ') ^ "|- " in
        aux (output ^ "\n" ^ new_indent) (new_indent::indent_list) (tail left)
      | ',' -> aux (output ^ "\n" ^ (List.hd indent_list)) indent_list (tail left)
      | ')' -> aux output (List.tl indent_list) (tail left)
      | ' ' -> aux output indent_list (tail left)
      | c -> aux (output ^ (Char.escaped c)) indent_list (tail left)
  in aux "" [""] str


(* let test_str = "LetRecFun(fib, (m, If(Op(Var(m), EQI, Integer(0)), Integer(1), If(Op(Var(m), EQI, Integer(1)), Integer(1), Op(App(Var(fib), Op (Var(m), SUB, Integer(1))), ADD, App(Var(fib), Op(Var(m), SUB, Integer(2))))))), App(Var(fib), UnaryOp(READ, Unit)))" *)
(* let _ = print_string (pp_no_bracket test_str) *)
