type token =
  | INT of (int)
  | IDENT of (string)
  | EOF
  | LPAREN
  | RPAREN
  | COMMA
  | COLON
  | SEMICOLON
  | ADD
  | SUB
  | MUL
  | NOT
  | EQUAL
  | LT
  | ANDOP
  | OROP
  | WHAT
  | UNIT
  | AND
  | TRUE
  | FALSE
  | IF
  | FI
  | THEN
  | ELSE
  | LET
  | REC
  | IN
  | BEGIN
  | END
  | BOOL
  | INTTYPE
  | UNITTYPE
  | ARROW
  | BAR
  | INL
  | INR
  | FST
  | SND
  | FUN
  | NUF
  | CASE
  | OF
  | REF
  | ASSIGN
  | BANG
  | WHILE
  | DO
  | OD

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Past.expr
