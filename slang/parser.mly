
/* Auxiliary code */

%{

let get_loc = Parsing.symbol_start_pos

%}

/* Tokens and types */
%token<int> INT
%token<string> IDENT
%token EOF LPAREN RPAREN COMMA COLON SEMICOLON ADD SUB MUL DIV NOT EQUAL LT ANDOP OROP
%token WHAT UNIT AND TRUE FALSE IF FI THEN ELSE LET REC IN BEGIN END BOOL INTTYPE UNITTYPE
%token ARROW BAR INL INR FST SND FUN NUF CASE OF REF ASSIGN BANG WHILE DO OD HASH

/*
The precedences must be listed from low to high.
*/
%left ADD SUB                     /* lowest precedence */
%nonassoc below_MUL
%left MUL DIV ANDOP OROP EQUAL ARROW  LT /* medium precedence */
%left ASSIGN
/*
%nonassoc THEN
%nonassoc ELSE
*/
%nonassoc UMINUS
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc UNIT INT WHAT IDENT TRUE FALSE LPAREN NOT BANG REF HASH /* highest precedence */

%nonassoc below_COMMA
%left COMMA /* expr/expr_comma_list (e,e,e) */

%start start
%type <Past.type_expr> texpr
%type <Past.expr> simple_expr
%type <Past.expr> expr
%type <Past.expr list> exprlist
%type <Past.expr> start

%%

/* Grammar  */

start:
| expr EOF { $1 }

/* problem
   -e  (unary minus)
    e e (application)
    e1 - e2  (is the e1(-e2) or e1-e2???)
*/

simple_expr:
| UNIT                               { Past.Unit (get_loc())}
| INT                                { Past.Integer (get_loc(), $1) }
| WHAT                               { Past.What (get_loc())}
| IDENT                              { Past.Var (get_loc(), $1) }
| TRUE                               { Past.Boolean (get_loc(), true)}
| FALSE                              { Past.Boolean (get_loc(), false)}
| LPAREN expr RPAREN                 { $2 }
| LPAREN expr_comma_list %prec below_COMMA RPAREN { Past.Tuple(get_loc(), List.rev $2) }
| NOT simple_expr               { Past.UnaryOp(get_loc(), Past.NOT, $2) }
| BANG simple_expr              { Past.Deref(get_loc(), $2) }
| REF simple_expr               { Past.Ref(get_loc(), $2) }

expr:
| simple_expr                        {  $1 }
| expr simple_expr                   { Past.App (get_loc(), $1, $2) }
| SUB expr %prec UNIT                { Past.UnaryOp(get_loc(), Past.NEG, $2) }
| expr ADD expr                      { Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr SUB expr                      { Past.Op(get_loc(), $1, Past.SUB, $3) }
| expr MUL expr                      { Past.Op(get_loc(), $1, Past.MUL, $3) }
| expr DIV expr                      { Past.Op(get_loc(), $1, Past.DIV, $3) }
| expr LT expr                       { Past.Op(get_loc(), $1, Past.LT, $3) }
| expr EQUAL expr                    { Past.Op(get_loc(), $1, Past.EQ, $3) }
| expr ANDOP expr                    { Past.Op(get_loc(), $1, Past.AND, $3) }
| expr OROP expr                     { Past.Op(get_loc(), $1, Past.OR, $3) }
| expr ASSIGN expr                   { Past.Assign(get_loc(), $1, $3) }
| BEGIN exprlist END                 { Past.Seq(get_loc(), $2) }
| IF expr THEN expr ELSE expr END     { Past.If(get_loc(), $2, $4, $6) }
| WHILE expr DO expr END              { Past.While(get_loc(), $2, $4) }
| FST expr %prec UMINUS              { Past.Fst(get_loc(), $2) }
| SND expr %prec UMINUS              { Past.Snd(get_loc(), $2) }
| HASH;INT expr %prec UMINUS         { Past.Proj(get_loc(), $2, $3) }
| INL texpr expr %prec UMINUS        { Past.Inl(get_loc(), $2, $3) }
| INR texpr expr %prec UMINUS        { Past.Inr(get_loc(), $2, $3) }
| FUN LPAREN IDENT COLON texpr RPAREN ARROW expr END
                                     { Past.Lambda(get_loc(), ($3, $5, $8)) }
| LET IDENT COLON texpr EQUAL expr IN expr END           { Past.Let (get_loc(), $2, $4, $6, $8) }
| LET IDENT LPAREN IDENT COLON texpr RPAREN COLON texpr EQUAL expr IN expr END
                                     { Past.LetFun (get_loc(), $2, ($4, $6, $11), $9, $13) }
| CASE expr OF
      INL LPAREN IDENT COLON texpr RPAREN ARROW expr
  BAR INR LPAREN IDENT COLON texpr RPAREN  ARROW expr
  END
                                     { Past.Case (get_loc(), $2, ($6, $8, $11), ($15, $17, $20)) }

exprlist:
|   expr                             { [$1] }
|   expr  SEMICOLON exprlist         { $1 :: $3  }

expr_comma_list:
| expr_comma_list; COMMA; expr { $3 :: $1 }
| expr; COMMA; expr { [$3; $1] }

texpr:
| BOOL                               { Past.TEbool  }
| INTTYPE                            { Past.TEint  }
| UNITTYPE                           { Past.TEunit  }
| texpr ARROW texpr                  { Past.TEarrow ($1, $3)}
| texpr_prod_list %prec below_MUL    { Past.TEproduct (List.rev $1) }
| texpr ADD texpr                    { Past.TEunion ($1, $3)}
| texpr REF                          { Past.TEref $1 }
| LPAREN texpr RPAREN                { $2 }

texpr_prod_list:
| texpr_prod_list; MUL; texpr { $3 :: $1 }
| texpr MUL texpr { [$3; $1] }
