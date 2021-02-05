
/* Auxiliary code */
%{

let get_loc = Parsing.symbol_start_pos
let next_free = ref 0
let new_free () = (next_free := !next_free + 1; Past.TEany (ref (Past.Free (string_of_int (!next_free)))))

%}

/* Tokens and types */
%token<int> INT
%token<string> IDENT
%token EOF LPAREN RPAREN COMMA COLON SEMICOLON ADD SUB MUL DIV NOT EQUAL LT ANDOP OROP
%token WHAT UNIT AND TRUE FALSE IF FI THEN ELSE LET REC IN BEGIN END BOOL INTTYPE UNITTYPE
%token ARROW BAR INL INR FST SND FUN NUF CASE OF REF ASSIGN BANG WHILE DO OD INDEX

%left ADD SUB                     /* lowest precedence */
%left ARROW /* edited to distinguish from MUL */
%left MUL DIV ANDOP OROP EQUAL  LT /* medium precedence */
%left ASSIGN
/*
%nonassoc THEN
%nonassoc ELSE
*/
%nonassoc UMINUS
/* pseudo-token to make types shift rather than reduce. inl (T ref) e rather than inl T (ref e) */
%right TEXPR
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc UNIT INT WHAT IDENT TRUE FALSE LPAREN NOT BANG REF /* highest precedence */

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
| LPAREN expr COMMA exprtuple RPAREN             { Past.Tuple (get_loc(), ($2::$4))}
| LPAREN expr RPAREN                 { $2 }

| NOT simple_expr               { Past.UnaryOp(get_loc(), Past.NOT, $2) }
| BANG simple_expr              { Past.Deref(get_loc(), $2) }
| REF simple_expr               { Past.Ref(get_loc(), $2) }

/* FST/SND kept for back-compatibility */
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
| FST expr %prec UMINUS              { Past.Index(get_loc(), 1, $2) }
| SND expr %prec UMINUS              { Past.Index(get_loc(), 2, $2) }
| INL texpr expr %prec UMINUS        { Past.Inl(get_loc(), $2, $3) }
| INR texpr expr %prec UMINUS        { Past.Inr(get_loc(), $2, $3) }
| INL expr %prec UMINUS        { Past.Inl(get_loc(), new_free(), $2) }
| INR expr %prec UMINUS        { Past.Inr(get_loc(), new_free(), $2) }
| FUN simpleIdent ARROW expr END
                                     { Past.Lambda(get_loc(), (fst $2, snd $2, $4)) }
| LET IDENT optionalType EQUAL expr IN expr END           { Past.Let (get_loc(), $2, $3 , $5, $7) }
| LET IDENT simpleIdent optionalType EQUAL expr IN expr END
            { Past.LetFun (get_loc(), $2, (fst $3, snd $3, $6), $4, $8) }
| LET LPAREN bindlist RPAREN  EQUAL expr IN expr END
                                     {Past.LetTuple (get_loc(), $3, $6, $8)}
| LET IDENT LPAREN IDENT optionalType COMMA bindlist RPAREN optionalType EQUAL expr IN expr END
                                     { Past.LetTupleFun (get_loc(), $2, ($4, $5)::$7, $11, $9, $13)}
| CASE expr OF
      INL simpleIdent ARROW expr
  BAR INR simpleIdent  ARROW expr
  END
                                     { Past.Case (get_loc(), $2, (fst $5, snd $5, $7), (fst $10, snd $10, $12)) }
 /* In sticking to SML style, tuple is 1-indexed */
| INDEX INT expr                       { Past.Index (get_loc(), $2, $3)}

/* For single argument and no type given, brackets optional */
simpleIdent:
| IDENT                             {($1, new_free())}
| LPAREN IDENT optionalType RPAREN  {($2, $3)}

exprlist:
|   expr                             { [$1] }
|   expr  SEMICOLON exprlist         { $1 :: $3  }

exprtuple:
| expr                  { [$1] }
| expr COMMA exprtuple  { $1 :: $3 }

optionalType:
|                       { new_free() }
| COLON texpr           { $2 }

/* Have to give some precedences explicitly due to needing multiple levels for
   TEprod - which uses a list of types instead of just single types */
texpr:
| texpr2 %prec TEXPR               { $1 }
| texpr ARROW texpr                { Past.TEarrow ($1, $3)}
| texpr ADD texpr                  { Past.TEunion ($1, $3)}
| texpr2 MUL tlist                 { Past.TEprod ($1::$3)}
tlist:
| texpr2 %prec TEXPR               { [$1] }
| texpr2 MUL tlist                 {$1 :: $3}
texpr2:
| BOOL                               { Past.TEbool }
| INTTYPE                            { Past.TEint  }
| UNITTYPE                           { Past.TEunit  }
| texpr2 REF                         { Past.TEref $1 }
| LPAREN texpr RPAREN                { $2 }

bindlist:
| IDENT optionalType {[($1, $2)]}
| IDENT optionalType COMMA bindlist {($1, $2) :: $4}



