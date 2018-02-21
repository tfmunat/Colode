/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEQUENCE LBLOCK RBLOCK LPAREN RPAREN LBRACE RBRACE SEMI COMMA PLUS MINUS TIMES DIVIDE
%token EXPONENT MODULUS ASSIGN ASSIGNADD ASSIGNMINUS ASSIGNTIMES ASSIGNDIVIDE NOT EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE ELIF FOR WHILE BREAK CONTINUE DEF INT BOOL FLOAT VOID
%token CHAR STRING LIST IMAGE PIXEL MATRIX COLON CONV
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT LITERALSTRING
%token <char> LITERALCHAR
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE NOELIF
%nonassoc ELSE ELIF 
%right ASSIGN ASSIGNADD ASSIGNMINUS ASSIGNDIVIDE ASSIGNTIMES
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE LEFT
%left EXPONENT MODULUS CONV ID SEQUENCE
%right NOT NEG 


%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls stmt { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
   DEF ID LPAREN formals_opt RPAREN COLON typ compound_stmt
     { { typ = $7;
	 fname = $2;
	 formals = $4;
	 locals = [];
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | VOID  { Void  }
  | CHAR  { Char }
  | LIST  { List }
  | STRING { String }
  | IMAGE  { Image }
  | PIXEL  { Pixel }
  | MATRIX { Matrix } 
/*
vdecl_list:
       { [] }
  | vdecl_list COMMA vdecl { $2 :: $1 }
*/
vdecl:
   typ ID SEQUENCE { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }
  | stmt_list SEQUENCE stmt { $3 :: $1 }

compound_stmt:
 | LBLOCK stmt_list RBLOCK { $2 }
/* | SEQUENCE compound_stmt { $2 }
 | compound_stmt SEQUENCE { $1}
*/

/* TODO adjust for (;;)  to match python style? */
stmt:
   expr SEQUENCE                           { Expr $1               }
  /*| SEQUENCE expr                            { Expr $2               }*/
  | RETURN expr_opt SEQUENCE                        { Return $2             }
  | compound_stmt                 { Block(List.rev $1)  }
  | IF expr COLON stmt %prec NOELSE { If($2, $4, Block([])) }
  | IF expr COLON stmt ELIF expr COLON stmt %prec NOELSE   { If($2, $4, If($6, $8, Block([])))  }
  | IF expr COLON stmt ELIF expr COLON stmt ELSE stmt  { If($2, $4, If($6, $8, $10))  }
  | IF expr COLON stmt ELSE stmt  %prec NOELIF  { If($2, $4, $6)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE expr COLON stmt           { While($2, $4)         }
  | vdecl                                   { Declare(fst $1, snd $1) }

expr_opt:
  |  { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1)            }
  | FLIT	     { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | LITERALCHAR       { CharLiteral($1)}
  | LITERALSTRING       { StringLiteral($1)}
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr CONV     expr { Binop($1, Conv,    $3)   }
  | MINUS expr %prec NEG { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | typ ID ASSIGN expr   { DeclAssign($1, $2, $4)         }
  | ID ASSIGNADD expr   { AssignAdd($1, $3)       }
  | ID ASSIGNMINUS expr   { AssignMinus($1, $3)    }
  | ID ASSIGNTIMES expr   { AssignTimes($1, $3)     }
  | ID ASSIGNDIVIDE expr   { AssignDivide($1, $3) }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }
  | array_lit          { $1 }

array_lit: LBRACE array_opt RBRACE { Array(List.rev $2) }

array_opt: { [] }
  | expr { [$1] }
  |  array_opt COMMA expr { $3 :: $1 }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
