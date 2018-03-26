/* Ocamlyacc parser for Colode */

%{ open Ast %}

%token SEQUENCE LBLOCK RBLOCK LPAREN RPAREN LBRACE RBRACE SEMI COMMA PLUS MINUS TIMES DIVIDE
%token EXPONENT MODULUS ASSIGN ASSIGNADD ASSIGNMINUS ASSIGNTIMES ASSIGNDIVIDE NOT EQ NEQ LT LEQ GT GEQ AND OR DOT
%token RETURN IF ELSE ELIF FOR WHILE BREAK CONTINUE DEF INT BOOL FLOAT VOID IN
%token CHAR STRING LIST IMAGE PIXEL MATRIX COLON CONV
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT LITERALSTRING
%token <char> LITERALCHAR
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE NOELIF ONED
%nonassoc ELSE ELIF  LBRACE LPAREN  RBRACE RPAREN
%right ASSIGN ASSIGNADD ASSIGNMINUS ASSIGNDIVIDE ASSIGNTIMES 
%left OR DOT
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS 
%left TIMES DIVIDE LEFT
%left EXPONENT MODULUS CONV ID SEMI 
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
  | typ LIST  { ArrayList($1) }
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
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

compound_stmt:
 | LBLOCK stmt_list RBLOCK { $2 }
  
stmt:
   expr SEMI                           { Expr $1               }
  | RETURN expr_opt SEMI                        { Return $2             }
  | IF expr stmt %prec NOELSE { If($2, $3, Block([])) }
  | IF expr stmt ELIF expr stmt %prec NOELSE   { If($2, $3, If($5, $6, Block([])))  }
  | IF expr stmt ELIF expr stmt ELSE stmt  { If($2, $3, If($5, $6, $8))  }
  | IF expr stmt ELSE stmt  %prec NOELIF  { If($2, $3, $5)        }
  | FOR expr IN expr stmt
                                            { For($2, $4, $5)   }
  | WHILE expr stmt           { While($2, $3)         }
  | vdecl                                   { Declare(fst $1, snd $1) }
  | compound_stmt {Block(List.rev $1)}

expr_opt:
  |  { Noexpr }
  | expr          { $1 }

member: 
 DOT ID { [$2] } 
 | member DOT ID { $3 :: $1 }

name: ID {Id($1)} 
  | expr LBRACE expr RBRACE %prec ONED { ArrayIndex($1,$3) }
  | expr LBRACE expr RBRACE LBRACE expr RBRACE { Array2DIndex($1,$3, $6) }

expr:
    LITERAL          { Literal($1)              }
  | FLIT	     { Fliteral($1)             }
  | BLIT             { BoolLit($1)              }
  | LITERALCHAR      { CharLiteral($1)          }
  | LITERALSTRING    { StringLiteral(String.sub $1 1 ((String.length $1)-2))            } 
  | ID               { Id($1)                   }
  | expr PLUS   expr { Binop($1, Add,   $3)     }
  | expr MINUS  expr { Binop($1, Sub,   $3)     }
  | expr TIMES  expr { Binop($1, Mult,  $3)     }
  | expr EXPONENT  expr { Binop($1, Exp,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)     }
  | expr EQ     expr { Binop($1, Equal, $3)     }
  | expr NEQ    expr { Binop($1, Neq,   $3)     }
  | expr LT     expr { Binop($1, Less,  $3)     }
  | expr LEQ    expr { Binop($1, Leq,   $3)     }
  | expr GT     expr { Binop($1, Greater, $3)   }
  | expr GEQ    expr { Binop($1, Geq,   $3)     }
  | expr AND    expr { Binop($1, And,   $3)     }
  | expr OR     expr { Binop($1, Or,    $3)     }
  | expr CONV     expr { Binop($1, Conv,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2)        }
  | NOT expr         { Unop(Not, $2)            }
  | name ASSIGN expr   { Assign($1, $3)         }
  | typ ID ASSIGN expr   { DeclAssign($1, $2, $4)   }
  | name ASSIGNADD expr   { AssignAdd($1, $3)       }
  | name ASSIGNMINUS expr   { AssignMinus($1, $3)   }
  | name ASSIGNTIMES expr   { AssignTimes($1, $3)   }
  | name ASSIGNDIVIDE expr   { AssignDivide($1, $3) }
  | ID LPAREN args_opt RPAREN { Call($1, $3)        }
  /*| LPAREN expr RPAREN { $2 } */
  | array_lit          { $1 }
  | expr LBRACE expr RBRACE %prec ONED { ArrayIndex($1,$3) }
  | expr LBRACE expr RBRACE LBRACE expr RBRACE { Array2DIndex($1,$3, $6) }
  | ID member     { MemberAccess(Id($1), List.rev $2) }


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
