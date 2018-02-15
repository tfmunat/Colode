(* Ocamllex scanner for MicroC *)

(*{ open Parser }*)
{ type token = SEQUENCE | INDENT | LPAREN | RPAREN | LBRACE | RBRACE | COMMA | PLUS | MINUS | TIMES | DIVIDE 
| EXPONENT | MODULUS | ASSIGN | ASSIGNADD | ASSIGNMINUS | ASSIGNTIMES | ASSIGNDIVIDE | EQ | NEQ 
| LT | LEQ | GT | GEQ | AND | OR | NOT | IF | ELSE | ELIF | FOR | WHILE | BREAK | CONTINUE | DEF 
| RETURN | INT | BOOL | FLOAT | CHAR | STRING | LIST | VOID | IMAGE | PIXEL | MATRIX 
| BLIT of bool | LITERAL of int | FLIT of string | LITERALSTRING of string | LITERALCHAR of char
| ID of string | EOF
}

let digit = ['0' - '9']
let digits = digit+
let char_lex = ['\x00' - '\x7F']
let string_lex = char_lex+
let strings = '\"' string_lex '\"'

rule token = parse
  [' ' '\r' '\t'] { token lexbuf } (* Whitespace *)
| "//"     { comment lexbuf }           (* Comments *)
| '\n'     { SEQUENCE }
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACE }
| ']'      { RBRACE }
| '{'      { LBLOCK }
| '}'      { RBLOCK }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { EXPONENT }
| '%'      { MODULUS }
| '='      { ASSIGN }
| "+="      { ASSIGNADD }
| "-="      { ASSIGNMINUS }
| "*="      { ASSIGNTIMES }
| "/="      { ASSIGNDIVIDE }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "**"     { CONV }
| "and"     { AND }
| "or"     { OR }
| "not"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "elif"   { ELIF }
| "for"    { FOR }
| "while"  { WHILE }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "def"		{ DEF }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "char"   { CHAR }
| "string" { STRING }
| "list"   { LIST }
| "void"   { VOID }
| "Image"  { IMAGE }
| "Pixel"  { PIXEL }
| "Matrix" { MATRIX }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| strings as s { LITERALSTRING(s) }
| '\'' (char_lex as c) '\'' { LITERALCHAR(c) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }
