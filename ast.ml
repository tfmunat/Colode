(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Conv | Exp

type uop = Neg | Not

type typ = Int | Bool | Float | Void | Char | List | String | Image | Pixel | Matrix

type bind = typ * string

type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | CharLiteral of char
  | StringLiteral of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | AssignAdd of expr * expr
  | AssignMinus of expr * expr
  | AssignTimes of expr * expr
  | AssignDivide of expr * expr
  | DeclAssign of typ * string * expr
  | Call of string * expr list
  | Array of expr list
  | ArrayIndex of expr * expr
  | Array2DIndex of expr * expr * expr
  | MemberAccess of expr * string list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * stmt
  | While of expr * stmt
  | Declare of typ * string

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = stmt list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Conv -> "**"
  | Exp -> "^"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | Char -> "char"
  | List -> "list"
  | String -> "string"
  | Image -> "image"
  | Pixel -> "pixel"
  | Matrix -> "matrix"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | AssignAdd(v, e) -> string_of_expr v ^ " += " ^ string_of_expr e
  | AssignMinus(v, e) -> string_of_expr v ^ " -= " ^ string_of_expr e
  | AssignTimes(v, e) -> string_of_expr v ^ " *= " ^ string_of_expr e
  | AssignDivide(v, e) -> string_of_expr v ^ " /= " ^ string_of_expr e
  | DeclAssign(t, v, e) -> (string_of_typ t) ^ v ^ " /= " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Array(l) -> "[" ^ (String.concat ", " (List.map string_of_expr l)) ^ "]"
  | ArrayIndex(a, b) -> string_of_expr a ^ "[" ^ string_of_expr b ^ "]"
  | Array2DIndex(a, b, c) -> string_of_expr a ^ "[" ^ string_of_expr b ^ "]" ^ "[" ^ string_of_expr c ^ "]"
  | MemberAccess(a, b) -> string_of_expr a ^ "." ^String.concat "." b
  | CharLiteral(c) -> "'" ^ Char.escaped c ^ "'"
  | StringLiteral(s) -> "\"" ^ s ^ "\""
  | Noexpr -> ""


let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3) ->
      "for " ^ string_of_expr e1  ^ " in " ^ string_of_expr e2  ^ string_of_stmt e3
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Declare(t, s) -> (string_of_typ t) ^ " " ^ s


let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (stmts, funcs) =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

