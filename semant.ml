(* Colode semantic checker *)
open Ast
open Sast

module StringMap = Map.Make(String)
let check (stmts, functions) =
    let make_err e = raise (Failure e) in
    let add_func map fd = 
        let dup_err = "Function with name " ^ fd.fname ^ " is already defined"
            and name = fd.fname
        in match fd with 
          _ when StringMap.mem name map -> make_err dup_err
        | _ -> StringMap.add name fd map
    in
    let built_in_funcs = List.fold_left add_func [
        {typ = Void; fname = "print"; formals = (String, "arg"); locals = []; body = [] }};
        ] StringMap.empty
    in
    let func_decls = List.fold_left add_func functions built_in_funcs in
    let find_func name = 
        try StringMap.find name func_decls
        with Not_found -> raise( Failure("Undeclared function: " ^ name))
    in
    let add_var map ventry = 
        let name = snd ventry in
        let dup_err = "Variable with name " ^ name ^" is a duplicate." in
        match ventry with
          _ when StringMap.mem name map -> make_err dup_err
        | _ -> StringMap.add name ventry
    in
    let find_var map name =
        try StringMap.find name map
        with Not_found -> raise( Failure("Undeclared variable: " ^ name))
    in
    let check_var_decl (var : Declare ) map = 
        let void_err = "Illegal void " ^ snd var 
            and dup_err = "Duplicate declaration: " ^ snd var
        in match var with
          (Void, _) -> raise (Failure void_err)
        | (typ, id) -> match StringMap.find_opt id map with
              Some(val) -> raise (Failure dup_err)
            | None -> SDeclare(typ, id)
    in
    let check_type_equal lvaluet rvaluet err =
        if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in
    let type_of_id map id = fst (find_var map id) in
    let rec check_expr exp map = match exp with
      Literal l -> (Int, SLiteral l, map)
    | Fliteral l -> (Float, SFLiteral l, map)
    | BoolLit l -> (Bool, SBoolLiteral l, map)
    | CharLiteral  l -> (Char, SCharLiteral l, map)
    | StringLiteral s -> (String, SStringLiteral s, map)
    | Id i -> (type_of_id map i, SId i, map)
    | Unop(op, e) as ex ->
        let (t, sx, map') = check_expr e map in
        let ty = match op with
              Neg when t = Int || t = Float || t = Image || t = Matrix -> t
            | Not when t = Bool -> Bool
            | _ -> make_err ("Illegal unary operator" ^ string_of_uop op ^ string_of_typ t ^ "in" ^ string_of_expr ex)
        in (ty, SUnop(op, (t, sx)), map')
    | Binop(e1, op, e2) as ex ->
        let (t1, e1', map') = check_expr e1 map 
          and (t2, e2', map'') = check_expr e2 map' 
        in
        let same = t1 = t2 in
        let ty = match op with
          Add | Sub | Mult | Div | Exp when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div | Exp when same && t1 = Float -> Float
          | Add | Sub | Mult | Div | Conv when same && t1 = Matrix -> Matrix
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> make_err ("Illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr e)
        in (ty, SBinop((t1, e1'), op, (t2, e2')), map'')
    | Assign(var, e) as ex -> 
        let left_t = type_of_id var 
        and (right_t, sx, map') = check_expr e map in
        let err = "illegal assignment " ^ string_of_typ left_t ^ " = " ^ string_of_typ right_t ^ " in " ^ string_of_expr ex
        in (check_type_equal left_t right_t err, SAssign(var, (right_t, sx)), map')
    | AssignAdd (var, e) as ex -> 
        let left_t = type_of_id var 
        and (right_t, sx, map') = check_expr e map in
        let err = "Illegal assignment " ^ string_of_typ left_t ^ " = " ^ string_of_typ right_t ^ " in " ^ string_of_expr ex
        in
        let cant_add_err = "Illegal assign-add on " 
        let ty = check_type_equal left_t right_t err
        in match ty with
          Int | Float | Matrix | String -> (ty, SAssign(var, (right_t, sx)), map')
          | _ -> make_err("")
    | AssignMinus of expr * expr
    | AssignTimes of expr * expr
    | AssignDivide of expr * expr
    | DeclAssign of typ * string * expr
    | Call(func, args) as call -> 
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
        make_err ("expecting " ^ string_of_int param_length ^ " arguments in " ^ string_of_expr call)
        else let check_call (param_t, _) e = 
            let (arg_t, sx, _) = check_expr e in 
            let err = "illegal argument found " ^ string_of_typ arg_t ^
              " expected " ^ string_of_typ param_t ^ " in " ^ string_of_expr e
            in (check_type_equal param_t arg_t err, sx)
        in 
        let args' = List.map2 check_call fd.formals args
        in (fd.typ, SCall(fname, args'), map)
    | Array of expr list
    | ArrayIndex of expr * expr
    | Array2DIndex of expr * expr * expr
    | MemberAccess of expr * string list
    | Noexpr
