(* Colode semantic checker *)
open Ast
open Sast

module StringMap = Map.Make(String)
type stmt_context = { current_func: func_decl option }
let check (stmts, functions) =
    let make_err e = raise (Failure e) in
    let add_func map fd = 
        let dup_err = "Function with name " ^ fd.fname ^ " is already defined"
            and name = fd.fname
        in match fd with 
          _ when StringMap.mem name map -> make_err dup_err
        | _ -> StringMap.add name fd map
    in
    let built_in_funcs = List.fold_left add_func StringMap.empty [
        {typ = Void; fname = "print"; formals = [(String, "arg")]; locals = []; body = [] };
        ]  (* TODO add other standard library functions*)
    in
    let func_decls = List.fold_left add_func built_in_funcs functions  in
    let find_func name = 
        try StringMap.find name func_decls
        with Not_found -> raise( Failure("Undeclared function: " ^ name))
    in
    let add_var map ventry = 
        let name = snd ventry in
        let dup_err = "Variable with name " ^ name ^" is a duplicate." in
        match ventry with
          _ when StringMap.mem name map -> make_err dup_err
        | _ -> StringMap.add name ventry map
    in
    let find_var map name =
        try StringMap.find name map
        with Not_found -> raise( Failure("Undeclared variable: " ^ name))
    in
    (* UNUSED let check_var_decl var map = 
        let void_err = "Illegal void " ^ snd var 
            and dup_err = "Duplicate declaration: " ^ snd var
        in match var with
          (Void, _) -> raise (Failure void_err)
        | (typ, id) -> match (StringMap.find_opt id map) with
              Some v -> raise (Failure dup_err)
            | None -> SDeclare(typ, id)
    in *)
        let check_type_equal lvaluet rvaluet err =
        if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in
    let type_of_id map id = fst (find_var map id) in
    let rec check_expr map exp  = match exp with
      Literal l -> (Int, SLiteral l, map)
    | Fliteral l -> (Float, SFliteral l, map)
    | BoolLit l -> (Bool, SBoolLit l, map)
    | CharLiteral  l -> (Char, SCharLiteral l, map)
    | StringLiteral s -> (String, SStringLiteral s, map)
    | Id i -> (type_of_id map i, SId i, map)
    | Unop(op, e) as ex ->
        let (t, sx, map') = check_expr map e  in
        let ty = match op with
              Neg when t = Int || t = Float || t = Image || t = Matrix -> t
            | Not when t = Bool -> Bool
            | _ -> make_err ("Illegal unary operator" ^ string_of_uop op ^ string_of_typ t ^ "in" ^ string_of_expr ex)
        in (ty, SUnop(op, (t, sx)), map')
    | Binop(e1, op, e2) as ex ->
        let (t1, e1', map') = check_expr map e1  
        in let (t2, e2', map'') = check_expr map' e2 
        in
        let same = t1 = t2 in
        let ty = match op with
          Add | Sub | Mult | Div | Exp when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div | Exp when same && t1 = Float -> Float
          | Add when same && t1 = Char -> Char
          | Add when same && t1 = String -> String
          | Add | Sub | Mult | Div | Conv when same && t1 = Matrix -> Matrix
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> make_err ("Illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr ex)
        in (ty, SBinop((t1, e1'), op, (t2, e2')), map'')
    | Assign(name, e) as ex -> 
        let err = "illegal assignment " ^ string_of_expr ex in
        let (left_t, sname, map') = check_name name map err in
        let (right_t, sx, map'') = check_expr map' e in
        (check_type_equal left_t right_t err, SAssign((left_t, sname), (right_t, sx)), map'')
    | AssignAdd (name, e) as ex -> 
        let err = "illegal assign-add " ^ string_of_expr ex in
        let (left_t, sname, map') = check_name name map err in
        let (right_t, sx, map'') = check_expr map' e in
        let ty = check_type_equal left_t right_t err
        in (match ty with
                  Int | Float | Matrix | String -> (ty, SAssignAdd((left_t, sname), (right_t, sx)), map'')
                | _ -> make_err err)
    | AssignMinus (name, e) as ex -> 
        let err = "illegal assign-minus " ^ string_of_expr ex in
        let (left_t, sname, map') = check_name name map err in
        let (right_t, sx, map'') = check_expr map' e in
        let ty = check_type_equal left_t right_t err
        in (match ty with
                  Int | Float | Matrix -> (ty, SAssignMinus((left_t, sname), (right_t, sx)), map'')
                | _ -> make_err err)
    | AssignTimes (name, e) as ex -> 
        let err = "illegal assign-times " ^ string_of_expr ex in
        let (left_t, sname, map') = check_name name map err in
        let (right_t, sx, map'') = check_expr map' e in
        let ty = check_type_equal left_t right_t err
        in (match ty with
                  Int | Float | Matrix -> (ty, SAssignTimes((left_t, sname), (right_t, sx)), map'')
                | _ -> make_err err)
    | AssignDivide (name, e) as ex -> 
        let err = "illegal assign-divide " ^ string_of_expr ex in
        let (left_t, sname, map') = check_name name map err in
        let (right_t, sx, map'') = check_expr map' e in
        let ty = check_type_equal left_t right_t err
        in (match ty with
                  Int | Float | Matrix -> (ty, SAssignDivide((left_t, sname), (right_t, sx)), map'')
                | _ -> make_err err)
    | DeclAssign (left_t, id, e) ->
        let (right_t, sx, map') = check_expr map e  in
        let err = "illegal argument found " ^ string_of_typ left_t ^ ", types must match in assignment." in
        let ty = check_type_equal left_t right_t err in
        let new_map = add_var map' (ty, id) in
        let right = (right_t, sx) in
        let da = SDeclAssign(ty, id, right) in
        (ty, da, new_map)
    | Call(func, args) as call -> 
        let fd = find_func func in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
        make_err ("expecting " ^ string_of_int param_length ^ " arguments in " ^ string_of_expr call)
        else let check_call (param_t, _) e = 
            let (arg_t, sx, _) = check_expr map e in
            let err = "illegal argument found " ^ string_of_typ arg_t ^
              " expected " ^ string_of_typ param_t ^ " in " ^ string_of_expr e
            in (check_type_equal param_t arg_t err, sx)
        in 
        let args' = List.map2 check_call fd.formals args
        in (fd.typ, SCall(func, args'), map)
    | Array(l) as exp ->
        if List.length l = 0 then (Void, SArray([]), map) (* make sure assignment allows an empty array*)
        else let sbody = List.map (check_expr map) l in 
            let err = "Illegal array literal, arrays are single type in " ^ string_of_expr exp in
            let match_type, _, _ = List.nth sbody 0 in
            let correct = List.for_all (fun (t, _, _) -> t = match_type) sbody in
            if correct then 
                let clean_body = List.map (fun (t, sx, _) -> (t,sx)) sbody in
                (match_type, SArray(clean_body), map)
            else make_err err
    | ArrayIndex(name, idx) ->
        let cannot_idx_err = "Illegal index on " ^ string_of_expr name in
        let invalid_idx_err = "Illegal index on " ^ string_of_expr name ^ ". Index must be numerical" in
        let (typ, sid, map') = match name with
              Id _ -> check_expr map name
            | _ -> make_err cannot_idx_err
        in
        let inner_typ = match typ with
              ArrayList lt -> lt
            | Pixel -> Float (* TODO support 1d index on matrix, inner type is then List *)
            | _ -> make_err cannot_idx_err
        in 
        let (idx_type, si, map'') = match idx with
              Literal _ -> check_expr map' idx
            | _ -> make_err invalid_idx_err
        in
        let arr = (typ, sid) in
        let index = (idx_type, si) in
        (inner_typ, SArrayIndex(arr, index), map'')
    | Array2DIndex (name, idx, idx2) ->
        let cannot_idx_err = "Illegal index on " ^ string_of_expr name in
        let invalid_idx_err = "Illegal index on " ^ string_of_expr name ^ ". Index must be numerical" in
        let (typ, sid, map') = match name with
              Id _ -> check_expr map name
            | _ -> make_err cannot_idx_err
        in
        let inner_typ = match typ with
              Matrix -> Float
            | _ -> make_err cannot_idx_err
        in 
        let (idx_type, si, map'') = match idx with
              Literal _ -> check_expr map' idx
            | _ -> make_err invalid_idx_err
        in 
        let (idx2_type, si2, map''') = match idx2 with
              Literal _ -> check_expr map'' idx2
            | _ -> make_err invalid_idx_err
        in
        let mat = (typ, sid) in 
        let index = (idx_type, si) in
        let index2 = (idx2_type, si2) in
        (inner_typ, SArray2DIndex(mat, index, index2), map''')
    | MemberAccess(_, _) ->  (Void, SNoexpr, map) (* Todo *)
    | Noexpr -> (Void, SNoexpr, map)
    and check_name (name : expr) map err : (Ast.typ * Sast.sx * (Ast.typ * StringMap.key) StringMap.t
) = match name with
        Id _ | ArrayIndex(_,_) | Array2DIndex(_,_,_) -> check_expr map name
        | _ -> make_err err
    in
    let check_bool_expr map e = 
      let (t', e', map') = check_expr map e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in
    let rec check_stmt map st (ctxt : stmt_context) = match st with
      Expr e -> let (ty, sx, map') = check_expr map e in (SExpr (ty, sx), map')
    | Return e -> let (ty, sx, map') = check_expr map e in
        let return_from_global_err = "Cannot return " ^ string_of_expr e ^ " from global context" in
        (match ctxt.current_func with
                  None -> if ty <> Void then make_err return_from_global_err else (SReturn(ty, sx), map')
                | Some(fd) -> (* UNUSED let invalid_return_err = "return gives " ^ string_of_typ ty ^ " expected " ^ string_of_typ fd.typ ^ " in " ^ string_of_expr e in  *)
                    if ty = fd.typ then (SReturn((ty, sx)), map') 
                    else make_err return_from_global_err)
    | If(pred, then_block, else_block) -> 
        let sthen, _ = check_stmt map then_block ctxt in
        let selse, _ = check_stmt map else_block ctxt in
        (SIf(check_bool_expr map pred, sthen, selse), map)
    | For(e1, e2, e3, st) -> 
        (* let invalid_err = "Invalid for loop cursor" in
        let invalid_iterator_err = "Invalid for loop iterator" in
        let err = "Name of for loop cursor already in use:" ^ string_of_expr cursor in
        let check_iterator map iterator =
            let (ty, sx, map') = check_expr map iterator in
            match ty with
              ArrayList _ | Pixel | Matrix | String -> (ty, sx, map')
            | _ -> make_err invalid_iterator_err
        in
        let (ty, sx, map') = check_iterator map iterator in
        let name = match cursor with
          Id n -> n | _ -> make_err invalid_err
        in
        if StringMap.mem name map' then make_err err else
        let it_ty = match ty with
            ArrayList(t) -> t | Pixel | Matrix -> Float | String -> Char | _ -> make_err invalid_iterator_err
        in
        let new_map = add_var map' (it_ty, name) in
        let (sblock, _) = check_stmt new_map block ctxt in *)
        let (ty1, sx1, m') = check_expr map e1
        let (ty3, sx3, m'') = check_expr m' e3
        SFor((ty1,sx1), check_bool_expr map e2, (ty3, sx3), fst (check_stmt map st ctxt)), map
    | While(p, s) -> SWhile(check_bool_expr map p, fst (check_stmt map s ctxt)), map
    | Declare(t, id) ->
        let new_map = add_var map (t, id) in
        (SDeclare(t, id), new_map)
    | Block stl -> 
        let (checked, map') = check_stmt_list map stl ctxt in
        (SBlock(checked), map)
    and check_stmt_list map sl (ctxt : stmt_context) = match sl with
        [Return _ as s] -> ([fst (check_stmt map s ctxt)], map)
        | Return _ :: _   -> raise (Failure "nothing may follow a return")
        | Block sl :: ss  -> check_stmt_list map (sl @ ss) ctxt(* Flatten blocks *)
        | s :: ss         -> let (sst, map') = check_stmt map s ctxt in
            let (slist, map'') = check_stmt_list map' ss ctxt in
            (sst :: slist, map'')
        | []              -> ([], map)
    in
    let check_func fd = 
        let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name (ty, name) m) StringMap.empty fd.formals
        in 
        {
            styp = fd.typ;
            sfname = fd.fname;
            sformals = fd.formals;
            sbody = let (blk, map') = check_stmt symbols (Block(fd.body)) {current_func= Some fd} in
                  match blk with SBlock(s1) -> s1 
                | _ -> make_err "Internal err... block didn't become block?";
        }
    in
    let sfunctions = List.map check_func functions in
    let (sstmt, _) = check_stmt_list StringMap.empty stmts {current_func= None} in
    (sstmt, sfunctions)
