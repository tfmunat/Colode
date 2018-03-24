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
	  Literal l -> (Int, SLiteral l)
	| Fliteral l -> (Float, SFLiteral l)
	| BoolLit l -> (Bool, SBoolLiteral l)
	| CharLiteral  l -> (Char, SCharLiteral l)
	| StringLiteral s -> (String, SStringLiteral s)
	| Id i -> (type_of_id map i, SId i)
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
