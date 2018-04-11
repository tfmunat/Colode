
module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)
let translate (statements, functions) =
	let make_err e = raise (Failure e) in
	let context = L.global_context () in
	(* Primitive types *)
	let i32_t = L.i32_type context
		and i8_t = L.i8_type context
		and i1_t = L.i1_type  context (* used to represent boolean type *)
		and float_t = L.double_type context
		and void_t = L.void_type context
		and char_t = L.i8_type context
	in
	(* Compound types *)
	let list_t = fun (inner_typ: L.lltype) -> L.struct_type context [| L.pointer_type inner_typ; i32_t (*length*); i32_t (*capacity*)|] in
	let string_t = L.struct_type context [| L.pointer_type char_t; i32_t (*length*); |] in 
	let matrix_t = L.struct_type context [| L.pointer_type float_t; i32_t (*width*); i32_t (*height*) |]  in 
	let image_t  = L.struct_type context [| i32_t (*width*); i32_t (* height *); L.pointer_type matrix_t; L.pointer_type matrix_t; L.pointer_type matrix_t; |] in
	let pixel_t = L.vector_type float_t 4 in
	(* Internal constants *)
	let zero = L.const_int (L.i32_type context) 0 in
	let one = L.const_int (L.i32_type context) 1 in
	let const_i32_of = L.const_int (L.i32_type context) in
	(* Main module *)
	let code_module = L.create_module context "Colode" in
	let rec ltype_of_typ = function
		  A.Int   -> i32_t
		| A.Bool  -> i1_t
		| A.Float -> float_t
		| A.Void  -> void_t
		| A.Char -> char_t
		| A.ArrayList t -> list_t (ltype_of_typ t)
		| A.String -> string_t
		| A.Image -> image_t
		| A.Matrix -> matrix_t
		| A.Pixel -> pixel_t
	in
	let print_t = L.function_type i32_t [| L.pointer_type char_t |] in
	let print_func = L.declare_function "puts" print_t code_module in
	let function_decls =
		let func_decl map fd =
			let name = fd.sfname in
			let formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fd.sformals) in
			let func_type = L.function_type (ltype_of_typ fd.styp) formal_types in
			StringMap.add name (L.define_function name func_type code_module, fd) map 
		in
		List.fold_left func_decl StringMap.empty functions
	in
	let lookups map name : L.llvalue = match StringMap.find_opt name map	with
	  Some v -> v | None -> make_err ("Couldn't find " ^ name)
    in
    let rec expr map builder (typ, sx) = match sx with
	  SLiteral i -> (L.const_int i32_t i, map)
	| SBoolLit b -> (L.const_int i1_t (if b then 1 else 0), map)
	| SFliteral l -> (L.const_float_of_string float_t l, map)
	| SCharLiteral c -> (L.const_int i8_t (Char.code c), map)
	| SStringLiteral s -> let alloc = L.build_alloca string_t "" builder in
		let str_global = L.build_global_string s "" builder in
		let str = L.build_bitcast str_global (L.pointer_type i8_t) "" builder in
		let str_field_loc = L.build_struct_gep alloc 0 "" builder in
		let str_len = L.const_int i32_t (String.length s) in
		let len_loc = L.build_struct_gep alloc 1 "" builder in 
		let _ = L.build_store str str_field_loc builder in
		let _ = L.build_store str_len len_loc builder
	in (alloc, map)
	| SNoexpr -> (L.const_int i32_t 0, map)
	| SId s -> (L.build_load (lookups map s) s builder, map)
	| SCall ("print", [ex]) -> let s_lval, _ = expr map builder ex in
		let s = L.build_struct_gep s_lval 0 "" builder in
		let lo = L.build_load s "" builder in
		(L.build_call print_func [|lo|] "" builder, map)
	| SAssign(lex, rex) -> let rval, m' = expr map builder rex in
		let addr = match (snd lex) with
		  SId s -> lookups map s
		  | SArrayIndex(id, idx) -> let name = match snd id with 
			    SId s -> s
			    | _ -> "err:cannot index non-id"
			in
		  	let a_addr = lookups map name in
		  	let data_field_loc = L.build_struct_gep a_addr 0 "" builder in
		  	let data_loc = L.build_load data_field_loc "" builder in
		  	let ival, _ = expr map builder idx in
		  	L.build_gep data_loc [| zero; ival |] "" builder 
		  | SArray2DIndex(id, idx, idx2) -> let name = match snd id with 
			    SId s -> s
			    | _ -> "err:cannot index non-id"
			in
		  	let a_addr = lookups map name in
		  	let data_field_loc = L.build_struct_gep a_addr 0 "" builder in
		  	let data_loc = L.build_load data_field_loc "" builder in
		  	let ival, _ = expr map builder idx in
		  	let ival2, _ = expr map builder idx2 in
		  	L.build_gep data_loc [| zero; ival; ival2 |] "" builder
		  | _ -> make_err "Cannot assign to a non-name type. This error should be caught by semantic checker."
		in
		let _ = L.build_store rval addr builder in 
		(rval, m')
	| SDeclAssign(ty, s, rex) -> let l_type = ltype_of_typ ty in
		let addr = L.build_alloca l_type s builder in
		let rval, m' = expr map builder rex in
		let m'' = StringMap.add s addr m' in
		let _ = L.build_store rval addr builder in 
		(rval, m'')
	| SArray sl -> let l_type = ltype_of_typ (match sl with [] -> A.Void | _ -> (fst (List.hd sl)) ) in 
		let ty = list_t l_type in
		let alloc = L.build_alloca ty "" builder in
		let data_field_loc = L.build_struct_gep alloc 0 "" builder in
		let len_loc = L.build_struct_gep alloc 1 "" builder in
		let cap_loc = L.build_struct_gep alloc 2 "" builder in
		let len = List.length sl in
		let cap = len * 2 in 
		let data_loc = match cap with 0 -> L.const_pointer_null l_type 
			| _ -> L.build_array_alloca l_type (const_i32_of cap) "" builder
		in
		let sto acc ex = 
			let value, m' = expr map builder ex in
			let item_loc = L.build_gep data_loc [|zero; const_i32_of acc |] "" builder in
			let _ = L.build_store value item_loc builder in
			acc + 1
		in
		let _ = List.fold_left sto 0 sl in
		let _ = L.build_store data_loc data_field_loc builder in
		let _ = L.build_store (const_i32_of len) len_loc builder in
		let _ = L.build_store (const_i32_of cap) cap_loc builder in
		(alloc, map)
	| _ -> make_err "Unimplemented"
	in
	let add_terminal builder fn = match L.block_terminator (L.insertion_block builder) with
	  Some _ -> ()
	| None -> ignore (fn builder) in
	let rec stmt map builder s = match s with
	  SBlock sl -> List.fold_left (fun (b, m) s -> stmt m b s) (builder, map) sl (* modify so that SBlock does not leak out scope, return map before fold*)
	| SExpr e -> let (_, m) = (expr map builder e) in (builder, m)
	| SDeclare(t, s) -> let l_type = ltype_of_typ t in
		let addr = L.build_alloca l_type s builder in
		let m' = StringMap.add s addr map in
		(builder, m')
	| _ -> make_err "Unimplemented"
	in
	let build_main sl = 
		let main_ty = L.function_type i32_t [||] in
		let main_func = L.define_function "main" main_ty code_module in
		let builder = L.builder_at_end context (L.entry_block main_func) in
		let () = ignore(stmt StringMap.empty builder (SBlock sl)) in
		ignore(L.build_ret (L.const_int i32_t 0) builder)
	in build_main statements; code_module