
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
	let zero = L.const_int i32_t 0 in
	let one = L.const_int i32_t 1 in
	let true_ = L.const_int i1_t 1 in
	let false_ = L.const_int i1_t 0 in
	let const_i32_of = L.const_int (L.i32_type context) in
	let const_float_of = L.const_float float_t in
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
	let pow_t = L.function_type float_t [| float_t; float_t |] in
	let pow_func = L.declare_function "pow" pow_t code_module in
	let function_decls =
		let func_decl map fd =
			let name = fd.sfname in
			let formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fd.sformals) in
			let func_type = L.function_type (ltype_of_typ fd.styp) formal_types in
			StringMap.add name (L.define_function name func_type code_module, fd) map 
		in
		List.fold_left func_decl StringMap.empty functions
	in
	let add_terminal builder fn = match L.block_terminator (L.insertion_block builder) with
		  Some _ -> ()
		| None -> ignore (fn builder) 
	in
	let lookups map name : L.llvalue = match StringMap.find_opt name map	with
	  Some v -> v | None -> make_err ("Couldn't find " ^ name)
    in
    let binop_char_concat lv rv name builder : L.llvalue = (* char + char = new string *)
    	let alloc = L.build_alloca string_t name builder in
    	let data_field_loc = L.build_struct_gep alloc 0 "" builder in
		let len_loc = L.build_struct_gep alloc 1 "" builder in
		let len = const_i32_of 2 in
		let data_loc = L.build_array_alloca char_t len "" builder in
		let fst_loc = L.build_gep data_loc [|zero; const_i32_of 0 |] "" builder in
		let snd_loc = L.build_gep data_loc [|zero; const_i32_of 1 |] "" builder in
		let _ = L.build_store lv fst_loc builder in
		let _ = L.build_store rv snd_loc builder in
		let _ = L.build_store data_loc data_field_loc builder in
		let _ = L.build_store len len_loc builder
		in alloc
	in
	let genIf (builder: L.llbuilder) (this : L.llvalue) (pred: L.llvalue) (then_s: L.llbuilder -> L.llbuilder) 
		(else_s: L.llbuilder -> L.llbuilder) 
		: L.llbuilder =
		let merge_bb = L.append_block context "if_merge" this in
		let branch_ins = L.build_br merge_bb in
		let then_bb = L.append_block context "if_then" this in
		let then_builder = then_s (L.builder_at_end context then_bb) in
		let () = add_terminal then_builder branch_ins in
		let else_bb = L.append_block context "if_else" this in
		let else_builder = else_s (L.builder_at_end context else_bb) in
		let () = add_terminal else_builder branch_ins in
		let _ = L.build_cond_br pred then_bb else_bb builder in 
		(L.builder_at_end context merge_bb)
	in
	let genWhile (builder: L.llbuilder) (this : L.llvalue) (pred: L.llbuilder -> L.llvalue * L.llbuilder) (body: L.llbuilder -> L.llbuilder) 
		: L.llbuilder =
		let pred_bb = L.append_block context "while" this in
		let _ = L.build_br pred_bb builder in
		let body_bb = L.append_block context "while_body" this in
		let body_bldr = body (L.builder_at_end context body_bb) in
		let () = add_terminal body_bldr (L.build_br pred_bb) in
		let pred_bldr = L.builder_at_end context pred_bb in
		let bool_val, pred_bldr = pred pred_bldr in
		let merge_bb = L.append_block context "while_merge" this in
		let _ = L.build_cond_br bool_val body_bb merge_bb pred_bldr in
		(L.builder_at_end context merge_bb)	in
	let binop_array_concat ty (this: L.llvalue) (*Llvm func def*) lv rv name builder : L.llvalue * L.llbuilder = (* array + array = new array *)
    	let l_type = ltype_of_typ ty in
    	let alloc = L.build_alloca l_type name builder in
		let data_field_loc = L.build_struct_gep alloc 0 "" builder in
		let len_loc = L.build_struct_gep alloc 1 "" builder in
		let cap_loc = L.build_struct_gep alloc 2 "" builder in
		let ldata_field_loc = L.build_struct_gep lv 0 "" builder in
		let ldata_loc = L.build_load ldata_field_loc "" builder in
		let llen_loc = L.build_struct_gep lv 1 "" builder in
		let llen = L.build_load llen_loc "" builder in
		let rdata_field_loc = L.build_struct_gep rv 0 "" builder in
		let rdata_loc = L.build_load rdata_field_loc "" builder in
		let rlen_loc = L.build_struct_gep rv 1 "" builder in
		let rlen = L.build_load rlen_loc "" builder in
		let len = L.build_add llen rlen "" builder in
		let cap = L.build_mul len (const_i32_of 2) "" builder in
		let pred = L.build_icmp L.Icmp.Eq cap zero "" builder in
		let data_loc = L.build_alloca (L.pointer_type l_type) "" builder in
		let builder = genIf builder this pred 
			(*Then*)
			(fun b -> let _ = L.build_store (L.const_pointer_null l_type) data_loc b in b )
			(*Else*)
			(fun b -> let alloc = L.build_array_alloca l_type cap "" builder in
				let _ = L.build_store alloc data_loc b in b )
		in
		let iter = L.build_alloca i32_t "iter" builder in
		let _ = L.build_store zero iter builder in
		let builder = genWhile builder this 
			(*pred*)
			(fun b -> let i = L.build_load iter "" b in
				(L.build_icmp L.Icmp.Slt i len "" b, b) )
			(*body*)
			(fun b ->
				let i = L.build_load iter "" b in
				let use_left = L.build_icmp L.Icmp.Slt i llen "" b in
				let lgep = L.build_gep ldata_loc [|zero; i |] "" b in
				let rindex = L.build_sub i llen "" b in
				let rgep = L.build_gep rdata_loc [|zero; rindex |] "" b in
				let gep = L.build_select use_left lgep rgep "" b in
				let value = L.build_load gep "" b in
				let new_addr = L.build_gep data_loc [|zero; i|] "" b in
				let _ = L.build_store value new_addr b in
			b)
		in
		let _ = L.build_store data_loc data_field_loc builder in
		let _ = L.build_store len len_loc builder in
		let _ = L.build_store cap cap_loc builder in
		alloc, builder
	in
	let binop_str_concat (this: L.llvalue) (*Llvm func def*) lv rv name builder : L.llvalue * L.llbuilder =
		let l_type = string_t in
		let alloc = L.build_alloca string_t name builder in
		let data_field_loc = L.build_struct_gep alloc 0 "" builder in
		let len_loc = L.build_struct_gep alloc 1 "" builder in
		(* let ldata_field_loc = L.build_struct_gep lv 0 "" builder in *)
		let ldata_loc = L.build_extractvalue lv 0 "" builder in
		(* let llen_loc = L.build_struct_gep lv 1 "" builder in *)
		let llen = L.build_extractvalue lv 1 "" builder in
		(* let rdata_field_loc = L.build_struct_gep rv 0 "" builder in *)
		let rdata_loc = L.build_extractvalue rv 0 "" builder in
		(* let rlen_loc = L.build_struct_gep rv 1 "" builder in *)
		let rlen = L.build_extractvalue rv 1 "" builder in
		let len = L.build_add llen rlen "" builder in
		let pred = L.build_icmp L.Icmp.Eq len zero "" builder in
		let builder = genIf builder this pred 
			(*Then*)
			(fun b -> let _ = L.build_store (L.const_bitcast (L.const_pointer_null i8_t) (L.pointer_type i8_t)) data_field_loc b in b )
			(*Else*)
			(fun b -> let alloc = L.build_array_alloca i8_t len "" builder in
				let _ = L.build_store alloc data_field_loc b in b )
		in
		let data_loc = L.build_load data_field_loc "" builder in
		let iter = L.build_alloca i32_t "iter" builder in
		let _ = L.build_store zero iter builder in
		let builder = genWhile builder this 
			(*pred*) 
			(fun b -> let i = L.build_load iter "" b in
				(L.build_icmp L.Icmp.Slt i len "" b, b) )
			(*body*) 
			(fun b ->
				let i = L.build_load iter "" b in
				let use_left = L.build_icmp L.Icmp.Slt i llen "" b in
(* 		let () = L.print_module "codegen.out" code_module in
		let () = L.dump_module code_module in *)
				let lgep = L.build_gep ldata_loc [|i |] "" b in
				let rindex = L.build_sub i llen "" b in
				let rgep = L.build_gep rdata_loc [| rindex |] "" b in
				let gep = L.build_select use_left lgep rgep "" b in
				let value = L.build_load gep "" b in
				let new_addr = L.build_gep data_loc [|i|] "" b in
				let _ = L.build_store value new_addr b in
				let incr = L.build_add i one "" b in
				let _ = L.build_store incr iter b in 
			b )
		in
		let _ = L.build_store data_loc data_field_loc builder in
		let _ = L.build_store len len_loc builder in
		let value = L.build_load alloc "" builder in
		value, builder
	in
	let binop_str_equal (this: L.llvalue) (*Llvm func def*) lv rv name builder : L.llvalue * L.llbuilder =
		let ldata_field_loc = L.build_struct_gep lv 0 "" builder in
		let ldata_loc = L.build_load ldata_field_loc "" builder in
		let llen_loc = L.build_struct_gep lv 1 "" builder in
		let llen = L.build_load llen_loc "" builder in
		let rdata_field_loc = L.build_struct_gep rv 0 "" builder in
		let rdata_loc = L.build_load rdata_field_loc "" builder in
		let rlen_loc = L.build_struct_gep rv 1 "" builder in
		let rlen = L.build_load rlen_loc "" builder in
		let pred = L.build_icmp L.Icmp.Ne rlen llen "" builder in
		(* let data_loc = L.build_alloca (pointer_type ty) "" builder *)
		let is_equal = L.build_alloca i1_t "is_equal" builder in
		let iter = L.build_alloca i32_t "iter" builder in
		let _ = L.build_store zero iter builder in
		let _ = L.build_store true_ is_equal builder in
		let builder = genIf builder this pred 
			(*Then*)
			(fun b -> let _ = L.build_store false_ is_equal builder in b ) 
			(*Else*)
			(fun b -> genWhile b this
				(*pred*)
				(fun b -> let i = L.build_load iter "" b in
					(L.build_icmp L.Icmp.Slt i llen "" b, b) )
				(*body*)
				(fun b -> let i = L.build_load iter "" b in
					let litem_loc = L.build_gep ldata_loc [| zero; i |] "" b in 
					let ritem_loc = L.build_gep rdata_loc [| zero; i |] "" b in
					let lchar = L.build_load litem_loc "" b in 
					let rchar = L.build_load ritem_loc "" b in
					let pred = L.build_icmp L.Icmp.Ne rchar lchar "" b in
					let b = genIf b this pred 
						(*then*)
						(fun b -> let _ = L.build_store false_ is_equal b in b) 
						(fun b -> b)  
					in b )
			)
		in is_equal, builder
	in
    let rec expr map builder (this: L.llvalue) (*Llvm func def*) (typ, sx) : (L.llvalue * L.llvalue StringMap.t * L.llbuilder) = match sx with
	  SLiteral i -> (L.const_int i32_t i, map, builder)
	| SBoolLit b -> (L.const_int i1_t (if b then 1 else 0), map, builder)
	| SFliteral l -> (L.const_float_of_string float_t l, map, builder)
	| SCharLiteral c -> (L.const_int i8_t (Char.code c), map, builder)
	| SStringLiteral s -> let alloc = L.build_alloca string_t "" builder in (* eventually figure out a way to store value in registers instead of making an extra allocation*)
		let str_global = L.build_global_string s "" builder in
		let str = L.build_bitcast str_global (L.pointer_type i8_t) "" builder in
		let str_field_loc = L.build_struct_gep alloc 0 "" builder in
		let str_len = L.const_int i32_t (String.length s) in
		let len_loc = L.build_struct_gep alloc 1 "" builder in 
		let _ = L.build_store str str_field_loc builder in
		let _ = L.build_store str_len len_loc builder in
		let value = L.build_load alloc "" builder
	in (value, map, builder)
	| SNoexpr -> (L.const_int i32_t 0, map, builder)
	| SId s -> (L.build_load (lookups map s) s builder, map, builder)
	| SCall ("print", [ex]) -> let s_lval, _, builder = expr map builder this ex in
		(* let s = L.build_struct_gep s_lval 0 "" builder in *)
		let lo = L.build_extractvalue s_lval 0 "" builder in
		(L.build_call print_func [|lo|] "" builder, map, builder)
	(*Add rest of built-in functions here *)
	| SCall (name, exl) -> let (ldef, fd) = StringMap.find name function_decls in
		let args = List.map (fun (a,b,c) -> a) (List.rev (List.map (expr map builder this) (List.rev exl))) in
		let call = L.build_call ldef (Array.of_list args) "" builder in
		(call, map, builder)
	| SAssign(lex, rex) -> let rval, m', builder = expr map builder this rex in
		let addr = match (snd lex) with
		  SId s -> lookups map s
		  | SArrayIndex(id, idx) -> let name = match snd id with 
			    SId s -> s
			    | _ -> "err:cannot index non-id"
			in
		  	let a_addr = lookups map name in
		  	let data_field_loc = L.build_struct_gep a_addr 0 "" builder in
		  	let data_loc = L.build_load data_field_loc "" builder in
		  	let ival, _, builder = expr map builder this idx in
		  	L.build_gep data_loc [| zero; ival |] "" builder 
		  | SArray2DIndex(id, idx, idx2) -> let name = match snd id with 
			    SId s -> s
			    | _ -> "err:cannot index non-id"
			in
		  	let a_addr = lookups map name in
		  	let data_field_loc = L.build_struct_gep a_addr 0 "" builder in
		  	let data_loc = L.build_load data_field_loc "" builder in
		  	let ival, _, builder = expr map builder this idx in
		  	let ival2, _, builder = expr map builder this idx2 in
		  	L.build_gep data_loc [| zero; ival; ival2 |] "" builder
		  | _ -> make_err "Cannot assign to a non-name type. This error should be caught by semantic checker."
		in
		let _ = L.build_store rval addr builder in 
		(rval, m', builder)
	| SDeclAssign(ty, s, rex) -> let l_type = ltype_of_typ ty in
		let addr = L.build_alloca l_type s builder in
		let rval, m', builder = expr map builder this rex in
		let m'' = StringMap.add s addr m' in
		let _ = L.build_store rval addr builder in 
		(rval, m'', builder)
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
		let sto (acc, builder) ex = 
			let value, m', builder = expr map builder this ex in
			let item_loc = L.build_gep data_loc [|zero; const_i32_of acc |] "" builder in
			let _ = L.build_store value item_loc builder in
			(acc + 1, builder)
		in
		let _, builder = List.fold_left sto (0, builder) sl in
		let _ = L.build_store data_loc data_field_loc builder in
		let _ = L.build_store (const_i32_of len) len_loc builder in
		let _ = L.build_store (const_i32_of cap) cap_loc builder in
		(alloc, map, builder)
	| SArrayIndex(id, idx) -> 
		let name = match snd id with 
		      SId s -> s
		    | _ -> "err:cannot index non-id"
		in
		let a_addr = lookups map name in
		let data_field_loc = L.build_struct_gep a_addr 0 "" builder in
		let data_loc = L.build_load data_field_loc "" builder in
		let ival, _, builder = expr map builder this idx in
		let i_addr = L.build_gep data_loc [| zero; ival |] "" builder in 
		let value = L.build_load i_addr "" builder in
		(value, map, builder)
	| SArray2DIndex(id, idx, idx2) -> 
		let name = match snd id with 
		      SId s -> s
		    | _ -> "err:cannot index non-id"
		in
		let a_addr = lookups map name in
		let data_field_loc = L.build_struct_gep a_addr 0 "" builder in
		let data_loc = L.build_load data_field_loc "" builder in
		let ival, _, builder = expr map builder this idx in
		let ival2, _, builder = expr map builder this idx2 in
		let i_addr = L.build_gep data_loc [| zero; ival; ival2 |] "" builder in
		let value = L.build_load i_addr "" builder in
		(value, map, builder)
	| SBinop(lex, op, rex) -> 
		let lval, m', builder = expr map builder this lex in
		let rval, m'', builder = expr m' builder this rex in
		let ty = fst lex in
		match ty with
		  A.Int ->
			(match op with
				A.Add -> L.build_add lval rval "" builder, map, builder
				| A.Sub -> L.build_sub lval rval "" builder, map, builder
				| A.Mult -> L.build_mul lval rval "" builder, map, builder
				| A.Div -> L.build_sdiv  lval rval "" builder, map, builder
				| A.Equal -> L.build_icmp L.Icmp.Eq lval rval "" builder, map, builder
				| A.Neq -> L.build_icmp L.Icmp.Ne lval rval "" builder, map, builder
				| A.Less -> L.build_icmp L.Icmp.Slt lval rval "" builder, map, builder
				| A.Leq -> L.build_icmp L.Icmp.Sle lval rval "" builder, map, builder
				| A.Greater -> L.build_icmp L.Icmp.Sgt lval rval "" builder, map, builder
				| A.Geq -> L.build_icmp L.Icmp.Sge lval rval "" builder, map, builder
				| A.And -> L.build_and lval rval "" builder, map, builder
				| A.Or -> L.build_or lval rval "" builder, map, builder
				| A.Exp -> let lfval = L.build_sitofp lval float_t "" builder in
					let rfval = L.build_sitofp rval float_t "" builder in
					let f_result = L.build_call pow_func [|lfval; rfval|] "" builder in
					let add_half = L.build_fadd f_result (const_float_of 0.5) "" builder in
					(L.build_fptosi add_half i32_t "" builder, map, builder)
				| A.Conv -> make_err "internal error, cannot perform this operation on integers"
			)
		| A.Float ->
			(match op with
				A.Add -> L.build_fadd lval rval "" builder, map, builder
				| A.Sub -> L.build_fsub lval rval "" builder, map, builder
				| A.Mult -> L.build_fmul lval rval "" builder, map, builder
				| A.Div -> L.build_fdiv lval rval "" builder, map, builder
				| A.Equal -> L.build_fcmp L.Fcmp.Oeq lval rval "" builder, map, builder
				| A.Neq -> L.build_fcmp L.Fcmp.One lval rval "" builder, map, builder
				| A.Less -> L.build_fcmp L.Fcmp.Olt lval rval "" builder, map, builder
				| A.Leq -> L.build_fcmp L.Fcmp.Ole lval rval "" builder, map, builder
				| A.Greater -> L.build_fcmp L.Fcmp.Ogt lval rval "" builder, map, builder
				| A.Geq -> L.build_fcmp L.Fcmp.Oge lval rval "" builder, map, builder
				| A.Exp -> L.build_call pow_func [|lval; rval|] "" builder, map, builder
				| _ -> make_err "internal error, cannot perform this operation on floats"
			)
		| A.Bool ->
			(match op with
				  A.Equal ->  L.build_icmp L.Icmp.Eq lval rval "" builder, map, builder
				| A.Neq -> L.build_icmp L.Icmp.Ne lval rval "" builder, map, builder
				| A.And -> L.build_and lval rval "" builder, map, builder
				| A.Or -> L.build_or lval rval "" builder, map, builder
				| _ -> make_err "internal error, cannot perform this operation on booleans"
			)
		| A.Char ->
			( match op with
				  A.Equal -> L.build_icmp L.Icmp.Eq lval rval "" builder, map, builder
				| A.Neq -> L.build_icmp L.Icmp.Ne lval rval "" builder, map, builder
				| A.Add	-> binop_char_concat lval rval "" builder, map, builder
				| _ -> make_err "internal error, cannot perform this operation on characters"
			)
		| A.ArrayList t ->
			( match op with
				  A.Add	-> let arr, b = binop_array_concat t this lval rval "" builder in (arr, map, b)
				| _ -> make_err "internal error, cannot perform this operation on characters"
			)
		| A.String ->
			( match op with
				  A.Equal -> let eq, b = binop_str_equal this lval rval "" builder in (eq, map, b)
				| A.Neq -> let eq, b = binop_str_equal this lval rval "" builder in
					(L.build_not eq "" b, map, b)
				| A.Add	-> let n_str, b =  binop_str_concat this lval rval "" builder in (n_str, map, b)
				| _ -> make_err "internal error, cannot perform this operation on characters"
			)
		| _ -> make_err "unimplemented"
		(* A.Matrix ->
			( match op with
				A.Add -> L.build_fadd
				| A.Sub -> L.build_fsub
				| A.Mult -> L.build_fmul
				| A.Div -> L.build_fdiv 
				| A.Equal -> L.build_fcmp L.Fcmp.Oeq
				| A.Conv ->
			)
		*)
	| _ -> make_err "Unimplemented"
	in
	let rec stmt map builder (this: L.llvalue) (*Llvm func def*) s = match s with
	  SBlock sl -> 
	  	let b, _ = List.fold_left (fun (b, m) s -> stmt m b this s) (builder, map) sl in
	  	(b, map)
	| SExpr e -> 
		let (_, m, builder) = (expr map builder this e) in (builder, m)
	| SDeclare(t, name) -> 
		let l_type = ltype_of_typ t in
		let addr = L.build_alloca l_type name builder in
		let m' = StringMap.add name addr map in
		(builder, m')
	| SIf(pred, then_stmt, else_stmt) -> 
		let bool_val, m', builder = expr map builder this pred in
		let merge_bb = L.append_block context "merge" this in
		let branch_ins = L.build_br merge_bb in
		let then_bb = L.append_block context "then" this in
		let then_builder, m'' = stmt m' (L.builder_at_end context then_bb) this then_stmt in
		let () = add_terminal then_builder branch_ins in
		let else_bb = L.append_block context "else" this in
		let else_builder, m'' = stmt m' (L.builder_at_end context else_bb) this else_stmt in
		let () = add_terminal else_builder branch_ins in
		let _ = L.build_cond_br bool_val then_bb else_bb builder in 
		(L.builder_at_end context merge_bb, m')
	| SWhile(predicate, body) ->
		let pred_bb = L.append_block context "while" this in
		let _ = L.build_br pred_bb builder in
		let body_bb = L.append_block context "while_body" this in
		let body_bldr, m' = stmt map (L.builder_at_end context body_bb) this body in
		let () = add_terminal body_bldr (L.build_br pred_bb) in
		let pred_bldr = L.builder_at_end context pred_bb in
		let bool_val, m'', pred_bldr = expr m' pred_bldr this predicate in
		let merge_bb = L.append_block context "merge" this in
		let _ = L.build_cond_br bool_val body_bb merge_bb pred_bldr in
		(L.builder_at_end context merge_bb, m'')
	| SFor(e1, e2, e3, body) -> stmt map builder this ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
	| _ -> make_err "Unimplemented"
	in
	let build_main sl = 
		let main_ty = L.function_type i32_t [||] in
		let main_func = L.define_function "main" main_ty code_module in
		let builder = L.builder_at_end context (L.entry_block main_func) in
		let builder, _ = stmt StringMap.empty builder main_func (SBlock sl) in
		ignore(L.build_ret (L.const_int i32_t 0) builder)
	in build_main statements; code_module