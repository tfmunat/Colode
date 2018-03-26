
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
	let pixel_t = L.vector_type float_t 4
	in
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
	let lookup map name : L.llvalue = match StringMap.find_opt name map	with
	  Some v -> v | None -> make_err ("Couldn't find " ^ name)
    in
    let rec expr map builder (typ, sx) = match sx with
	  SLiteral i -> (L.const_int i32_t i, map)
	| SBoolLit b -> (L.const_int i1_t (if b then 1 else 0), map)
	| SFliteral l -> (L.const_float_of_string float_t l, map)
	| SCharLiteral c -> (L.const_int i8_t (Char.code c), map)
	| SStringLiteral s -> (L.const_string context s, map)
	| SNoexpr -> (L.const_int i32_t 0, map)
	| SId s -> (L.build_load (lookup map s) s builder, map)
	| SCall ("print", [ex]) -> (L.build_call print_func [|fst (expr map builder ex)|] "puts" builder, map)
	| _ -> make_err "Unimplemented"
	in
	let add_terminal builder fn = match L.block_terminator (L.insertion_block builder) with
	  Some _ -> ()
	| None -> ignore (fn builder) in
	let rec stmt map builder s = match s with
	  SBlock sl -> List.fold_left (fun (b, m) s -> stmt m b s) (builder, map) sl
	| SExpr e -> let (_, m) = (expr map builder e) in (builder, m)
	| _ -> make_err "Unimplemented"
	in
	let build_main sl = 
		let main_ty = L.function_type i32_t [||] in
		let main_func = L.define_function "main" main_ty code_module in
		let builder = L.builder_at_end context (L.entry_block main_func) in
		ignore(stmt StringMap.empty builder (SBlock sl))
	in build_main statements; code_module