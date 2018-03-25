module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

let translate (statements, functions) =
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
	let list_t = fun (inner_typ: lltype) -> L.struct_type context [| L.qualified_pointer_type L.address_space inner_typ; i32_t (*length*); i32_t (*capacity*)|] in
	let string_t = L.struct_type context [| L.qualified_pointer_type L.address_space char_t; i32_t (*length*); |] in 
	let matrix_t (width:int) (height:int) = L.array_type (L.array_type width) height in 
	let image_t (width:int) (height:int) = L.struct_type context [| i32_t (*width*); i32_t (* height *) L.qualified_pointer_type L.address_space (matrix_t width height); L.qualified_pointer_type L.address_space (matrix_t width height); L.qualified_pointer_type L.address_space (matrix_t width height); |] in
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
	let print_t = L.function_type (L.integer_type context) [| L.qualified_pointer_type L.address_space char_t |] in
	let print_func = L.declare_function "print" print_t code_module in
	let function_decls =
		let func_decl map fd =
			let name = fd.sfname in
			let formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fd.sformals)
			let func_type = L.function_type (ltype_of_typ fd.styp) formal_types in
			StringMap.add name (L.define_function name func_type code_module, fd) map 
		in
		List.fold_left func_decl StringMap.empty functions
	in
	