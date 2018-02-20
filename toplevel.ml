(* Top-level of the compiler: scan & parse the input, check the resulting AST, generate LLVM IR, and dump the mudule *)

type action = Ast | LLVM_IR | Compile

let _ =
        let action = if Array.length Sys.argv > 1 then
                List.assoc Sys.argv.(1) [ ("-a", Ast);
                        ("-l", LLVM_IR);
                        ("-c", Compile)]
        else Compile in
        let lexbuf = Lexing.from_channel stdin in
        let ast = Parser.program Scanner.token lexbuf in
        (match action with
                Ast -> print_string (Ast.string_of_program ast)
                | LLVM_IR -> print_string ("LLVM_IR not implemented yet")
                | Compile -> print_string ("Compile not implemented yet"))
