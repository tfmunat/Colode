**Building instructions**

	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc ast.ml parser.mli parser.ml scanner.ml
	ocamlc -o top ast.cmo parser.cmo scanner.cmo toplevel.ml

**Running** eventually these should be replaced by instructions on running test runner

	cat example.colode | ./top -a