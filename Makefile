target: build

build:
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis toplevel.native

.PHONY: clean tester

clean:
	rm -rf *.cmo *.cmx *.o *.cmi a.out parser.ml parser.mli scanner.ml _build toplevel.native top

tester:
	for t in Tests/*.cld; \
	do \
  	  echo "Running test $$t"; \
  	  ./toplevel.native < $$t; \
  	  echo "-----------------------------------------"; \
	done
