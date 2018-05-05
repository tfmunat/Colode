target: build matrix.o

build: 
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis toplevel.native -cflag -g -tag debug
debug: matrix.o
	ocamlbuild -no-hygiene -use-ocamlfind -pkgs llvm toplevel.byte -cflag -g -tag debug

.PHONY: clean tester hw

clean:
	rm -rf *.cmo *.cmx *.o *.cmi a.out parser.ml parser.mli scanner.ml _build toplevel.native top

tester:
	for t in ../Tests/*.cld; \
	do \
  	  echo "Running test $$t"; \
  	  ./toplevel.native < $$t; \
  	  echo "-----------------------------------------"; \
	done

hw:
	./helloworld.sh
matrix.o :
	gcc -g -c matrix.c -o matrix.o