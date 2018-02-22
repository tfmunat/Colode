target: build

build:
	ocamlbuild toplevel.native

.PHONY: clean tester

clean:
	rm -rf *.cmo *.cmi a.out parser.ml parser.mli scanner.ml _build toplevel.native top

tester:
	bash tester.sh
