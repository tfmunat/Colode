Our sample program is located in the file "print.colode". It contains a program that prints the string "Hello World!" to standard output.

To run the test suite, open a terminal and run test.sh. This script compiles the sample program and executes it, testing the output (using grep) to ensure that it matches "Hello World!".

### Group


### Build Colode compiler
------
	ocamlbuild toplevel.native

### Run tests
------
	./tester.sh

### Can use Makefile
------
        make -> to build
        make clean -> to clean
        make tester -> to run tests

### Current issues
------
	Comments in the same line as code will not be parsed correctly.
>>>>>>> 0995ed187747e37327d2d33e5b968c0a41c14fa5:README.md
