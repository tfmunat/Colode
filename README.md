### Group
------
*Steven Bonilla*					sb3914@columbia.edu<br>
*Takuma Yasuda*						ty2353@columbia.edu<br>
*Willie Koomson*					wvk2101@columbia.edu<br>
*Tahmid Munat*						tfm2109@columbia.edu<br>
*Dimitri Borgers*					djb2195@columbia.edu

### Build Colode compiler
------
* Assuming you have the relevant ocamlbuild and ocamlfind packages <br>
	`make`

### Run tests
------
	./tester.sh


### Compile Hello World
------
	make hw

This test validates that the output of the sample program in `print.colode` is "Hello World!"
### Automation and validation
	./helloworld.sh

### Commands
------
    make -> to build
    make clean -> to clean
    make tester -> to run tests
    make hw -> to build helloworld and compare it to the test suite

### Current issues
------
Comments in the same line as code will not be parsed correctly.
