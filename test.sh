compiler="./toplevel.native"
cc="gcc"

make clean
make
$compiler -l "print.colode" > "test.ll" &&
if lli test.ll | tee | grep -q "Hello World!"; then
	echo "print.colode test successful!"
else
	echo "print.colode test failed; output did not match 'Hello World!'"
fi
rm -f test.ll 
