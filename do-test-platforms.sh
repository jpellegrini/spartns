#!/bin/bash
#
# This script runs tests on all supported platforms
#


## Associative array (Bash 4 feature):
declare -A implementations=(
	["SBCL"]="sbcl --script"
	["Clisp"]="clisp"
	["ECL"]="ecl -load"
	["GCL"]="gcl -load"
	["ABCL"]="java -jar /home/jeronimo/pkg/lisp/abcl/abcl.jar --batch --load"
	["XCL"]="/home/jeronimo/pkg/lisp/xcl/xcl --load"
	)

echo "- - - - -"
for name in "${!implementations[@]}";  do
	echo "$name: "
	${implementations["$name"]} do-tests.lisp;
	echo "- - - - -"
done

