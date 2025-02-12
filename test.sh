#!/bin/bash

if [ -z "$1" ]; then 
    echo "$0 <filename>"
    exit 1
fi

if [ main.ml -nt main ]; then
    ocamlc main.ml -o main 
fi

cool --parse "$1"
./main "$1-ast"

cool --class-map "$1" --out temp_ref

diff -b -B -w temp_ref.cl-type "$1-type"
