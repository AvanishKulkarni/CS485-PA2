#!/bin/bash

if [ -z "$1" ]; then 
    echo "$0 <filename>"
    exit 1
fi

if [ main.ml -nt main ]; then
    ocamlc main.ml -o main 
fi
rm -f *.cl-ast
rm -f *.cl-type
rm -f reference_error.txt
rm -f test_error.txt

cool --parse "$1"
./main "$1-ast" > test_error.txt

cool --class-map "$1" --out temp_ref > reference_error.txt

if [ -f "temp_ref.cl-type" ]; then
    diff -b -B -w temp_ref.cl-type "$1-type"
else
    diff -b -B -w reference_error.txt test_error.txt
fi