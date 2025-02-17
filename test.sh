#!/bin/bash
if [ main.ml -nt main ]; then
    ocamlc main.ml -o main 
fi
count=0
total=0
function run_tests() {
    rm -f *.cl-ast
    rm -f *.cl-type
    rm -f test_cases/*.cl-ast
    rm -f test_cases/*.cl-type
    rm -f reference_error.txt
    rm -f test_error.txt
    cool --parse "$1"
    ./main "$1-ast" > test_error.txt
    cool --class-map "$1" --out temp_ref > reference_error.txt

    if [ -f "temp_ref.cl-type" ]; then
        diff -b -B -w temp_ref.cl-type "$1-type" > /dev/null
    else
        diff -b -B -w reference_error.txt test_error.txt > /dev/null
    fi
    if [ $? -eq 0 ]; then
        echo "Passed $1"
        count=$((count + 1))
    else 
        echo "Failed $1"
    fi
    total=$((total +1))
}

if [ -n "$1" ]; then 
    run_tests $1
else
    
    for file in test_cases/*; do
        run_tests $file
        echo ""
    done
    echo ""
    echo "Passed $count/$total test cases"
    rm -f test_cases/*.cl-ast
    rm -f test_cases/*.cl-type
fi