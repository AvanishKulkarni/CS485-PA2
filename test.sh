#!/bin/bash
if [ main.ml -nt main ]; then
    ocamlc main.ml -o main 
fi
goodCount=0
badCount=0
goodTotal=0
badTotal=0
function run_tests() {
    rm -f *.cl-ast
    rm -f *.cl-type
    rm -f good/*.cl-ast
    rm -f good/*.cl-type
    rm -f bad/*.cl-ast
    rm -f bad/*.cl-type
    rm -f reference_error.txt
    rm -f test_error.txt
    cool --parse "$1"
    ./main "$1-ast" > test_error.txt
    cool --type "$1" --out temp_ref > reference_error.txt

    if [ -f "temp_ref.cl-type" ]; then
        diff -b -B -w temp_ref.cl-type "$1-type" > /dev/null
    else
        diff -b -B -w reference_error.txt test_error.txt > /dev/null
    fi
    if [ $? -eq 0 ]; then
        echo "Passed $1"
        return 0
    else 
        echo "Failed $1"
        return 1
    fi
}

rm -f good/*.cl-ast
rm -f good/*.cl-type
rm -f bad/*.cl-ast
rm -f bad/*.cl-type

if [ -n "$1" ]; then 
    run_tests $1
    cool --parse "$1"
    echo "Our Output"
    ./main "$1-ast"
    echo "Referenced Compiler"
    cool "$1"
else
    
    for file in good/*; do
        if run_tests $file; then
            goodCount=$((goodCount + 1))
        fi
        goodTotal=$((goodTotal + 1))
        echo ""
    done
    
    for file in bad/*; do
        if run_tests $file; then
            badCount=$((badCount + 1))
        fi
        badTotal=$((badTotal + 1))
        echo ""
    done
    echo "Passed $goodCount/$goodTotal good test cases"
    echo "Passed $badCount/$badTotal bad test cases"
    count=$((goodCount + badCount))
    total=$((goodTotal + badTotal))
    echo "Passed $count/$total test cases"
fi
