#!/bin/bash
if [ main.ml -nt main ]; then
    ocamlopt main.ml -o main 
fi
goodCount=0
badCount=0
goodTotal=0
badTotal=0
expCount=0
expTotal=0
function run_tests() {
    rm -f *.cl-ast
    rm -f *.cl-type
    rm -f good/*.cl-ast
    rm -f good/*.cl-type
    rm -f bad/*.cl-ast
    rm -f bad/*.cl-type
    rm -f expression_cases/*.cl-ast
    rm -f expression_cases/*.cl-type
    rm -f reference_error.txt
    rm -f test_error.txt
    cool --parse "$1"
    ./main "$1-ast" > test_error.txt
    cool "$1" --type --out temp_ref > reference_error.txt

    if [ -f "temp_ref.cl-type" ]; then
        diff -b -B -w temp_ref.cl-type "$1-type" > /dev/null
    else
        diff -b -B -w reference_error.txt test_error.txt > /dev/null
    fi
    if [ $? -eq 0 ]; then
        echo -e "\e[32;1mPassed\e[0m $1"
        return 0
    else 
        echo -e "\e[31;1mFailed\e[0m $1"
        return 1
    fi
}

rm -f good/*.cl-ast
rm -f good/*.cl-type
rm -f bad/*.cl-ast
rm -f bad/*.cl-type
rm -f expression_cases/*.cl-ast
rm -f expression_cases/*.cl-type

if [ -n "$1" ]; then 
    run_tests $1
    cool --parse "$1"
    echo "Our Output"
    ./main "$1-ast"
    echo "Referenced Compiler"
    cool --type "$1"
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
    for file in expression_cases/*; do
        if run_tests $file; then
            expCount=$((expCount + 1))
        fi
        expTotal=$((expTotal + 1))
        echo ""
    done
    echo "Passed $goodCount/$goodTotal good test cases"
    echo "Passed $badCount/$badTotal bad test cases"
    echo "Passed $expCount/$expTotal exp test cases"
    count=$((goodCount + badCount + expCount))
    total=$((goodTotal + badTotal + expTotal))
    echo "Passed $count/$total test cases"
fi