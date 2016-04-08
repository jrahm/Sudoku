#!/bin/bash

red=$(printf "\e[01;31m")
green=$(printf "\e[01;32m")
reset=$(printf "\e[00m")

for i in $(ls test/*.txt) ; do
    bn=$(basename $i)


    if [ -e test/soln/$bn ] ; then
        calculated=$(dist/build/sudoku/sudoku $i)
        expected=$(cat test/soln/$bn)

    echo "running $bn"
        if [ "$calculated" != "$expected" ] ; then
            ( echo "Expected does not equal calculated!"
              echo "Expected: "
              echo "$expected"
              echo ""
              echo "Calculated: "
              echo "$calculated"
              echo ""
              echo -e "$bn: ${red}FAIL${reset}" ) >&2

        else

            echo -e "$bn: ${green}PASS${reset}"
        fi
    fi

done
