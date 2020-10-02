#!/bin/bash

for i in `seq 1 500`; do
    cd dir_${i}
    if [ ${i: -1} == 0 ]; then
        awk '{ if (NR == 4) print "cats are better than dogs?"; else print $0}' test.txt > test2.txt
    elif [ ${i: -1} == 1 ]; then
        awk '{ if (NR == 4) print "eat beets"; else print $0}' test.txt > test2.txt
    elif [ ${i: -1} == 4 ]; then
        awk '{ if (NR == 4) print "squash are great"; else print $0}' test.txt > test2.txt
    elif [ ${i: -1} == 5 ]; then
        awk '{ if (NR == 4) print "dogs are better than cats"; else print $0}' test.txt > test2.txt
    elif [ ${i: -1} == 7 ]; then
        awk '{ if (NR == 4) print "hello world"; else print $0}' test.txt > test2.txt
    else
        awk '{ if (NR == 4) print ""; else print $0}' test.txt > test2.txt
    fi
    mv test2.txt test.txt
    cd ..
done
