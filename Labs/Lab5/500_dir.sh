#!/bin/bash

for i in `seq 1 500`; do
    mkdir "./dir_${i}/";
    echo "line 1" >> ./dir_${i}/test.txt
    echo "line 2" >> ./dir_${i}/test.txt
    echo "line 3" >> ./dir_${i}/test.txt
    echo "line 4" >> ./dir_${i}/test.txt
    echo "line 5" >> ./dir_${i}/test.txt
done

