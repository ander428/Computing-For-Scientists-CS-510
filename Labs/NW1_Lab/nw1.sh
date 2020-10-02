#!/bin/bash

# grep -ni "frame"

# grep -A 4120009 NW1.csv >| NW_1.csv

# tail -n+120010 NW1.csv >| NW1_2.csv
split -l 120006 NW1.csv
mv xaa NW1_A.csv
mv xab NW1_B.csv