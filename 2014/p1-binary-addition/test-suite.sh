#!/bin/sh

# run binary-addition.py on all files of the form test*.txt
for f in test*.txt; do
    python binary-addition.py < $f
done
