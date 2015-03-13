#!/bin/sh

# run base-addition.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc base-addition.hs < $f
done
