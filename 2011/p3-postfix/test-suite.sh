#!/bin/sh

# run postfix.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc postfix.hs < $f
done
