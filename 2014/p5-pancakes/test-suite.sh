#!/bin/sh

# run pancakes.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc pancakes.hs < $f
done
