#!/bin/sh

# run assembly-line.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc assembly-line.hs < $f
done
