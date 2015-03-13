#!/bin/sh

# run sum-free.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc sum-free.hs < $f
done
