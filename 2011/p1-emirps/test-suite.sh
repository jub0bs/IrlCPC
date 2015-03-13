#!/bin/sh

# run emirps.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc emirps.hs < $f
done
