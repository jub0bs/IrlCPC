#!/bin/sh

# run emirps.hs on all files of the form emirps-test*.txt
for f in emirps-test*.txt; do
    runghc emirps.hs < $f
done
