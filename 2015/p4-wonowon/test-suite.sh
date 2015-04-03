#!/bin/sh

# run wonowon.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc wonowon.hs < $f
done
