#!/bin/sh

# run look-and-say.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc look-and-say.hs < $f
done
