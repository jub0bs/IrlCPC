#!/bin/sh

# run look-and-say.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc perfect-pancakes-preparation.hs < $f
done
