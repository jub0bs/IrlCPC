#!/bin/sh

# run boolean.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc boolean.hs < $f
done
