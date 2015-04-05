#!/bin/sh

# run superprimes.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc superprimes.hs < $f
done
