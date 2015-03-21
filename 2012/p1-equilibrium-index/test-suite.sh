#!/bin/sh

# run equilibrium-index on all files of the form test*.txt
for f in test*.txt; do
    runghc equilibrium-index.hs < $f
done
