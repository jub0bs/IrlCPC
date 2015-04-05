#!/bin/sh

# run equilibrium.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc equilibrium.hs < $f
done
