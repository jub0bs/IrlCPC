#!/bin/sh

# run encryption.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc encryption.hs < $f
done
