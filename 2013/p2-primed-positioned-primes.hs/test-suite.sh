#!/bin/sh

# run primes-positioned-primes.hs on all files of the form test*.txt
for f in test*.txt; do
    runghc primed-positioned-primes.hs < $f
done
