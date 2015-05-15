#!/bin/sh

# Run test suite
#
# * Run program (first arg) on source (second arg.), taking as inputs the
#   files of the form "test*.txt" in the current directory;
# * Write the output to "test*.txt.out";
# * Compare files "test*.txt.out" and "test*.txt.sol", and exit with a
#   nonzero status if any difference is found.
#
# USAGE:
#
#       sh test-suite.sh runghc boolean.hs
#
# or
#
#       sh test-suite.sh python boolean.py
#
# Tip: for convenience, define the following alias:
#
#       alias testsuite="sh ../../run_test_suite.sh"

cmd=$1
src=$2

set -- test[*].txt test*.txt
case $1$2 in
    ('test[*].txttest*.txt')
      printf "No test files found\n"
      exit 1
    (*) ;;
esac

for f in test*.txt; do
  $cmd $src < "$f" > "$f.out"
  if ! diff "$f.out" "$f.sol"; then
    printf "Test failed on file %s\n" "$f"
    exit 1
  fi
done

printf "Test suite passed\n"
exit 0
