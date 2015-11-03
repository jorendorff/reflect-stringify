#!/bin/bash

# test.sh - Run tests.
# To run this:
#     SRCDIR=path/to/gecko/srcdir

set -eu

# The directory containing this script should be the reflect-stringify source
# directory. Convert that directory to an absolute path.
DIR=`(cd $(dirname $0) && echo $PWD)`

# Convert $SRCDIR to an absolute path.
SRCDIR=`(cd ${SRCDIR} && echo $PWD)`

(cd "${SRCDIR}/js/src/tests";
 python jstests.py $@ --test-reflect-stringify="${DIR}/reflect-stringify.js" "${JS}")

(cd "${SRCDIR}/js/src/jit-test";
 python jit_test.py $@ --test-reflect-stringify="${DIR}/reflect-stringify.js" "${JS}")
