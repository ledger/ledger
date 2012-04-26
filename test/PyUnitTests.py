#!/bin/bash

set -e

PYTHONPATH="%builddir%/.libs":$PYTHONPATH \
LD_LIBRARY_PATH="%builddir%/.libs":$LD_LIBRARY_PATH \
DYLD_LIBRARY_PATH="%builddir%/.libs":$DYLD_LIBRARY_PATH \
    %python% "%builddir%"/test/python/ConvertedTests.py

PYTHONPATH="%builddir%/.libs":$PYTHONPATH \
LD_LIBRARY_PATH="%builddir%/.libs":$LD_LIBRARY_PATH \
DYLD_LIBRARY_PATH="%builddir%/.libs":$DYLD_LIBRARY_PATH \
    %python% "%srcdir%"/test/python/UnitTests.py
