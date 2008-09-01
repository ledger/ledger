#!/bin/sh

PYTHONPATH="%builddir%":"%srcdir%":$PYTHONPATH \
LD_LIBRARY_PATH="%builddir%/.libs":$LD_LIBRARY_PATH \
DYLD_LIBRARY_PATH="%builddir%/.libs":$DYLD_LIBRARY_PATH \
    %python% "%srcdir%"/test/UnitTests.py
