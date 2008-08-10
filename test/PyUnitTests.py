#!/bin/sh

PYTHONPATH="%builddir%":"%srcdir%":$PYTHONPATH \
DYLD_LIBRARY_PATH="%builddir%/.libs":$DYLD_LIBRARY_PATH \
    %python% "%srcdir%"/test/UnitTests.py
