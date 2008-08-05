#!/bin/sh

PYTHONPATH="%builddir%":"%srcdir%":$PYTHONPATH \
DYLD_LIBRARY_PATH="%builddir%/.libs":"%builddir%/gdtoa/.libs":$DYLD_LIBRARY_PATH \
    python "%srcdir%"/tests/python/UnitTests.py
