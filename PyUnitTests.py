#!/bin/sh

PYTHONPATH="%builddir%":"%srcdir%":$PYTHONPATH \
    python "%srcdir%"/tests/python/UnitTests.py
