#!/usr/bin/env python3
# Regression test for GitHub issue #513:
# Reading a journal containing Python snippets from an *external* Python
# interpreter yielded dozens of
#   RuntimeWarning: to-Python converter for <type> already registered
# messages on Python <=3.11, and a fatal PyImport_AppendInittab abort on
# Python >=3.12, because python_interpreter_t::initialize() attempted to
# start a second Python runtime when one was already running.
#
# This test must be run via the external Python interpreter (not the
# `ledger python` command), because the bug only manifests when ledger
# is loaded as a Python extension module with Py_Initialize already done
# by the embedding interpreter.

import warnings

# Elevate every RuntimeWarning to a hard error.  Under the bug, importing
# ledger and parsing a journal with a `python` directive produces a
# cascade of "to-Python converter ... already registered" warnings, which
# this converts into a failing test.
warnings.simplefilter("error", RuntimeWarning)

import lpy

journal = lpy.core.read_journal_from_string("""
python
    print("Hello from python block")

2017-01-01 Payee
    Expenses:Food      $1.00
    Assets:Cash
""")

# Sanity-check that the bindings still work end-to-end after the
# journal-embedded python block has run.
for xact in journal.xacts:
    for post in xact.posts:
        print("%s %s" % (post.amount, post.account))

print("OK")
