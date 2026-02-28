#!/usr/bin/env python3
# Regression test for GitHub issue #1205:
# Accessing .filename or .modtime on a FileInfo object raised:
#   TypeError: No Python class registered for C++ class
#   boost::optional<boost::filesystem::path>
# because no Python converter was registered for boost::optional<path>.

import ledger
import os

journal = ledger.read_journal_from_string("""
2024/01/01 Payee
    Expenses:Food    $10.00
    Assets:Cash
""")

sources = list(journal.sources())
assert len(sources) > 0, "Expected at least one source"

fi = sources[0]

# filename should be None for an in-memory journal (read from string)
assert fi.filename is None, "Expected filename to be None for in-memory journal"

# from_stream should be True for an in-memory journal
assert fi.from_stream is True, "Expected from_stream to be True for in-memory journal"

# modtime should be accessible without TypeError
_ = fi.modtime

print("FileInfo.filename and FileInfo.modtime are accessible")
