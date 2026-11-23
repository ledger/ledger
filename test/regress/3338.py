#!/usr/bin/env python3
"""Check that a --file option line inside a journal cannot crash the run (#3338).

A journal that names another file with --file clears the live file list
while read_data() is still iterating over it, which segfaults.  Reading
from a snapshot of the list keeps the run alive: the journals named on
the command line are all read, and the file named inside the journal is
left for a later read, as in the single-file case.
"""

import argparse
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


A = """--file other.ledger

2024/01/01 A
    Assets:A    1 FOO
    Equity
"""
B = """2024/01/02 B
    Assets:B    2 BAR
    Equity
"""
OTHER = """2024/01/03 O
    Assets:O    3 BAZ
    Equity
"""
SELF = """--file self.ledger
"""


class FileListSnapshot(unittest.TestCase):
    def setUp(self):
        self.folder = tempfile.TemporaryDirectory(prefix="ledger-3338-")
        self.addCleanup(self.folder.cleanup)
        self.root = Path(self.folder.name)
        (self.root / "a.ledger").write_text(A, encoding="utf-8")
        (self.root / "b.ledger").write_text(B, encoding="utf-8")
        (self.root / "other.ledger").write_text(OTHER, encoding="utf-8")
        (self.root / "self.ledger").write_text(SELF, encoding="utf-8")
        # Match LedgerHarness's environment isolation, including sanitiser settings.
        allowed = {
            "PATH", "HOME", "TZ", "TZDIR", "LANG", "LC_ALL", "LC_CTYPE", "TERM",
            "TMPDIR", "TEMP", "TMP", "SYSTEMROOT", "COMSPEC", "MSYSTEM", "MINGW_PREFIX",
            "ASAN_OPTIONS", "LSAN_OPTIONS", "UBSAN_OPTIONS", "MSAN_OPTIONS", "TSAN_OPTIONS",
        }
        self.env = {key: os.environ[key] for key in os.environ
                    if key in allowed or key.startswith(("LLVM_PROFILE_", "GCOV_"))}
        self.env.update(TZ="America/Chicago", LC_ALL="C")

    def invoke(self, files, args=()):
        return subprocess.run(
            [LEDGER, "--args-only",
             *[arg for name in files for arg in ("-f", str(self.root / name))],
             *args],
            capture_output=True, text=True, encoding="utf-8",
            env=self.env, timeout=10,
        )

    def test_file_option_in_journal(self):
        result = self.invoke(("a.ledger", "b.ledger"),
                             ("bal", "--format", "%(account)|%(total)\\n"))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, "")
        self.assertIn("Assets:A", result.stdout)
        self.assertIn("Assets:B", result.stdout)
        self.assertNotIn("Assets:O", result.stdout)

    def test_self_referential_file(self):
        result = self.invoke(("self.ledger",), ("bal",))
        self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ledger", required=True)
    LEDGER = parser.parse_args().ledger
    unittest.main(argv=[__file__], verbosity=2)
