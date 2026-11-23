#!/usr/bin/env python3
"""Check that a successful cumulative balance cannot contaminate the next report (#3337)."""

import argparse
import os
from pathlib import Path
import shlex
import subprocess
import tempfile
import unittest


JOURNAL = """2026-01-01 First
  Assets:Cash  $10
  Income

2026-01-02 Second
  Assets:Cash  $20
  Income
"""
FORMATS = {
    "reg": "%(payee)|%(account)|%(amount)|%(total)\\n",
    "bal": "%(account)|%(total)\\n",
}
ROWS = {
    "reg": "First|Assets:Cash|$10|$10\nSecond|Assets:Cash|$20|$30\n",
    "bal": "Assets:Cash|$30\nIncome|$-30\n",
}


class ReportRecovery(unittest.TestCase):
    def setUp(self):
        self.folder = tempfile.TemporaryDirectory(prefix="ledger-3337-")
        self.addCleanup(self.folder.cleanup)
        self.root = Path(self.folder.name)
        self.journal = self.root / "journal.dat"
        self.journal.write_text(JOURNAL, encoding="utf-8")
        self.outputs = []
        # Match LedgerHarness's environment isolation, including sanitiser settings.
        allowed = {
            "PATH", "HOME", "TZ", "TZDIR", "LANG", "LC_ALL", "LC_CTYPE", "TERM",
            "TMPDIR", "TEMP", "TMP", "SYSTEMROOT", "COMSPEC", "MSYSTEM", "MINGW_PREFIX",
            "ASAN_OPTIONS", "LSAN_OPTIONS", "UBSAN_OPTIONS", "MSAN_OPTIONS", "TSAN_OPTIONS",
        }
        self.env = {key: os.environ[key] for key in os.environ
                    if key in allowed or key.startswith(("LLVM_PROFILE_", "GCOV_"))}
        self.env.update(TZ="America/Chicago", LC_ALL="C")

    def command(self, kind, format=None, options=(), output=True):
        args = [kind, "--no-titles", "--format", format or FORMATS[kind], *options]
        if kind == "bal":
            args.extend(["--flat", "--no-total"])
        if output:
            path = self.root / f"report-{len(self.outputs)}.txt"
            self.outputs.append(path)
            args.extend(["--output", path.as_posix()])
        if kind == "reg":
            args.append("Assets")
        return shlex.join(args)

    def invoke(self, args=(), commands=None):
        return subprocess.run(
            [LEDGER, "--args-only", "--columns=80", "-f", str(self.journal), *args],
            input=commands, capture_output=True, text=True, encoding="utf-8",
            env=self.env, timeout=10,
        )

    def repl(self, commands, failures=()):
        diagnostics = []
        for command in failures:
            batch = self.invoke(shlex.split(command))
            self.assertEqual(batch.returncode, 1, batch.stderr)
            diagnostics.append(batch.stderr)
        result = self.invoke(commands="\n".join([*commands, "quit", ""]))
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stderr, "".join(diagnostics))
        self.assertEqual(result.stderr.count("Error:"), len(failures), result.stderr)
        self.assertEqual(result.stderr.count("Error: Divide by zero"), len(failures), result.stderr)
        self.assertEqual(self.journal.read_text(encoding="utf-8"), JOURNAL)
        return result

    def rows(self, index=-1):
        return self.outputs[index].read_text(encoding="utf-8")

    def fresh(self, kind):
        self.repl([self.command(kind)])
        self.assertEqual(self.rows(), ROWS[kind])
        return self.rows()

    def test_successful_cumulative_balance_then_register(self):
        # A successful `bal --group-by payee --group-by-cumulative` must not
        # leave per-posting report state behind for the next `reg` (#3337).
        fresh = self.fresh("reg")
        self.repl([
            self.command("bal", options=("--group-by", "payee", "--group-by-cumulative")),
            self.command("reg"),
        ])
        self.assertEqual(self.rows(), fresh)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ledger", required=True)
    LEDGER = parser.parse_args().ledger
    unittest.main(argv=[__file__], verbosity=2)
