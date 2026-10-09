#!/usr/bin/env python3
"""Check that a failed REPL report cannot contaminate the next report (#3273)."""

import argparse
import os
from pathlib import Path
import shlex
import subprocess
import tempfile
import unittest


JOURNAL = """2026-01-01 First
  ; Bucket: First
  Assets:Cash  $10
  Income

2026-01-02 Second
  ; Bucket: Second
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
        self.folder = tempfile.TemporaryDirectory(prefix="ledger-3273-")
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

    def test_immediate_successor_matrix(self):
        for failed_kind in FORMATS:
            for next_kind in FORMATS:
                with self.subTest(failed=failed_kind, next=next_kind):
                    fresh = self.fresh(next_kind)
                    failing = self.command(failed_kind, "%(1/0)", output=False)
                    self.repl([
                        failing,
                        self.command(next_kind),
                    ], failures=(failing,))
                    self.assertEqual(self.rows(), fresh)

    def test_partial_output(self):
        failures = {
            "reg": ('PARTIAL|%(payee)|%(account)|%(amount)|'
                    '%(payee == "Second" ? 1/0 : total)\\n',
                    "PARTIAL|First|Assets:Cash|$10|$10\n"),
            "bal": ('PARTIAL|%(account)|%(account == "Income" ? 1/0 : total)\\n',
                    "PARTIAL|Assets:Cash|$30\n"),
        }
        for failed_kind, (format, partial) in failures.items():
            for next_kind in FORMATS:
                with self.subTest(failed=failed_kind, next=next_kind):
                    fresh = self.fresh(next_kind)
                    failing = self.command(failed_kind, format, output=False)
                    result = self.repl([
                        failing,
                        self.command(next_kind),
                    ], failures=(failing,))
                    self.assertEqual(result.stdout.count(partial), 1, result.stdout)
                    self.assertEqual(self.rows(), fresh)

    def test_group_failure(self):
        for kind in FORMATS:
            for threshold in (5, 15):
                for next_kind in FORMATS:
                    with self.subTest(report=kind, threshold=threshold, next=next_kind):
                        fresh = self.fresh(next_kind)
                        value = "amount" if kind == "reg" else "total"
                        format = "PARTIAL|" + FORMATS[kind].replace(
                            "%(total)", f"%(abs({value}) > ${threshold} ? 1/0 : total)")
                        failing = self.command(kind, format, ("--group-by", "payee"), output=False)
                        result = self.repl([
                            failing,
                            self.command(next_kind),
                        ], failures=(failing,))
                        partial = ("PARTIAL|First|Assets:Cash|$10|$10\n" if kind == "reg"
                                   else "PARTIAL|Assets:Cash|$10\nPARTIAL|Income|$-10\n")
                        self.assertEqual(result.stdout.count(partial), int(threshold == 15))
                        self.assertEqual(self.rows(), fresh)

    def test_cumulative_and_pivot_failure(self):
        cases = [
            (("--group-by", "payee", "--group-by-cumulative"),
             "PARTIAL|%(account)|%(abs(total) > $15 ? 1/0 : total)\\n",
             "PARTIAL|Assets:Cash|$10\nPARTIAL|Income|$-10\n"),
            (("--pivot", "Bucket"),
             "PARTIAL|%(account)|%(account =~ /Second/ ? 1/0 : total)\\n",
             "PARTIAL|Bucket:First:Assets:Cash|$10\nPARTIAL|Bucket:First:Income|$-10\n"),
        ]
        for options, format, partial in cases:
            for next_kind in FORMATS:
                with self.subTest(options=options, next=next_kind):
                    fresh = self.fresh(next_kind)
                    failing = self.command("bal", format, options, output=False)
                    result = self.repl([
                        failing,
                        self.command(next_kind),
                    ], failures=(failing,))
                    self.assertEqual(result.stdout.count(partial), 1, result.stdout)
                    self.assertEqual(self.rows(), fresh)

    def test_successful_groups(self):
        cases = [
            ("reg", ("--group-by", "payee"),
             "First|Assets:Cash|$10|$10\nSecond|Assets:Cash|$20|$20\n"),
            ("bal", ("--group-by", "payee"),
             "Assets:Cash|$10\nIncome|$-10\nAssets:Cash|$20\nIncome|$-20\n"),
            ("bal", ("--group-by", "payee", "--group-by-cumulative"),
             "Assets:Cash|$10\nIncome|$-10\nAssets:Cash|$30\nIncome|$-30\n"),
            ("bal", ("--pivot", "Bucket"),
             "Bucket:First:Assets:Cash|$10\nBucket:First:Income|$-10\n"
             "Bucket:Second:Assets:Cash|$20\nBucket:Second:Income|$-20\n"),
        ]
        for kind, options, expected in cases:
            with self.subTest(report=kind, options=options):
                self.repl([self.command(kind, options=options)])
                self.assertEqual(self.rows(), expected)

    def test_repeated_reports_and_failures(self):
        commands = []
        expected = []
        failures = []
        for _ in range(2):
            for failed_kind, next_kind in (("reg", "bal"), ("bal", "reg")):
                failing = self.command(failed_kind, "%(1/0)", output=False)
                commands.extend([
                    self.command(next_kind),
                    failing,
                    self.command(next_kind),
                ])
                failures.append(failing)
                expected.extend([ROWS[next_kind], ROWS[next_kind]])
        self.repl(commands, failures=failures)
        self.assertEqual([path.read_text(encoding="utf-8") for path in self.outputs], expected)

    def test_early_errors_and_empty_journal(self):
        for empty in (False, True):
            with self.subTest(empty=empty):
                if empty:
                    self.journal.write_text("", encoding="utf-8")
                commands = [self.command("reg"), "unknown-3273", "unknown-3273",
                            self.command("bal"), self.command("reg")]
                result = self.invoke(commands="\n".join([*commands, "quit", ""]))
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stderr.count("Error:"), 2, result.stderr)
                self.assertEqual(result.stderr.count("Unrecognized command 'unknown-3273'"), 2)
                self.assertEqual(self.rows(-2), "" if empty else ROWS["bal"])
                self.assertEqual(self.rows(), "" if empty else ROWS["reg"])
                self.assertEqual(self.journal.read_text(encoding="utf-8"), "" if empty else JOURNAL)

    def test_script_and_batch_exit_status(self):
        first = self.command("reg")
        failing = self.command("reg", "%(1/0)", output=False)
        sentinel = self.command("bal")
        script = self.root / "commands.txt"
        script.write_text("\n".join([first, failing, sentinel, ""]), encoding="utf-8")
        result = self.invoke(["--script", str(script)])
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertEqual(result.stderr.count("Error: Divide by zero"), 1, result.stderr)
        self.assertEqual(self.rows(0), ROWS["reg"])
        self.assertFalse(self.outputs[1].exists(), "Script continued after the failed report")
        batch = self.invoke(["reg", "--format", "%(1/0)", "Assets"])
        self.assertEqual(batch.returncode, 1, batch.stderr)
        self.assertEqual(batch.stderr.count("Error: Divide by zero"), 1, batch.stderr)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ledger", required=True)
    LEDGER = parser.parse_args().ledger
    unittest.main(argv=[__file__], verbosity=2)
