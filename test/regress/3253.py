#!/usr/bin/env python3
# Regression test for GitHub issue #3253:
# Interactive sessions must give the results the command line gives under
# --effective (or --aux-date).
#
# --effective changes how the journal is parsed as well as how it is
# reported.  The command line applies it before reading the journal, so
# balance assertions are checked in effective-date order, and the prices
# and lots that costs create are dated by effective dates.  An interactive
# session reads the journal once, at startup, and used to do so without
# applying --effective even when it was given at startup.
#
# Each case feeds commands to one interactive session and requires its
# output, less the banner, to equal what the same commands print when run
# one at a time on the command line.  The .test harness cannot drive an
# interactive session, whose banner differs from build to build, so this
# script does.  test/regress/3253.test covers --script sessions.
#
# Usage: 3253.py LEDGER

import difflib
import os
import re
import shlex
import subprocess
import sys
import tempfile

AGGY = """\
2021/12/31 Opening
    Assets:Broker                         1 AGGY
    Equity:Opening Balances

2021/12/31=2022/01/04 Later
    Assets:Broker                         1 AGGY @@ $2.00
    Assets:Cash
"""

# The journal from #2071: the assertion on the last posting holds only in
# effective-date order, so without --effective the journal does not load.
ASSERTIONS = """\
2021-01-01 first
  acc1  10 r = 10 r
  income:opening

2021-02-01 third
  expenses    ;[=2021-06-01]
  acc1  -10 r ;[=2021-06-01]

2021-04-01 second
  expenses  5 r   ;[=2021-03-01]
  acc1  -5 r = 5 r ;[=2021-03-01]
"""


class Ledger:
    def __init__(self, path):
        self.path = path
        self.env = {k: v for k, v in os.environ.items() if not k.startswith("LEDGER")}
        self.env["TZ"] = "America/Chicago"
        self.banner = self.run(["--version"]).stdout

    def run(self, args, stdin=""):
        return subprocess.run(
            [self.path, "--args-only", "--columns=80", *args],
            input=stdin, capture_output=True, text=True, env=self.env)

    def session(self, startup, commands):
        """Run COMMANDS in one interactive session: (stdout, stderr)."""
        p = self.run(startup, "".join(c + "\n" for c in commands))
        out = p.stdout
        if not out.startswith(self.banner):
            return "(no banner) " + out, p.stderr
        out = out[len(self.banner):]
        # A build without editline or readline prints the prompt ("] ")
        # itself, before reading each command.
        return re.sub(r"^(?:\]+ )+", "", out, flags=re.M), p.stderr

    def command_line(self, startup, commands):
        """Run each of COMMANDS on the command line: (stdout, stderr)."""
        out = err = ""
        for command in commands:
            p = self.run(startup + shlex.split(command))
            out += p.stdout
            err += p.stderr
        return out, err


def check(ledger, name, startup, commands, anchors):
    """The session must print what the command line prints, and the command
    line must print each of ANCHORS, so that both cannot be wrong alike."""
    got = ledger.session(startup, commands)
    want = ledger.command_line(startup, commands)
    ok = True
    for label, g, w in (("stdout", got[0], want[0]), ("stderr", got[1], want[1])):
        if g != w:
            print("FAIL: %s: session %s differs from the command line:" % (name, label))
            sys.stdout.writelines(difflib.unified_diff(
                w.splitlines(True), g.splitlines(True), "command line", "session"))
            ok = False
    for anchor in anchors:
        if anchor not in want[0]:
            print("FAIL: %s: command line output lacks %r" % (name, anchor))
            ok = False
    if ok:
        print("OK: %s" % name)
    return ok


def main():
    if len(sys.argv) != 2:
        print("usage: %s LEDGER" % sys.argv[0])
        return 2
    ledger = Ledger(sys.argv[1])

    with tempfile.TemporaryDirectory() as tmp:
        def journal(name, text):
            path = os.path.join(tmp, name)
            with open(path, "w", encoding="utf-8") as fh:
                fh.write(text)
            return path

        aggy = journal("aggy.ledger", AGGY)
        assertions = journal("assertions.ledger", ASSERTIONS)

        results = [
            check(ledger, "prices and lots, --effective at startup",
                  ["-f", aggy, "--effective"],
                  ["pricedb", "register --exchange $ Assets", "balance --lots Broker"],
                  ["P 2022/01/04 00:00:00 AGGY $2", "[2022/01/04]"]),
            check(ledger, "assertions, --effective at startup",
                  ["-f", assertions, "--effective"],
                  ["balance", "register acc1"],
                  ["5 r  expenses"]),
        ]

    return 0 if all(results) else 1


if __name__ == "__main__":
    sys.exit(main())
