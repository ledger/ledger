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
# applying --effective even when it was given at startup; given to a single
# command, --effective could never reach the journal at all.  A session
# now reads its journal again whenever a command turns --effective on or
# off.
#
# Each case runs commands in one session and requires them to print what
# they print when run one at a time on the command line.  The .test harness
# cannot drive an interactive session, so this script does; to be
# independent of the line editor and of the banner, every command but push
# and pop writes its report to a file of its own.  test/regress/3253.test
# covers --script sessions.
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

# market() values the posting as the journal is read.  With no commodity to
# value it in, it takes the default commodity if one is set, as by the D
# directive that comes after it, or else that of the latest price: EUR.
VALUED = """\
P 2022/01/01 ZZZ $2
P 2022/01/03 ZZZ 3 EUR

2022/01/04 Valued
    Expenses:Misc                         (market(1 ZZZ, [2022/01/04]))
    Assets:Cash

D $1,000.00
"""

# An option line, in an included file: --invert makes the report show
# income as positive.  The bonus has an effective date.
INVERT = "--invert\n"
INVERTED = """\
include invert.ledger

2024/01/01 Salary
    Assets:Bank                          $100.00
    Income:Salary

2024/01/03=2024/01/05 Bonus
    Assets:Bank                           $50.00
    Income:Salary
"""

# The reverse: the assertion holds only in primary-date order, by which the
# purchase, whose cost records a price, comes before it.
PRIMARY_ONLY = """\
2021/01/01 Opening
    Assets:Broker                         1 AGGY
    Equity:Opening Balances

2021/02/01=2021/06/01 Buy
    Assets:Broker                         1 AGGY @@ $2.00
    Assets:Cash

2021/03/01=2021/03/01 Check
    Assets:Broker                         0 AGGY = 2 AGGY
"""

# The same, with a python directive, which points Python's `journal` at the
# journal being read.
PYTHON_PRIMARY_ONLY = "python\n    pass\n\n" + PRIMARY_ONLY

COUNT_XACTS = """\
import __main__
print("xacts: %d" % len(list(__main__.journal.xacts)))
"""

OTHER = """\
2020/01/01 Other
    Assets:Other                          1 XYZ @@ $9.00
    Assets:Cash
"""

# A data file that selects --effective itself: the command line meets the
# line only as it reads the journal, too late to read it in effective-date
# order, though in time to report it so.
EFFECTIVE_LINE = "--effective\n\n" + AGGY
EFFECTIVE_LINE_PRIMARY_ONLY = "--effective\n\n" + PRIMARY_ONLY

# A data file that names another with a --file line, which adds that file
# to those being read.
FILE_LINE = """\
--file other.ledger

2024/01/01=2024/01/05 Main
    Assets:Bank                           $10.00
    Income:Main
"""

# The init file's option is overridden on the command line below, and its
# alias names the account the journal posts to.
INIT = """\
--date-format %Y-%m-%d
alias Brk=Assets:Broker
"""

ALIASED = AGGY.replace("Assets:Broker   ", "Brk             ")


class Ledger:
    def __init__(self, path, home):
        self.path = path
        self.home = home
        # A fresh HOME holds no ~/.ledgerrc or ~/.pricedb to read.
        self.env = {k: v for k, v in os.environ.items()
                    if not k.startswith(("LEDGER", "PRICE_")) and k != "XDG_CONFIG_HOME"}
        self.env["TZ"] = "America/Chicago"
        self.env["HOME"] = home

    def run(self, args, stdin="", args_only=True):
        return subprocess.run(
            [self.path] + (["--args-only"] if args_only else []) + ["--columns=80", *args],
            input=stdin, capture_output=True, text=True, env=self.env, cwd=self.home)

    def session(self, startup, commands, args_only=True):
        """Run COMMANDS in one interactive session: (output, stderr)."""
        lines, outputs = [], []
        for n, command in enumerate(commands):
            if command.split()[0] in ("push", "pop"):
                lines.append(command)
                continue
            output = os.path.join(self.home, "output-%d" % n)
            if os.path.exists(output):
                os.remove(output)
            lines.append("%s --output %s" % (command, os.path.basename(output)))
            outputs.append(output)
        p = self.run(startup, "".join(line + "\n" for line in lines), args_only)
        out = ""
        for output in outputs:
            if os.path.exists(output):
                with open(output, encoding="utf-8") as fh:
                    out += fh.read()
        return out, p.stderr

    def script(self, startup, commands, stdin=""):
        """Run COMMANDS as a --script session: (stdout, stderr)."""
        with open(os.path.join(self.home, "commands"), "w", encoding="utf-8") as fh:
            fh.write("".join(c + "\n" for c in commands))
        p = self.run(startup + ["--script", "commands"], stdin)
        return p.stdout, p.stderr

    def command_line(self, startup, commands, stdin="", args_only=True):
        """Run each of COMMANDS on the command line: (stdout, stderr)."""
        out = err = ""
        for command in commands:
            p = self.run(startup + shlex.split(command), stdin, args_only)
            out += p.stdout
            err += p.stderr
        return out, err


def check(name, got, want, anchors):
    """The session must print what the command line prints (WANT), and the
    command line must print each of ANCHORS, so that both cannot be wrong
    in the same way."""
    ok = True
    for label, g, w in (("output", got[0], want[0]), ("errors", got[1], want[1])):
        if g != w:
            print("FAIL: %s: session %s differ from the command line's:" % (name, label))
            sys.stdout.writelines(difflib.unified_diff(
                w.splitlines(True), g.splitlines(True), "command line", "session"))
            ok = False
    for anchor in anchors:
        if anchor not in want[0] + want[1]:
            print("FAIL: %s: command line output lacks %r" % (name, anchor))
            ok = False
    if ok:
        print("OK: %s" % name)
    return ok


def main():
    if len(sys.argv) != 2:
        print("usage: %s LEDGER" % sys.argv[0])
        return 2

    with tempfile.TemporaryDirectory() as home:
        for name, text in (("aggy.ledger", AGGY), ("assertions.ledger", ASSERTIONS),
                           ("valued.ledger", VALUED), ("invert.ledger", INVERT),
                           ("inverted.ledger", INVERTED), ("primary-only.ledger", PRIMARY_ONLY),
                           ("python.ledger", PYTHON_PRIMARY_ONLY), ("count.py", COUNT_XACTS),
                           ("other.ledger", OTHER), ("effective-line.ledger", EFFECTIVE_LINE),
                           ("effective-line-primary-only.ledger", EFFECTIVE_LINE_PRIMARY_ONLY),
                           ("file-line.ledger", FILE_LINE), ("init", INIT),
                           ("aliased.ledger", ALIASED)):
            with open(os.path.join(home, name), "w", encoding="utf-8") as fh:
                fh.write(text)
        ledger = Ledger(os.path.abspath(sys.argv[1]), home)

        def same(name, startup, commands, anchors, args_only=True):
            return check(name, ledger.session(startup, commands, args_only),
                         ledger.command_line(startup, commands, args_only=args_only),
                         anchors)

        aggy = ["-f", "aggy.ledger"]
        results = [
            same("prices and lots, --effective at startup",
                 aggy + ["--effective"],
                 ["pricedb", "register --exchange $ Assets", "balance --lots Broker"],
                 ["P 2022/01/04 00:00:00 AGGY $2", "[2022/01/04]"]),
            same("assertions, --effective at startup",
                 ["-f", "assertions.ledger", "--effective"],
                 ["balance", "register acc1"],
                 ["5 r  expenses"]),
            # Read again, the journal keeps the init file's declarations, and
            # the init file's options yield still to the command line's.
            same("init file declarations and options, read again",
                 ["--init-file", "init", "-f", "aliased.ledger",
                  "--date-format", "%d.%m.%Y"],
                 ["register Assets", "reload", "register Assets",
                  "register --effective Assets", "register Assets"],
                 ["31.12.2021", "04.01.2022", "Assets:Broker"],
                 args_only=False),
            # Read again, the journal is read without the default commodity
            # that its D directive set when it was read before.
            same("default commodity, read again",
                 ["-f", "valued.ledger"],
                 ["register Misc", "reload", "register Misc"],
                 ["EUR3"]),
            # Read again, the data files' option lines, in effect since the
            # first reading, are not applied once more: --invert does not
            # invert back.
            same("option lines in a data file, read again",
                 ["-f", "inverted.ledger"],
                 ["balance Income", "reload", "balance Income",
                  "balance --effective Income", "balance Income"],
                 ["$150.00  Income:Salary"]),

            # The commands from the issue.
            same("--effective given to a command",
                 aggy,
                 ["pricedb --effective", "register --exchange $ --effective Assets"],
                 ["P 2022/01/04 00:00:00 AGGY $2", "Commodities revalued"]),
            # Each change of mode reads the journal again, and no price
            # outlives the reading that recorded it.
            same("--effective turned on and off",
                 aggy,
                 ["pricedb", "pricedb --effective", "balance --lots --effective Broker",
                  "pricedb", "register --exchange $ Assets", "balance --lots Broker",
                  "pricedb --aux-date", "pricedb --effective --primary-date"],
                 ["P 2021/12/31 00:00:00 AGGY $2", "[2021/12/31]", "[2022/01/04]"]),
            # Reading the journal under --effective fails, as on the command
            # line; the session keeps the journal it had, and its prices,
            # among which is none from the failed reading.
            same("a failed reading keeps the journal",
                 ["-f", "primary-only.ledger"],
                 ["balance", "balance --effective", "balance --effective", "pricedb",
                  "balance --lots Broker", "balance --effective", "pricedb"],
                 ["Balance assertion off by 1 AGGY", "P 2021/02/01 00:00:00 AGGY $2"]),
            # Standard input can be read only once, but it is read again.
            check("a journal read from standard input",
                  ledger.script(["-f", "-"], ["pricedb", "pricedb --effective", "pricedb"],
                                AGGY),
                  ledger.command_line(["-f", "-"],
                                      ["pricedb", "pricedb --effective", "pricedb"], AGGY),
                  ["P 2022/01/04 00:00:00 AGGY $2"]),
            # An --effective line in a data file does not decide how the
            # journal is read, as on the command line, but a command's own
            # --effective or --primary-date does.
            same("an --effective line in a data file",
                 ["-f", "effective-line.ledger"],
                 ["pricedb", "balance --lots Broker", "pricedb --effective",
                  "pricedb --primary-date", "pricedb"],
                 ["P 2021/12/31 00:00:00 AGGY $2", "P 2022/01/04 00:00:00 AGGY $2",
                  "[2021/12/31]"]),
            same("an --effective line in a data file, read in primary-date order",
                 ["-f", "effective-line-primary-only.ledger"],
                 ["balance", "balance --lots Broker"],
                 ["[2021/02/01]"]),
            # A -f given to a command takes effect at the next reload, not
            # when a change of mode has the journal read again.
            check("a command's -f",
                  ledger.session(aggy, ["push -f other.ledger", "pop",
                                        "pricedb --effective", "reload", "pricedb"]),
                  (ledger.command_line(aggy, ["pricedb --effective"])[0]
                   + ledger.command_line(["-f", "other.ledger"], ["pricedb"])[0], ""),
                  ["P 2022/01/04 00:00:00 AGGY $2", "P 2020/01/01 00:00:00 XYZ $9"]),
        ]

        # Read again, a --file line in a data file adds the file it names,
        # as when the journal was first read, rather than emptying the list
        # of files as it is being read.  (The command line, where -f has been
        # given before the command re-arms it, still empties it.)
        once = ledger.session(["-f", "file-line.ledger"], ["balance Assets"])
        thrice = ledger.session(["-f", "file-line.ledger"],
                                ["balance Assets", "balance --effective Assets",
                                 "balance Assets"])
        if "XYZ" in once[0] and thrice == (once[0] * 3, ""):
            print("OK: a --file line in a data file, read again")
        else:
            print("FAIL: a --file line in a data file, read again: %r" % (thrice,))
            results.append(False)

        # A python directive points Python's `journal` at the journal being
        # read.  When a reading fails, it must point again at the journal
        # the session keeps, not at the one thrown away.
        if "with Python support" in ledger.run(["--version"]).stdout:
            p = ledger.run(["-f", "python.ledger"],
                           "python count.py\nbalance --effective\npython count.py\n")
            counts = re.findall(r"xacts: \d+", p.stdout)
            if counts == ["xacts: 3", "xacts: 3"] and "Balance assertion" in p.stderr:
                print("OK: Python's journal after a failed reading")
            else:
                print("FAIL: Python's journal after a failed reading: %r\n%s"
                      % (counts, p.stderr))
                results.append(False)
        else:
            print("SKIP: Python's journal after a failed reading: no Python support")

    return 0 if all(results) else 1


if __name__ == "__main__":
    sys.exit(main())
