#!/usr/bin/env python3
# Regression test for GitHub issue #3294:
#
# --args-only promises a run controlled by the command line alone, and since
# 2009 it has turned the --price-db option off so that ~/.pricedb is not read.
# In 2013 the ~/.pricedb default moved out of the option and into
# session_t::read_data(), which falls back to it whenever --price-db is unset,
# so turning the option off stopped having any effect: --args-only went on
# valuing commodities with prices from $HOME/.pricedb.  The same change made
# the fallback for an unset HOME ./.ledgerrc rather than ./.pricedb, so an init
# file in the current directory was also parsed as price history, even under
# --args-only, which skips init files.  Because that happens after the command
# line has been processed, a --file in that file replaced the journal named by
# -f.
#
# A .test file cannot exercise any of this: the harness always passes
# --args-only and can neither set nor unset HOME, nor choose the working
# directory.  This script runs ledger directly in scratch directories with a
# controlled environment.  Each price file uses its own price, so the valuation
# shows which one, if any, was read: $1.00 from $HOME/.pricedb, $2.00 from
# ./.pricedb, $3.00 from ./.ledgerrc.

import os
import shlex
import subprocess
import sys
import tempfile

JOURNAL = """\
2025/01/06 * Test
    Assets:A    10 FOO
    Equity:X
"""

OTHER_JOURNAL = """\
2025/02/01 * Other
    Assets:B    5 BAR
    Equity:Y
"""


def balance_line(amount):
    return f"{amount:>20}  Assets:A\n"


UNVALUED = balance_line("10 FOO")


def make_dir(root, name, files):
    directory = os.path.join(root, name)
    os.mkdir(directory)
    for filename, text in files.items():
        with open(os.path.join(directory, filename), "w") as f:
            f.write(text)
    return directory


def main():
    if len(sys.argv) != 2:
        print("usage: 3294.py <path-to-ledger>", file=sys.stderr)
        return 2

    ledger = os.path.abspath(sys.argv[1])
    failed = 0

    # Keep the sanitizer and coverage settings ctest runs under, but nothing
    # else -- above all no LEDGER_* variable, XDG_CONFIG_HOME or HOME, so that
    # no init file or price database of whoever runs the tests is read.
    base_env = {
        k: v
        for k, v in os.environ.items()
        if k.endswith("SAN_OPTIONS") or k.startswith(("LLVM_PROFILE_", "GCOV_"))
    }
    base_env["TZ"] = "America/Chicago"

    def check(description, args, expected, cwd, home=None):
        nonlocal failed
        env = dict(base_env)
        if home is not None:
            env["HOME"] = home
        try:
            result = subprocess.run(
                [ledger] + args,
                cwd=cwd,
                env=env,
                capture_output=True,
                text=True,
                timeout=60,
            )
        except subprocess.TimeoutExpired:
            result = None
        if result and result.returncode == 0 and result.stdout == expected:
            print(f"OK: {description}")
            return
        failed += 1
        print(f"FAIL: {description}")
        print(f"  command:  ledger {shlex.join(args)}")
        print(f"  cwd:      {cwd}")
        print(f"  HOME:     {home if home is not None else '(unset)'}")
        print(f"  expected: {expected!r}")
        if result is None:
            print("  actual:   (timed out after 60s)")
            return
        print(f"  actual:   {result.stdout!r} (exit {result.returncode})")
        if result.stderr:
            print(f"  stderr:   {result.stderr.strip()}")

    with tempfile.TemporaryDirectory() as root:
        journal = os.path.join(root, "journal.ledger")
        with open(journal, "w") as f:
            f.write(JOURNAL)
        other = os.path.join(root, "other.ledger")
        with open(other, "w") as f:
            f.write(OTHER_JOURNAL)
        # Only the two-argument form of --init-file takes effect before the
        # default init file would be read.
        empty_init = os.path.join(root, "empty.ledgerrc")
        open(empty_init, "w").close()

        home = make_dir(root, "home", {".pricedb": "P 2025/01/01 FOO $1.00\n"})
        empty = make_dir(root, "empty", {})
        with_pricedb = make_dir(root, "pricedb", {".pricedb": "P 2025/01/01 FOO $2.00\n"})
        with_rc_price = make_dir(root, "rc-price", {".ledgerrc": "P 2025/01/01 FOO $3.00\n"})
        with_rc_file = make_dir(root, "rc-file", {".ledgerrc": f"--file {other}\n"})

        value = ["-f", journal, "bal", "-X", "$", "Assets"]

        check(
            "--args-only does not read $HOME/.pricedb",
            ["--args-only"] + value,
            UNVALUED,
            cwd=empty,
            home=home,
        )
        check(
            "--args-only still reads the file --price-db names",
            ["--args-only", "--price-db", os.path.join(home, ".pricedb")] + value,
            balance_line("$10.00"),
            cwd=empty,
            home=empty,
        )
        check(
            "without --args-only, $HOME/.pricedb is read by default",
            value,
            balance_line("$10.00"),
            cwd=empty,
            home=home,
        )
        check(
            "without HOME, ./.pricedb is read by default",
            value,
            balance_line("$20.00"),
            cwd=with_pricedb,
        )
        check(
            "without HOME, --args-only does not read ./.pricedb",
            ["--args-only"] + value,
            UNVALUED,
            cwd=with_pricedb,
        )
        check(
            "without HOME, ./.ledgerrc is not read as a price database",
            ["--init-file", empty_init] + value,
            UNVALUED,
            cwd=with_rc_price,
        )
        check(
            "without HOME, a --file in ./.ledgerrc does not replace -f",
            ["-f", journal, "bal", "Assets"],
            UNVALUED,
            cwd=with_rc_file,
        )

    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
