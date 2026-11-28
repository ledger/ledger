#!/usr/bin/env python3
# A `python` directive runs while the journal is being parsed, so it can write
# a file that a later `include` reads.  The include directory listing is
# cached for the length of one parse; the cache must notice a directory that
# changed after it was listed.  Five cases:
#   1. a literal include of a file the python block just wrote;
#   2. a glob include that must now match a file written after the first
#      glob include listed the same directory;
#   3. the same as 1, but the python block puts the directory's modification
#      time back to the value it had when the cache listed it, as happens when
#      the file is created within one clock tick of the listing.  A first
#      python block sets that time just before the first include, so the
#      listing falls inside the racy window however slowly ledger starts.
#   4. the same as 1, but the directory is aged an hour back after the
#      journal is written and nothing restores its time, the ordinary case of
#      a journal directory older than the two-second window: only the
#      comparison of the cached stamp with the directory's current
#      modification time can notice the new file;
#   5. the same as 2 under the same aged directory.
# A .test file cannot write files, so this script builds the tree in a scratch
# directory and runs ledger itself.  It skips (77) when ledger has no Python.

import os
import subprocess
import sys
import tempfile
import time


def xact(date, payee, account, amount):
    return f"{date} {payee}\n    {account}    {amount}\n    Cash\n"


def writer(name, text, extra=""):
    return f"python\n    open({name!r}, 'w').write({text!r})\n{extra}\n"


def main():
    if len(sys.argv) != 2:
        print("usage: include-after-generate.py <path-to-ledger>", file=sys.stderr)
        return 2
    ledger = os.path.abspath(sys.argv[1])
    failures = 0

    def check(label, files, journal, expected, aged=False):
        nonlocal failures
        with tempfile.TemporaryDirectory() as directory:
            for name, text in files.items():
                with open(os.path.join(directory, name), "w") as f:
                    f.write(text)
            with open(os.path.join(directory, "j.dat"), "w") as f:
                f.write(journal)
            if aged:
                # An hour old, so the listing the cache takes is not racy;
                # nothing puts the time back after the python block writes.
                old = time.time_ns() - 3600 * 10**9
                os.utime(directory, ns=(old, old))
            result = subprocess.run(
                [ledger, "--args-only", "-f", "j.dat", "bal", "--flat", "--no-total",
                 "--format", "%(account) %(display_total)\\n"],
                cwd=directory, capture_output=True, text=True)
            if "Python support is missing" in result.stderr:
                print("SKIP: ledger was built without Python")
                sys.exit(77)
            if result.returncode != 0 or result.stdout != expected:
                failures += 1
                print(f"FAIL: {label}: exit {result.returncode}\n--- stdout\n{result.stdout}"
                      f"--- stderr\n{result.stderr}--- expected\n{expected}")
            else:
                print(f"OK: {label}")

    a = xact("2024/01/01", "A", "A", "$1.00")
    g = xact("2024/01/02", "G", "G", "$2.00")

    check("include of a file written by a python block",
          {"a.dat": a},
          "include a.dat\n" + writer("gen.dat", g) + "include gen.dat\n",
          "A $1.00\nCash $-3.00\nG $2.00\n")

    check("glob include after a python block wrote another match",
          {"g0.dat": a},
          "include g*.dat\n" + writer("g1.dat", g) + "include g*.dat\n",
          "A $2.00\nCash $-4.00\nG $2.00\n")

    check("file written within the clock tick of the listing",
          {"a.dat": a},
          "python\n    import os, time\n    stamp = time.time_ns()\n"
          "    os.utime('.', ns=(stamp, stamp))\n\n"
          "include a.dat\n"
          + writer("gen.dat", g, "    os.utime('.', ns=(stamp, stamp))\n")
          + "include gen.dat\n",
          "A $1.00\nCash $-3.00\nG $2.00\n")

    check("include of a file written into an aged directory",
          {"a.dat": a},
          "include a.dat\n" + writer("gen.dat", g) + "include gen.dat\n",
          "A $1.00\nCash $-3.00\nG $2.00\n", aged=True)

    check("glob include after a python block wrote another match into an aged directory",
          {"g0.dat": a},
          "include g*.dat\n" + writer("g1.dat", g) + "include g*.dat\n",
          "A $2.00\nCash $-4.00\nG $2.00\n", aged=True)

    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
