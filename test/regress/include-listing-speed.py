#!/usr/bin/env python3
# Every `include` used to list and sort every directory on its path, so 500
# includes of one file in a directory of 5,000 files took about 250 times as
# long as the same 500 transactions in one file (10.3 s against 0.04 s on a
# Debug build).  A directory is now listed once per parse and a path
# component without glob characters is looked up by name in that listing.
#
# The script compares two runs of the same ledger on the same machine, so a
# slow runner (sanitizers, coverage) slows both.  The bound is 25 times the
# one-file run plus a quarter of a second, the best of three runs each; the
# cost before the fix was about 250 times.
#
# An include walks every ancestor of its path (split_glob_path starts at the
# root), and a directory modified in the last two seconds is re-listed on
# every use.  The tree is therefore built under the current directory (ctest
# runs the script in build/test, which nothing else lists during the test),
# not under $TMPDIR, whose size and churn the test cannot control: on one
# machine the default $TMPDIR held 40,475 entries.  Every directory the test
# creates is aged, and the script waits out the window for the current
# directory, whose modification time the creation of the scratch directory
# just changed.

import os
import subprocess
import sys
import tempfile
import time

FILES = 5000
INCLUDES = 500
ENTRY = "2024/01/01 X\n    Expenses:Food    $1.00\n    Assets:Cash\n\n"


def best_of_three(ledger, journal, env):
    best = None
    for _ in range(3):
        start = time.monotonic()
        result = subprocess.run([ledger, "--args-only", "-f", journal, "bal"], env=env,
                                capture_output=True, text=True, timeout=600)
        elapsed = time.monotonic() - start
        if result.returncode != 0 or "$500.00  Expenses:Food" not in result.stdout:
            print(f"FAIL: unexpected output (exit {result.returncode}): "
                  f"{result.stdout!r} {result.stderr.strip()}")
            sys.exit(1)
        best = elapsed if best is None else min(best, elapsed)
    return best


def main():
    if len(sys.argv) != 2:
        print("usage: include-listing-speed.py <path-to-ledger>", file=sys.stderr)
        return 2
    ledger = os.path.abspath(sys.argv[1])
    env = {
        k: v
        for k, v in os.environ.items()
        if k.endswith("SAN_OPTIONS") or k.startswith(("LLVM_PROFILE_", "GCOV_"))
    }
    env["TZ"] = "America/Chicago"

    with tempfile.TemporaryDirectory(dir=os.getcwd()) as scratch:
        root = os.path.join(os.path.realpath(scratch), "work")
        os.mkdir(root)
        d = os.path.join(root, "d")
        os.mkdir(d)
        for i in range(FILES):
            open(os.path.join(d, f"f{i}.dat"), "w").close()
        with open(os.path.join(d, "inc.dat"), "w") as f:
            f.write(ENTRY)
        many = os.path.join(root, "many.dat")
        one = os.path.join(root, "one.dat")
        with open(many, "w") as f:
            f.write("include d/inc.dat\n" * INCLUDES)
        with open(one, "w") as f:
            f.write(ENTRY * INCLUDES)
        # A directory modified in the last two seconds is re-listed on every
        # include (the cache cannot yet tell a same-tick change from none).
        # Age every directory this script created so the test measures the
        # cache, not that rule; the current directory, which mkdtemp just
        # modified and which the script does not own, is waited out instead.
        old = time.time() - 3600
        for p in (d, root, scratch):
            os.utime(p, (old, old))
        time.sleep(2.5)

        t_one = best_of_three(ledger, one, env)
        t_many = best_of_three(ledger, many, env)

    bound = 25 * t_one + 0.25
    print(f"one file: {t_one:.3f}s; {INCLUDES} includes: {t_many:.3f}s; bound {bound:.3f}s")
    if t_many > bound:
        print("FAIL: includes cost more than the bound")
        return 1
    print("OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
