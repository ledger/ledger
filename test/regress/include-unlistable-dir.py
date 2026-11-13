#!/usr/bin/env python3
# An `include` of a plain path must read the file whenever `-f` could, even
# when an ancestor directory may be searched but not listed (mode 0311).
# expand_glob_path() used to list every directory on the path to match each
# component as a glob, so such an ancestor made the include fail with a raw
# filesystem_error.  Only a component holding a glob metacharacter needs its
# directory listed; that case still fails, now with a parse error naming the
# directory.
#
# A .test file cannot change directory permissions, so this script builds
# the tree in a scratch directory and runs ledger itself.

import os
import shlex
import stat
import subprocess
import sys
import tempfile

INCLUDED = """\
2024/01/01 Inside
    Expenses:Food    $10.00
    Assets:Cash
"""

BALANCE = """\
             $-10.00  Assets:Cash
              $10.00  Expenses:Food
--------------------
                   0
"""


def main():
    if len(sys.argv) != 2:
        print("usage: include-unlistable-dir.py <path-to-ledger>", file=sys.stderr)
        return 2
    if sys.platform == "win32" or os.geteuid() == 0:
        # Windows has no search-only directories, and root lists any
        # directory whatever its mode.
        print("SKIP: needs POSIX permissions enforced for this user")
        return 77

    ledger = os.path.abspath(sys.argv[1])
    env = {
        k: v
        for k, v in os.environ.items()
        if k.endswith("SAN_OPTIONS") or k.startswith(("LLVM_PROFILE_", "GCOV_"))
    }
    env["TZ"] = "America/Chicago"
    failed = 0

    with tempfile.TemporaryDirectory() as root:
        root = os.path.realpath(root)
        locked = os.path.join(root, "locked")
        os.makedirs(os.path.join(locked, "sub"))
        target = os.path.join(locked, "sub", "inc.dat")
        with open(target, "w") as f:
            f.write(INCLUDED)
        journals = {
            "absolute path": f"include {target}\n",
            "relative path": "include locked/sub/inc.dat\n",
            "glob below the locked directory": "include locked/sub/*.dat\n",
            "glob through the locked directory": "include locked/*/inc.dat\n",
            "missing file below the locked directory": "include locked/sub/missing.dat\n",
            "the same path included twice": "include locked/sub/inc.dat\ninclude locked/sub/inc.dat\n",
        }
        for i, text in enumerate(journals.values()):
            with open(os.path.join(root, f"main{i}.dat"), "w") as f:
                f.write(text)

        os.chmod(locked, stat.S_IWUSR | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)  # 0311
        try:
            if os.access(locked, os.R_OK):
                print("SKIP: the locked directory is still readable")
                return 77
            for i, description in enumerate(journals):
                journal = os.path.join(root, f"main{i}.dat")
                args = ["--args-only", "--columns=80", "-f", journal, "bal"]
                result = subprocess.run([ledger] + args, env=env, capture_output=True,
                                        text=True, timeout=60)
                if description == "glob through the locked directory":
                    ok = (result.returncode == 1 and result.stdout == ""
                          and f"Cannot list directory {locked}" in result.stderr
                          and "filesystem error" not in result.stderr)
                elif description == "missing file below the locked directory":
                    ok = (result.returncode == 1 and result.stdout == ""
                          and "File to include was not found" in result.stderr)
                elif description == "the same path included twice":
                    ok = result.returncode == 0 and result.stdout == BALANCE.replace(
                        "$-10.00", "$-20.00").replace("$10.00", "$20.00")
                else:
                    ok = result.returncode == 0 and result.stdout == BALANCE
                if ok:
                    print(f"OK: {description}")
                    continue
                failed += 1
                print(f"FAIL: {description}")
                print(f"  command: ledger {shlex.join(args)}")
                print(f"  stdout:  {result.stdout!r} (exit {result.returncode})")
                print(f"  stderr:  {result.stderr.strip()}")
        finally:
            os.chmod(locked, stat.S_IRWXU)

    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
