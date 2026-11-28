#!/usr/bin/env python3
# A `ledger/` package directory on PYTHONPATH must not turn ledger's
# embedded `ledger` module into a package.
#
# Ledger registers its bindings as the builtin module `ledger`.  At
# interpreter start-up it used to scan sys.path for `ledger/__init__.py`
# and, on finding one, set `ledger.__path__` to that directory, so that
# `import ledger.<name>` loaded files from whatever directory matched.  No
# build or install creates such a package (the bindings install as one
# extension module), so the scan only ever fired on a stray directory.
#
# A .test file cannot exercise this: the harness cannot set PYTHONPATH.
# This script runs ledger directly with a scratch `ledger/` package on
# PYTHONPATH, once through a `python` directive in a journal and once
# through the `python` command, and checks both times that the builtin
# module has no __path__ and that `import ledger.extra` does not load the
# stray package's extra.py.

import os
import subprocess
import sys
import tempfile

PROBE = """\
import ledger
print("package:", hasattr(ledger, "__path__"))
try:
    import ledger.extra
    print("extra: imported")
except ImportError:
    print("extra: ImportError")
"""

PROBE_OUTPUT = "package: False\nextra: ImportError\n"

JOURNAL = (
    "python\n"
    + "".join("    " + line + "\n" for line in PROBE.splitlines())
    + "\n"
    + "2024/01/01 Payee\n"
    + "    Expenses:Food    $10.00\n"
    + "    Assets:Cash\n"
)

BALANCE_OUTPUT = (
    "             $-10.00  Assets:Cash\n"
    "              $10.00  Expenses:Food\n"
    "--------------------\n"
    "                   0\n"
)


def run(ledger, args, env, cwd, expected, label):
    result = subprocess.run([ledger, "--args-only"] + args, env=env, cwd=cwd,
                            capture_output=True, text=True)
    if result.returncode == 0 and result.stdout == expected and not result.stderr:
        return True
    print("FAILED: %s" % label, file=sys.stderr)
    print("exit status: %d" % result.returncode, file=sys.stderr)
    print("expected stdout:\n" + expected, file=sys.stderr)
    print("actual stdout:\n" + result.stdout, file=sys.stderr)
    print("stderr:\n" + result.stderr, file=sys.stderr)
    return False


def main():
    if len(sys.argv) != 2:
        print("usage: python-stray-ledger-package.py <path-to-ledger>", file=sys.stderr)
        return 2
    ledger = sys.argv[1]

    with tempfile.TemporaryDirectory() as root:
        pythonpath = os.path.join(root, "pythonpath")
        package = os.path.join(pythonpath, "ledger")
        os.makedirs(package)
        with open(os.path.join(package, "__init__.py"), "w"):
            pass
        with open(os.path.join(package, "extra.py"), "w") as f:
            f.write("X = 42\n")

        journal = os.path.join(root, "journal.dat")
        with open(journal, "w") as f:
            f.write(JOURNAL)
        script = os.path.join(root, "probe.py")
        with open(script, "w") as f:
            f.write(PROBE)

        env = dict(os.environ)
        env["PYTHONPATH"] = pythonpath

        ok = run(ledger, ["-f", journal, "balance"], env, root,
                 PROBE_OUTPUT + BALANCE_OUTPUT, "python directive")
        ok = run(ledger, ["python", script], env, root,
                 PROBE_OUTPUT, "python command") and ok

    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
