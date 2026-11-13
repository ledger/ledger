#!/usr/bin/env python3
# An encrypted journal must read exactly like the same journal in plain text.
#
# Ledger reads a gpg-encrypted journal through decrypted_stream_t, whose
# stream buffer holds up to 4096 decrypted bytes ahead of the parser.  After
# an `account` directive the parser measures the indentation of the next line
# by saving its position with tellg() and seeking back with seekg().  The
# buffer used to answer tellg() with the position of the bytes it had already
# read ahead, and to throw those bytes away, so everything after the first
# indented account sub-directive was silently lost.  Error messages also
# re-read the encrypted file with seekg() to quote the offending lines.
#
# A .test file cannot exercise this: the harness reads plain files.  This
# script makes a throwaway GnuPG home with an unprotected key, encrypts each
# fixture to it, and checks that ledger reports exactly the same for the
# encrypted copy as for the plain one.  It exits 77 (skipped) when gpg is not
# installed.

import difflib
import os
import shutil
import subprocess
import sys
import tempfile

UID = "Ledger Test <ledger-test@example.invalid>"
RECIPIENT = "ledger-test@example.invalid"

SMALL = """\
account Assets:Cash
    note cash on hand

2024/01/01 Grocery
    Expenses:Food    $10.00
    Assets:Cash

2024/01/02 Bakery
    Expenses:Food    $5.00
    Assets:Cash
"""

# More than one 4096-byte buffer of account directives before the entries.
LARGE = (
    "".join(
        f"account Expenses:Cat{i:03d}\n    note category {i}\n" for i in range(200)
    )
    + "\n"
    + SMALL
)

UNBALANCED = (
    SMALL
    + """
2024/01/03 Unbalanced
    Expenses:Food    $1.00
    Assets:Cash     $-2.00
"""
)

# (fixture name, journal text, expected exit status, [(ledger command, text
# the plain-file output must contain)])
CASES = [
    ("small", SMALL, 0, [(["reg"], "Bakery"), (["bal"], "Expenses:Food")]),
    ("large", LARGE, 0, [(["reg"], "Bakery"), (["bal"], "Expenses:Food")]),
    ("unbalanced", UNBALANCED, 1, [(["reg"], "Unbalanced remainder")]),
]


def gpg(home, *args):
    subprocess.run(
        ["gpg", "--batch", "--quiet", "--homedir", home, *args],
        check=True,
        stdout=subprocess.DEVNULL,
    )


def make_home():
    # gpg-agent keeps its sockets in the home directory on macOS, and a Unix
    # socket path is limited to about 104 bytes, so a deep TMPDIR keeps the
    # agent from starting.  Use /tmp instead when the default is too deep.
    home = tempfile.mkdtemp(prefix="lgpg")
    if len(home) > 72 and os.path.isdir("/tmp"):
        os.rmdir(home)
        home = tempfile.mkdtemp(prefix="lgpg", dir="/tmp")
    return home


def run_ledger(ledger, home, journal, command):
    env = dict(os.environ, GNUPGHOME=home)
    proc = subprocess.run(
        [ledger, "--args-only", "--columns", "80", "-f", journal, *command],
        env=env,
        capture_output=True,
        text=True,
    )
    # Error context names the file; name both copies alike.
    return proc.returncode, proc.stdout, proc.stderr.replace(journal, "JOURNAL")


def main():
    if len(sys.argv) != 2:
        print("usage: gpg-encrypted-journal.py <path-to-ledger>", file=sys.stderr)
        return 2
    ledger = sys.argv[1]
    if shutil.which("gpg") is None:
        print("gpg not found; skipping")
        return 77

    home = make_home()
    failures = 0
    try:
        gpg(home, "--pinentry-mode", "loopback", "--passphrase", "",
            "--quick-generate-key", UID, "default", "default", "never")
        for name, text, status, commands in CASES:
            plain = os.path.join(home, name + ".dat")
            encrypted = os.path.join(home, name + ".gpg")
            with open(plain, "w") as f:
                f.write(text)
            gpg(home, "--trust-model", "always", "--yes", "--output", encrypted,
                "--recipient", RECIPIENT, "--encrypt", plain)
            for command, marker in commands:
                label = f"{name} {' '.join(command)}"
                want = run_ledger(ledger, home, plain, command)
                got = run_ledger(ledger, home, encrypted, command)
                # The plain run must itself show the whole journal, so that
                # equal output cannot mean both lost the same entries.
                if want[0] != status or marker not in want[1] + want[2]:
                    print(f"FAIL {label}: unexpected plain-file result {want!r}")
                    failures += 1
                    continue
                if got != want:
                    print(f"FAIL {label}: encrypted journal differs from plain")
                    print(f"  exit status: plain {want[0]}, encrypted {got[0]}")
                    for which, a, b in (("stdout", want[1], got[1]),
                                        ("stderr", want[2], got[2])):
                        for line in difflib.unified_diff(
                                a.splitlines(), b.splitlines(),
                                f"plain {which}", f"encrypted {which}",
                                lineterm=""):
                            print("  " + line)
                    failures += 1
    finally:
        if shutil.which("gpgconf"):
            subprocess.run(["gpgconf", "--homedir", home, "--kill", "gpg-agent"],
                           check=False)
        shutil.rmtree(home, ignore_errors=True)

    if failures:
        print(f"FAILED ({failures})")
        return 1
    print("OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
