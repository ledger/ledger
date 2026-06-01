#!/usr/bin/env python3
# Regression test for GitHub issue #3229 (Debian bug #1138597):
#
# The ledger Python extension module is built by copying the build-tree
# libledger shared library and is installed with install(PROGRAMS ...).  Because
# it is not an install(TARGETS ...) artifact, CMake never sanitizes its RPATH at
# install time, so the build-tree RPATH leaked through into the installed
# module.  When CMAKE_INSTALL_RPATH is set, CMake pads that build-tree RPATH with
# empty entries to reserve space for the (longer) install RPATH, producing a
# DT_RUNPATH that is a list of empty path elements, e.g. "[:::::::::::::]".
#
# ELF dynamic linkers interpret an empty RPATH/RUNPATH element as the current
# working directory.  A module with such an entry therefore searches CWD ahead
# of the system library directories, which permits library-search-path
# hijacking (CWE-426 / CWE-427).
#
# This test inspects the built Python module and fails if any of its RPATH or
# RUNPATH dynamic entries contains an empty path element.  After the fix the
# module carries no RPATH/RUNPATH at all, so the test passes.
#
# The check is only meaningful on ELF platforms; the CMake test wiring guards it
# with "UNIX AND NOT APPLE".

import os
import re
import shutil
import subprocess
import sys


def read_rpath_entries(path):
    """Return a list of (tag, value) RPATH/RUNPATH dynamic entries for an ELF
    file, or None if no inspection tool is available."""
    if shutil.which("readelf"):
        out = subprocess.run(
            ["readelf", "-d", path], capture_output=True, text=True
        ).stdout
        # e.g. "0x...(RUNPATH)  Library runpath: [:::::::]"
        pattern = re.compile(r"\((RUNPATH|RPATH)\).*\[(.*)\]")
        return [
            (m.group(1), m.group(2))
            for m in (pattern.search(line) for line in out.splitlines())
            if m
        ]
    if shutil.which("objdump"):
        out = subprocess.run(
            ["objdump", "-x", path], capture_output=True, text=True
        ).stdout
        # e.g. "  RUNPATH              :::::::"
        pattern = re.compile(r"^\s*(RUNPATH|RPATH)\s+(.*)$")
        return [
            (m.group(1), m.group(2).strip())
            for m in (pattern.match(line) for line in out.splitlines())
            if m
        ]
    return None


def main():
    if len(sys.argv) != 2:
        print("usage: 3229.py <path-to-ledger-python-module>", file=sys.stderr)
        return 2

    module = sys.argv[1]

    if not os.path.isfile(module):
        # The module is expected to exist whenever this test runs (it is only
        # registered when the Python bindings are built).  A missing file is a
        # real problem, not a reason to pass silently.
        print("FAIL: ledger Python module not found: %s" % module)
        return 1

    entries = read_rpath_entries(module)
    if entries is None:
        # Without readelf/objdump we cannot inspect the binary; skip rather than
        # report a spurious failure.  CI Linux runners ship binutils.
        print("SKIP: neither readelf nor objdump is available", file=sys.stderr)
        return 0

    # An empty element (the whole value empty, a leading/trailing ':', or "::")
    # is the vulnerability: the loader treats it as the current directory.
    insecure = [
        (tag, value) for tag, value in entries if "" in value.split(":")
    ]

    if insecure:
        print(
            "FAIL: ledger Python module %s has insecure RPATH/RUNPATH entries"
            % module
        )
        for tag, value in insecure:
            print("    %s = [%s]" % (tag, value))
        print(
            "An empty RPATH/RUNPATH element is searched as the current working "
            "directory and permits library-search-path hijacking."
        )
        return 1

    if entries:
        for tag, value in entries:
            print("OK: %s = [%s] (no empty elements)" % (tag, value))
    else:
        print("OK: %s carries no RPATH/RUNPATH" % module)
    return 0


if __name__ == "__main__":
    sys.exit(main())
