#!/usr/bin/env python3
"""Check that "acprep info" reports the version in doc/LedgerVersion.cmake."""

import os
import re
import subprocess
import sys

source = sys.argv[1]

fields = {}
with open(os.path.join(source, 'doc', 'LedgerVersion.cmake')) as f:
    for line in f:
        match = re.match(r'set\(Ledger_VERSION_(MAJOR|MINOR|PATCH|DATE)\s+(\d+)\)',
                         line)
        if match:
            fields[match.group(1)] = match.group(2)
expected = '%(MAJOR)s.%(MINOR)s.%(PATCH)s-%(DATE)s' % fields

run = subprocess.run([sys.executable, '-I', os.path.join(source, 'acprep'), 'info'],
                     cwd=source, stdout=subprocess.PIPE,
                     stderr=subprocess.STDOUT, text=True)
match = re.search(r'Current version\s+=> (\S+)', run.stdout)
actual = match.group(1) if match else None
if run.returncode != 0 or actual != expected:
    print('FAIL: acprep info exited %d and reported version %s, expected %s:\n%s'
          % (run.returncode, actual, expected, run.stdout))
    sys.exit(1)
print('OK acprep info reports %s' % actual)
