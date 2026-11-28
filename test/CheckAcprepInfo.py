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

# The gcov flavor gets its coverage flags from USE_GCOV alone: no CLANG_GCOV
# (which linked every configure probe against clang 3.1's libprofile_rt),
# no hand-added -fprofile-arcs/-ftest-coverage and no -lgcov.
env = {k: v for k, v in os.environ.items()
       if k not in ('CXX', 'CXXFLAGS', 'LDFLAGS')}
run = subprocess.run([sys.executable, '-I', os.path.join(source, 'acprep'),
                      '--debug', 'gcov', 'info'],
                     cwd=source, env=env, stdout=subprocess.PIPE,
                     stderr=subprocess.STDOUT, text=True)
args = run.stdout.partition('CMake arguments')[2].split()
stray = [a for a in args
         if 'CLANG_GCOV' in a or a == '-lgcov'
         or a.startswith(('-DCMAKE_CXX_FLAGS=', '-DCMAKE_EXE_LINKER_FLAGS='))]
if (run.returncode != 0 or '-DUSE_GCOV=ON' not in args
        or '-DDISABLE_ASSERTS=ON' not in args or stray):
    print('FAIL: acprep gcov info exited %d; expected -DUSE_GCOV=ON and '
          '-DDISABLE_ASSERTS=ON and no %s:\n%s'
          % (run.returncode, stray, run.stdout))
    sys.exit(1)
print('OK acprep gcov flavor configures with -DUSE_GCOV=ON only')
