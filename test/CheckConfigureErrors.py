#!/usr/bin/env python3
"""Check that the configure-time guards in CMakeLists.txt stop
configuration and name what they reject.

Each case reconfigures the source tree in a scratch build directory seeded
from this build's CMakeCache.txt, so that every dependency resolves as it
did here, and overrides the one setting the guard must reject.
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile

parser = argparse.ArgumentParser()
parser.add_argument('--cmake', required=True)
parser.add_argument('--source', required=True)
parser.add_argument('--cache', required=True)
args = parser.parse_args()


def required_version(name):
    with open(os.path.join(args.source, 'CMakeLists.txt')) as f:
        match = re.search(r'^set\(Required_%s_Version (\S+)\)' % name,
                          f.read(), re.M)
    return match.group(1)


def configure(scratch, overrides):
    build = os.path.join(scratch, 'build')
    os.mkdir(build)
    # The copied cache must not name the directories it was created for.
    with open(args.cache) as src, \
         open(os.path.join(build, 'CMakeCache.txt'), 'w') as dst:
        for line in src:
            if not line.startswith(('CMAKE_CACHEFILE_DIR:',
                                    'CMAKE_HOME_DIRECTORY:')):
                dst.write(line)
    run = subprocess.run([args.cmake, '-S', args.source, '-B', build]
                         + overrides,
                         stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                         text=True)
    return run.returncode, ' '.join(run.stdout.split())


def fake_header(scratch, name, defines):
    include = os.path.join(scratch, 'include')
    os.makedirs(include, exist_ok=True)
    with open(os.path.join(include, name), 'w') as f:
        for macro, value in defines:
            f.write('#define %s %s\n' % (macro, value))
    return include


failures = 0


def expect_error(label, overrides, message, headers=()):
    global failures
    with tempfile.TemporaryDirectory() as scratch:
        extra = ['-D%s=%s' % (var, fake_header(scratch, name, defines))
                 for var, name, defines in headers]
        status, output = configure(scratch, overrides + extra)
    pattern = (r'CMake Error at \S*CMakeLists\.txt:\d+ \(message\): '
               + re.escape(message))
    if status == 0 or not re.search(pattern, output):
        failures += 1
        print('FAIL %s: exit %d, expected "%s" in:\n%s'
              % (label, status, message, output))
    else:
        print('OK %s' % label)


expect_error('python-without-library',
             ['-DUSE_PYTHON=ON', '-DBUILD_LIBRARY=OFF'],
             'Building the python module requires BUILD_LIBRARY=ON.')

expect_error('gmp-too-old', [],
             'Ledger requires GMP version %s or greater, but only GMP '
             'version 5.0.0 was found' % required_version('Gmp'),
             [('GMP_PATH', 'gmp.h',
               [('__GNU_MP_VERSION', 5),
                ('__GNU_MP_VERSION_MINOR', 0),
                ('__GNU_MP_VERSION_PATCHLEVEL', 0)])])

expect_error('mpfr-too-old', [],
             'Ledger requires MPFR version %s or greater, but only MPFR '
             'version 3.0.0 was found' % required_version('Mpfr'),
             [('MPFR_PATH', 'mpfr.h',
               [('MPFR_VERSION_MAJOR', 3),
                ('MPFR_VERSION_MINOR', 0),
                ('MPFR_VERSION_PATCHLEVEL', 0)])])

sys.exit(1 if failures else 0)
