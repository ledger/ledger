#!/usr/bin/env python3
"""Check that the installed header tree is complete.

Every header named in LEDGER_INCLUDES (src/CMakeLists.txt) is installed
under include/ledger.  If one of them includes another src/ header with
#include "...", that header must be installed too, or a program that
includes the installed headers cannot compile.

Usage: installed-headers.py SOURCE_DIR
"""

import re
import sys
from pathlib import Path

src = Path(sys.argv[1]) / 'src'
cmake = (src / 'CMakeLists.txt').read_text()

installed = set()
for body in re.findall(r'(?:set|list)\(\s*(?:APPEND\s+)?LEDGER_INCLUDES\s([^)]*)\)', cmake):
    installed.update(word for word in body.split() if word.endswith('.h'))

if 'journal.h' not in installed:
    print('could not read LEDGER_INCLUDES from %s' % (src / 'CMakeLists.txt'))
    sys.exit(1)

missing = []
for header in sorted(installed):
    path = src / header
    if not path.exists():
        continue
    for line in path.read_text(errors='replace').splitlines():
        m = re.match(r'\s*#\s*include\s+"([^"]+)"', line)
        if m and (src / m.group(1)).exists() and m.group(1) not in installed:
            missing.append('%s includes %s, which is not installed' % (header, m.group(1)))

for message in missing:
    print(message)
sys.exit(1 if missing else 0)
