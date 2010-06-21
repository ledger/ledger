#!/usr/bin/env python

import sys
import re
import os

from os.path import *
from subprocess import Popen, PIPE

ledger_binary = sys.argv[1]
source_topdir = sys.argv[2]

documented_options = []
for line in open(join(source_topdir, 'doc', 'ledger.1')):
    match = re.match('\.It Fl \\\\-([-A-Za-z]+)', line)
    if match:
        option = match.group(1)
        if option not in documented_options:
            documented_options.append(option)

pipe   = Popen('%s --debug option.names parse true' % ledger_binary,
               shell=True, stdout=PIPE, stderr=PIPE)
errors = 0

untested_options = [
    'anon',
    'args-only',
    'cache',
    'debug',
    'download',
    'file',
    'force-color',
    'force-pager',
    'full-help',
    'help',
    'help-calc',
    'help-comm',
    'help-disp',
    'import',
    'init-file',
    'no-color',
    'options',
    'price-db',
    'price-exp',
    'revalued-total',
    'script',
    'seed',
    'trace',
    'verbose',
    'verify',
    'version'
]

for line in pipe.stderr:
    match = re.search('\[DEBUG\] Option: (.*)', line)
    if match:
        option = match.group(1)

        option = re.sub('_', '-', option)
        option = re.sub('-$', '', option)

        if option not in untested_options and \
           not exists(join(source_topdir, 'test', 'baseline',
                           'opt-%s.test' % option)):
            print "Baseline test missing for --%s" % option
            errors += 1

        if option not in documented_options:
            print "Man page entry missing for --%s" % option
            errors += 1
        else:
            documented_options.remove(option)

known_alternates = [
    'cost',
    'first',
    'import',
    'last',
    'leeway',
    'period-sort'
]

for option in documented_options:
    if option not in known_alternates:
        print "Man page entry for unknown option --%s" % option

sys.exit(errors)
