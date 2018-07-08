#!/usr/bin/env python

# convert.py: This script converts a Boost.Test unit test into an
# equivalent Python unit test.
#
# Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# - Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#
# - Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
#
# - Neither the name of New Artisans LLC nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import re
import sys
import os

source = os.path.abspath(sys.argv[1])
base   = os.path.splitext(source)[0]
target = os.path.abspath(sys.argv[2])

dirname = os.path.dirname(target)
if not os.path.isdir(dirname):
    try: os.makedirs(dirname)
    except: pass

fd = open(source, "r")
fo = open(target, "w")

fo.write('''# -*- coding: utf-8 -*-

import unittest
import exceptions
import operator

from ledger import *
from StringIO import *
from datetime import *

internalAmount = Amount.exact

class %sTestCase(unittest.TestCase):
    testSession = None''' % os.path.basename(base))

not_for_python = 0
class_name = None

for line in fd.readlines():
    if re.match('^#ifndef NOT_FOR_PYTHON', line):
        not_for_python += 1
        continue
    elif not_for_python > 0:
        if re.match('^#endif // NOT_FOR_PYTHON', line):
            not_for_python -= 1
        continue

    if re.match('^(using|CPP|[#{}/])', line):
        continue
    if re.match('^\s+[{}]\s+$', line):
        continue

    if not re.search('assert', line):
        match = re.match('^};', line)
        if match:
            continue
        match = re.match('BOOST_.*_TEST_SUITE', line)
        if match:
            continue

        match = re.match('^struct (.*?) {', line)
        if match:
            class_name = match.group(1)
            continue

        if class_name:
            match = re.search('(~)?%s\(\) {' % class_name, line)
            if match:
                if match.group(1):
                    fo.write('    def tearDown(self):\n')
                else:
                    fo.write('    def setUp(self):\n')
                continue

        match = re.match('BOOST_AUTO_TEST_CASE\((.+?)\)', line)
        if match:
            fo.write('    def %s(self):\n' % match.group(1))
            continue

        match = re.match('void [^:]+::(test[^(]+|setUp|tearDown)\(\)', line)
        if match:
            fo.write('    def %s(self):\n' % match.group(1))
            continue

        match = re.search('  ([a-z:_<>]+?)&?\s+([a-z0-9_]+)(\((.+?)\))?;', line)
        if match:
            if match.group(1) != "std::string":
                line = '  %s = %s(%s)\n' % (match.group(2), match.group(1),
                                            match.group(4) or "")
            else:
                line = ''

        match = re.search('  ([a-z:_<>]+?)&?\s+([a-z0-9]+)\s*=\s*([^(]+);', line)
        if match:
            line = '  %s = %s(%s)\n' % (match.group(2), match.group(1),
                                        match.group(3))

        match = re.search('  ([a-z:_<>]+?)\s+([a-z0-9]+)\s*=\s*(.+?)$', line)
        if match:
            line = '  %s = %s\n' % (match.group(2), match.group(3))

    line = re.sub('BOOST_CHECK_NE', 'self.assertNotEqual', line)
    line = re.sub('BOOST_CHECK_EQUAL', 'self.assertEqual', line)
    line = re.sub('BOOST_CHECK_THROW\(([^,]+), ([^,)]+?)\)',
                  'self.assertRaises(\\2, lambda: \\1)', line)
    line = re.sub('BOOST_CHECK', 'self.assertTrue', line)

    # jww (2010-06-20): Determine this list automatically by scanning
    # the class_ lines in src/py_*.cc
    line = re.sub('amount_t::precision_t\(([^)]+?)\)', '\\1', line)
    line = re.sub('amount_t::', 'Amount.', line)
    line = re.sub('Amount\.PARSE_', 'AmountParse.', line)
    line = re.sub('commodity_t\(([^)]+?)\)', '\\1', line)
    line = re.sub('commodity_t::', 'Commodity.', line)
    line = re.sub('balance_t::', 'Balance.', line)
    line = re.sub('balance_pair_t::', 'BalancePair.', line)
    line = re.sub('value_t::', 'Value.', line)

    line = re.sub('amount_t', 'Amount', line)
    line = re.sub('commodity_t', 'Commodity', line)
    line = re.sub('balance_t', 'Balance', line)
    line = re.sub('balance_pair_t', 'BalancePair', line)
    line = re.sub('value_t', 'Value', line)

    line = re.sub("PARSE_DEFAULT",    "ParseFlags.Default", line)
    line = re.sub("PARSE_PARTIAL",    "ParseFlags.Partial", line)
    line = re.sub("PARSE_SINGLE",     "ParseFlags.Single", line)
    line = re.sub("PARSE_NO_MIGRATE", "ParseFlags.NoMigrate", line)
    line = re.sub("PARSE_NO_REDUCE",  "ParseFlags.NoReduce", line)
    line = re.sub("PARSE_NO_ASSIGN",  "ParseFlags.NoAssign", line)
    line = re.sub("PARSE_NO_DATES",   "ParseFlags.NoDates", line)
    line = re.sub("PARSE_OP_CONTEXT", "ParseFlags.OpContext", line)
    line = re.sub("PARSE_SOFT_FAIL",  "ParseFlags.SoftFail", line)

    line = re.sub('ledger::', '', line)
    line = re.sub('std::istringstream', 'StringIO', line)
    line = re.sub('std::ostringstream', 'StringIO', line)
    line = re.sub('set_session_context\(&session\)',
                  'self.testSession = session()\n        set_session_context(self.testSession)', line)
    line = re.sub('set_session_context\(\)',
                  'set_session_context()\n        self.testSession = None', line)
    line = re.sub('([a-z_]+?)_t\b', '\\1', line)
    line = re.sub('("[^"]+")', 'u\\1', line)
    line = re.sub('std::string\(([^)]+?)\)', '\\1', line)
    line = re.sub('string\(([^)]+?)\)', '\\1', line)
    line = re.sub('\.print\(([^)]+?)\)', '.print_(\\1)', line)
    line = re.sub('true', 'True', line)
    line = re.sub('false', 'False', line)
    line = re.sub('CURRENT_TIME\(\)', 'datetime.now()', line)
    line = re.sub('CURRENT_DATE\(\)', 'date.today()', line)
    line = re.sub('commodity\(\)', 'commodity', line)
    line = re.sub('precision\(\)', 'precision', line)
    line = re.sub('([0-9]+)[FL]', '\\1', line)
    line = re.sub('([0-9]+)UL', '\\1L', line)
    line = re.sub(';', '', line)
    line = re.sub('//', '#', line)
    line = re.sub('->', '.', line)
    line = re.sub('(\s+|\()(\S+?) \? (.+?) : (.+?)\)',
                  '\\1\\3 if \\2 else \\4)', line)
    line = re.sub('if \((.+?)\)( {)?$', 'if \\1:', line)
    line = re.sub('(} )?else( {)?$', 'else:', line)

    line = re.sub('amount_error', 'exceptions.ArithmeticError', line)
        
    match = re.match('^  ', line)
    if match:
        fo.write('      ' + line)
    else:
        fo.write(line)

fo.write('''
 
def suite():
    return unittest.TestLoader().loadTestsFromTestCase(%sTestCase)

if __name__ == '__main__':
    unittest.main()
''' % os.path.basename(base))

fo.close()
fd.close()
