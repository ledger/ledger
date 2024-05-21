#!/usr/bin/env python3

import sys
import re
import os
import argparse

parser = argparse.ArgumentParser( prog='CheckComments')
parser.add_argument('-v', '--debug', dest='debug', action='store_true')
parser.add_argument('-l', '--limit', dest='ok', type=float, default=100.0)
parser.add_argument('path', nargs='+', help='Path to source file to check comments')
args = parser.parse_args()

errors = 0

for path in args.path:
    other_depth   = 0
    brace_depth   = 0
    code_count    = 0
    comment_count = 0
    long_comment  = None
    long_comments = {}
    kind          = "function"
    function_name = "<unknown>"
    ctor_dtor     = False
    linenum       = 0

    print(f'checking {path}')
    fd = open(path, 'r')
    for line in fd.readlines():
        linenum += 1

        match = re.search(r'/\*\*(.*)', line)
        if match:
            long_comment = re.sub(r'\s+', '', match.group(1))
            continue
        elif long_comment:
            match = re.search(r'(.*)\*/', line)
            if match:
                long_comment += re.sub(r'\s+', '', match.group(1))
                comment_count = len(long_comment)
                long_comment = None
            else:
                long_comment += re.sub(r'\s+', '', line[:-1])
            continue

        if brace_depth == 0:
            match = re.search(r'(namespace|enum|class|struct|union)', line)
            if match:
                kind = match.group(1)
                if args.debug: print("kind =", kind)
            elif kind == "function":
                match = re.search(r'(\S+)\(', line)
                if match:
                    function_name = match.group(1)
                    long_comments[function_name] = comment_count
                    comment_count = 0
                    if args.debug: print("name found %s" % function_name)

        if re.search(r'{', line) and not re.search('@{', line):
            if kind == "function":
                brace_depth += 1
                if args.debug: print("brace_depth =", brace_depth)
            else:
                other_depth += 1
                kind = "function"
                if args.debug: print("other_depth =", other_depth)

        if re.search(r'}', line) and not re.search('@}', line):
            if brace_depth > 0:
                brace_depth -= 1
                if args.debug: print("brace_depth =", brace_depth)

                if brace_depth == 0:
                    if args.debug: print("function done")
                    if function_name in long_comments:
                        comment_count += long_comments[function_name]
                    if code_count == 0:
                        percent = args.ok
                        print("%7s  %4d/%4d  %s:%d: %s" % \
                            ("empty", comment_count, code_count,
                             os.path.basename(path), linenum,
                             function_name))
                        errors += 1
                    else:
                        percent = 100.0 * (float(comment_count) /
                                           float(code_count))
                    if percent < args.ok and not ctor_dtor:
                        print("%6.0f%%  %4d/%4d  %s:%d: %s" % \
                            (percent, comment_count, code_count,
                             os.path.basename(path), linenum,
                             function_name))
                        errors += 1
                    code_count    = 0
                    comment_count = 0
                    kind          = "function"
                    function_name = "<unknown>"
                    ctor_dtor     = False
            else:
                other_depth -= 1
                if args.debug: print("other_depth =", other_depth)

        if brace_depth > 0:
            if re.search(r'TRACE_[CD]TOR', line):
                ctor_dtor = True

            line = re.sub(r'\s+', '', line[:-1])

            match = re.search(r'//(.*)', line)
            if match:
                comment = match.group(1)
                line = re.sub(r'//.*', '', line)
            else:
                comment = None

            if line:
                code_count    += len(line)
            if comment:
                comment_count += len(comment)

sys.exit(errors)
