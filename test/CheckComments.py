import sys
import re
import os

ok = 100.0
if sys.argv[1] == '-l':
    ok = float(sys.argv[2])
    sys.argv = [sys.argv[0]] + sys.argv[3:]

debug = False
if sys.argv[1] == '-v':
    debug = True
    sys.argv = [sys.argv[0]] + sys.argv[2:]

errors = 0

for path in sys.argv[1:]:
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

    fd = open(path, 'r')
    for line in fd.readlines():
        linenum += 1

        match = re.search('/\*\*(.*)', line)
        if match:
            long_comment = re.sub('\s+', '', match.group(1))
            continue
        elif long_comment:
            match = re.search('(.*)\*/', line)
            if match:
                long_comment += re.sub('\s+', '', match.group(1))
                comment_count = len(long_comment)
                long_comment = None
            else:
                long_comment += re.sub('\s+', '', line[:-1])
            continue

        if brace_depth == 0:
            match = re.search('(namespace|enum|class|struct|union)', line)
            if match:
                kind = match.group(1)
                if debug: print "kind =", kind
            elif kind == "function":
                match = re.search('(\S+)\(', line)
                if match:
                    function_name = match.group(1)
                    long_comments[function_name] = comment_count
                    comment_count = 0
                    if debug: print "name found %s" % function_name

        if re.search('{', line) and not re.search('@{', line):
            if kind == "function":
                brace_depth += 1
                if debug: print "brace_depth =", brace_depth
            else:
                other_depth += 1
                kind = "function"
                if debug: print "other_depth =", other_depth

        if re.search('}', line) and not re.search('@}', line):
            if brace_depth > 0:
                brace_depth -= 1
                if debug: print "brace_depth =", brace_depth

                if brace_depth == 0:
                    if debug: print "function done"
                    if function_name in long_comments:
                        comment_count += long_comments[function_name]
                    if code_count == 0:
                        percent = ok
                        print "%7s  %4d/%4d  %s:%d: %s" % \
                            ("empty", comment_count, code_count,
                             os.path.basename(path), linenum,
                             function_name)
                        errors += 1
                    else:
                        percent = 100.0 * (float(comment_count) /
                                           float(code_count))
                    if percent < ok and not ctor_dtor:
                        print "%6.0f%%  %4d/%4d  %s:%d: %s" % \
                            (percent, comment_count, code_count,
                             os.path.basename(path), linenum,
                             function_name)
                        errors += 1
                    code_count    = 0
                    comment_count = 0
                    kind          = "function"
                    function_name = "<unknown>"
                    ctor_dtor     = False
            else:
                other_depth -= 1
                if debug: print "other_depth =", other_depth

        if brace_depth > 0:
            if re.search("TRACE_[CD]TOR", line):
                ctor_dtor = True

            line = re.sub('\s+', '', line[:-1])

            match = re.search('//(.*)', line)
            if match:
                comment = match.group(1)
                line = re.sub('//.*', '', line)
            else:
                comment = None

            if line:
                code_count    += len(line)
            if comment:
                comment_count += len(comment)

sys.exit(errors)
