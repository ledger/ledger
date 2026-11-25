#!/usr/bin/env python3
# Run the Python examples of the manual's "Extending with Python"
# chapter, so that they stay valid Python 3 and name only API the
# bindings provide.  Usage (from the source root):
#
#   ledger python test/regress/python-manual-examples.py \
#       doc/ledger3.texi test/input/sample.dat
#
# Every untagged @smallexample in the chapter is a Python script,
# except a shell transcript (a block with a line beginning "$ "),
# which is skipped and not numbered.  A script is compiled, then run
# with "sample.dat" pointing at the given journal, and its output
# lines are counted.  The one example that is a journal with a
# `python` directive has its directive body compiled only (its
# assertion is meant to fail).

import contextlib
import io
import re
import sys
import textwrap

texi_path, sample_path = sys.argv[1], sys.argv[2]

with open(texi_path, encoding="utf-8") as f:
    texi = f.read()

start = texi.index("@node Extending with Python,")
end = texi.index("@node Ledger for Developers,")
chapter = texi[start:end]

blocks = re.findall(r"^@smallexample\n(.*?)^@end smallexample$", chapter, re.S | re.M)
scripts = [re.sub(r"@([@{}])", r"\1", block) for block in blocks]
scripts = [code for code in scripts if not re.search(r"^\$ ", code, re.M)]

for number, code in enumerate(scripts, 1):
    if code.startswith("python\n"):
        body = []
        for line in code.splitlines()[1:]:
            if line and not line[0].isspace():
                break
            body.append(line)
        compile(textwrap.dedent("\n".join(body)), "example %d" % number, "exec")
        print("example %d: python directive compiles" % number)
        continue
    code = code.replace('"sample.dat"', repr(sample_path))
    compiled = compile(code, "example %d" % number, "exec")
    output = io.StringIO()
    with contextlib.redirect_stdout(output):
        exec(compiled, {"__name__": "__main__"})
    print("example %d: %d lines" % (number, len(output.getvalue().splitlines())))
