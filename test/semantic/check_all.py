#!/usr/bin/env python3
"""Suite-wide oracle bisimulation: balances and balance rejections.

With --gross, every compared file is additionally compared on its
gross debit and credit totals against C++
`balance --flat --no-total --amount '(amount > 0 ? amount : 0)'` and
its negative counterpart (the column sums of the --dc option's
report expression, src/report.h at 76b9dc2b2:752-754), binned
GROSS:PASS, GROSS:FAIL or GROSS:SKIP:<reason>, with the refusal
ratchet of gross-refusals.txt and a telescoping detector whose net
rows are equal while its gross rows differ.  The gross comparison
has a blind spot of its own: an error the net rows hide stays hidden
under it only when it cancels in every account it touches and within
each account's debits and within its credits (the ledger-semantics README, the
--gross paragraph of "The oracle"); the refusal reason
lot-annotated-completion, whose conditions Journal.Result.lotFanout
in the oracle records, is the known shape that cannot be reproduced
in column sums at all.

With --value, every compared file whose own `bal`, `balance` or `b`
command lines name one exchange target T (`-X T`, `-XT`, `--exchange
T`, `--exchange=T`) is additionally compared on its market-valued
balances: the oracle receives `--exchange T` for that file and its
`>> value` rows are compared with C++
`balance --flat --no-total -X T`, binned VALUE:PASS, VALUE:FAIL or
VALUE:SKIP:<reason>, with the refusal ratchet of value-refusals.txt
and a detector whose net rows are equal while a planted C++-only
price move changes its value rows.  A file whose commands name the
target in any other form (several targets, or one written with a
comma, colon or equals sign, as the list and conversion forms read)
is binned VALUE:SKIP:spec-form and not compared; the ratchet holds
those files as well, so every skip the comparison reports is one
the ratchet records.  When the oracle
cannot value the file as C++ does it prints one line
`?? exchange REASON [COMM]` instead of its value rows, keeping its
net rows (the ledger-semantics README, the --exchange paragraph of "The
oracle"); an announced REASON is a visible skip by reason, an
unannounced one a FAIL.  A file whose commands carry --value-expr
and the related options is refused by the oracle (value-expression;
the oracle models no value expression, S46) and so never compared.

For EVERY positive `.test` file in test/baseline, test/regress, and
test/manual — positive meaning no test block expects a nonzero exit
(`-> N`) or an `__ERROR__` section — the journal portion (everything
before the first `test` block, exactly as RegressTests.py delineates
it) is replayed through BOTH the C++ ledger (`balance --flat
--no-total`) and the Lean oracle driver, and the normalized balance
triples must agree. This runs as the `SemanticBisimulation` ctest, so
`make check` / `make test` / `ctest` all include it.

Classification (every file lands in exactly one bin; the artifact
lists them all — absence of output is never silent success):

  PASS            — oracle and C++ agree exactly.
  FAIL            — divergence (unadjudicated; each one either
                    exposes a C++ defect or a convention the oracle
                    has not yet recorded — see the bisimulation
                    section of the ledger-semantics README).
  NEG:unbalanced  : a negative test whose every expected error is
                    "Transaction does not balance" (the strict
                    classifier below): C++ rejects the extracted
                    journal as unbalanced and the oracle rejects it
                    with a balance reason at the same entry, except
                    for visibly counted include-context skips.  A
                    NEG:unbalanced file on which either side disagrees
                    is a FAIL.
  SKIP:negative   : any other test that intentionally exercises
                    errors; its journal is not claimed valid.
  SKIP:no-journal — no journal text before the first test block.
  SKIP:out-of-scope — the oracle parser rejected the journal, with
                    the construct named; the goal state, reached by
                    the current oracle, is zero entries here.
  SKIP:cpp-parse  — plain `ledger balance` cannot parse the journal
                    without the test's own flags (comparison tuple
                    mismatch, e.g. --input-date-format); recorded,
                    not compared.
  SKIP:python     — the journal embeds a `python` block: embedded
                    Python is a Turing-complete escape from the
                    journal language, so the file is inherently
                    non-comparable.

The strict NEG classifier admits a negative test only when (a) its
__ERROR__ sections contain at least one `Error:` line and every
`Error:` line is exactly "Error: Transaction does not balance", (b)
no test command reads a journal other than the file itself (`-f`
or `--file` naming anything but $FILE; noncanonical long file aliases
and short-option bundles containing `f` are conservatively excluded),
(c) every `include` in the extracted journal resolves to files next to
the test, and (d) that journal embeds no python block.  Its per-test
flags are derived exactly as for positive tests.  The oracle's rejection
must match NEG_REASON_RE, the balance reason documented in the oracle
protocol section of the ledger-semantics README: `line N: unbalanced transaction ...`,
prefixed by `periodic transaction: ` for a `~` template.  The oracle
models non-permissive balancing, so the C++ side of this comparison
runs without --permissive.  C++ must exit with a positive error status;
signal termination is a failure even if it printed a balance error.
Without includes, the first C++ rejection must name the oracle's
transaction header, or, for a periodic template, a line in the same
indented block.  The oracle numbers include-spliced lines; C++ reports
file-local positions, even for entries in the main file after an include.
The entry check is therefore conservatively skipped for any journal
containing an include line, provided C++ reports a rejection context.
The summary counts these files and the artifact lists them under
neg_line_check_skipped and in details.  Missing contexts fail the
comparison.  An include skip confirms only rejection for imbalance,
not agreement on the rejected entry.

The oracle's extension lines are part of its opt-in output protocol
(the ledger-semantics README, "Building and testing").  Lines beginning `>> dr `,
`>> cr ` or `?? gross ` are gross-comparison output, which the oracle
prints only for a file whose arguments carry `--gross`; the harness
requests it, per file, exactly when running with --gross.  Lines
beginning `>> value ` or `?? exchange ` are value-comparison output,
which the oracle prints only for a file whose arguments carry
`--exchange`; the harness requests it, per file, exactly when
--value runs and the file's own commands name one target.  Any such
line in a file whose arguments do not request it is a FAIL
("unrequested protocol extension line"): a leak into the default
output must never be dropped silently, because a comparator running
without the flag would read it as part of the balance rows.

Exit codes: 0 all comparisons pass and the ratchet bins are empty;
1 any FAIL, any GROSS:FAIL, any VALUE:FAIL, or any entry in
SKIP:out-of-scope or SKIP:cpp-parse (both reached zero against the
full suite and are held there — an entry reappearing means one side
stopped reading a journal it used to read, which is a regression,
not a skip); 77 the Lean toolchain or a built oracle is unavailable
(ctest SKIP_RETURN_CODE — skipped, visibly); 2 the harness itself
cannot run (missing binary, missing fixtures, a missing value
refusal ratchet, oracle invocation failure).
"""

import argparse
import datetime
import json
import os
import posixpath
import re
import shlex
import signal
import subprocess
import shutil
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import sweep  # normalization + C++ balance parsing

TEST_RE = re.compile(r"^test\s+(.*?)(?:\s*->\s*([0-9]+))?\s*$")
# The only expected error a NEG:unbalanced test may carry
# (src/xact.cc, xact_base_t::finalize and verify).
BALANCE_ERROR = "Error: Transaction does not balance"
# The oracle's balance rejection, as documented in the ledger-semantics README.
NEG_REASON_RE = re.compile(
    r"^(?:periodic transaction: )?line \d+: unbalanced transaction\b")
ORACLE_LINE_RE = re.compile(r"^(periodic transaction: )?line (\d+):")
# src/xact.cc:finalize/verify and src/item.cc:item_context name the entry;
# src/textual.cc:parse names the last parsed line of a periodic template.
CPP_BALANCING_RE = re.compile(
    r'^While balancing transaction from "([^"]*)", lines? (\d+)(?:-\d+)?:$', re.M)
CPP_PARSING_RE = re.compile(r'^While parsing file "([^"]*)", line (\d+):$', re.M)
CPP_CONTEXT_RE = re.compile(
    r'^While (?:parsing file|balancing transaction from) "([^"]*)"', re.M)
# No C++ context names no entry: the same-entry check fails closed.
NO_CONTEXT_REASON = ("C++ rejection context not found; the rejected"
                     " entry cannot be confirmed")

# The oracle's gross output (the ledger-semantics README, the --gross flag), and the
# C++ value expressions whose column sums the two sides agree on
# (src/report.h, the --dc option's amount expression at
# 76b9dc2b2:752-754).
GROSS_DR_EXPR = "(amount > 0 ? amount : 0)"
GROSS_CR_EXPR = "(amount < 0 ? -amount : 0)"
# The oracle's value output (the ledger-semantics README, the --exchange flag).
EXTENSION_PREFIXES = (">> dr ", ">> cr ", ">> value ",
                      "?? gross ", "?? exchange ")
# gross-refusals.txt: REASON<TAB>repo-relative path, one per line;
# '#' opens a comment.  The ratchet holds the measured refusal set.
GROSS_REFUSALS = os.path.join(HERE, "gross-refusals.txt")
GROSS_DIVERGENT = os.path.join(HERE, "gross-divergent.txt")
NEG_PERMISSIVE = os.path.join(HERE, "neg-oracle-permissive.txt")
ORACLE_DIVERGENT = os.path.join(HERE, "oracle-divergent.txt")
GROSS = {"PASS": [], "FAIL": [], "SKIP": []}
GROSS_ROWS = {"dr": 0, "cr": 0, "files": 0}
# value-refusals.txt: the same ratchet shape, for the `?? exchange`
# refusals of the value comparison.
VALUE_REFUSALS = os.path.join(HERE, "value-refusals.txt")
# The reasons of `?? exchange REASON [COMM]` that the oracle announces
# (ledger-semantics S48: Ledger/Driver.lean, README.md, AGENTS.md).
# Any other reason is a FAIL.
VALUE_REASONS = frozenset({
    "no-terminus", "price-expr", "unread-price", "exchange-order",
    "value-expression", "posting-date", "fixated-lot", "equivalence",
    "commodity-alias", "price-precision", "null-commodity", "percent",
    "time-unit", "no-price-path", "alternative-price-path",
    "zero-inverse-price", "rounds-to-zero"})
# Report commands a `test` line may name; only `bal`, `balance` and
# `b` lines name exchange targets the comparison follows.
REPORT_COMMANDS = {"bal", "balance", "b", "reg", "register", "r",
                   "print", "p", "budget", "equity", "cleared", "stats",
                   "prices", "pricedb", "pricemap", "xact", "entry",
                   "source", "csv", "accounts", "payees", "commodities",
                   "tags", "select", "convert", "emacs", "xml", "json",
                   "lisp", "echo", "script", "python", "server", "org"}
VALUE = {"PASS": [], "FAIL": [], "SKIP": []}
VALUE_ROWS = {"rows": 0, "files": 0}


def value_targets(path):
    """The exchange targets named on the file's own `bal`, `balance`
    or `b` command lines (`-X T`, `-XT`, `--exchange T`,
    `--exchange=T`), in order of appearance; [] when none."""
    targets = []
    try:
        with open(path, encoding="utf-8") as fh:
            lines = fh.read().splitlines()
    except UnicodeDecodeError:
        return targets
    for line in lines:
        m = TEST_RE.match(line.rstrip("\r"))
        if not m:
            continue
        try:
            toks = shlex.split(m.group(1))
        except ValueError:
            toks = m.group(1).split()
        cmd = next((t for t in toks if t in REPORT_COMMANDS), None)
        if cmd not in ("bal", "balance", "b"):
            continue
        for i, tk in enumerate(toks):
            t = None
            if tk in ("-X", "--exchange") and i + 1 < len(toks):
                t = toks[i + 1]
            elif tk.startswith("--exchange="):
                t = tk.split("=", 1)[1]
            elif tk.startswith("-X") and len(tk) > 2:
                t = tk[2:]
            if t is not None:
                targets.append(t.replace("\\", "").strip())
    return targets


def command_flags(line, cpp_flags):
    """Fold the input-affecting flags of one `test` command line into
    cpp_flags (first occurrence wins); True if it names
    --decimal-comma."""
    dc = False
    toks = line.split()
    for i, tk in enumerate(toks):
        if tk == "--now" and i + 1 < len(toks):
            pair = [tk, toks[i + 1].strip("'\"")]
            if pair[0] not in cpp_flags:
                cpp_flags.extend(pair)
        elif tk.startswith("--now="):
            if "--now" not in cpp_flags:
                cpp_flags.extend(["--now", tk.split("=", 1)[1].strip("'\"")])
        elif tk == "--recursive-aliases":
            if tk not in cpp_flags:
                cpp_flags.append(tk)
        elif tk == "--input-date-format" and i + 1 < len(toks):
            pair = [tk, toks[i + 1].strip("'\"")]
            if pair[0] not in cpp_flags:
                cpp_flags.extend(pair)
        elif tk.startswith("--input-date-format="):
            if "--input-date-format" not in cpp_flags:
                cpp_flags.extend(["--input-date-format",
                                  tk.split("=", 1)[1].strip("'\"")])
        elif tk == "--decimal-comma":
            dc = True
            if tk not in cpp_flags:
                cpp_flags.append(tk)
    return dc


def has_python_block(journal):
    """A python block anywhere makes a journal non-comparable."""
    return any(ln == "python" or ln.startswith("python ")
               for ln in (l.strip() for l in journal.splitlines()))


def has_eval_preamble(journal):
    """C++ prints the value of eval/expr directives at the head of
    every report; the balance comparison strips that preamble by
    parsing, the gross one must strip it by offset.  The text passed
    here is the journal plus its copied include closure, since a
    directive inside an include prints into the report head just the
    same; over-approximating is harmless, because the no-limit probe
    then prints nothing and no strip is applied."""
    return re.search(r"^(?:eval|expr)\s", journal, re.M) is not None


def classify_negative_test(path):
    """-> (journal_text, cpp_flags, dc) when a negative test meets the
    strict NEG classifier (module docstring), else None."""
    try:
        with open(path, encoding="utf-8") as fh:
            text = fh.read()
    except UnicodeDecodeError:
        return None
    journal_lines, commands, errors = [], [], []
    in_journal, in_error, negative = True, False, False
    cpp_flags, dc = [], False
    for line in text.splitlines():
        line = line.rstrip("\r")
        if line.startswith("test "):
            in_journal, in_error = False, False
            commands.append(line)
            m = TEST_RE.match(line)
            if m and m.group(2) and m.group(2) != "0":
                negative = True
            dc = command_flags(line, cpp_flags) or dc
        elif in_journal:
            journal_lines.append(line)
        elif line.startswith("__ERROR__"):
            negative = in_error = True
        elif line.rstrip() == "end test":
            in_error = False
        elif in_error and line.startswith("Error:"):
            errors.append(line.strip())
    if not negative or not errors or any(e != BALANCE_ERROR for e in errors):
        return None
    for command in commands:
        try:
            tokens = iter(shlex.split(command))
        except ValueError:
            return None
        for token in tokens:
            if token in ("-f", "--file"):
                if next(tokens, None) != "$FILE":
                    return None
            elif token.startswith("--file="):
                if token[len("--file="):] != "$FILE":
                    return None
            elif token.split("=", 1)[0] in ("--f", "--file_", "--file-", "--f_", "--f-"):
                # src/option.cc:find_option and src/option.h:OPT_ accept these aliases.
                return None
            elif token.startswith("-") and not token.startswith("--") and "f" in token[1:]:
                # src/option.cc:process_arguments reads short-option bundles,
                # not attached operands: -fPATH does not mean -f PATH.
                return None
    journal = "\n".join(journal_lines) + "\n"
    if has_python_block(journal):
        return None
    for m in INCLUDE_RE.finditer(journal):
        name = m.group(1)
        if len(name) >= 2 and name[0] in "'\"" and name[-1] == name[0]:
            name = name[1:-1]
        matches = include_matches(os.path.dirname(path), name)
        if not matches or not all(os.path.isfile(p) for p in matches):
            return None
    if "--now" not in cpp_flags:
        cpp_flags.extend(["--now", "2026/08/26"])  # as for positives
    return journal, cpp_flags, dc


def negative_classifier_failures(directory, journal):
    """Exercise the NEG admission boundary with extracted-journal fixtures."""
    journal = journal.rstrip("\n") + "\n"
    block = "test balance -> 1\n__ERROR__\n" + BALANCE_ERROR + "\nend test\n"
    text = journal + block
    cases = [("balance error", text, True),
             ("positive", journal + "test balance\nend test\n", False),
             ("no error reason", journal + "test balance -> 1\nend test\n", False),
             ("other error", text.replace(BALANCE_ERROR, "Error: other"), False),
             ("mixed errors", text.replace(BALANCE_ERROR,
                                           BALANCE_ERROR + "\nError: other"), False),
             ("python", "python\npass\nend python\n" + text, False),
             ("missing include", "include missing.dat\n" + text, False),
             ("include", "include selftest-neg-control.dat\n" + text, True),
             ("directory include", "include .\n" + text, False)]
    for flag in ('-f $FILE', '--file "$FILE"', '--file=$FILE'):
        cases.append((flag, text.replace("balance ->", "balance " + flag + " ->"), True))
    for flag in ('-f"$FILE"', '-f other.dat', '-fother.dat', '--file=other.dat',
                 '--file "other file.dat"', '"-f" other.dat', '-f', '--file',
                 '-Vf other.dat', '-fV other.dat', '-Vf "$FILE"',
                 '--f other.dat', '--f "$FILE"'):
        cases.append((flag, text.replace("balance ->", "balance " + flag + " ->"), False))
    for alias in ('--f', '--file_', '--file-', '--f_', '--f-'):
        for name in ('other.dat', '$FILE'):
            flag = alias + '=' + name
            cases.append((flag, text.replace("balance ->", "balance " + flag + " ->"), False))
    cases.append(("bad quoting", text.replace("balance ->", 'balance " ->'), False))
    failures = []
    path = os.path.join(directory, "selftest-neg-classifier.test")
    for name, fixture, want in cases:
        with open(path, "w", encoding="utf-8") as fh:
            fh.write(fixture)
        if (classify_negative_test(path) is not None) != want:
            failures.append(name)
    return failures


def same_entry(reason, stderr, cpp_path, journal):
    """-> (failure reasons, include skip) for the first C++ rejection."""
    m = ORACLE_LINE_RE.match(reason)
    if not m:
        return ["the oracle's reason names no line"], False
    periodic, n = bool(m.group(1)), int(m.group(2))
    # C++ can recover and report more errors; later contexts prove nothing
    # about the first rejection (src/textual.cc:parse).
    context = re.split(r"^Error:", stderr, maxsplit=1, flags=re.M)[0]

    def same(path):
        return os.path.realpath(path) == os.path.realpath(cpp_path)

    first = CPP_CONTEXT_RE.search(context)
    # Driver.splice numbers expanded lines, while src/item.cc:item_context
    # and src/textual.cc:parse retain file-local positions. Even a main-file
    # line can collide with a different expanded entry after an include.
    if first and INCLUDE_RE.search(journal):
        return [], True
    bal = CPP_BALANCING_RE.search(context)
    if bal and same(bal.group(1)):
        a = int(bal.group(2))
        if periodic or n != a:
            return [f"oracle rejected a different entry: line {n}, C++ the"
                    f" transaction at line {a}"], False
        return [], False
    if not bal and periodic:
        parsed = CPP_PARSING_RE.search(context)
        if parsed and same(parsed.group(1)):
            last = int(parsed.group(2))
            lines = journal.splitlines()
            if (1 <= n <= last <= len(lines)
                    and all(l[:1] in (" ", "\t") for l in lines[n:last])):
                return [], False
            return [f"oracle rejected a different entry: line {n}, C++ the"
                    f" template ending at line {last}"], False
    return [NO_CONTEXT_REASON], False


def neg_failures(oracle_res, cpp, cpp_path, journal):
    """-> (failure reasons, include skip) for a NEG:unbalanced comparison.
    oracle_res is the oracle's result for the file (a set of rows,
    ("ERROR", reason), or None); cpp is the C++ `balance` run on cpp_path,
    whose text is journal.  A skip must be reported, not silently passed."""
    why = []
    errors = [line.strip() for line in cpp.stderr.splitlines()
              if line.startswith("Error:")]
    if cpp.returncode <= 0 or not errors or any(e != BALANCE_ERROR for e in errors):
        why.append("C++ did not reject the extracted journal as unbalanced")
    if oracle_res is None:
        why.append("no oracle output for journal")
    elif not isinstance(oracle_res, tuple):
        why.append("oracle accepted a journal classified as unbalanced")
    elif not NEG_REASON_RE.match(oracle_res[1]):
        why.append("oracle rejected it for another reason: "
                   + oracle_res[1][:160])
    if why:
        return why, False
    return same_entry(oracle_res[1], cpp.stderr, cpp_path, journal)


def classify_test_file(path):
    """-> (journal_text or None, skip_reason or None, cpp_flags, dc)"""
    try:
        with open(path, encoding="utf-8") as fh:
            text = fh.read()
    except UnicodeDecodeError:
        return None, "out-of-scope: non-utf8", [], False
    journal_lines = []
    in_journal = True
    negative = False
    cpp_flags = []
    dc = False
    for line in text.splitlines():
        line = line.rstrip("\r")
        if line.startswith("test "):
            in_journal = False
            m = TEST_RE.match(line)
            if m and m.group(2) and m.group(2) != "0":
                negative = True
            # input-affecting flags of the test's own command become
            # part of the comparison tuple for this file (report
            # options such as -X stay out: the net comparison runs
            # the default report, and the value comparison reads its
            # targets from these lines separately)
            dc = command_flags(line, cpp_flags) or dc

        elif in_journal:
            journal_lines.append(line)
        elif line.startswith("__ERROR__"):
            negative = True
    if negative:
        return None, "negative", [], False
    if not any(l.strip() and not l.lstrip().startswith(";")
               for l in journal_lines):
        return None, "no-journal", [], False
    if "--now" not in cpp_flags:
        # Deterministic "current time", passed identically to BOTH
        # sides: dangling timeclock check-ins auto-close at --now
        # (timelog.cc close()), so an unpinned clock would make the
        # comparison depend on the day it runs.  The value itself is
        # arbitrary; any fixed date later than every journal date in
        # the corpus behaves the same, and this one is kept only
        # because the zero-divergence baseline was measured with it.
        cpp_flags.extend(["--now", "2026/08/26"])
    return "\n".join(journal_lines) + "\n", None, cpp_flags, dc


# src/textual.cc:read_line/read_next_directive and
# src/textual_directives.cc:general_directive strip a BOM and up to two prefixes.
# The characters mask_t::assign_glob() (src/mask.cc) escapes when it
# reads an include path component as a regex.
GLOB_META = "\\^$.|?*+()[{}"


def glob_regex(comp):
    """The regex mask_t::assign_glob() builds for one include path
    component.  `?` and `*` are wildcards, a `[` opens a set that runs
    to the next `]` and is an ordinary character when no `]` follows, a
    backslash makes the next character literal, and every other
    character is literal."""
    out = []
    i = 0
    while i < len(comp):
        c = comp[i]
        close = comp.find("]", i + 1) if c == "[" else -1
        if c == "?":
            out.append(".")
        elif c == "*":
            out.append(".*")
        elif close != -1:
            out.append(comp[i:close + 1])
            i = close
        else:
            if c == "\\" and i + 1 < len(comp):
                i += 1
                c = comp[i]
            out.append("\\" + c if c in GLOB_META else c)
        i += 1
    return "".join(out)


def include_matches(src_dir, pattern):
    """The files `include PATTERN` reads, resolved against src_dir the
    way include_directive() resolves it: the path is normalized, then
    each component is matched against the entries of the directories
    reached so far, descending only into directories and keeping only
    regular files."""
    pattern = posixpath.normpath(pattern)
    found = [src_dir]
    if pattern.startswith("/"):
        found, pattern = ["/"], pattern.lstrip("/")
    comps = pattern.split("/")
    for k, comp in enumerate(comps):
        if comp in (os.curdir, os.pardir):
            found = [os.path.join(d, comp) for d in found]
        else:
            try:
                rx = re.compile(glob_regex(comp), re.DOTALL)
            except re.error:
                return []
            found = [os.path.join(d, name)
                     for d in found if os.path.isdir(d)
                     for name in sorted(os.listdir(d)) if rx.fullmatch(name)]
        keep = os.path.isfile if k == len(comps) - 1 else os.path.isdir
        found = [p for p in found if keep(p)]
    return found


INCLUDE_RE = re.compile(r"^\s*\ufeff?[@!]{0,2}include\s+(.+?)\s*$", re.M)


def copy_includes(journal, src_dir, dst_dir, depth=4):
    """Copy files referenced by include lines (globs included) next to
    the extracted journal so both sides resolve them identically.
    Returns the texts of the copied files and of their own includes,
    for callers that need the include closure's content."""
    if depth == 0:
        return ""
    texts = []
    # A relative include may step above the journal's own directory
    # (test/regress does this), which lands the copy in the shared
    # session directory one level up.  That is allowed; escaping the
    # session directory itself is not.
    session_root = os.path.dirname(os.path.abspath(dst_dir))
    for m in INCLUDE_RE.finditer(journal):
        name = m.group(1)
        if len(name) >= 2 and name[0] in "'\"" and name[-1] == name[0]:
            name = name[1:-1]
        for src in include_matches(src_dir, name):
            rel = os.path.relpath(src, src_dir)
            dst = os.path.join(dst_dir, rel)
            if not os.path.abspath(dst).startswith(session_root + os.sep):
                continue
            if os.path.isfile(src) and not os.path.exists(dst):
                os.makedirs(os.path.dirname(dst), exist_ok=True)
                with open(src, "rb") as fi, open(dst, "wb") as fo:
                    fo.write(fi.read())
                with open(src, encoding="utf-8", errors="replace") as fh:
                    inc = fh.read()
                texts.append(inc)
                texts.append(copy_includes(inc, src_dir, dst_dir,
                                           depth - 1))
    return "".join(texts)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ledger", default=None)
    ap.add_argument("--source", default=None)
    ap.add_argument("--gross", action="store_true",
                    help="also compare each file's gross debit and "
                         "credit totals (oracle --gross; off by default, "
                         "D27)")
    ap.add_argument("--gross-refusals", default=GROSS_REFUSALS,
                    help="the ratchet file holding the expected "
                         "GROSS:SKIP set (for the self-test)")
    ap.add_argument("--gross-divergent", default=GROSS_DIVERGENT,
                    help="the ratchet file holding the files whose "
                         "gross rows are known to diverge")
    ap.add_argument("--neg-permissive", default=NEG_PERMISSIVE,
                    help="the ratchet file holding the unbalanced "
                         "negatives the oracle is known to accept")
    ap.add_argument("--oracle-divergent", default=ORACLE_DIVERGENT,
                    help="the ratchet file holding the files whose "
                         "balance rows are known to diverge from the "
                         "oracle's")
    ap.add_argument("--value", action="store_true",
                    help="also compare each file's market-valued "
                         "balances, where its own bal lines name one "
                         "exchange target (oracle --exchange; off by "
                         "default, D30)")
    ap.add_argument("--value-refusals", default=VALUE_REFUSALS,
                    help="the ratchet file holding the expected "
                         "VALUE:SKIP set (for the self-test)")
    ap.add_argument("dirs", nargs="*")
    args = ap.parse_args()
    gross_on = args.gross
    value_on = args.value
    # The known-divergence ratchets, loaded up front so the per-file
    # comparisons can consult them: a listed file is binned with a
    # visible skip instead of a FAIL, and the ratchet check after the
    # loop confirms every listed file still diverges.
    divergent_listed = set()
    if gross_on and os.path.isfile(args.gross_divergent):
        with open(args.gross_divergent, encoding="utf-8") as fh:
            for ln in fh:
                ln = ln.rstrip("\n")
                if ln and not ln.startswith("#"):
                    divergent_listed.add(ln.strip())
    permissive_listed = set()
    if os.path.isfile(args.neg_permissive):
        with open(args.neg_permissive, encoding="utf-8") as fh:
            for ln in fh:
                ln = ln.rstrip("\n")
                if ln and not ln.startswith("#"):
                    permissive_listed.add(ln.strip())
    divergent_rows_listed = set()
    if os.path.isfile(args.oracle_divergent):
        with open(args.oracle_divergent, encoding="utf-8") as fh:
            for ln in fh:
                ln = ln.rstrip("\n")
                if ln and not ln.startswith("#"):
                    divergent_rows_listed.add(ln.strip())
    repo = os.path.abspath(args.source) if args.source else sweep.REPO
    ledger = args.ledger or os.path.join(repo, "build", "ledger")
    env = {**os.environ, "TZ": os.environ.get("TZ", "America/Chicago")}
    dirs = args.dirs or [os.path.join(repo, "test", d)
                         for d in ("baseline", "regress", "manual")]

    lean_dir = os.environ.get("LEDGER_LEAN_DIR", "")
    if not lean_dir or not os.path.isfile(
            os.path.join(lean_dir, "lakefile.lean")):
        print("SKIP: LEDGER_LEAN_DIR names no oracle tree (the pinned one"
              " is `nix build .#oracle --no-link --print-out-paths`)")
        sys.exit(77)
    # Probe the toolchain exactly once, up front.  The Nix build points
    # LEDGER_LEAN_DIR at the oracle the flake pins and puts the matching
    # lake on PATH, and the dev shell supplies the same lake.  Past this
    # point a failing oracle exits 2: substring-matching error text to
    # decide between SKIP and FAIL would let a real divergence read as
    # green.
    if not shutil.which("lake"):
        print("SKIP: LEDGER_LEAN_DIR set but lake not on PATH")
        sys.exit(77)

    if not os.path.isfile(ledger):
        print(f"cannot compare: ledger binary not found at {ledger}",
              file=sys.stderr)
        sys.exit(2)

    tests = []
    for d in dirs:
        for root, _, files in os.walk(d):
            for f in sorted(files):
                if f.endswith(".test"):
                    tests.append(os.path.join(root, f))

    bins = {"PASS": [], "FAIL": [], "NEG:unbalanced": [],
            "NEG:oracle-permissive": [], "SKIP:negative": [],
            "SKIP:known-divergence": [],
            "SKIP:no-journal": [], "SKIP:python": [],
            "SKIP:out-of-scope": [], "SKIP:cpp-parse": []}
    details = []
    neg_skipped = []

    with tempfile.TemporaryDirectory() as tmp:
        candidates = []  # (test_path, dat_path, cpp_flags, dc)
        neg_candidates = []  # the same, for NEG:unbalanced files
        journal_text_of = {}  # dat_path -> journal text plus its copied
        # include closure, for the gross comparison's eval/expr
        # preamble strip
        vtarget, spec_form = {}, {}  # dat_path -> target / off-form set
        detector_inputs = []  # fixtures that ride the oracle batch but
        # are never live candidates, like neg_fixtures
        for i, tpath in enumerate(tests):
            journal, skip, cpp_flags, dc = classify_test_file(tpath)
            if journal is not None and has_python_block(journal):
                # a python block anywhere in the journal makes the
                # file non-comparable, whatever its name
                journal, skip = None, "python"
            rel = os.path.relpath(tpath, repo)
            neg = classify_negative_test(tpath) if skip == "negative" else None
            if neg is not None:
                njournal, nflags, ndc = neg
                fdir = os.path.join(tmp, f"{i:05d}")
                os.makedirs(fdir, exist_ok=True)
                dat = os.path.join(fdir, os.path.basename(tpath))
                with open(dat, "w", encoding="utf-8") as fh:
                    fh.write(njournal)
                copy_includes(njournal, os.path.dirname(tpath), fdir)
                neg_candidates.append((tpath, dat, nflags, ndc))
            elif skip == "negative":
                bins["SKIP:negative"].append(rel)
            elif skip == "no-journal":
                bins["SKIP:no-journal"].append(rel)
            elif skip == "python":
                bins["SKIP:python"].append(rel)
            elif skip and skip.startswith("out-of-scope"):
                bins["SKIP:out-of-scope"].append(rel)
                details.append({"file": rel, "status": "SKIP:out-of-scope",
                                "why": skip})
            else:
                assert journal is not None
                fdir = os.path.join(tmp, f"{i:05d}")
                os.makedirs(fdir, exist_ok=True)
                dat = os.path.join(fdir, os.path.basename(tpath))
                with open(dat, "w", encoding="utf-8") as fh:
                    fh.write(journal)
                inc_texts = copy_includes(journal,
                                          os.path.dirname(tpath), fdir)
                candidates.append((tpath, dat, cpp_flags, dc))
                journal_text_of[dat] = journal + inc_texts
                if value_on:
                    ts = value_targets(tpath)
                    if ts:
                        uniq = sorted(set(ts))
                        if (len(uniq) > 1
                                or any(c in uniq[0] for c in ",:=")
                                or uniq[0].endswith("!") or not uniq[0]):
                            # several targets, or one written in a
                            # form the harness does not read: not
                            # comparable, and visibly so
                            spec_form[dat] = uniq
                        else:
                            vtarget[dat] = uniq[0]

        # Detector fixtures ride the oracle batch through
        # detector_inputs, never the live candidate loop: a planted
        # one-sided
        # defect must FAIL, and the pristine control must PASS, on
        # every run — a comparator that has never been observed to
        # fail provides no evidence when it passes.  Perturbing the
        # shared journal would move both live sides identically, so
        # the defect is planted on the C++ side only.  The fixture
        # is mandatory: without it the run is unproved, so its
        # absence is a harness error, not a quiet omission.
        demo_src = os.path.join(repo, "test", "semantic", "demo.dat")
        if not os.path.isfile(demo_src):
            print(f"detector fixture missing: {demo_src}",
                  file=sys.stderr)
            sys.exit(2)
        with open(demo_src, encoding="utf-8") as fh:
            j = fh.read()
        p1 = os.path.join(tmp, "selftest.dat")
        with open(p1, "w") as fh:
            fh.write(j)
        p2 = os.path.join(tmp, "selftest-perturbed.dat")
        with open(p2, "w") as fh:
            fh.write(j.replace("$150.00", "$151.00"))
        detector_inputs.append(("__selftest__", p1, [], False))
        selftest = (p1, p2)

        # Gross detector fixture (--gross only): a telescoping journal
        # whose balance assignments C++ resolves and the oracle turns
        # into plain amounts.  The perturbation (dropping the second
        # assignment's '= ') must leave every net row equal (that is
        # the blind spot the net comparison cannot see) and move a
        # gross row.  Both fixtures ride the oracle batch; the
        # fixture is mandatory when --gross runs (S40's draft).
        tele = None
        if gross_on:
            tele_src = os.path.join(HERE, "telescope.dat")
            if not os.path.isfile(tele_src):
                print(f"gross detector fixture missing: {tele_src}",
                      file=sys.stderr)
                sys.exit(2)
            with open(tele_src, encoding="utf-8") as fh:
                tj = fh.read()
            if "= $300" not in tj:
                print("gross detector fixture lacks its planted site"
                      " '= $300'", file=sys.stderr)
                sys.exit(2)
            t1 = os.path.join(tmp, "telescope.dat")
            with open(t1, "w") as fh:
                fh.write(tj)
            t2 = os.path.join(tmp, "telescope-perturbed.dat")
            with open(t2, "w") as fh:
                fh.write(tj.replace("= $300", "$300"))
            detector_inputs.append(("__telescope__", t1, [], False))
            tele = (t1, t2)

        # Value detector fixture (--value only): the valuation-footing
        # journal, whose dated AAPL price the oracle reads and the
        # planted copy moves $170 to $171 on the C++ side only, so
        # every net row stays equal and a value row moves.  The
        # fixture is mandatory when --value runs (S48's draft).
        vdet = None
        if value_on:
            fsrc = os.path.join(HERE, "footing.dat")
            if not os.path.isfile(fsrc):
                print(f"value detector fixture missing: {fsrc}",
                      file=sys.stderr)
                sys.exit(2)
            with open(fsrc, encoding="utf-8") as fh:
                fj = fh.read()
            if "P 2024/03/01 AAPL $170" not in fj:
                print("value detector fixture lacks its planted site"
                      " 'P 2024/03/01 AAPL $170'", file=sys.stderr)
                sys.exit(2)
            v1 = os.path.join(tmp, "value-detector.dat")
            with open(v1, "w") as fh:
                fh.write(fj)
            v2 = os.path.join(tmp, "value-detector-perturbed.dat")
            with open(v2, "w") as fh:
                fh.write(fj.replace("P 2024/03/01 AAPL $170",
                                    "P 2024/03/01 AAPL $171"))
            detector_inputs.append(("__valuedet__", v1,
                                    ["--now", "2026/08/26"], False))
            vtarget[v1] = "$"
            vdet = (v1, v2)

        # NEG detector fixtures, derived from the same journal: the
        # control (the Grocer transaction given a wrong counter-amount)
        # must pass the NEG comparison; the pristine journal, which
        # both sides accept, and a journal both sides reject for a
        # non-balance reason (a C directive with a zero left side)
        # must each fail it.  All ride the oracle batch.
        neg_control, n = re.subn(
            r"^(\s+Expenses:Food\s+\$150\.00\n\s+Assets:Checking)\s*$",
            r"\1      $-149.00", j, count=1, flags=re.M)
        if n != 1:
            print("NEG detector fixture cannot be derived from " + demo_src,
                  file=sys.stderr)
            sys.exit(2)
        n1 = os.path.join(tmp, "selftest-neg-control.dat")
        n2 = os.path.join(tmp, "selftest-neg-reason.dat")
        with open(n1, "w") as fh:
            fh.write(neg_control)
        with open(n2, "w") as fh:
            fh.write("C 0 h = 60 m\n" + j)
        neg_fixtures = [("__neg_selftest__", n1, [], False),
                        ("__neg_selftest__", n2, [], False)]

        # Three included lines shift First from source line 3 to oracle
        # line 6, the source line of Second: equality alone proves nothing.
        with open(os.path.join(tmp, "selftest-neg-padding.dat"), "w") as fh:
            fh.write("; padding one\n; padding two\n; padding three\n")
        include_control = ("include selftest-neg-padding.dat\n\n"
                           "2026/01/01 First\n    A    $1\n    B    $-2\n"
                           "2026/01/02 Second\n    C    $3\n    D    $-4\n")
        include_periodic = ("\ufeff@!include selftest-neg-padding.dat\n\n"
                            "~ monthly\n    A    $1\n    B    $-2\n")
        ni = os.path.join(tmp, "selftest-neg-include.dat")
        np = os.path.join(tmp, "selftest-neg-include-periodic.dat")
        for path, text in ((ni, include_control), (np, include_periodic)):
            with open(path, "w", encoding="utf-8") as fh:
                fh.write(text)
            neg_fixtures.append(("__neg_selftest__", path, [], False))

        # An include-less periodic collision, so the template block
        # rule of same_entry is itself exercised on live diagnostics:
        # the oracle rejects the unbalanced yearly template at its
        # header (line 5) while C++ reports the template's last
        # posting line (8), so nothing but the indented-block rule can
        # confirm the same entry.  The monthly template balances, so a
        # comparator matching any template, or any line, would pass a
        # real rejection of the other one.
        periodic_text = ("~ monthly\n"
                         "    A    $1\n"
                         "    B    $-1\n"
                         "\n"
                         "~ yearly\n"
                         "    A    $2\n"
                         "    B    $-1\n"
                         "    C    $-2\n")
        n4 = os.path.join(tmp, "selftest-neg-periodic.dat")
        with open(n4, "w", encoding="utf-8") as fh:
            fh.write(periodic_text)
        neg_fixtures.append(("__neg_selftest__", n4, [], False))

        classifier_failures = negative_classifier_failures(tmp, neg_control)
        if classifier_failures:
            bins["FAIL"].append("<neg-classifier-selftest>")
            details.append({"file": "<neg-classifier-selftest>", "status": "FAIL",
                            "why": "NEG classifier fixtures: " + ", ".join(classifier_failures)})
        else:
            print("selftest: NEG classifier fixtures green")

        # Batched oracle runs (chunked argv).
        oracle = {}  # dat_path -> set(rows) | ("ERROR", msg)
        styles = {}  # dat_path -> set of decimal-comma commodities
        gdr, gcr = {}, {}  # dat_path -> set of gross debit/credit rows
        gref = {}  # dat_path -> '?? gross' refusal reason
        vrows = {}  # dat_path -> set of `>> value` rows
        vref = {}  # dat_path -> '?? exchange' refusal reason
        unreq = {}  # dat_path -> an extension line in a file that
        # did not request its flag: a leak into the default output
        CHUNK = 800
        oracle_inputs = candidates + neg_candidates + neg_fixtures \
            + detector_inputs
        for lo in range(0, len(oracle_inputs), CHUNK):
            chunk = oracle_inputs[lo:lo + CHUNK]
            argv = []
            for _, d, fl, dc in chunk:
                if dc:
                    argv.append("--decimal-comma")
                if "--now" in fl:
                    argv.extend(["--now", fl[fl.index("--now") + 1]])
                if "--recursive-aliases" in fl:
                    argv.append("--recursive-aliases")
                if "--input-date-format" in fl:
                    argv.extend(["--input-date-format",
                                 fl[fl.index("--input-date-format") + 1]])
                if gross_on:
                    argv.append("--gross")
                if value_on and d in vtarget:
                    argv.extend(["--exchange", vtarget[d]])
                argv.append(d)
            # Oracle invocation: lake runs directly in the oracle tree.
            # Toolchain absence was probed once above and is the ONLY
            # skip: any failure past this point is a red result, never
            # a SKIPPED one.
            cmd = ["lake", "env", "lean",
                   "--run", "Ledger/Driver.lean", *argv]
            drv = subprocess.run(
                cmd, cwd=lean_dir, capture_output=True,
                text=True, timeout=3600, env=sweep.oracle_env(env))
            if drv.returncode != 0:
                msg = (drv.stderr or drv.stdout)[-500:]
                print("oracle driver invocation failed:\n" + msg,
                      file=sys.stderr)
                sys.exit(2)
            cur = None
            for line in drv.stdout.splitlines():
                if line.startswith("== "):
                    cur = line[3:].strip()
                    oracle[cur] = set()
                    styles[cur] = set()
                elif line.startswith("!! ERROR") and cur:
                    oracle[cur] = ("ERROR", line[len("!! ERROR "):])
                elif line.startswith("%% dc ") and cur:
                    c = line[len("%% dc "):]
                    styles[cur].add("" if c == "*" else c)
                elif cur and line.startswith(EXTENSION_PREFIXES):
                    # Comparison output of --gross and --exchange.
                    # Each kind is diverted only for a file whose
                    # arguments requested it; in any other file it is
                    # a leak into the default output.
                    if line.startswith((">> dr ", ">> cr ", "?? gross ")):
                        if not gross_on:
                            unreq.setdefault(cur, line)
                        elif line.startswith(">> dr "):
                            gdr.setdefault(cur, set()).add(
                                line[len(">> dr "):].strip())
                        elif line.startswith(">> cr "):
                            gcr.setdefault(cur, set()).add(
                                line[len(">> cr "):].strip())
                        else:
                            gref[cur] = line[len("?? gross "):].strip()
                    elif value_on and cur in vtarget:
                        if line.startswith(">> value "):
                            vrows.setdefault(cur, set()).add(
                                line[len(">> value "):].strip())
                        else:
                            vref[cur] = line[len("?? exchange "):].strip()
                    else:
                        unreq.setdefault(cur, line)
                elif cur and line.strip():
                    if isinstance(oracle[cur], set):
                        oracle[cur].add(line.strip())

        def cpp_gross(dat, cpp_flags, dc, dc_comms, journal):
            """C++'s gross debit and credit rows for one journal, as
            parse_cpp_balance sees them, or (None, why).  Each column
            is the report's own --dc column: the value expression
            keeps the positive (negative) part of every posting amount
            and zeroes the rest, so the column sums posting by posting
            as the oracle's --gross totals do."""
            pre = ""
            if has_eval_preamble(journal):
                # eval/expr directives print their value at the head of
                # every report.  A no-limit balance reproduces that
                # preamble exactly, so the gross columns can be parsed
                # at a common offset past it (S40's draft located the
                # strip); without it the preamble's bare amount line
                # would fold into the first account row as a phantom
                # posting (regress/1883's "-30.00 GBP").
                pr = subprocess.run(
                    [ledger, *cpp_flags, "--permissive", "-f", dat,
                     "balance", "--flat", "--no-total", "--limit",
                     "false"],
                    capture_output=True, text=True, env=env)
                if pr.returncode != 0:
                    return None, (f"eval preamble probe: C++ exit"
                                  f" {pr.returncode}:"
                                  f" {pr.stderr.strip()[-200:]}")
                pre = pr.stdout
            out = {}
            for tag, expr in (("dr", GROSS_DR_EXPR), ("cr", GROSS_CR_EXPR)):
                g = subprocess.run(
                    [ledger, *cpp_flags, "--permissive", "-f", dat,
                     "balance", "--flat", "--no-total", "--amount", expr],
                    capture_output=True, text=True, env=env)
                if g.returncode != 0:
                    return None, f"{tag}: C++ exit {g.returncode}: " \
                                 f"{g.stderr.strip()[-200:]}"
                text = g.stdout
                if pre:
                    if not text.startswith(pre):
                        return None, (f"{tag}: eval preamble not found"
                                      " at the head of the output")
                    text = text[len(pre):]
                try:
                    out[tag] = sweep.parse_cpp_balance(
                        text, decimal_comma=dc, dc_comms=dc_comms)
                except ValueError as e:
                    return None, f"{tag}: unparseable C++ output: {e}"
            return out, None

        def gross_compare(rel, dat, cpp_flags, dc, dc_comms, journal):
            """One file's gross comparison into the GROSS bins."""
            if dat in gref:
                GROSS["SKIP"].append((gref[dat], rel))
                return
            cpp, err = cpp_gross(dat, cpp_flags, dc, dc_comms, journal)
            if err:
                GROSS["FAIL"].append((rel, err))
                return
            diffs = []
            for tag, o in (("dr", gdr.get(dat, set())),
                           ("cr", gcr.get(dat, set()))):
                GROSS_ROWS[tag] += len(cpp[tag])
                if cpp[tag] != o:
                    diffs.append((tag, sorted(o - cpp[tag])[:6],
                                  sorted(cpp[tag] - o)[:6]))
            if cpp["dr"] or cpp["cr"]:
                GROSS_ROWS["files"] += 1
            if diffs and rel in divergent_listed:
                # A known gross divergence (the oracle's automated-
                # transaction gross differs from C++'s column sums;
                # recorded in the semantics repository), binned
                # visibly and held to the ratchet below.
                GROSS["SKIP"].append(("known-divergence", rel))
            elif diffs:
                GROSS["FAIL"].append((rel, repr(diffs)))
            else:
                GROSS["PASS"].append(rel)

        def cpp_value(dat, cpp_flags, dc, dc_comms, journal, target):
            """C++'s valued rows for one journal, as parse_cpp_balance
            sees them, or (None, why)."""
            pre = ""
            if has_eval_preamble(journal):
                # eval/expr directives print their value at the head of
                # every report; the same strip as for the gross
                # columns applies (regress/1883's "-30.00 GBP").
                pr = subprocess.run(
                    [ledger, *cpp_flags, "--permissive", "-f", dat,
                     "balance", "--flat", "--no-total", "--limit",
                     "false"],
                    capture_output=True, text=True, env=env)
                if pr.returncode != 0:
                    return None, (f"eval preamble probe: C++ exit"
                                  f" {pr.returncode}:"
                                  f" {pr.stderr.strip()[-200:]}")
                pre = pr.stdout
            g = subprocess.run(
                [ledger, *cpp_flags, "--permissive", "-f", dat,
                 "balance", "--flat", "--no-total", "-X", target],
                capture_output=True, text=True, env=env)
            if g.returncode != 0:
                return None, f"C++ exit {g.returncode}: " \
                             f"{g.stderr.strip()[-200:]}"
            text = g.stdout
            if pre:
                if not text.startswith(pre):
                    return None, ("eval preamble not found"
                                  " at the head of the output")
                text = text[len(pre):]
            try:
                return sweep.parse_cpp_balance(
                    text, decimal_comma=dc, dc_comms=dc_comms), None
            except ValueError as e:
                return None, f"unparseable C++ output: {e}"

        def value_compare(rel, dat, cpp_flags, dc, dc_comms, journal):
            """One file's value comparison into the VALUE bins."""
            if dat in spec_form:
                VALUE["SKIP"].append(("spec-form", rel))
                return
            if dat not in vtarget:
                return
            if dat in vref:
                reason = vref[dat].split()[0] if vref[dat] else ""
                if reason in VALUE_REASONS:
                    VALUE["SKIP"].append((reason, rel))
                else:
                    VALUE["FAIL"].append(
                        (rel, "unannounced refusal: ?? exchange "
                         + vref[dat]))
                return
            cpp, err = cpp_value(dat, cpp_flags, dc, dc_comms, journal,
                                 vtarget[dat])
            if err:
                VALUE["FAIL"].append((rel, err))
                return
            o = vrows.get(dat, set())
            VALUE_ROWS["rows"] += len(cpp)
            if cpp:
                VALUE_ROWS["files"] += 1
            if cpp != o:
                VALUE["FAIL"].append(
                    (rel, repr((sorted(o - cpp)[:6],
                                sorted(cpp - o)[:6]))))
            else:
                VALUE["PASS"].append(rel)

        for tpath, dat, cpp_flags, dc in candidates:
            rel = os.path.relpath(tpath, repo)
            res = oracle.get(dat)
            if res is None:
                bins["FAIL"].append(rel)
                details.append({"file": rel, "status": "FAIL",
                                "why": "no oracle output for journal"})
                continue
            if isinstance(res, tuple):
                bins["SKIP:out-of-scope"].append(rel)
                details.append({"file": rel, "status": "SKIP:out-of-scope",
                                "why": res[1]})
                continue
            dc_comms = frozenset(styles.get(dat, set()))
            # --permissive quiets balance-assertion failures on the
            # C++ side.  This is deliberate: the extracted journals
            # lose the per-test command lines whose flags sometimes
            # relax those assertions, the suite's own expectations
            # already test assertion behavior, and what is compared
            # here is the computed balances, which --permissive does
            # not alter.
            cpp = subprocess.run(
                [ledger, *cpp_flags, "--permissive", "-f", dat,
                 "balance", "--flat", "--no-total"],
                capture_output=True, text=True, env=env)
            if cpp.returncode != 0:
                bins["SKIP:cpp-parse"].append(rel)
                details.append({"file": rel, "status": "SKIP:cpp-parse",
                                "why": cpp.stderr.strip()[-200:]})
                continue
            try:
                cpp_rows = sweep.parse_cpp_balance(
                    cpp.stdout, decimal_comma=dc, dc_comms=dc_comms)
            except ValueError as e:
                bins["FAIL"].append(rel)
                details.append({"file": rel, "status": "FAIL",
                                "why": f"unparseable C++ output: {e}"})
                continue
            if dat in unreq:
                bins["FAIL"].append(rel)
                details.append({"file": rel, "status": "FAIL",
                                "why": "unrequested protocol extension line: "
                                       + unreq[dat]})
                continue
            if gross_on:
                gross_compare(rel, dat, cpp_flags, dc, dc_comms,
                              journal_text_of.get(dat, ""))
            if value_on:
                value_compare(rel, dat, cpp_flags, dc, dc_comms,
                              journal_text_of.get(dat, ""))
            if cpp_rows == res:
                bins["PASS"].append(rel)
            elif rel in divergent_rows_listed:
                # A known oracle departure in the balance rows (the
                # oracle's displayed-row rounding is half away from
                # zero where C++ rounds the value's binary expansion to
                # even; recorded at Ledger/Driver.lean displayAmount).
                # Binned visibly, never silently, and held to the
                # ratchet below.
                bins["SKIP:known-divergence"].append(rel)
                details.append({
                    "file": rel, "status": "SKIP:known-divergence",
                    "only_oracle": sorted(res - cpp_rows)[:8],
                    "only_cpp": sorted(cpp_rows - res)[:8]})
            else:
                bins["FAIL"].append(rel)
                details.append({
                    "file": rel, "status": "FAIL",
                    "only_oracle": sorted(res - cpp_rows)[:8],
                    "only_cpp": sorted(cpp_rows - res)[:8]})

        if selftest:
            p1, p2 = selftest
            res = oracle.get(p1)
            def cpp_rows_of(path):
                out = subprocess.run(
                    [ledger, "-f", path, "balance", "--flat", "--no-total"],
                    capture_output=True, text=True, env=env)
                return sweep.parse_cpp_balance(out.stdout)
            control_ok = isinstance(res, set) and cpp_rows_of(p1) == res
            fired = (not isinstance(res, set)) or cpp_rows_of(p2) != res
            if control_ok and fired:
                print("selftest: both detector fixtures green")
            else:
                bins["FAIL"].append("<selftest>")
                details.append({
                    "file": "<selftest>", "status": "FAIL",
                    "why": f"detector fixtures: control_ok={control_ok} "
                           f"fired_on_planted_defect={fired}"})

        if tele:
            t1, t2 = tele
            tres = oracle.get(t1)

            def cpp_net(path):
                out = subprocess.run(
                    [ledger, "-f", path, "balance", "--flat", "--no-total"],
                    capture_output=True, text=True, env=env)
                return sweep.parse_cpp_balance(out.stdout)

            og = {"dr": gdr.get(t1, set()), "cr": gcr.get(t1, set())}
            gross1, err1 = cpp_gross(t1, [], False, frozenset(), "")
            gross2, err2 = cpp_gross(t2, [], False, frozenset(), "")
            control_ok = (isinstance(tres, set) and t1 not in gref
                          and cpp_net(t1) == tres
                          and err1 is None and gross1 == og)
            net_blind = isinstance(tres, set) and cpp_net(t2) == cpp_net(t1)
            # A C++ error on the perturbed fixture is not a firing:
            # the detector must see gross rows that differ, not an
            # error standing in for them.
            fired = err2 is None and gross2 != og
            if control_ok and net_blind and fired:
                print("gross selftest: telescoping detector green"
                      " (net rows equal, gross rows differ)")
            else:
                GROSS["FAIL"].append(("<gross-selftest>",
                                      f"control_ok={control_ok} "
                                      f"net_blind={net_blind} fired={fired}"))

        if vdet:
            v1, v2 = vdet
            ores = vrows.get(v1, set())
            c1, e1 = cpp_value(v1, ["--now", "2026/08/26"], False,
                               frozenset(), "", "$")
            c2, e2 = cpp_value(v2, ["--now", "2026/08/26"], False,
                               frozenset(), "", "$")
            vctl = ((not e1) and v1 not in vref and bool(ores)
                    and c1 == ores)
            # A C++ error on the perturbed fixture is not a firing:
            # the detector must see value rows that differ, not an
            # error standing in for them.
            vfired = (not e2) and c2 != ores
            if vctl and vfired:
                print("value selftest: detector green (control agrees,"
                      " planted price fired)")
            else:
                VALUE["FAIL"].append(("<value-selftest>",
                                      f"control_ok={vctl} fired={vfired}"))

        def cpp_reject(path, flags):
            return subprocess.run([ledger, *flags, "-f", path, "balance"],
                                  capture_output=True, text=True, env=env)

        for tpath, dat, nflags, _ in neg_candidates:
            rel = os.path.relpath(tpath, repo)
            with open(dat, encoding="utf-8") as fh:
                ntext = fh.read()
            why, skipped = neg_failures(oracle.get(dat), cpp_reject(dat, nflags), dat, ntext)
            if not why:
                bins["NEG:unbalanced"].append(rel)
                if skipped:
                    neg_skipped.append(rel)
                    details.append({"file": rel, "status": "NEG:unbalanced",
                                    "why": "same-entry check skipped: include context"})
            elif rel in permissive_listed:
                # A known oracle departure: the oracle accepts what C++
                # rejects as unbalanced (the oracle's transaction
                # extension does not run C++'s per-group balance check;
                # recorded in the semantics repository).  Binned
                # visibly, never silently, and the ratchet check below
                # fails the run when the entry goes stale.
                bins["NEG:oracle-permissive"].append(rel)
                details.append({"file": rel, "status": "NEG:oracle-permissive",
                                "why": why})
            else:
                bins["FAIL"].append(rel)
                details.append({"file": rel, "status": "FAIL",
                                "bin": "NEG:unbalanced", "why": why})

        # Each side must independently reject for imbalance.  Reuse
        # the live results to plant one-sided acceptance, missing
        # oracle output and signal termination after balance diagnostics.
        cpp_control = cpp_reject(n1, [])
        w_control = neg_failures(oracle.get(n1), cpp_control, n1, neg_control)
        w_accept = neg_failures(oracle.get(p1), cpp_control, n1, neg_control)
        w_cpp_accept = neg_failures(oracle.get(n1), cpp_reject(p1, []), p1, j)
        w_reason = neg_failures(oracle.get(n2), cpp_reject(n2, []), n2, "C 0 h = 60 m\n" + j)
        w_missing = neg_failures(None, cpp_control, n1, neg_control)
        cpp_signal = subprocess.CompletedProcess(
            cpp_control.args, -signal.SIGTERM, cpp_control.stdout, cpp_control.stderr)
        w_signal = neg_failures(oracle.get(n1), cpp_signal, n1, neg_control)

        # A rejection whose context is stripped names no entry; the
        # comparison must fail closed rather than confirm.
        cpp_noctx = subprocess.CompletedProcess(
            cpp_control.args, cpp_control.returncode, cpp_control.stdout,
            "".join(l + "\n" for l in cpp_control.stderr.splitlines()
                    if not CPP_CONTEXT_RE.match(l)))
        w_noctx = neg_failures(oracle.get(n1), cpp_noctx, n1, neg_control)

        # The bare shape, with no diagnostic text but the Error lines
        # themselves, must take the same fail-closed branch.
        cpp_noctx2 = subprocess.CompletedProcess(
            cpp_control.args, cpp_control.returncode, cpp_control.stdout,
            "".join(l + "\n" for l in cpp_control.stderr.splitlines()
                    if l.startswith("Error:")))
        w_noctx2 = neg_failures(oracle.get(n1), cpp_noctx2, n1, neg_control)

        # C++ alone rejects Landlord before Grocer; merely agreeing on
        # imbalance must not pass the same-entry comparison.
        other_text, n = re.subn(
            r"^([ \t]+Expenses:Rent[ \t]+\$2000\.00\n[ \t]+Assets:Checking)[ \t]*$",
            r"\1      $-1999.00", neg_control, count=1, flags=re.M)
        if n != 1:
            print("NEG other-entry fixture cannot be derived from " + demo_src,
                  file=sys.stderr)
            sys.exit(2)
        n3 = os.path.join(tmp, "selftest-neg-other-entry.dat")
        with open(n3, "w") as fh:
            fh.write(other_text)
        w_entry, entry_skipped = neg_failures(oracle.get(n1), cpp_reject(n3, []), n3, other_text)

        # Balance First only on the C++ side. Both this collision and the
        # agreeing control must be visible skips, never confirmed entries.
        ni_other = os.path.join(tmp, "selftest-neg-include-other.dat")
        include_other = include_control.replace("B    $-2", "B    $-1")
        with open(ni_other, "w") as fh:
            fh.write(include_other)
        cpp_include = cpp_reject(ni, [])
        cpp_include_other = cpp_reject(ni_other, [])
        w_include = neg_failures(oracle.get(ni), cpp_include, ni, include_control)
        w_include_other = neg_failures(oracle.get(ni), cpp_include_other, ni_other, include_other)
        w_include_periodic = neg_failures(oracle.get(np), cpp_reject(np, []), np, include_periodic)
        include_res = oracle.get(ni)
        include_line = (ORACLE_LINE_RE.match(include_res[1])
                        if isinstance(include_res, tuple) else None)
        include_contexts = [CPP_BALANCING_RE.search(out.stderr)
                            for out in (cpp_include, cpp_include_other)]
        include_lines = [ctx.group(2) if ctx else None for ctx in include_contexts]

        # The periodic block rule, both directions, against real
        # diagnostics: the live fixture must confirm the same entry
        # (header 5 against reported end 8), a stubbed context that
        # ends the template before the oracle's header must fail it,
        # and so must a torn block with a de-indented posting still
        # covered by the reported end.  The stubs reuse the live
        # oracle result and rejection; only the text they judge is
        # shifted, in the manner of the stripped-context probes.
        cpp_periodic = cpp_reject(n4, [])
        w_periodic = neg_failures(oracle.get(n4), cpp_periodic, n4,
                                  periodic_text)
        cpp_periodic_shift = subprocess.CompletedProcess(
            cpp_periodic.args, cpp_periodic.returncode, cpp_periodic.stdout,
            re.sub(r'^(While parsing file "[^"]*"), line \d+:$',
                   r"\1, line 3:", cpp_periodic.stderr, count=1, flags=re.M))
        w_periodic_shift = neg_failures(oracle.get(n4), cpp_periodic_shift,
                                        n4, periodic_text)
        periodic_torn = periodic_text.replace(
            "    A    $2\n    B    $-1\n    C    $-2",
            "    A    $2\nB    $-1\n    C    $-2")
        w_periodic_torn = neg_failures(oracle.get(n4), cpp_periodic, n4,
                                       periodic_torn)

        neg_ok = (w_control == ([], False)
                  and w_accept == (["oracle accepted a journal classified as unbalanced"], False)
                  and w_cpp_accept == (["C++ did not reject the extracted journal as unbalanced"],
                                       False)
                  and any(w.startswith("oracle rejected it for another")
                          for w in w_reason[0])
                  and w_missing == (["no oracle output for journal"], False)
                  and w_signal == (["C++ did not reject the extracted journal as unbalanced"],
                                   False)
                  and w_noctx == ([NO_CONTEXT_REASON], False)
                  and w_noctx2 == ([NO_CONTEXT_REASON], False)
                  and not entry_skipped
                  and any(w.startswith("oracle rejected a different entry") for w in w_entry)
                  and w_include == w_include_other == w_include_periodic == ([], True)
                  and include_line and include_line.group(2) == "6"
                  and include_lines == ["3", "6"]
                  and w_periodic == ([], False)
                  and w_periodic_shift == (["oracle rejected a different entry: line 5, C++ the"
                                            " template ending at line 3"], False)
                  and w_periodic_torn == (["oracle rejected a different entry: line 5, C++ the"
                                           " template ending at line 8"], False))
        if neg_ok:
            print("selftest: NEG detector fixtures green")
        else:
            bins["FAIL"].append("<neg-selftest>")
            details.append({
                "file": "<neg-selftest>", "status": "FAIL",
                "why": f"NEG detector fixtures: control={w_control} "
                       f"accepted={w_accept} cpp_accepted={w_cpp_accept} "
                       f"other_reason={w_reason} missing={w_missing} signal={w_signal} "
                       f"no_context={w_noctx} no_context2={w_noctx2} "
                       f"other_entry={w_entry} entry_skipped={entry_skipped} "
                       f"include={w_include} include_other={w_include_other} "
                       f"include_periodic={w_include_periodic} "
                       f"include_oracle={include_res} include_cpp_lines={include_lines} "
                       f"periodic={w_periodic} periodic_shift={w_periodic_shift} "
                       f"periodic_torn={w_periodic_torn}"})

    def run_line(cmd, **kw):
        try:
            out = subprocess.run(cmd, capture_output=True, text=True,
                                 **kw).stdout
            return (out.splitlines() or ["unknown"])[0].strip() or "unknown"
        except OSError:
            return "unknown"

    ledger_version = run_line([ledger, "--version"], env=env)
    git_rev = run_line(["git", "rev-parse", "HEAD"], cwd=repo)
    # In the Nix build the oracle is a store path with no repository;
    # the flake passes the semantics revision in LEDGER_LEAN_REV.
    oracle_rev = (os.environ.get("LEDGER_LEAN_REV")
                  or run_line(["git", "-C", lean_dir, "rev-parse", "HEAD"]))
    artifact = {
        "tuple": {
            "oracle_rev": oracle_rev,
            "utc": datetime.datetime.now(datetime.timezone.utc).isoformat(),
            "git_rev": git_rev, "ledger_version": ledger_version,
            "tz": env["TZ"], "capture": "test/semantic/check_all.py",
            "cpp_flags_common": ["--permissive", "balance", "--flat",
                                 "--no-total"],
            "cpp_flags_note": ("per-file flags (--now, --decimal-comma,"
                               " --recursive-aliases,"
                               " --input-date-format) are derived from"
                               " each test's own command lines"),
            "neg_cpp_flags_common": ["balance"],
            "neg_reason_pattern": NEG_REASON_RE.pattern},
        "counts": {k: len(v) for k, v in bins.items()},
        "neg_line_check_skipped": neg_skipped,
        "details": details,
        "bins": {k: v for k, v in bins.items() if k != "PASS"},
    }
    # The gross refusal ratchet (D29): gross-refusals.txt holds the
    # measured refusal set as REASON<TAB>path.  An unlisted refusal
    # means the lot-annotated-completion test now fires where it did
    # not, and a stale entry means a refused file now compares; the
    # file must change deliberately with the rule that forced it.
    gratchet = []
    if gross_on:
        listed = set()
        if os.path.isfile(args.gross_refusals):
            with open(args.gross_refusals, encoding="utf-8") as fh:
                for ln in fh:
                    ln = ln.rstrip("\n")
                    if ln and not ln.startswith("#"):
                        reason, path = ln.split("\t", 1)
                        listed.add((reason, path.strip()))
        else:
            print(f"gross refusal ratchet missing: {args.gross_refusals}",
                  file=sys.stderr)
            gratchet.append("missing")
        seen = {(r, pth) for r, pth in GROSS["SKIP"]
                if r != "known-divergence"}
        for r, pth in sorted(seen - listed):
            GROSS["FAIL"].append((pth, f"refusal not in the ratchet: {r}"))
        for r, pth in sorted(listed - seen):
            GROSS["FAIL"].append((pth, f"stale ratchet entry: {r}"))
        # The known-divergence ratchet: every listed file must have
        # diverged (been skipped with the known-divergence reason),
        # and a divergence on an unlisted file stays a FAIL.
        diverged = {pth for r, pth in GROSS["SKIP"]
                    if r == "known-divergence"}
        for pth in sorted(divergent_listed - diverged):
            GROSS["FAIL"].append((pth, "stale divergent ratchet entry"))
        for pth in sorted(diverged - divergent_listed):
            GROSS["FAIL"].append((pth, "divergence not in the ratchet"))

    # The value refusal ratchet: the same ratchet shape as the gross
    # one, holding the measured `?? exchange` set over this corpus.
    # A missing ratchet file is a harness error (exit 2), not a run
    # whose refusals went unchecked: S48's draft read one as an empty
    # list, and its resumed verification repaired the draft there.
    vratchet = []
    if value_on:
        listed = set()
        if os.path.isfile(args.value_refusals):
            with open(args.value_refusals, encoding="utf-8") as fh:
                for ln in fh:
                    ln = ln.rstrip("\n")
                    if ln and not ln.startswith("#"):
                        reason, path = ln.split("\t", 1)
                        listed.add((reason, path.strip()))
        else:
            print(f"value refusal ratchet missing: {args.value_refusals}",
                  file=sys.stderr)
            sys.exit(2)
        seen = set(VALUE["SKIP"])
        for r, pth in sorted(seen - listed):
            VALUE["FAIL"].append((pth, f"refusal not in the ratchet: {r}"))
        for r, pth in sorted(listed - seen):
            VALUE["FAIL"].append((pth, f"stale ratchet entry: {r}"))

    # The gross bins ride the artifact's details so the record stays
    # reviewable; PASS lists are the harness's own evidence of how
    # much each comparison covered.
    if gross_on:
        artifact["counts"]["GROSS:PASS"] = len(GROSS["PASS"])
        artifact["counts"]["GROSS:FAIL"] = len(GROSS["FAIL"])
        artifact["gross_rows"] = dict(GROSS_ROWS)
        artifact["gross_refusals"] = [f"{r}\t{rel}"
                                      for r, rel in sorted(GROSS["SKIP"])]
        for rel, why in GROSS["FAIL"]:
            artifact["details"].append({"file": rel, "status": "GROSS:FAIL",
                                        "why": why})
    if value_on:
        artifact["counts"]["VALUE:PASS"] = len(VALUE["PASS"])
        artifact["counts"]["VALUE:FAIL"] = len(VALUE["FAIL"])
        artifact["value_rows"] = dict(VALUE_ROWS)
        artifact["value_refusals"] = [f"{r}\t{rel}"
                                      for r, rel in sorted(VALUE["SKIP"])]
        for rel, why in VALUE["FAIL"]:
            artifact["details"].append({"file": rel, "status": "VALUE:FAIL",
                                        "why": why})
    stamp = datetime.datetime.now(datetime.timezone.utc).strftime(
        "%Y%m%dT%H%M%SZ")
    try:
        os.makedirs(os.path.join(HERE, "artifacts"), exist_ok=True)
        apath = os.path.join(HERE, "artifacts", f"check-all-{stamp}.json")
        with open(apath, "w") as fh:
            json.dump(artifact, fh, indent=2)
    except OSError:
        # Read-only source tree (e.g. a Nix sandbox): keep the record.
        apath = os.path.join(tempfile.gettempdir(),
                             f"check-all-{stamp}.json")
        with open(apath, "w") as fh:
            json.dump(artifact, fh, indent=2)

    total = sum(len(v) for v in bins.values())
    print(f"semantic bisimulation over {total} .test files:")
    for k in ("PASS", "FAIL", "NEG:unbalanced", "SKIP:out-of-scope", "SKIP:cpp-parse",
              "SKIP:negative", "SKIP:no-journal", "SKIP:python"):
        extra = ""
        if k == "NEG:unbalanced" and neg_skipped:
            extra = (f" ({len(neg_skipped)} without the same-entry check:"
                     " include context)")
        print(f"  {k:<18} {len(bins[k])}{extra}")
    for d in details:
        if d["status"] == "FAIL":
            print(f"  FAIL {d['file']}")
            for k in ("why", "only_oracle", "only_cpp"):
                if k in d:
                    print(f"       {k}: {d[k]}")
    if gross_on:
        print(f"  gross rows compared: dr {GROSS_ROWS['dr']},"
              f" cr {GROSS_ROWS['cr']}, in {GROSS_ROWS['files']} files"
              " with a nonempty C++ column")
        print(f"  GROSS:PASS          {len(GROSS['PASS'])}")
        print(f"  GROSS:FAIL          {len(GROSS['FAIL'])}")
        for r in sorted(set(r for r, _ in GROSS["SKIP"])):
            print(f"  GROSS:SKIP:{r}"
                  f" {sum(1 for x, _ in GROSS['SKIP'] if x == r)}")
        for r, rel in sorted(GROSS["SKIP"]):
            print(f"  refused {r}\t{rel}")
        for rel, why in GROSS["FAIL"]:
            print(f"  GROSS:FAIL {rel}: {why}")
    if value_on:
        print(f"  value rows compared: {VALUE_ROWS['rows']} in"
              f" {VALUE_ROWS['files']} files with a nonempty C++ side")
        print(f"  VALUE:PASS          {len(VALUE['PASS'])}")
        print(f"  VALUE:FAIL          {len(VALUE['FAIL'])}")
        for r in sorted(set(r for r, _ in VALUE["SKIP"])):
            print(f"  VALUE:SKIP:{r}"
                  f" {sum(1 for x, _ in VALUE['SKIP'] if x == r)}")
        for r, rel in sorted(VALUE["SKIP"]):
            print(f"  refused {r}\t{rel}")
        for rel, why in VALUE["FAIL"]:
            print(f"  VALUE:FAIL {rel}: {why}")
    print(f"artifact: {os.path.relpath(apath, repo)}")
    # Ratchet: both bins below reached zero against the full suite,
    # and gates only tighten.  An entry here means one side stopped
    # reading a journal it used to read — an oracle that rejects
    # what it once parsed, or a C++ ledger that errors where it once
    # balanced — and letting that bin as a skip would turn either
    # regression green.
    # The known row-divergence ratchet: every listed file must have
    # diverged, and a divergence on an unlisted file stays a FAIL.
    diverged_rows = set(bins["SKIP:known-divergence"])
    if divergent_rows_listed - diverged_rows:
        for rel in sorted(divergent_rows_listed - diverged_rows):
            print(f"stale oracle-divergent ratchet entry: {rel}")
    if diverged_rows - divergent_rows_listed:
        for rel in sorted(diverged_rows - divergent_rows_listed):
            bins["FAIL"].append(rel)
            details.append({"file": rel, "status": "FAIL",
                            "why": "divergence not in the ratchet"})
    ratchet = bins["SKIP:out-of-scope"] or bins["SKIP:cpp-parse"]
    if ratchet and not bins["FAIL"]:
        print("ratchet: SKIP:out-of-scope and SKIP:cpp-parse must stay"
              " empty; a comparable journal is no longer compared")
    sys.exit(1 if (bins["FAIL"] or ratchet or gratchet or vratchet
                   or (gross_on and GROSS["FAIL"])
                   or (value_on and VALUE["FAIL"])) else 0)


if __name__ == "__main__":
    main()
