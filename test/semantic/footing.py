#!/usr/bin/env python3
"""Valuation footing: `bal -V --unrealized` and `bal -X --unrealized`
must total zero.

A balance report under -V or -X with --unrealized books the change in
value of every holding to the equity accounts Equity:Unrealized Gains
and Equity:Unrealized Losses, so for a journal that balances at cost
the report must total zero, and its unrealized rows must equal minus
the total of the matching `bal -G` report.  The C++ suite states this
intent in test/baseline/opt-unrealized-gains.test and the other
unfiltered unrealized tests, and the regress/unrealized-footing-*.test
files pin it for the journal shapes that once broke it.  This check
replays every positive `.test` journal in test/baseline, test/regress
and test/manual (extracted exactly as test/semantic/check_all.py
extracts it) through these C++ reports, where C is the one commodity
of the journal's costs:

  pricedb                  whether the journal records any price
  bal -B --flat            eligibility: the journal balances at cost
  bal -V --unrealized      the total must be zero
  bal -G                   the unrealized rows must be minus this total
  bal -X C --unrealized    the total must be zero
  bal -G -X C              the unrealized rows must be minus this total
  bal                      on a journal that records no price, whether
                           the valuation reports change anything

It compares C++ with C++, needs no Lean oracle, and runs as the
ValuationFooting ctest.  A valuation error shared by a report and its
-G counterpart passes it.  The contract holds only for unfiltered
reports: a report limited to some accounts omits the other side of
each transaction, and regress/cov2-revalued-opts.test expects $1000.00
under `bal Assets:Broker` by design.  The generated revaluation
postings pass the report's account filters
(test/regress/coverage-report-unrealized.test), which is why a
filtered report cannot foot.

The -X report converts into C, the commodity in which the journal
already balances.  It checks the -X path of the report, and it also
covers journals on which -V leaves unvalued a holding that -X
converts, as for a commodity declared with N
(regress/unrealized-unpriced-holding.test).  Because the costs are
already in C, the -X report never converts a cost, so it cannot show
the two sides of a transaction converted at different rates, which
regress/unrealized-footing-posting-date.test pins.  It is not run when
`bal -B --flat` prints no row, so that no commodity is C, when C is the
null commodity, which -X cannot name, or when the symbol of C holds a
comma, a colon or an equals sign, which -X reads as a list or a
conversion.  The -V report is compared when its rows are in one
commodity, which is C, or the commodity into which -V values C when C
itself has a price.  The -X report is compared when its rows are in C.
A report whose rows are in more than one commodity, or, under -X, in a
commodity other than C, has left a holding unvalued and cannot total
zero, so it is dropped only for a journal recorded in
EXPECTED_UNVALUED, with the reports it drops and the reason; anywhere
else it is a FOOT:FAIL.  A file passes when at least one of the two
reports is compared and every compared report foots.  The summary
counts the passing files by the reports compared, by the reason each
other report was dropped, and by what the compared reports print: an
unrealized row, which is the revaluation the check exists for; rows
without one, where every holding is valued at its cost and the report
must book no revaluation; or nothing.

A journal on which `pricedb` lists no price is not thereby vacuous.
pricedb leaves out the prices of a commodity declared with N, of time
units, of lot prices and of virtual costs, and -V or -X may still value
such a holding (regress/1602.test, regress/1688.test).  Such a journal
is therefore checked in full, and it is binned FOOT:skip-no-prices in
place of FOOT:PASS only when every valuation report run is compared,
foots, and prints exactly what `bal --depth 1` prints, and every -G
report prints nothing, so that the check is observed to be vacuous on
it.

Only the net amount of each row is checked.  Under --dc the report
shows debit and credit columns, each revalued against its own cost,
but C++ offers no report of the debit and credit totals at cost to
compare them with: -B and --dc each replace the base of the report's
amount expression, and the one given later wins.
regress/unrealized-footing-debit-credit.test pins those columns.

Amounts are read in the file's decimal style, and an amount whose
grouping is invalid in that style is re-read in the other, because C++
can learn a decimal comma for one commodity from the journal alone.  An
amount valid in both styles, one separator followed by three digits as
in 0,500, is read in the file's style even when C++ printed it in the
other.  Such a misreading scales the amount by exactly 1000 and never
changes whether it is zero, so it cannot make a total foot, and it
could hide a mismatch only where an unrealized row and its gain differ
by that factor.

Every report runs with --args-only, so that neither the environment
nor an init file adds options, as in the suite's other harnesses
(test/LedgerHarness.py), and with HOME set to an empty directory,
because without --price-db C++ reads $HOME/.pricedb even under
--args-only (session_t::read_data in src/session.cc).  A price the
user keeps there would otherwise decide the verdicts.  Every report
also runs with --real (virtual postings need not balance) and one
fixed --now in place of the test's own, so that the verdicts do not
depend on the day the suite runs.  A posting dated after that --now is
still valued at it: under -V or -X with --unrealized a balance report
builds its running total from the cost of each posting and books a
single revaluation at the terminus, whatever the date of the last
posting (uses_cost_baseline and changed_value_posts::flush in
src/filters.cc; regress/unrealized-footing-after-terminus.test).  The
valuation reports run in tree output at --depth 1, with the unrealized
accounts renamed to <Unrealized>:Gains and <Unrealized>:Losses (the
constructor of changed_value_posts in src/filters.cc reads both names
and their --unrealized-gains and --unrealized-losses overrides) so
that their sum is one row.  Only the sum is comparable: C++ books a
revaluation to Gains or to Losses by the sign of the net difference
(changed_value_posts::output_revaluation, src/filters.cc), which
test/baseline/opt-unrealized.test pins, since there an AAPL gain and a
QQQQ loss net to zero and no unrealized row appears.  The eligibility
report runs --flat, so that a cost commodity shows even where a family
nets to zero.

Classification (every file lands in exactly one bin):

  FOOT:PASS                    every compared report totals zero and
                               its unrealized row is minus its gain
                               total.
  FOOT:FAIL                    a compared report does not foot; C++
                               refuses a report outside
                               EXPECTED_REFUSALS, or drops one whose
                               rows are not in one commodity outside
                               EXPECTED_UNVALUED; an entry of
                               EXPECTED_REFUSALS, EXPECTED_NONFOOTING
                               or EXPECTED_UNVALUED no longer behaves
                               as it records; the C++ output cannot be
                               parsed; or the .test file is not UTF-8,
                               which check_all.py bins
                               SKIP:out-of-scope, a ratchet bin.
  FOOT:skip-no-prices          `pricedb` lists no price, and the check
                               is observed to be vacuous: every
                               valuation report run is compared, foots
                               and prints what `bal --depth 1` prints,
                               and every -G report prints nothing.
  FOOT:skip-unbalanced-at-cost `bal -B` does not total zero (automated
                               postings, timeclock entries, residuals).
  FOOT:skip-multi-commodity    the flat `bal -B` rows use more than
                               one commodity.  This is a limit of
                               eligibility: with costs in several
                               commodities no single valuation
                               collapses the report into one, and the
                               check has no commodity to hold it to.
                               Some of these reports do not foot, and
                               the C01 tests
                               regress/unrealized-unconvertible-cost,
                               unrealized-unconvertible-proceeds and
                               unrealized-null-commodity pin such
                               output.
  FOOT:skip-unvalued           neither report is compared, because the
                               rows of each are not in one commodity,
                               as recorded in EXPECTED_UNVALUED with
                               the reason.
  FOOT:skip-value-expr         the journal defines its own valuation
                               (value directive or sub-directive, a
                               Value tag, define, or ((...)) amounts).
  FOOT:skip-stdout-directive   an eval or expr directive writes to
                               standard output while parsing.
  FOOT:skip-expected-nonfooting
                               C++ leaves the journal unfooted, as
                               recorded in EXPECTED_NONFOOTING with the
                               reason and every report that fails.
  FOOT:skip-cpp-refusal        C++ refuses a report, as recorded in
                               EXPECTED_REFUSALS with the reason and
                               the text of the error.
  SKIP:negative, SKIP:no-journal, SKIP:python   as in check_all.py.

The artifact counts every bin.  It names the files of every bin except
FOOT:PASS, FOOT:skip-no-prices, SKIP:negative and SKIP:no-journal; of
the passing files it names each one that compared one report, with the
reason for the other, and each one that prints no unrealized row.

EXPECTED_REFUSALS, EXPECTED_NONFOOTING and EXPECTED_UNVALUED record
what the check accepts without comparing, each entry exactly as C++
behaves: the text of the refusal; the total, the unrealized row and
the gain of every report that fails; or the reports whose rows are not
in one commodity.  An entry that behaves otherwise, including one that
foots or compares a report it records as dropped, is a FOOT:FAIL that
asks for the entry to be removed or corrected.  A journal that no
table names is checked in full, so a new test on which C++ refuses a
report, leaves a report unfooted, or leaves a holding unvalued fails
until it is recorded here with a reason of its own.  A new test whose
-V report values every holding into one commodity other than C, as an
ordinary price test does, is compared and needs no entry.

A detector rides every run.  It is reported on a line of its own, is
not counted as a file, and a red detector fails the run.  First, the
fixture test/semantic/footing.dat must PASS with both reports compared
and an unrealized row, and four planted defects must each FAIL on it
in the report they perturb and in no other: the -V report, and then
the -X report, limited to Assets:Brokerage, which omits the other side
of each purchase (the total is no longer zero), and the fixture with
its final price perturbed in the copy that `bal -G`, and then `bal -G
-X`, reads alone (the unrealized row no longer matches the gain).  A
second fixture, UNPRICED_FIXTURE, on which pricedb lists no price
because its one commodity is declared with N, must PASS with the -X
report alone and an unrealized row, so that a journal without prices
is not skipped unseen.  Canned C++ output that cannot be parsed must
FAIL.  Of three canned journals without prices, the one on which -X
alone revalues and the one whose fixated price -V and -X apply at cost
must PASS, and the one on which no report changes anything must land
in FOOT:skip-no-prices, while a canned journal with a price on which
no report changes anything must PASS.  The control, the four defects,
the unparseable output and the vacuous journal then pass through the
same classification, tally and exit decision as the journals: the
control alone must pass, the vacuous journal alone must fail the run
because nothing in it is compared, and the control with the four
defects and the unparseable output must hold each of those five in
FOOT:FAIL and fail the run.  Second, synthetic results built from the
tables pass through the same classification: each recorded entry must
land in its skip bin or its PASS, and a result that departs from its
entry, or that matches an entry under a file the table does not name,
must land in FOOT:FAIL.  Third, on the default corpus, each journal
of EXPECTED_COMPARED must PASS, compare the reports listed, and print
an unrealized row.  These are the tests the check exists for, and the
requirement guards the classification of each file, part of which is
shared with check_all.py, and the bins that C++ output decides,
against a change that moves them out of the comparison.  It is not
applied when directories are given.

Exit codes: 0 no FOOT:FAIL, at least one FOOT:PASS, and the detector
green; 1 otherwise, or when no .test file is found; 2 the check cannot
run (missing binary or fixture).
"""

import argparse
import datetime
import json
import os
import re
import subprocess
import sys
import tempfile
from collections import Counter, defaultdict
from concurrent.futures import ThreadPoolExecutor
from decimal import Decimal

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import check_all  # journal extraction, shared with the bisimulation
import sweep      # amount normalization

NOW = ["--now", "2026/08/26"]
COMMON = ["--permissive", "--real"]  # after the per-file flags and NOW
UNREALIZED = "<Unrealized>"
LIMIT = ("Assets:Brokerage",)
SEP_RE = re.compile(r"^\s*-{10,}\s*$")
VALUE_RE = re.compile(r"(?im)^\s*value\s|;\s*value::?|^\s*define\s|\(\(")
STDOUT_RE = re.compile(r"(?m)^(?:eval|expr)\s")

PRICES = ("pricedb",)
BASIS = ("balance", "--flat", "-B")
PLAIN = ("balance", "--depth", "1")
REPORTS = {"prices": PRICES, "basis": BASIS, "plain": PLAIN}


def valuation_report(valuation):
    """valuation is ("-V",) or ("-X", C)."""
    return ("balance", "--depth", "1", *valuation, "--unrealized",
            "--unrealized-gains", UNREALIZED + ":Gains",
            "--unrealized-losses", UNREALIZED + ":Losses")


def gain_report(valuation):
    exchange = valuation if valuation[0] == "-X" else ()
    return ("balance", "--depth", "1", "-G", *exchange)


# Journals on which C++ refuses a valuation report: the text that the
# error must contain, and the reason.
EXPECTED_REFUSALS = {
    "test/regress/1101.test": {
        "stderr": "Cannot convert a balance with multiple commodities"
                  " to an amount",
        "reason": "a lot price that is itself a two-commodity balance"
                  " cannot be valued (market() in bal -V and bal -G)",
    },
    "test/regress/value-expr-basket-order.test": {
        "stderr": "Cannot convert a balance with multiple commodities"
                  " to an amount",
        "reason": "the basket's value expression is a balance of two"
                  " commodities, which market() cannot convert to one"
                  " amount (bal -V)",
    },
}

# Journals that C++ leaves unfooted: every report that fails, with its
# total, unrealized row and gain, and the reason.  On regress/999-a the
# -X report prints a total that leaves unconverted what its rows
# convert, as before the G7 fix.  The rewrite journals are unfooted by
# design: uses_cost_baseline (src/filters.cc) keeps the revaluation
# between postings for a journal with an account-rewrite or
# payee-rewrite directive, because rewrite_posts copies each posting
# that a rule matches (for payee-rewrite, every posting of a
# transaction whose payee it matches) into a transaction of its own,
# dated by the posting.  On these journals that revaluation books no
# gain under -V or -X.
_REWRITE = ("{} directive: uses_cost_baseline keeps the revaluation"
            " between postings, and bal -V --unrealized and bal -X $"
            " --unrealized each total $200 with no unrealized row while"
            " bal -G and bal -G -X $ report $200; the test pins the -V"
            " report")
_REWRITE_FAILS = {"total": {"$": "200"}, "unrealized": {},
                  "gain": {"$": "200"}}
EXPECTED_NONFOOTING = {
    "test/regress/999-a.test": {
        "failures": {"-X": {"total": {"$": "-1000", "AAPL": "1000"},
                            "unrealized": {}, "gain": {}}},
        "reason": ("AAPL is bought at a virtual cost, (@) $1, which"
                   " records no price; bal -X $ --unrealized converts"
                   " the Assets row to $1000 but prints the total"
                   " unconverted, $-1000 and 1000 AAPL, with no"
                   " unrealized row, and bal -G -X $ prints nothing, as"
                   " on 3.4.1 and on the build before the G7 fix")},
    "test/regress/unrealized-account-rewrite.test": {
        "failures": {"-V": _REWRITE_FAILS, "-X": _REWRITE_FAILS},
        "reason": _REWRITE.format("an account-rewrite")},
    "test/regress/unrealized-payee-rewrite.test": {
        "failures": {"-V": _REWRITE_FAILS, "-X": _REWRITE_FAILS},
        "reason": _REWRITE.format("a payee-rewrite")},
}

# Journals on which a valuation report is not compared because its rows
# are not in one commodity: the -V report when they are in more than
# one, the -X report when one is not C.  Each entry gives the reports
# dropped for that reason, and the reason.  On any journal not listed
# here such a report is a FOOT:FAIL.
MIXED = "values its rows into more than one commodity"
OUTSIDE = "holds a commodity other than C"
_NO_COST = ("bal -B --flat prints no row, since every account nets to"
            " zero at cost, so no commodity is C, and -V values the"
            " holdings of these exchanges into several commodities")
_N = "{} is declared with N, so -V leaves it unvalued, and -X $ values it"
_VIRTUAL = ("AAPL{} is bought at a virtual cost, which records no price,"
            " and neither report values it")
EXPECTED_UNVALUED = {
    "test/baseline/opt-exchange.test": ("-V", _NO_COST),
    "test/baseline/opt-lots-actual.test": (
        "-X", "c is priced in s, which C++ shows in h through the time"
              " units that session_t registers (src/session.cc), and -X"
              " h finds no price from c into h"),
    "test/regress/04C5E1CA.test": (
        "-V -X", "two units of the null commodity are bought at $3.97"
                 " each, which records no price, and neither report"
                 " values them"),
    "test/regress/1602.test": ("-V", _N.format("ACME")),
    "test/regress/1795.test": (
        "-V", "-V leaves the holdings in ETH, the commodity in which BTC"
              " is priced, although ETH is priced in USD; -X USD values"
              " them"),
    "test/regress/1936.test": (
        "-V -X", "EUR carries a lot price in USD and is bought at virtual"
                 " costs, which record no price, and neither report"
                 " values it"),
    "test/regress/2109.test": (
        "-V -X", "FOO carries lot prices in $ and no cost, which records"
                 " no price, and neither report values it"),
    "test/regress/2482.test": (
        "-V -X", "BTC is bought at a virtual total cost in USD, which"
                 " records no price; at cost only the BTC fee remains, so"
                 " C is BTC, and neither report values the USD paid"),
    "test/regress/3227.test": ("-V", _NO_COST),
    "test/regress/3231.test": ("-V", _NO_COST),
    "test/regress/3232.test": ("-V", _NO_COST),
    "test/regress/512.test": (
        "-V", "the one price is of bit, the unit into which a C directive"
              " scales BTC; -V leaves the BTC holding unvalued, and -X"
              " USD values it"),
    "test/regress/958.test": (
        "-V", "the widget carries a fixated lot price, {=$10}, and a C"
              " directive scales widget into thingy, which has no price;"
              " -V leaves the widget unvalued, and -X $ values it"),
    "test/regress/999-a.test": (
        "-V", "AAPL, bought at a virtual cost, records no price, so -V"
              " leaves it unvalued"),
    "test/regress/cov4-directive-N-nomarket.test": (
        "-V", _N.format("MYSTOCK")),
    "test/regress/cov4-directive-nomarket.test": ("-V", _N.format("EUR")),
    "test/regress/coverage-annotate-parse-at-break.test": (
        "-V -X", _VIRTUAL.format(", which carries a lot price,")),
    "test/regress/coverage-virtual-cost.test": (
        "-V -X", _VIRTUAL.format("")),
    "test/regress/coverage-wave11-post-cost-virtual.test": (
        "-V -X", _VIRTUAL.format("")),
    "test/regress/unrealized-basis-exchange.test": (
        "-V -X", "ACME, bought at a virtual cost, records no price, and"
                 " neither report values it, as the test states"),
    "test/regress/unrealized-lot-price-fallback.test": (
        "-V -X", "no holding has a price in $, and neither report values"
                 " a balance of lots at their lot prices, as the test"
                 " states"),
    "test/regress/unrealized-unpriced-holding.test": (
        "-V", "ACME is declared with N, so -V leaves it unvalued, as the"
              " test pins; -X $ values it"),
    "test/regress/3314_inference_order.test": (
        "-V", "the journal's brackets exchange among several commodities"
              " at inferred prices, and -V values each holding into the"
              " commodity it is priced in, USD and ZAR; -X USD values"
              " them and foots"),
    "test/regress/3314_swap_total.test": (
        "-V -X", "BTC and ETH are swapped against EUR through lot prices,"
                 " which record no market price, so -V leaves the"
                 " holdings unvalued and -X EUR has no price from BTC or"
                 " ETH into EUR"),
    "test/regress/3327_3.test": (
        "-V -X", "the split's XYZ shares record no price, so -V leaves"
                 " the holdings in $ and XYZ, and -X $ has no price from"
                 " XYZ into $"),
    "test/regress/lot-strip-keeps-fixated.test": (
        "-V -X", "the lots' {=$5} and {$5} are lot prices, which record"
                 " no market price, so -V leaves AAPL unvalued and -X $"
                 " values only the $ rows"),
    "test/regress/xact-price-inference-reversed.test": (
        "-X", "the reversed inferred price leaves one row priced in EUR"
              " that -X $ has no price from EUR for, beside the $ rows;"
              " -V values them and foots"),
}

# The tests the check exists for: on the default corpus each must PASS,
# compare the reports listed, and print an unrealized row.
EXPECTED_COMPARED = {
    "test/baseline/opt-unrealized-gains.test": "-V -X",
    "test/regress/coverage-report-unrealized.test": "-V -X",
    "test/regress/flat-unrealized.test": "-V -X",
    "test/regress/unrealized-excluded-reports.test": "-V -X",
    "test/regress/unrealized-footing-after-terminus.test": "-V -X",
    "test/regress/unrealized-footing-debit-credit.test": "-V -X",
    "test/regress/unrealized-footing-later-posting.test": "-V -X",
    "test/regress/unrealized-footing-posting-date.test": "-V -X",
    "test/regress/unrealized-footing-same-day-lots.test": "-V -X",
    "test/regress/unrealized-footing-two-lots.test": "-V -X",
    "test/regress/unrealized-footing-unfiltered.test": "-V -X",
    "test/regress/unrealized-unpriced-holding.test": "-X",
}


class Unparseable(ValueError):
    pass


def amount(text, dc):
    """Normalize one C++ amount; an amount whose comma grouping is
    invalid in the file's decimal style is re-read in the other."""
    try:
        return sweep.norm_amount(text, dc)
    except ValueError:
        try:
            return sweep.norm_amount(text, not dc)
        except ValueError as e:
            raise Unparseable(str(e))


def parse_balance(out, dc):
    """-> (rows: acct -> {comm: Decimal}, total: {comm: Decimal}) from a
    depth-1 tree or flat balance: the total block when printed, else
    the sum of the (at most one) displayed row."""
    lines = [l for l in out.splitlines() if l.strip()]
    idx = next((i for i, l in enumerate(lines) if SEP_RE.match(l)), None)
    rows = defaultdict(lambda: defaultdict(Decimal))
    pending = []
    for l in (lines if idx is None else lines[:idx]):
        m = re.match(r"^\s*(\S.*?)\s\s+(\S.*)$", l)
        if m and re.search(r"\d", m.group(1)):
            for p in pending + [m.group(1)]:
                if p.strip() != "0":
                    c, a = amount(p, dc)
                    rows[m.group(2).strip()][c] += Decimal(a)
            pending = []
        else:
            pending.append(l.strip())
    if pending:
        raise Unparseable(f"amount lines without an account: {pending!r}")
    total = defaultdict(Decimal)
    if idx is not None:
        for l in lines[idx + 1:]:
            if l.strip() != "0":
                c, a = amount(l, dc)
                total[c] += Decimal(a)
    else:
        for r in rows.values():
            for c, v in r.items():
                total[c] += v
    nz = lambda d: {c: v for c, v in d.items() if v != 0}
    return {k: nz(v) for k, v in rows.items()}, nz(total)


def show(d):
    return {c: str(v) for c, v in sorted(d.items())}


def verdict(run, dc):
    """Judge one journal from its C++ reports: -> (bin, detail).

    run(name) returns the finished process of one report, where name
    is "prices", "basis" or "plain" (the reports of REPORTS), "-V" or
    "-V gain", or, with the cost commodity as a second argument, "-X"
    or "-X gain".  The detail of a "refusal" is the whole standard
    error of the refused report.  The detail of FOOT:PASS is a dict:
    "compared", the reports compared; "dropped", why each other report
    was not compared; and "prints", what the compared reports and their
    -G counterparts print ("unrealized-row", "rows-only" or "nothing").
    The detail of FOOT:skip-unvalued holds the "dropped" dict alone.
    That of a FOOT:FAIL on a compared report is a dict whose "failures"
    maps every compared report that does not foot to its total,
    unrealized row and gain.  FOOT:skip-no-prices is returned only in
    place of a FOOT:PASS on which every report run was compared."""
    pdb = run("prices")
    if pdb.returncode != 0:
        return "refusal", pdb.stderr.strip()
    b, v, g = run("basis"), run("-V"), run("-V gain")
    for p in (b, v, g):
        if p.returncode != 0:
            return "refusal", p.stderr.strip()
    try:
        basis_rows, basis = parse_balance(b.stdout, dc)
        if basis:
            return "FOOT:skip-unbalanced-at-cost", show(basis)
        cost_comms = {c for r in basis_rows.values() for c in r}
        if len(cost_comms) > 1:
            return "FOOT:skip-multi-commodity", sorted(cost_comms)
        reports = [("-V", v, g)]
        dropped = {}
        cost = next(iter(cost_comms), "")
        if not cost_comms:
            dropped["-X"] = {"why": "not run: bal -B --flat prints no"
                                    " row, so there is no cost"
                                    " commodity"}
        elif not cost:
            dropped["-X"] = {"why": "not run: C is the null commodity"}
        elif any(ch in cost for ch in ",:="):
            dropped["-X"] = {"why": "not run: the symbol of C holds"
                                    " a comma, a colon or an equals"
                                    " sign"}
        else:
            x, gx = run("-X", cost), run("-X gain", cost)
            for p in (x, gx):
                if p.returncode != 0:
                    return "refusal", p.stderr.strip()
            reports.append(("-X", x, gx))
        compared = []
        for name, rp, gp in reports:
            rows, total = parse_balance(rp.stdout, dc)
            comms = {c for r in rows.values() for c in r}
            if name == "-V" and len(comms) > 1:
                dropped[name] = {"why": MIXED, "commodities": sorted(comms)}
                continue
            if name == "-X" and comms - cost_comms:
                dropped[name] = {"why": OUTSIDE,
                                 "commodities": sorted(comms - cost_comms)}
                continue
            _, gain = parse_balance(gp.stdout, dc)
            compared.append((name, rows, total, gain))
    except Unparseable as e:
        return "FOOT:FAIL", f"unparseable C++ output: {e}"
    if not compared:
        return "FOOT:skip-unvalued", {"dropped": dropped}
    failures = {}
    for name, rows, total, gain in compared:
        unrealized = rows.get(UNREALIZED, {})
        if total or unrealized != {c: -a for c, a in gain.items()}:
            failures[name] = {"total": show(total),
                              "unrealized": show(unrealized),
                              "gain": show(gain)}
    if failures:
        return "FOOT:FAIL", {"cost_commodity": cost,
                             "failures": failures, "dropped": dropped}
    if not pdb.stdout.strip() and len(compared) == len(reports):
        plain = run("plain")
        if plain.returncode == 0 and all(
                rp.stdout == plain.stdout and not gp.stdout.strip()
                for _, rp, gp in reports):
            return "FOOT:skip-no-prices", None
    if any(rows.get(UNREALIZED) for _, rows, _, _ in compared):
        prints = "unrealized-row"
    elif any(any(rows.values()) or gain for _, rows, _, gain in compared):
        prints = "rows-only"
    else:
        prints = "nothing"
    return "FOOT:PASS", {"compared": [n for n, *_ in compared],
                         "dropped": dropped, "prints": prints}


class Checker:
    def __init__(self, ledger, env):
        self.ledger, self.env = ledger, env

    def run(self, flags, dat, report):
        return subprocess.run(
            [self.ledger, "--args-only", *flags, *COMMON, "-f", dat,
             *report],
            capture_output=True, text=True, env=self.env)

    def check(self, dat, flags, dc, v_query=(), x_query=(),
              gain_dat=None, xgain_dat=None):
        """Run the reports of one journal and judge them with verdict():
        -> (bin, detail).  v_query and x_query limit the -V and the -X
        report alone, and gain_dat and xgain_dat, when given, are the
        files that `bal -G` and `bal -G -X` read (the detector's
        one-sided perturbations)."""
        def run(name, cost=None):
            valuation = ("-X", cost) if cost else ("-V",)
            if name == "-V":
                report, on = valuation_report(valuation) + v_query, dat
            elif name == "-X":
                report, on = valuation_report(valuation) + x_query, dat
            elif name == "-V gain":
                report, on = gain_report(valuation), gain_dat or dat
            elif name == "-X gain":
                report, on = gain_report(valuation), xgain_dat or dat
            else:
                report, on = REPORTS[name], dat
            return self.run(flags, on, report)
        return verdict(run, dc)


def canned(outputs):
    """A run() for verdict() that returns the given standard output of
    each report, and nothing for the others, without running C++."""
    def run(name, cost=None):
        return subprocess.CompletedProcess([], 0, outputs.get(name, ""), "")
    return run


def classify(rel, b, why):
    """Apply EXPECTED_REFUSALS, EXPECTED_NONFOOTING and
    EXPECTED_UNVALUED to one result of verdict(): -> (bin, detail)."""
    refusal = EXPECTED_REFUSALS.get(rel)
    nonfooting = EXPECTED_NONFOOTING.get(rel)
    unvalued = EXPECTED_UNVALUED.get(rel)
    if b == "refusal":
        if refusal and refusal["stderr"] in why:
            return "FOOT:skip-cpp-refusal", refusal["reason"]
        if refusal:
            return "FOOT:FAIL", ("EXPECTED_REFUSALS entry refuses for"
                                 " another reason; correct it: "
                                 + why[-200:])
        return "FOOT:FAIL", "C++ refused a report: " + why[-200:]
    if refusal:
        return "FOOT:FAIL", ("EXPECTED_REFUSALS entry no longer refuses;"
                             f" remove it: {b}: {why}")
    if nonfooting:
        if (b == "FOOT:FAIL" and isinstance(why, dict)
                and why["failures"] == nonfooting["failures"]):
            b, why = "FOOT:skip-expected-nonfooting", {
                "reason": nonfooting["reason"], "observed": why}
        elif b == "FOOT:PASS":
            return "FOOT:FAIL", ("EXPECTED_NONFOOTING entry now foots;"
                                 " remove it")
        else:
            return "FOOT:FAIL", ("EXPECTED_NONFOOTING entry no longer"
                                 f" fails as recorded; correct it: {b}:"
                                 f" {why}")
    elif b == "FOOT:FAIL":
        return b, why
    if b == "FOOT:skip-expected-nonfooting":
        dropped = why["observed"]["dropped"]
    elif b in ("FOOT:PASS", "FOOT:skip-unvalued"):
        dropped = why["dropped"]
    elif unvalued:
        return "FOOT:FAIL", ("EXPECTED_UNVALUED entry no longer applies;"
                             f" remove or correct it: {b}: {why}")
    else:
        return b, why
    left = " ".join(sorted(n for n, d in dropped.items()
                           if d["why"] in (MIXED, OUTSIDE)))
    if unvalued and left != unvalued[0]:
        return "FOOT:FAIL", ("EXPECTED_UNVALUED entry records"
                             f" {unvalued[0]!r} but the reports whose"
                             f" rows are not in one commodity are"
                             f" {left!r}; remove or correct it: {b}:"
                             f" {why}")
    if left and not unvalued:
        return "FOOT:FAIL", ("the rows of a report are not in one"
                             " commodity, and EXPECTED_UNVALUED in"
                             " test/semantic/footing.py does not record"
                             f" it with a reason: {b}: {why}")
    return b, why


def classify_selftest():
    """Pass synthetic results built from the tables through classify():
    -> the cases that land in another bin than they must."""
    other = "test/regress/not-in-any-table.test"
    ref_rel, ref = sorted(EXPECTED_REFUSALS.items())[0]
    nf_rel, nf = next((rel, e) for rel, e in
                      sorted(EXPECTED_NONFOOTING.items())
                      if sorted(e["failures"]) == ["-V", "-X"]
                      and rel not in EXPECTED_UNVALUED)
    nfu_rel, nfu = next((rel, e) for rel, e in
                        sorted(EXPECTED_NONFOOTING.items())
                        if EXPECTED_UNVALUED.get(rel, ("",))[0] == "-V")
    uv_rel = next(rel for rel, (reports, _) in
                  sorted(EXPECTED_UNVALUED.items())
                  if reports == "-V" and rel not in EXPECTED_NONFOOTING)
    mixed = {"why": MIXED, "commodities": ["$", "K"]}
    outside = {"why": OUTSIDE, "commodities": ["K"]}
    fail = "FOOT:FAIL"

    def passed(dropped):
        return {"compared": [n for n in ("-V", "-X") if n not in dropped],
                "dropped": dropped, "prints": "unrealized-row"}

    def failed(failures, dropped=None):
        return {"cost_commodity": "$", "failures": failures,
                "dropped": dropped or {}}

    wrong = {"total": {"$": "999"}, "unrealized": {}, "gain": {"$": "200"}}
    cases = [  # (label, file, bin, detail, the bin classify() must give)
        ("recorded refusal", ref_rel, "refusal",
         "Error: " + ref["stderr"], "FOOT:skip-cpp-refusal"),
        ("recorded refusal, another error", ref_rel, "refusal",
         "Error: Divide by zero", fail),
        ("recorded refusal that no longer refuses", ref_rel,
         "FOOT:PASS", passed({}), fail),
        ("refusal not recorded", other, "refusal",
         "Error: " + ref["stderr"], fail),
        ("recorded non-footing", nf_rel, fail,
         failed(nf["failures"]), "FOOT:skip-expected-nonfooting"),
        ("recorded non-footing that foots", nf_rel, "FOOT:PASS",
         passed({}), fail),
        ("recorded non-footing, another -V failure", nf_rel, fail,
         failed({**nf["failures"], "-V": wrong}), fail),
        ("recorded non-footing, -V alone fails", nf_rel, fail,
         failed({"-V": nf["failures"]["-V"]}), fail),
        ("recorded non-footing, -X alone fails", nf_rel, fail,
         failed({"-X": nf["failures"]["-X"]}), fail),
        ("recorded non-footing, a report unrecorded as unvalued", nf_rel,
         fail, failed(nf["failures"], {"-V": mixed}), fail),
        ("non-footing not recorded", other, fail,
         failed(nf["failures"]), fail),
        ("recorded non-footing with its recorded unvalued report",
         nfu_rel, fail, failed(nfu["failures"], {"-V": mixed}),
         "FOOT:skip-expected-nonfooting"),
        ("recorded non-footing without its recorded unvalued report",
         nfu_rel, fail, failed(nfu["failures"]), fail),
        ("recorded unvalued report", uv_rel, "FOOT:PASS",
         passed({"-V": mixed}), "FOOT:PASS"),
        ("recorded unvalued report that is compared", uv_rel,
         "FOOT:PASS", passed({}), fail),
        ("recorded unvalued report, another report unvalued", uv_rel,
         "FOOT:PASS", passed({"-X": outside}), fail),
        ("recorded unvalued journal in another bin", uv_rel,
         "FOOT:skip-no-prices", None, fail),
        ("unvalued -V report not recorded", other, "FOOT:PASS",
         passed({"-V": mixed}), fail),
        ("unvalued -X report not recorded", other, "FOOT:PASS",
         passed({"-X": outside}), fail),
        ("unvalued journal not recorded", other, "FOOT:skip-unvalued",
         {"dropped": {"-V": mixed, "-X": outside}}, fail),
    ]
    faults = []
    for label, rel, b, why, want in cases:
        got = classify(rel, b, why)[0]
        if got != want:
            faults.append(f"{label}: {got}, not {want}")
    return faults


# Bins whose files the artifact counts but does not name: the check is
# vacuous on them, or they are decided before any report runs.
COUNT_ONLY = ("FOOT:skip-no-prices", "SKIP:negative", "SKIP:no-journal")


class Tally:
    """The bins and summary counts of a run.  The journals and the
    detector's planted results pass through the same add(), failed()
    and exit_code()."""

    def __init__(self):
        self.bins = defaultdict(list)
        self.details = []          # the files that carry information
        self.passed = {}           # file -> its FOOT:PASS detail
        self.passed_by = Counter()     # reports compared -> files
        self.dropped_by = Counter()    # report and reason dropped -> files
        self.prints_by = defaultdict(list)  # what the passing reports print
        self.pass_dropped = {}     # file -> why its other reports were dropped

    def add(self, rel, b, why=None):
        self.bins[b].append(rel)
        if b == "FOOT:PASS":
            self.passed[rel] = why
            self.passed_by[" ".join(why["compared"])] += 1
            self.prints_by[why["prints"]].append(rel)
            if why["dropped"]:
                self.pass_dropped[rel] = why["dropped"]
                for name, d in why["dropped"].items():
                    self.dropped_by[f"{name} {d['why']}"] += 1
        elif b not in COUNT_ONLY:
            self.details.append({"file": rel, "status": b, "why": why})

    def failed(self):
        """-> why the run fails on these files, or None: a FOOT:FAIL, or
        no file compared and found to foot."""
        if self.bins.get("FOOT:FAIL"):
            return f"{len(self.bins['FOOT:FAIL'])} FOOT:FAIL"
        if not self.bins.get("FOOT:PASS"):
            return "no journal was compared and found to foot"
        return None

    def exit_code(self, detector_ok):
        return 1 if self.failed() or not detector_ok else 0


# The detector's second fixture, shaped like regress/1602.test: pricedb
# lists no price, because ACME is declared with N, yet bal -X $ values
# the holding at $75 and books a revaluation.
UNPRICED_FIXTURE = """\
N ACME

P 2020/06/01 ACME $75.00

2020/01/01 Buy ACME
    Assets:Investments   10 ACME @ $50.00
    Assets:Cash
"""

# Canned C++ output for verdict(), and the bin it must give: output it
# cannot parse (a -V amount line that names no account); three journals
# without prices, one on which -X alone revalues, one whose fixated
# price -V and -X apply at cost (the shape of
# test/baseline/dir-fixed.test), and one on which no report changes
# anything; and a journal with a price on which no report changes
# anything.
_ROWS = "  $10  A\n  $-10  B\n--------------------\n  0\n"
_HELD = "  10 X  A\n  $-10  B\n--------------------\n  $-10\n  10 X\n"
CANNED = {
    "unparseable": ({"prices": "P 2024/03/01 00:00:00 AAPL $170\n",
                     "-V": "  $10\n"}, "FOOT:FAIL"),
    "no prices, -X revalues": ({
        "basis": _ROWS, "plain": _ROWS, "-V": _ROWS,
        "-X": ("  $15  A\n  $-10  B\n  $-5  " + UNREALIZED
               + "\n--------------------\n  0\n"),
        "-X gain": "  $5  A\n"}, "FOOT:PASS"),
    "no prices, fixated price": ({
        "basis": _ROWS, "plain": _HELD, "-V": _ROWS, "-X": _ROWS},
        "FOOT:PASS"),
    "no prices, identity": ({
        "basis": _ROWS, "plain": _ROWS, "-V": _ROWS, "-X": _ROWS},
        "FOOT:skip-no-prices"),
    "prices, identity": ({
        "prices": "P 2024/03/01 00:00:00 AAPL $170\n", "basis": _ROWS,
        "plain": _ROWS, "-V": _ROWS, "-X": _ROWS}, "FOOT:PASS"),
}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--ledger", default=None)
    ap.add_argument("--source", default=None)
    ap.add_argument("dirs", nargs="*")
    args = ap.parse_args()

    repo = os.path.abspath(args.source) if args.source else sweep.REPO
    ledger = args.ledger or os.path.join(repo, "build", "ledger")
    env = {**os.environ, "TZ": os.environ.get("TZ", "America/Chicago")}
    dirs = args.dirs or [os.path.join(repo, "test", d)
                         for d in ("baseline", "regress", "manual")]
    if not os.path.isfile(ledger):
        print(f"cannot check: ledger binary not found at {ledger}",
              file=sys.stderr)
        sys.exit(2)
    fixture = os.path.join(repo, "test", "semantic", "footing.dat")
    if not os.path.isfile(fixture):
        print(f"detector fixture missing: {fixture}", file=sys.stderr)
        sys.exit(2)

    tally = Tally()
    with tempfile.TemporaryDirectory() as tmp:
        home = os.path.join(tmp, "home")  # holds no .pricedb
        os.makedirs(home)
        checker = Checker(ledger, {**env, "HOME": home})
        jobs = []
        tests = sorted(os.path.join(root, f) for d in dirs
                       for root, _, files in os.walk(d)
                       for f in files if f.endswith(".test"))
        if not tests:
            print("valuation footing: no .test file found under "
                  + ", ".join(dirs), file=sys.stderr)
            sys.exit(1)
        for i, tpath in enumerate(tests):
            rel = os.path.relpath(tpath, repo)
            journal, skip, flags, dc = check_all.classify_test_file(tpath)
            if journal is not None and check_all.has_python_block(journal):
                skip = "python"
            if skip in ("negative", "no-journal", "python"):
                tally.add(rel, "SKIP:" + skip)
                continue
            if skip:  # not UTF-8: a failure here, as in the bisimulation
                tally.add(rel, "FOOT:FAIL", skip)
                continue
            if VALUE_RE.search(journal):
                tally.add(rel, "FOOT:skip-value-expr")
                continue
            if STDOUT_RE.search(journal):
                tally.add(rel, "FOOT:skip-stdout-directive")
                continue
            if "--now" in flags:
                k = flags.index("--now")
                flags = flags[:k] + flags[k + 2:]
            fdir = os.path.join(tmp, f"{i:05d}")
            os.makedirs(fdir)
            dat = os.path.join(fdir, os.path.basename(tpath))
            with open(dat, "w", encoding="utf-8") as fh:
                fh.write(journal)
            check_all.copy_includes(journal, os.path.dirname(tpath), fdir)
            jobs.append((rel, dat, flags + NOW, dc))

        # Detector: the fixture, each valuation report limited to one
        # account, the perturbed price read by one -G report alone, and
        # the unpriced fixture.
        with open(fixture, encoding="utf-8") as fh:
            fj = fh.read()
        perturbed, n = re.subn(r"^(P \S+ AAPL) \$170$", r"\1 $171", fj,
                               count=1, flags=re.M)
        if n != 1:
            print("detector perturbation not applicable to " + fixture,
                  file=sys.stderr)
            sys.exit(2)
        pg = os.path.join(tmp, "footing-perturbed.dat")
        with open(pg, "w", encoding="utf-8") as fh:
            fh.write(perturbed)
        unpriced = os.path.join(tmp, "footing-unpriced.dat")
        with open(unpriced, "w", encoding="utf-8") as fh:
            fh.write(UNPRICED_FIXTURE)
        planted = {  # name -> (keyword arguments, report that must fail)
            "-V limited": ({"v_query": LIMIT}, "-V"),
            "-X limited": ({"x_query": LIMIT}, "-X"),
            "-G perturbed": ({"gain_dat": pg}, "-V"),
            "-G -X perturbed": ({"xgain_dat": pg}, "-X"),
        }

        with ThreadPoolExecutor(max_workers=os.cpu_count() or 4) as ex:
            results = list(ex.map(
                lambda j: (j[0], checker.check(j[1], j[2], j[3])), jobs))
            control = checker.check(fixture, NOW, False)
            defects = {k: checker.check(fixture, NOW, False, **kw)
                       for k, (kw, _) in planted.items()}
            unpriced_result = checker.check(unpriced, NOW, False)

    for rel, (b, why) in results:
        tally.add(rel, *classify(rel, b, why))
    classify_faults = classify_selftest()
    canned_results = {k: verdict(canned(out), False)
                      for k, (out, _) in CANNED.items()}
    unparseable = canned_results["unparseable"]
    # The planted results, through the same classification, tally and
    # exit decision as the journals: each defect and the unparseable
    # output must land in FOOT:FAIL and fail the run, and the control
    # alone must pass.
    probe = Tally()
    probe.add("<control>", *classify("<control>", *control))
    control_alone = probe.exit_code(True)
    vacuous = Tally()  # compares nothing, so it must fail the run
    vacuous.add("<vacuous>", *classify(
        "<vacuous>", *canned_results["no prices, identity"]))
    for k, d in [*defects.items(), ("unparseable", unparseable)]:
        probe.add(f"<{k}>", *classify(f"<{k}>", *d))
    witness_faults = []
    if not args.dirs:
        where = {rel: k for k, v in tally.bins.items() for rel in v}
        for rel, reports in sorted(EXPECTED_COMPARED.items()):
            p = tally.passed.get(rel)
            if p is None:
                witness_faults.append(f"{rel}: {where.get(rel, 'missing')}")
            elif (p["compared"] != reports.split()
                  or p["prints"] != "unrealized-row"):
                witness_faults.append(f"{rel}: compares {p['compared']},"
                                      f" prints {p['prints']}")
    detector_ok = (
        control[0] == "FOOT:PASS"
        and control[1]["compared"] == ["-V", "-X"]
        and control[1]["prints"] == "unrealized-row"
        and all(d[0] == "FOOT:FAIL" and isinstance(d[1], dict)
                and list(d[1]["failures"]) == [planted[k][1]]
                for k, d in defects.items())
        and unpriced_result[0] == "FOOT:PASS"
        and unpriced_result[1]["compared"] == ["-X"]
        and unpriced_result[1]["prints"] == "unrealized-row"
        and all(canned_results[k][0] == want
                for k, (_, want) in CANNED.items())
        and control_alone == 0
        and vacuous.exit_code(True) == 1
        and probe.bins["FOOT:PASS"] == ["<control>"]
        and probe.bins["FOOT:FAIL"] == [f"<{k}>" for k in
                                        [*defects, "unparseable"]]
        and probe.exit_code(True) == 1
        and not classify_faults and not witness_faults)
    failed = tally.failed()
    code = tally.exit_code(detector_ok)

    def first_line(cmd, **kw):
        try:
            out = subprocess.run(cmd, capture_output=True, text=True,
                                 **kw).stdout
            return (out.splitlines() or ["unknown"])[0].strip() or "unknown"
        except OSError:
            return "unknown"

    x = ("-X", "C")
    artifact = {
        "tuple": {
            "utc": datetime.datetime.now(datetime.timezone.utc).isoformat(),
            "git_rev": first_line(["git", "rev-parse", "HEAD"], cwd=repo),
            "ledger_version": first_line([ledger, "--args-only",
                                          "--version"], env=env),
            "tz": env["TZ"], "home": "an empty directory",
            "capture": "test/semantic/footing.py",
            "cpp_flags_common": ["--args-only", *NOW, *COMMON],
            "cpp_flags_note": ("per-file flags (--decimal-comma,"
                               " --recursive-aliases,"
                               " --input-date-format) are derived from"
                               " each test's own command lines, as in"
                               " check_all.py; the test's own --now is"
                               " replaced by the common one"),
            "reports": {
                **{k: list(v) for k, v in REPORTS.items()},
                "-V": list(valuation_report(("-V",))),
                "-V gain": list(gain_report(("-V",))),
                "-X": list(valuation_report(x)),
                "-X gain": list(gain_report(x))}},
        "detector": {"ok": detector_ok, "control": control,
                     "defects": defects, "unpriced": unpriced_result,
                     "canned": canned_results,
                     "planted_bins": dict(sorted(probe.bins.items())),
                     "classify_faults": classify_faults,
                     "witnesses_required": not args.dirs,
                     "witness_faults": witness_faults},
        "counts": {k: len(v) for k, v in sorted(tally.bins.items())},
        "pass_compared": dict(sorted(tally.passed_by.items())),
        "pass_dropped_counts": dict(sorted(tally.dropped_by.items())),
        "pass_prints_counts": {k: len(v) for k, v in
                               sorted(tally.prints_by.items())},
        "details": tally.details,
        "bins": {k: v for k, v in sorted(tally.bins.items())
                 if k != "FOOT:PASS" and k not in COUNT_ONLY},
        "pass_dropped": dict(sorted(tally.pass_dropped.items())),
        "pass_prints": {k: v for k, v in sorted(tally.prints_by.items())
                        if k != "unrealized-row"},
    }
    stamp = datetime.datetime.now(datetime.timezone.utc).strftime(
        "%Y%m%dT%H%M%SZ")
    try:
        os.makedirs(os.path.join(HERE, "artifacts"), exist_ok=True)
        apath = os.path.join(HERE, "artifacts", f"footing-{stamp}.json")
        with open(apath, "w") as fh:
            json.dump(artifact, fh, indent=2)
    except OSError:
        apath = os.path.join(tempfile.gettempdir(), f"footing-{stamp}.json")
        with open(apath, "w") as fh:
            json.dump(artifact, fh, indent=2)

    if detector_ok:
        print("selftest: footing detector fixtures green")
    else:
        print("selftest: footing detector RED")
        print(f"       control: {control}")
        for k, d in defects.items():
            print(f"       {k}: {d}")
        print(f"       unpriced: {unpriced_result}")
        for k, d in canned_results.items():
            print(f"       canned {k}: {d}")
        print(f"       planted bins: {dict(probe.bins)}")
        for f in classify_faults:
            print(f"       classification: {f}")
        for f in witness_faults:
            print(f"       EXPECTED_COMPARED: {f}")
    if args.dirs:
        print("EXPECTED_COMPARED not required: directories given")
    print(f"valuation footing over {len(tests)} .test files:")
    for k in sorted(tally.bins):
        print(f"  {k:<29} {len(tally.bins[k])}")
    if tally.passed_by:
        print("  FOOT:PASS by reports compared: " + ", ".join(
            f"{k} {v}" for k, v in sorted(tally.passed_by.items())))
        print("  FOOT:PASS by what they print: " + ", ".join(
            f"{k} {len(v)}" for k, v in sorted(tally.prints_by.items())))
    for k, v in sorted(tally.dropped_by.items()):
        print(f"  FOOT:PASS with {k}: {v}")
    for d in tally.details:
        if d["status"] == "FOOT:FAIL":
            print(f"  FAIL {d['file']}\n       why: {d['why']}")
    if failed:
        print(f"valuation footing fails: {failed}")
    print(f"artifact: {os.path.relpath(apath, repo)}")
    sys.exit(code)


if __name__ == "__main__":
    main()
