#!/usr/bin/env python3
"""Suite-wide oracle bisimulation: every positive test, on `make check`.

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
                    section of lean/README.md).
  SKIP:negative   — the test intentionally exercises errors; its
                    journal is not claimed valid.
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

Exit codes: 0 all compared files PASS and the ratchet bins are empty;
1 any FAIL, or any entry in SKIP:out-of-scope or SKIP:cpp-parse (both
reached zero against the full suite and are held there — an entry
reappearing means one side stopped reading a journal it used to read,
which is a regression, not a skip); 77 the Lean toolchain or a built
oracle is unavailable (ctest SKIP_RETURN_CODE — skipped, visibly);
2 the harness itself cannot run (missing binary, missing fixtures,
oracle invocation failure).
"""

import argparse
import datetime
import json
import os
import re
import subprocess
import shutil
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import sweep  # normalization + C++ balance parsing

TEST_RE = re.compile(r"^test\s+(.*?)(?:\s*->\s*([0-9]+))?\s*$")


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
            # part of the comparison tuple for this file
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


INCLUDE_RE = re.compile(r"^\s*!?include\s+(.+?)\s*$", re.M)


def copy_includes(journal, src_dir, dst_dir, depth=4):
    """Copy files referenced by include lines (globs included) next to
    the extracted journal so both sides resolve them identically."""
    if depth == 0:
        return
    import glob as _glob
    # A relative include may step above the journal's own directory
    # (test/regress does this), which lands the copy in the shared
    # session directory one level up.  That is allowed; escaping the
    # session directory itself is not.
    session_root = os.path.dirname(os.path.abspath(dst_dir))
    for m in INCLUDE_RE.finditer(journal):
        name = m.group(1).strip("'\"").replace("\\", "")
        for src in sorted(_glob.glob(os.path.join(src_dir, name))):
            rel = os.path.relpath(src, src_dir)
            dst = os.path.join(dst_dir, rel)
            if not os.path.abspath(dst).startswith(session_root + os.sep):
                continue
            if os.path.isfile(src) and not os.path.exists(dst):
                os.makedirs(os.path.dirname(dst), exist_ok=True)
                with open(src, "rb") as fi, open(dst, "wb") as fo:
                    fo.write(fi.read())
                with open(src, encoding="utf-8", errors="replace") as fh:
                    copy_includes(fh.read(), src_dir, dst_dir, depth - 1)


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

    lean_dir = os.environ.get("LEDGER_LEAN_DIR", os.path.join(repo, "lean"))
    if not os.path.isfile(os.path.join(lean_dir, "lakefile.lean")):
        print("SKIP: Lean oracle tree not present (submodule not"
              " initialized and LEDGER_LEAN_DIR unset)"); sys.exit(77)
    # Probe the toolchain exactly once, up front.  The Nix build sets
    # LEDGER_LEAN_DIR to the semantics flake's oracle output and puts
    # the matching lake on PATH; development runs use the pinned dev
    # shell of the lean/ submodule.  Past this point a
    # failing oracle exits 2: substring-matching error text to decide
    # between SKIP and FAIL would let a real divergence read as green.
    direct_lake = "LEDGER_LEAN_DIR" in os.environ
    if direct_lake and not shutil.which("lake"):
        print("SKIP: LEDGER_LEAN_DIR set but lake not on PATH")
        sys.exit(77)
    if not direct_lake and not shutil.which("nix"):
        print("SKIP: nix unavailable for the lean/ dev shell")
        sys.exit(77)
    if not direct_lake and not os.path.isdir(os.path.join(lean_dir, ".lake")):
        # The submodule is checked out but the oracle has never been
        # built here; running now would make lake fetch and build the
        # full Mathlib dependency tree as a side effect of `ctest`.
        # That is a decision for the developer, not the test.
        print("SKIP: oracle not built in " + lean_dir + " — run\n"
              "  nix develop ./lean --command bash -c"
              " 'lake exe cache get && lake build'\n"
              "once (or set LEDGER_LEAN_DIR to a built oracle tree)")
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

    bins = {"PASS": [], "FAIL": [], "SKIP:negative": [],
            "SKIP:no-journal": [], "SKIP:python": [],
            "SKIP:out-of-scope": [], "SKIP:cpp-parse": []}
    details = []

    with tempfile.TemporaryDirectory() as tmp:
        candidates = []  # (test_path, dat_path, cpp_flags, dc)
        for i, tpath in enumerate(tests):
            journal, skip, cpp_flags, dc = classify_test_file(tpath)
            if journal is not None and any(
                    ln == "python" or ln.startswith("python ")
                    for ln in (l.strip() for l in journal.splitlines())):
                # a python block anywhere in the journal makes the
                # file non-comparable, whatever its name
                journal, skip = None, "python"
            rel = os.path.relpath(tpath, repo)
            if skip == "negative":
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
                copy_includes(journal, os.path.dirname(tpath), fdir)
                candidates.append((tpath, dat, cpp_flags, dc))

        # Detector fixtures ride the same batch: a planted one-sided
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
        candidates.append(("__selftest__", p1, [], False))
        selftest = (p1, p2)

        # Batched oracle runs (chunked argv).
        oracle = {}  # dat_path -> set(rows) | ("ERROR", msg)
        styles = {}  # dat_path -> set of decimal-comma commodities
        CHUNK = 800
        for lo in range(0, len(candidates), CHUNK):
            chunk = candidates[lo:lo + CHUNK]
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
                argv.append(d)
            # Oracle invocation.  When LEDGER_LEAN_DIR is set (the Nix
            # build points it at the semantics flake's oracle output and
            # puts the matching lake on PATH), run lake directly in that
            # tree; otherwise run inside the pinned dev shell of the
            # lean/ submodule (`nix develop` with that tree as cwd), so
            # a stray lake on PATH never bypasses the pinned toolchain.
            # Toolchain absence was probed once above and is the ONLY
            # skip: any failure past this point is a red result, never
            # a SKIPPED one.
            if direct_lake:  # noqa: simple dispatch, probed above
                cmd = ["lake", "env", "lean",
                       "--run", "Ledger/Driver.lean", *argv]
            else:
                cmd = ["nix", "develop", "--command", "lake", "env", "lean",
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
                elif cur and line.strip():
                    if isinstance(oracle[cur], set):
                        oracle[cur].add(line.strip())

        for tpath, dat, cpp_flags, dc in candidates:
            if tpath == "__selftest__":
                continue
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
            if cpp_rows == res:
                bins["PASS"].append(rel)
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
    # the flake passes the semantics revision in LEDGER_LEAN_REV.  In
    # a git hook, GIT_DIR would make git answer for the ledger
    # repository instead.
    oracle_rev = (os.environ.get("LEDGER_LEAN_REV")
                  or run_line(["git", "-C", lean_dir, "rev-parse", "HEAD"],
                              env=sweep.oracle_env(env)))
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
                               " each test's own command lines")},
        "counts": {k: len(v) for k, v in bins.items()},
        "details": details,
        "bins": {k: v for k, v in bins.items() if k != "PASS"},
    }
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
    for k in ("PASS", "FAIL", "SKIP:out-of-scope", "SKIP:cpp-parse",
              "SKIP:negative", "SKIP:no-journal", "SKIP:python"):
        print(f"  {k:<18} {len(bins[k])}")
    for d in details:
        if d["status"] == "FAIL":
            print(f"  FAIL {d['file']}")
            for k in ("why", "only_oracle", "only_cpp"):
                if k in d:
                    print(f"       {k}: {d[k]}")
    print(f"artifact: {os.path.relpath(apath, repo)}")
    # Ratchet: both bins below reached zero against the full suite,
    # and gates only tighten.  An entry here means one side stopped
    # reading a journal it used to read — an oracle that rejects
    # what it once parsed, or a C++ ledger that errors where it once
    # balanced — and letting that bin as a skip would turn either
    # regression green.
    ratchet = bins["SKIP:out-of-scope"] or bins["SKIP:cpp-parse"]
    if ratchet and not bins["FAIL"]:
        print("ratchet: SKIP:out-of-scope and SKIP:cpp-parse must stay"
              " empty; a comparable journal is no longer compared")
    sys.exit(1 if (bins["FAIL"] or ratchet) else 0)


if __name__ == "__main__":
    main()
