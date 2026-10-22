#!/usr/bin/env python3
"""Differential corpus sweep over a hand-kept manifest.

Runs the C++ ledger (`balance --flat --no-total`) and the Lean oracle
driver on every manifest file, normalizes both sides to canonical
`(commodity, amount, account)` triples, and diffs.  check_all.py, the
ctest entry point, imports this module for the normalization and
C++-balance parsing; running this file directly is a developer tool
for sweeping the manifest (and any generated journals) by hand.

Discipline (see the bisimulation section of lean/README.md):
  - every run writes an artifact (absence of output is never silent
    success) into test/semantic/artifacts/, embedding the comparison
    tuple: git revision, ledger --version, file digest, TZ;
  - files marked out of scope are *checked to be rejected* by the
    driver — a parser that silently accepts an out-of-scope construct
    is itself a defect;
  - divergences are recorded raw as `unadjudicated-divergence`:
    deciding which side is wrong is adjudication, and the instrument
    does not adjudicate.

Usage: [LEDGER=path] python3 test/semantic/sweep.py
Exit 0 iff every in-scope file PASSes and every out-of-scope file is
rejected by the driver.
"""

import datetime
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))

# (repo-relative path, scope, note)
MANIFEST = [
    ("test/semantic/demo.dat", "in", "golden demo journal"),
    ("test/input/parsing.dat", "in", "tabs, -$ prefix, no trailing newline"),
    ("test/input/transfer.dat", "in", "suffix commodity 'bytes'"),
    ("test/input/demo.ledger", "in", "plain 13-xact journal"),
    ("test/input/standard.dat", "in", "precision-tolerant balancing, virtuals"),
    ("test/input/sample.dat", "in", "auto-xacts, periodic, costs, unicode"),
    ("test/input/drewr.dat", "in", "auto-xacts, periodic"),
    ("test/input/drewr3.dat", "in", "auto-xacts"),
    ("test/input/wow.dat", "in", "C/D directives ignored as display-only — empirical check"),
    ("test/input/divzero.dat", "in",
     "expression amounts; the historical divide-by-zero is fixed in this build"),
]


def norm_number(num: str, decimal_comma: bool = False) -> str:
    num = num.replace("'", "")
    if decimal_comma:
        num = num.replace(".", "\x00").replace(",", ".").replace("\x00", ",")
    if "," in num:
        groups = num.split(".")[0].split(",")
        if not (1 <= len(groups[0]) <= 3) or any(len(g) != 3 for g in groups[1:]) \
                or "," in (num.split(".", 1) + [""])[1]:
            raise ValueError(f"ambiguous comma grouping: {num!r}")
    num = num.replace(",", "")
    if "." in num:
        ip, fp = num.split(".", 1)
        fp = fp.rstrip("0")
        ip = str(int(ip)) if ip else "0"
        return ip + ("." + fp if fp else "")
    return str(int(num))


NUM = r"[\d.,']+"


def norm_amount(s: str, decimal_comma: bool = False, dc_comms=frozenset()):
    """'$ -1,000.00' / '-$30' / '59820 bytes' / '"a b" 1' -> (comm, canon)."""
    s = s.strip()
    neg = False
    if s.startswith("-"):
        neg, s = True, s[1:].strip()
    comm = num = None
    m = re.match(r'^"([^"]+)"\s*(-?)\s*(' + NUM + r')$', s)
    if m:
        comm, num = m.group(1), m.group(3)
        if m.group(2) == "-":
            neg = not neg
    if comm is None:
        m = re.match(r'^(-?)\s*(' + NUM + r')\s*"([^"]+)"$', s)
        if m:
            comm, num = m.group(3), m.group(2)
            if m.group(1) == "-":
                neg = not neg
    if comm is None:
        m = re.match(r"^([^\d-][^\d]*?)\s*(-?)\s*(" + NUM + r")$", s)
        if m:
            comm, num = m.group(1), m.group(3)
            if m.group(2) == "-":
                neg = not neg
    if comm is None:
        # 'N2 1,234.500': a prefix symbol that needs quotes, here for its
        # digit, prints without them when a space sets it off from the
        # number (commodity_t::print elides them; regress/3312)
        m = re.match(r"^([^\d\s-].*?)\s+(-?)\s*(" + NUM + r")$", s)
        if m:
            comm, num = m.group(1), m.group(3)
            if m.group(2) == "-":
                neg = not neg
    if comm is None:
        m = re.match(r"^(-?)\s*(" + NUM + r")\s*(.*)$", s)
        if not m:
            raise ValueError(f"unparseable amount: {s!r}")
        if m.group(1) == "-":
            neg = not neg
        num, comm = m.group(2), m.group(3).strip()
    if len(comm) >= 2 and comm.startswith('"') and comm.endswith('"'):
        comm = comm[1:-1]  # C++ re-quotes commodities needing quotes
    dc = decimal_comma or comm in dc_comms
    canon = norm_number(num, dc)
    if canon in ("0", "0.0"):
        neg = False
    return comm, ("-" if neg else "") + canon


def parse_cpp_balance(out: str, decimal_comma: bool = False, dc_comms=frozenset()):
    """balance --flat --no-total output -> set of 'comm|amount|account'.
    Multi-commodity accounts print stacked amount-only lines; the
    account name arrives on the last line of the stack."""
    rows, pending = [], []
    for line in out.splitlines():
        if not line.strip():
            continue
        m = re.match(r"^\s*(\S.*?)\s\s+(\S.*)$", line)
        if m and re.search(r"\d", m.group(1)):
            for p in pending:
                rows.append((p, m.group(2).strip()))
            pending = []
            rows.append((m.group(1), m.group(2).strip()))
        else:
            pending.append(line.strip())
    if pending:
        raise ValueError(f"dangling amount lines without account: {pending!r}")
    out_rows = set()
    for amt, acct in rows:
        comm, canon = norm_amount(amt, decimal_comma, dc_comms)
        out_rows.add(f"{comm}|{canon}|{acct}")
    return out_rows


def manifest():
    """Static manifest plus any generated journals present."""
    entries = list(MANIFEST)
    gendir = os.path.join(HERE, "generated")
    if os.path.isdir(gendir):
        for f in sorted(os.listdir(gendir)):
            if f.endswith(".dat"):
                entries.append(
                    (os.path.relpath(os.path.join(gendir, f), REPO), "in",
                     "generated (seed in file header)"))
    return entries


def main():
    ledger = os.environ.get("LEDGER", os.path.join(REPO, "build", "ledger"))
    env = {**os.environ, "TZ": "America/Chicago"}

    ledger_version = subprocess.run(
        [ledger, "--version"], capture_output=True, text=True, env=env
    ).stdout.splitlines()[0]
    git_rev = subprocess.run(
        ["git", "rev-parse", "HEAD"], capture_output=True, text=True, cwd=REPO
    ).stdout.strip()

    # One batched driver invocation: single olean load for all files.
    entries = manifest()
    abs_paths = [os.path.join(REPO, p) for p, _, _ in entries]
    lean_dir = os.environ.get("LEDGER_LEAN_DIR", os.path.join(REPO, "lean"))
    if not os.path.isfile(os.path.join(lean_dir, "lakefile.lean")):
        print("SKIP: Lean oracle tree not present"); sys.exit(77)
    direct_lake = "LEDGER_LEAN_DIR" in os.environ
    if direct_lake and not shutil.which("lake"):
        print("SKIP: LEDGER_LEAN_DIR set but lake not on PATH"); sys.exit(77)
    if not direct_lake and not shutil.which("nix"):
        print("SKIP: nix unavailable for the lean/ dev shell"); sys.exit(77)
    if direct_lake:
        cmd = ["lake", "env", "lean", "--run", "Ledger/Driver.lean"]
    else:
        cmd = ["nix", "develop", "--command", "lake", "env", "lean",
               "--run", "Ledger/Driver.lean"]
    oracle_env = {**env, "GIT_CONFIG_COUNT": "1",
                  "GIT_CONFIG_KEY_0": "safe.directory",
                  "GIT_CONFIG_VALUE_0": "*"}
    drv = subprocess.run(
        [*cmd, *abs_paths],
        cwd=lean_dir, capture_output=True, text=True,
        timeout=1800, env=oracle_env,
    )
    if drv.returncode != 0:
        print("driver invocation failed:", file=sys.stderr)
        print(drv.stdout[-2000:], file=sys.stderr)
        print(drv.stderr[-2000:], file=sys.stderr)
        sys.exit(2)

    # Split driver output into per-file blocks.
    blocks = {}
    current = None
    for line in drv.stdout.splitlines():
        if line.startswith("== "):
            current = os.path.relpath(line[3:].strip(), REPO)
            blocks[current] = []
        elif current is not None:
            blocks[current].append(line)

    results = []
    failures = 0
    for path, scope, note in entries:
        entry: dict = {
            "path": path,
            "scope": scope,
            "note": note,
            "digest": hashlib.sha256(
                open(os.path.join(REPO, path), "rb").read()
            ).hexdigest()[:16],
        }
        block = blocks.get(path, [])
        drv_error = next((l for l in block if l.startswith("!! ERROR")), None)

        if scope == "cpp-rejects":
            cpp = subprocess.run(
                [ledger, "-f", os.path.join(REPO, path),
                 "balance", "--flat", "--no-total"],
                capture_output=True, text=True, env=env)
            if cpp.returncode != 0:
                entry["status"] = "CPP-REJECTS (as documented)"
                entry["cpp_stderr"] = cpp.stderr.strip()[-160:]
            else:
                entry["status"] = "FAIL: C++ unexpectedly accepted"
                failures += 1
        elif scope == "out":
            # Fail-closed check: the driver MUST reject the file.
            if drv_error:
                entry["status"] = "OUT-OF-SCOPE (rejected as expected)"
                entry["driver_error"] = drv_error
            else:
                entry["status"] = "FAIL: out-of-scope file ACCEPTED by driver"
                failures += 1
        else:
            if drv_error:
                entry["status"] = "FAIL: driver rejected in-scope file"
                entry["driver_error"] = drv_error
                failures += 1
            else:
                oracle_rows = set(l for l in block if l.strip())
                cpp = subprocess.run(
                    [ledger, "-f", os.path.join(REPO, path),
                     "balance", "--flat", "--no-total"],
                    capture_output=True, text=True, env=env,
                )
                if cpp.returncode != 0:
                    entry["status"] = "FAIL: C++ ledger errored"
                    entry["cpp_stderr"] = cpp.stderr[-500:]
                    failures += 1
                else:
                  try:
                    cpp_rows = parse_cpp_balance(cpp.stdout)
                    if cpp_rows == oracle_rows:
                        entry["status"] = "PASS"
                        entry["rows"] = len(cpp_rows)
                    else:
                        entry["status"] = "unadjudicated-divergence"
                        entry["only_oracle"] = sorted(oracle_rows - cpp_rows)
                        entry["only_cpp"] = sorted(cpp_rows - oracle_rows)
                        failures += 1
                  except ValueError as e:
                    entry["status"] = "FAIL: unparseable C++ output"
                    entry["why"] = str(e)[:200]
                    failures += 1
        results.append(entry)

    artifact = {
        "tuple": {
            "utc": datetime.datetime.now(datetime.timezone.utc).isoformat(),
            "git_rev": git_rev,
            "ledger_version": ledger_version,
            "tz": "America/Chicago",
            "capture": "test/semantic/sweep.py",
        },
        "results": results,
    }
    os.makedirs(os.path.join(HERE, "artifacts"), exist_ok=True)
    stamp = datetime.datetime.now(datetime.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    apath = os.path.join(HERE, "artifacts", f"sweep-{stamp}.json")
    with open(apath, "w") as f:
        json.dump(artifact, f, indent=2)

    for e in results:
        print(f"{e['status']:<45} {e['path']}")
        for k in ("only_oracle", "only_cpp"):
            for row in e.get(k, [])[:6]:
                print(f"    {k}: {row}")
    print(f"\nartifact: {os.path.relpath(apath, REPO)}")
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
