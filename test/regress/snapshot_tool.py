#!/usr/bin/env python3
# test/snapshot.py is the before/after output check for behavioral and
# performance changes, so what it captures must depend only on the ledger
# binary and the journal:
#
#   - every curated combo names options ledger accepts (an option ledger
#     rejects captures nothing but "Illegal option", for every build);
#   - two captures of the --only-bench reports by the same binary are
#     byte-identical, which needs XML object addresses masked and the clock
#     pinned with --now;
#   - the clock is the one RegressTests.NOW pins, 2030/06/15.

import subprocess
import sys
import tempfile
from pathlib import Path


def main():
    if len(sys.argv) != 3:
        print("usage: snapshot_tool.py <ledger> <source-tree>", file=sys.stderr)
        return 2

    ledger = Path(sys.argv[1])
    source = Path(sys.argv[2])
    tool = source / "test" / "snapshot.py"
    journal = source / "test" / "input" / "drewr3.dat"
    failures = []

    with tempfile.TemporaryDirectory() as tmp:

        def capture(name, *flags):
            out = Path(tmp) / name
            run = subprocess.run(
                [sys.executable, "-B", str(tool), str(ledger), str(journal),
                 str(out), "--jobs", "4", *flags],
                capture_output=True, text=True)
            if run.returncode != 0:
                failures.append(f"snapshot.py {' '.join(flags)} exited "
                                f"{run.returncode}: {run.stderr.strip()}")
            return out

        curated = capture("curated", "--only-curated")
        for path in sorted(curated.glob("*.txt")):
            if "Illegal option" in path.read_text(encoding="utf-8"):
                failures.append(f"{path.name}: ledger rejects an option of this combo")

        first = capture("bench-1", "--only-bench")
        second = capture("bench-2", "--only-bench")
        names = sorted(p.name for p in first.glob("*.txt")
                       if p.name != "_snapshot_meta.txt")
        if len(names) != 28:
            failures.append(f"--only-bench captured {len(names)} reports, not 28")
        for name in names:
            other = second / name
            if not other.is_file() or (first / name).read_bytes() != other.read_bytes():
                failures.append(f"{name}: two captures by the same binary differ")

        stats = first / "stats.txt"
        if not stats.is_file() or \
           "Days since last post:     6771" not in stats.read_text(encoding="utf-8"):
            failures.append("stats.txt: the clock is not pinned to 2030/06/15")

        # A combo's own --now (mined from a test) still wins over the pin.
        # This block passes on the unfixed snapshot.py too (it pins no --now
        # at all); it guards the order of BASE_ARGS, which must come before
        # the combo's options so that ledger's last --now wins.
        tree = Path(tmp) / "tree" / "test" / "regress"
        tree.mkdir(parents=True)
        (tree / "own-now.test").write_text("test stats --now 2011/12/08\nend test\n",
                                           encoding="utf-8")
        mined = capture("mined", "--only-mined", "--mine-from", str(tree.parent.parent))
        own = mined / "stats--now-2011{slash}12{slash}08.txt"
        if not own.is_file() or \
           "Days since last post:        7" not in own.read_text(encoding="utf-8"):
            failures.append(f"{own.name}: the combo's own --now does not override the pin")

    for failure in failures:
        print(f"FAIL: {failure}")
    if not failures:
        print("OK: snapshot.py captures only accepted options, reproducibly")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
