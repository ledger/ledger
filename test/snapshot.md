# Ledger Snapshot Testing Tool

`test/snapshot.py` captures the behavioral output of every sensible
command/option combination against a ledger data file. By running it before
and after a code change, you can diff the results to detect unintended side
effects across the entire CLI surface.

## Requirements

- Python 3.10+
- A built `ledger` binary, preferably a Debug build (see
  [Combination Sources](#combination-sources))

## Quick Start

Run the commands in this guide from the top of the source tree.

```bash
# Capture a baseline snapshot (curated combos only, ~450 combos, <1s)
python3 test/snapshot.py ./build/ledger test/input/drewr3.dat /tmp/before --only-curated

# Full snapshot including mined combos from tests and docs (~3300 combos, ~5s)
python3 test/snapshot.py ./build/ledger test/input/drewr3.dat /tmp/before --mine-from .
```

## Detecting Behavioral Divergence

The primary use case is comparing behavior before and after a change on
`main`:

```bash
# 1. Build the current main branch
git checkout main
cmake -B build -DCMAKE_BUILD_TYPE=Debug && cmake --build build

# 2. Capture the "before" snapshot in an empty directory
rm -rf /tmp/before
python3 test/snapshot.py ./build/ledger test/input/drewr3.dat /tmp/before --mine-from .

# 3. Apply your changes and rebuild
git checkout my-feature-branch
cmake --build build

# 4. Capture the "after" snapshot in an empty directory
rm -rf /tmp/after
python3 test/snapshot.py ./build/ledger test/input/drewr3.dat /tmp/after --mine-from .

# 5. Diff the snapshots
diff -r --exclude='_snapshot_meta.txt' /tmp/before /tmp/after
```

Any difference in a file that both snapshots contain represents changed
behavior; review each diff to determine whether the change is intentional.
A file that only one snapshot contains (an `Only in` line from `diff`) means
instead that the set of combos changed, for example because the branch adds a
test or an option. The tool never removes files from its output directory,
which is why steps 2 and 4 start from an empty one: a file left by an earlier
run would show up as an `Only in` line too.

### Expected Non-Determinism

Some output is inherently non-deterministic and will always differ between
runs:

- **Timing values** in `--debug`/`--verbose` output (e.g., `2ms` vs `1ms`)
- **The current time** in output that shows the value of `now`
- **Memory addresses** in XML output (heap pointer IDs)
- **`--anon` output** (intentionally randomized)
- **Occasional timeouts** depending on system load

These can be safely ignored or filtered out.

## CLI Reference

```
python3 test/snapshot.py <ledger-binary> <data-file> <output-dir> [options]
```

### Positional Arguments

| Argument | Description |
|---|---|
| `ledger-binary` | Path to the compiled ledger executable |
| `data-file` | Ledger journal file to run commands against |
| `output-dir` | Directory to write output files (created if needed) |

### Options

| Flag | Default | Description |
|---|---|---|
| `--mine-from PATH` | auto | Mine additional combos from `.test` files and docs under PATH |
| `--verbose` | off | Print each combo as it finishes; with `--dry-run`, tag each with its source |
| `--dry-run` | off | List all combos without executing them |
| `--jobs N` | CPU count | Number of parallel workers |
| `--timeout N` | 30 | Per-combo timeout in seconds |
| `--only-command CMD` | all | Only run combos for one command |
| `--only-curated` | off | Skip mined combos |
| `--only-mined` | off | Skip curated combos |

When `--mine-from` is not given, the tool assumes the binary was built in a
directory directly below the top of a source tree, as `./build/ledger` is, and
mines that tree if it has a `test/` directory.

## How It Works

### Combination Sources

The tool generates command/option combinations from two sources:

1. **Curated combinations** (~360): A built-in catalog of single-option and
   multi-option combinations covering known interaction patterns. Each option
   is mapped to the commands it meaningfully applies to, preventing
   nonsensical pairings.

2. **Mined combinations** (~2900, skipped with `--only-curated`): Extracted
   from existing test files (`test/baseline/`, `test/regress/`,
   `test/manual/`), the Texinfo manual (`doc/ledger3.texi`), and the man page
   (`doc/ledger.1`). These capture real-world usage patterns.

After generation, the tool validates coverage by discovering all options via
`ledger --debug option.names parse true` and generating fallback combinations
for any gaps. This check is skipped when `--only-command` or `--only-mined` is
given. Only a Debug build (`-DCMAKE_BUILD_TYPE=Debug`) prints the output that
option discovery reads; with any other build the tool silently discovers no
options and checks only those in its own catalog.

### Output Format

Each combination produces one file in the output directory:

```
# Command: ledger --args-only --columns 80 -f drewr3.dat balance --flat --no-total
# Exit code: 0
          $ 1,366.00  Assets:Checking
             $ 30.00  Assets:Checking:Business
         $ -6,200.00  Equity:Opening Balances
          $ 5,500.00  Expenses:Auto
             $ 20.00  Expenses:Books
            $ 300.00  Expenses:Escrow
            $ 334.00  Expenses:Food:Groceries
            $ 500.00  Expenses:Interest:Mortgage
         $ -2,000.00  Income:Salary
            $ -30.00  Income:Sales
            $ -20.00  Liabilities:MasterCard
            $ 200.00  Liabilities:Mortgage:Principal
           $ -243.60  Liabilities:Tithe
# --- stderr ---
```

### Environment

All runs use a sanitized environment matching the test harness:
`TZ=America/Chicago`, `--args-only`, and `--columns=80` for reproducibility.

## Tips

- Use `--dry-run` to preview which combinations will be tested.
- Use `--only-command balance` to focus on one command during development.
- Use a RAM disk or SSD for the output directory; a full run writes ~3300
  small files.
- The `_snapshot_meta.txt` file in each output directory records the ledger
  binary path, data file, timestamp, and combo counts for provenance.
