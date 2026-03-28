# Ledger Snapshot Testing Tool

`test/snapshot.py` captures the behavioral output of every sensible
command/option combination against a ledger data file. By running it before
and after a code change, you can diff the results to detect unintended side
effects across the entire CLI surface.

## Requirements

- Python 3.10+
- A built `ledger` binary

## Quick Start

```bash
# Capture a baseline snapshot (curated combos only, ~438 combos, <1s)
python3 test/snapshot.py ./build/ledger test/input/drewr3.dat /tmp/before

# Full snapshot including mined combos from tests and docs (~3000 combos, ~5s)
python3 test/snapshot.py ./build/ledger test/input/drewr3.dat /tmp/before --mine-from .
```

## Detecting Behavioral Divergence

The primary use case is comparing behavior before and after a change on
`main`:

```bash
# 1. Build the current main branch
git checkout main
cmake -B build && cmake --build build

# 2. Capture the "before" snapshot
python3 test/snapshot.py ./build/ledger test/input/drewr3.dat /tmp/before --mine-from .

# 3. Apply your changes and rebuild
git checkout my-feature-branch
cmake --build build

# 4. Capture the "after" snapshot
python3 test/snapshot.py ./build/ledger test/input/drewr3.dat /tmp/after --mine-from .

# 5. Diff the snapshots
diff -r --exclude='_snapshot_meta.txt' /tmp/before /tmp/after
```

Any differences represent changed behavior. Review each diff to determine
whether the change is intentional.

### Expected Non-Determinism

Some output is inherently non-deterministic and will always differ between
runs:

- **Timing values** in `--debug`/`--verbose` output (e.g., `2ms` vs `1ms`)
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
| `--mine-from PATH` | off | Mine additional combos from `.test` files and docs under PATH |
| `--verbose` | off | Print each combo as it executes |
| `--dry-run` | off | List all combos without executing them |
| `--jobs N` | CPU count | Number of parallel workers |
| `--timeout N` | 30 | Per-combo timeout in seconds |
| `--commands CMD,...` | all | Only run specific commands (comma-separated) |

## How It Works

### Combination Sources

The tool generates command/option combinations from two sources:

1. **Curated combinations** (~438): A built-in catalog of single-option and
   multi-option combinations covering known interaction patterns. Each option
   is mapped to the commands it meaningfully applies to, preventing
   nonsensical pairings.

2. **Mined combinations** (~2500+, with `--mine-from`): Extracted from
   existing test files (`test/baseline/`, `test/regress/`, `test/manual/`),
   the Texinfo manual (`doc/ledger3.texi`), and the man page (`doc/ledger.1`).
   These capture real-world usage patterns.

After generation, the tool validates coverage by discovering all options via
`ledger --debug option.names parse true` and generating fallback combinations
for any gaps.

### Output Format

Each combination produces one file in the output directory:

```
# Command: ledger --args-only --columns 80 -f drewr3.dat balance --flat --no-total
# Exit code: 0
              $-3,804.00  Assets
               $1,396.00    Checking
              $-5,200.00    Savings
               $1,000.00  Equity:Opening Balances
               $6,654.00  Expenses
# --- stderr ---
```

### Environment

All runs use a sanitized environment matching the test harness:
`TZ=America/Chicago`, `--args-only`, and `--columns=80` for reproducibility.

## Tips

- Use `--dry-run` to preview which combinations will be tested.
- Use `--commands balance,register` to focus on specific commands during
  development.
- Use a RAM disk or SSD for the output directory; a full run writes ~3000
  small files.
- The `_snapshot_meta.txt` file in each output directory records the ledger
  binary path, data file, timestamp, and combo counts for provenance.
