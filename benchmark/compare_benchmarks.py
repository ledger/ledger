#!/usr/bin/env python3
"""Compare two benchmark result files and detect performance regressions.

Usage: compare_benchmarks.py <baseline.json> <candidate.json> [--threshold 2.0]

Exit codes:
    0 - No regressions detected
    1 - Regression exceeding threshold detected
    2 - Usage or file error
"""

import argparse
import json
import sys
from pathlib import Path


def load_results(path: Path) -> dict[str, dict]:
    """Load benchmark results from JSON file, keyed by command name."""
    with open(path) as f:
        data = json.load(f)
    return {b["name"]: b for b in data["benchmarks"]}


def compare(baseline_path: Path, candidate_path: Path, threshold: float) -> int:
    """Compare two benchmark result sets.

    Returns 0 if no regressions exceed threshold, 1 otherwise.
    """
    baseline = load_results(baseline_path)
    candidate = load_results(candidate_path)

    if not baseline:
        print("Error: baseline has no benchmark results", file=sys.stderr)
        return 2

    regression_found = False
    results = []

    for name in sorted(baseline.keys()):
        if name not in candidate:
            print(f"Warning: benchmark '{name}' missing from candidate results")
            continue

        base_median = baseline[name]["median"]
        cand_median = candidate[name]["median"]

        if base_median == 0:
            change_pct = 0.0
        else:
            change_pct = ((cand_median - base_median) / base_median) * 100.0

        status = "PASS"
        if change_pct > threshold:
            status = "FAIL"
            regression_found = True
        elif change_pct < -threshold:
            status = "IMPROVED"

        results.append({
            "name": name,
            "baseline_ms": base_median * 1000,
            "candidate_ms": cand_median * 1000,
            "change_pct": change_pct,
            "status": status,
        })

    # Print results table
    print(f"\n{'Benchmark':<25} {'Baseline (ms)':>14} {'Candidate (ms)':>14} {'Change':>10} {'Status':>10}")
    print("-" * 77)

    for r in results:
        sign = "+" if r["change_pct"] >= 0 else ""
        print(
            f"{r['name']:<25} {r['baseline_ms']:>14.2f} {r['candidate_ms']:>14.2f} "
            f"{sign}{r['change_pct']:>8.2f}% {r['status']:>10}"
        )

    print("-" * 77)
    print(f"Threshold: {threshold}%")

    if regression_found:
        print(f"\nFAILED: Performance regression(s) detected exceeding {threshold}% threshold.")
        return 1
    else:
        print("\nPASSED: No performance regressions detected.")
        return 0


def main():
    parser = argparse.ArgumentParser(
        description="Compare benchmark results and detect performance regressions."
    )
    parser.add_argument("baseline", type=Path, help="Baseline benchmark results JSON")
    parser.add_argument("candidate", type=Path, help="Candidate benchmark results JSON")
    parser.add_argument(
        "--threshold",
        type=float,
        default=2.0,
        help="Regression threshold percentage (default: 2.0)",
    )
    args = parser.parse_args()

    if not args.baseline.exists():
        print(f"Error: baseline file not found: {args.baseline}", file=sys.stderr)
        sys.exit(2)
    if not args.candidate.exists():
        print(f"Error: candidate file not found: {args.candidate}", file=sys.stderr)
        sys.exit(2)

    sys.exit(compare(args.baseline, args.candidate, args.threshold))


if __name__ == "__main__":
    main()
