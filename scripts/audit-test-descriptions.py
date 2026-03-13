#!/usr/bin/env python3
"""
Audit all ledger .test files for description-vs-content mismatches.

Uses Claude Code (claude CLI) with the sonnet model to review each test file
and determine whether comments, transaction payees, and other descriptions
accurately reflect what the test actually exercises.

Results are written incrementally to a JSONL file for resume support.
A summary report is printed at the end.

Usage:
    python3 scripts/audit-test-descriptions.py [--max-concurrent 10] [--resume]
"""

import argparse
import asyncio
import json
import os
import re
import subprocess
import sys
import time
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
# Default files; overridden with --output-prefix
RESULTS_FILE = REPO_ROOT / "scripts" / "audit-results.jsonl"
SUMMARY_FILE = REPO_ROOT / "scripts" / "audit-summary.json"

REVIEW_PROMPT = """\
You are auditing a ledger test file for accuracy of its descriptions.

## The file to review

Path: {filepath}

```
{content}
```

## Ledger test file format

- Lines starting with `;` are comments (descriptions)
- Transaction payee fields (after the date) often describe what's being tested
- `test <command>` begins a test block; expected output follows until `end test`
- `test <command> -> <exit_code>` tests for a specific exit code
- `__ERROR__` marks expected stderr output
- `$FILE` is replaced with the test file path at runtime

## Your task

Check whether the descriptions (comments, transaction payees/narrations) accurately
describe what the test file actually tests. Look for:

1. **Comment/description says X but journal data or test commands exercise Y**
   Example: comment says "Virtual total price" but syntax is per-unit `(@)` not total `(@@)`
2. **Transaction payee describes a scenario that doesn't match the posting data**
   Example: payee says "Negative balance test" but all postings are positive
3. **Comment references wrong issue or wrong feature**
4. **Test claims to check an error case but actually tests success (or vice versa)**
5. **Description mentions a specific option/command but the test block uses something different**

## Response format

Respond with ONLY a JSON object (no markdown fencing):
{{
  "verdict": "ok" | "mismatch" | "unclear",
  "confidence": 0.0-1.0,
  "issues": [
    {{
      "line": <line_number>,
      "description_says": "<what the description claims>",
      "actually_tests": "<what the test actually does>",
      "severity": "high" | "medium" | "low"
    }}
  ],
  "summary": "<one-line summary>"
}}

- "ok": descriptions accurately reflect the test content
- "mismatch": one or more descriptions are misleading or wrong
- "unclear": descriptions are vague/absent so accuracy can't be judged
- Only report genuine mismatches, not minor stylistic issues
- confidence: how sure you are (0.9+ for clear cases)
"""


def collect_test_files():
    """Collect test files that have meaningful descriptions to audit."""
    files = []

    # 1. Issue-numbered regression tests (highest priority)
    for f in sorted(REPO_ROOT.glob("test/regress/*.test")):
        name = f.stem
        # Include issue-numbered tests and named tests (not hex hashes without comments)
        if re.match(r"^\d+(_\d+)?$", name) or re.match(r"^\d+_py$", name):
            files.append(("regress-issue", f))
        elif name.startswith("coverage-"):
            # Check if coverage test has meaningful comments
            try:
                first_lines = f.read_text()[:500]
                if first_lines.startswith(";"):
                    files.append(("coverage", f))
            except Exception:
                pass
        elif re.match(r"^[0-9A-F]{8}$", name):
            # Hex-hash tests: only include if they have comments
            try:
                first_line = f.read_text().split("\n")[0]
                if first_line.startswith(";"):
                    files.append(("regress-hex", f))
            except Exception:
                pass
        else:
            # Other named tests (e.g., "total-1.test", "virt-balance-after-real-post.test")
            files.append(("regress-named", f))

    # 2. Baseline tests
    for f in sorted(REPO_ROOT.glob("test/baseline/*.test")):
        files.append(("baseline", f))

    # 3. Manual tests
    for f in sorted(REPO_ROOT.glob("test/manual/*.test")):
        files.append(("manual", f))

    return files


def load_completed(results_file):
    """Load already-completed file paths from the results file."""
    completed = set()
    if results_file.exists():
        with open(results_file) as fh:
            for line in fh:
                line = line.strip()
                if line:
                    try:
                        rec = json.loads(line)
                        completed.add(rec["file"])
                    except (json.JSONDecodeError, KeyError):
                        pass
    return completed


async def review_file(filepath, semaphore, results_fh, stats):
    """Review a single test file using Claude CLI."""
    async with semaphore:
        rel = str(filepath.relative_to(REPO_ROOT))
        try:
            content = filepath.read_text()
        except Exception as e:
            result = {
                "file": rel,
                "category": "error",
                "verdict": "error",
                "error": str(e),
            }
            results_fh.write(json.dumps(result) + "\n")
            results_fh.flush()
            stats["errors"] += 1
            return result

        # Skip very short files (< 3 lines) or empty files
        if len(content.strip().split("\n")) < 3:
            result = {
                "file": rel,
                "verdict": "skipped",
                "summary": "File too short to audit",
            }
            results_fh.write(json.dumps(result) + "\n")
            results_fh.flush()
            stats["skipped"] += 1
            return result

        prompt = REVIEW_PROMPT.format(filepath=rel, content=content)

        try:
            # Unset CLAUDECODE env var to allow nested invocations
            env = os.environ.copy()
            env.pop("CLAUDECODE", None)
            proc = await asyncio.create_subprocess_exec(
                "claude",
                "-p", prompt,
                "--model", "sonnet",
                "--output-format", "text",
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                env=env,
            )
            stdout, stderr = await asyncio.wait_for(
                proc.communicate(), timeout=120
            )
            raw = stdout.decode("utf-8", errors="replace").strip()

            # Try to extract JSON from the response
            # Claude might wrap it in markdown code fences
            json_str = raw
            if "```" in json_str:
                match = re.search(r"```(?:json)?\s*\n?(.*?)\n?```", json_str, re.DOTALL)
                if match:
                    json_str = match.group(1).strip()

            parsed = json.loads(json_str)
            result = {"file": rel, **parsed}

        except asyncio.TimeoutError:
            result = {
                "file": rel,
                "verdict": "error",
                "error": "timeout after 120s",
            }
            stats["errors"] += 1
        except json.JSONDecodeError:
            result = {
                "file": rel,
                "verdict": "error",
                "error": "failed to parse JSON",
                "raw_output": raw[:500] if raw else "",
            }
            stats["errors"] += 1
        except Exception as e:
            result = {
                "file": rel,
                "verdict": "error",
                "error": str(e),
            }
            stats["errors"] += 1

        verdict = result.get("verdict", "error")
        if verdict == "mismatch":
            stats["mismatches"] += 1
        elif verdict == "ok":
            stats["ok"] += 1
        elif verdict == "unclear":
            stats["unclear"] += 1

        results_fh.write(json.dumps(result) + "\n")
        results_fh.flush()

        stats["done"] += 1
        total = stats["total"]
        done = stats["done"]
        mismatches = stats["mismatches"]
        elapsed = time.time() - stats["start_time"]
        rate = done / elapsed if elapsed > 0 else 0
        eta = (total - done) / rate if rate > 0 else 0
        print(
            f"\r[{done}/{total}] "
            f"ok={stats['ok']} mismatch={mismatches} "
            f"unclear={stats['unclear']} err={stats['errors']} "
            f"skip={stats['skipped']} "
            f"({rate:.1f}/s, ETA {eta/60:.0f}m)",
            end="", flush=True,
        )

        return result


async def main():
    parser = argparse.ArgumentParser(description="Audit test file descriptions")
    parser.add_argument(
        "--max-concurrent", type=int, default=10,
        help="Max concurrent Claude CLI calls (default: 10)",
    )
    parser.add_argument(
        "--resume", action="store_true",
        help="Resume from previous run, skipping already-reviewed files",
    )
    parser.add_argument(
        "--category", type=str, default=None,
        help="Only audit files in this category (regress-issue, coverage, baseline, etc.)",
    )
    parser.add_argument(
        "--limit", type=int, default=0,
        help="Limit number of files to process (0 = all)",
    )
    parser.add_argument(
        "--output-prefix", type=str, default=None,
        help="Output file prefix (default: scripts/audit-results)",
    )
    args = parser.parse_args()

    global RESULTS_FILE, SUMMARY_FILE
    if args.output_prefix:
        RESULTS_FILE = Path(args.output_prefix + ".jsonl")
        SUMMARY_FILE = Path(args.output_prefix + "-summary.json")
    elif args.category:
        RESULTS_FILE = REPO_ROOT / "scripts" / f"audit-{args.category}.jsonl"
        SUMMARY_FILE = REPO_ROOT / "scripts" / f"audit-{args.category}-summary.json"

    os.makedirs(REPO_ROOT / "scripts", exist_ok=True)

    all_files = collect_test_files()
    print(f"Found {len(all_files)} test files with descriptions to audit")

    # Category breakdown
    cats = {}
    for cat, _ in all_files:
        cats[cat] = cats.get(cat, 0) + 1
    for cat, count in sorted(cats.items()):
        print(f"  {cat}: {count}")

    # Filter by category if specified
    if args.category:
        all_files = [(c, f) for c, f in all_files if c == args.category]
        print(f"Filtered to {len(all_files)} files in category '{args.category}'")

    # Resume support
    completed = set()
    if args.resume:
        completed = load_completed(RESULTS_FILE)
        before = len(all_files)
        all_files = [
            (c, f)
            for c, f in all_files
            if str(f.relative_to(REPO_ROOT)) not in completed
        ]
        print(f"Resume: skipping {before - len(all_files)} already-reviewed files")

    if args.limit > 0:
        all_files = all_files[: args.limit]
        print(f"Limited to {len(all_files)} files")

    if not all_files:
        print("Nothing to audit.")
        return

    stats = {
        "total": len(all_files),
        "done": 0,
        "ok": 0,
        "mismatches": 0,
        "unclear": 0,
        "errors": 0,
        "skipped": 0,
        "start_time": time.time(),
    }

    mode = "a" if args.resume else "w"
    semaphore = asyncio.Semaphore(args.max_concurrent)

    with open(RESULTS_FILE, mode) as results_fh:
        tasks = [
            review_file(filepath, semaphore, results_fh, stats)
            for _, filepath in all_files
        ]
        await asyncio.gather(*tasks)

    print()  # newline after progress
    elapsed = time.time() - stats["start_time"]
    print(f"\nCompleted in {elapsed:.0f}s")
    print(f"  OK:        {stats['ok']}")
    print(f"  Mismatch:  {stats['mismatches']}")
    print(f"  Unclear:   {stats['unclear']}")
    print(f"  Errors:    {stats['errors']}")
    print(f"  Skipped:   {stats['skipped']}")

    # Generate summary of mismatches
    mismatches = []
    all_results = []
    with open(RESULTS_FILE) as fh:
        for line in fh:
            line = line.strip()
            if not line:
                continue
            try:
                rec = json.loads(line)
                all_results.append(rec)
                if rec.get("verdict") == "mismatch":
                    mismatches.append(rec)
            except json.JSONDecodeError:
                pass

    summary = {
        "total_reviewed": len(all_results),
        "mismatches": len(mismatches),
        "mismatch_files": [
            {
                "file": m["file"],
                "summary": m.get("summary", ""),
                "issues": m.get("issues", []),
                "confidence": m.get("confidence", 0),
            }
            for m in sorted(mismatches, key=lambda x: -x.get("confidence", 0))
        ],
    }
    with open(SUMMARY_FILE, "w") as fh:
        json.dump(summary, fh, indent=2)

    if mismatches:
        print(f"\n=== MISMATCHES ({len(mismatches)}) ===")
        for m in sorted(mismatches, key=lambda x: -x.get("confidence", 0)):
            conf = m.get("confidence", 0)
            print(f"\n  {m['file']} (confidence: {conf:.0%})")
            print(f"    {m.get('summary', 'No summary')}")
            for issue in m.get("issues", []):
                sev = issue.get("severity", "?")
                print(f"    [{sev}] line {issue.get('line', '?')}: "
                      f"says \"{issue.get('description_says', '?')}\" "
                      f"but actually: \"{issue.get('actually_tests', '?')}\"")

    print(f"\nFull results: {RESULTS_FILE}")
    print(f"Summary:      {SUMMARY_FILE}")


if __name__ == "__main__":
    asyncio.run(main())
