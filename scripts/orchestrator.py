#!/usr/bin/env python3
"""Orchestrator that processes open GitHub issues in parallel using Claude Code
to generate regression tests for the ledger accounting tool."""

import argparse
import json
import logging
import os
import re
import shutil
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field, asdict
from pathlib import Path

from prompt_template import render_prompt

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger(__name__)

# Labels that indicate non-bug issues we should skip
SKIP_LABELS = {"enhancement", "build", "python", "documentation", "question"}


@dataclass
class Issue:
    number: int
    title: str
    body: str
    labels: list[str] = field(default_factory=list)


@dataclass
class Result:
    issue_number: int
    issue_title: str
    status: str  # "fixed", "broken", "skip", "error"
    notes: str = ""
    test_file: str = ""
    test_passes: bool = False
    claude_output: str = ""


def fetch_issues(repo: str, limit: int = 300) -> list[Issue]:
    """Fetch open issues from GitHub, excluding non-bug labels."""
    log.info("Fetching open issues from %s...", repo)
    result = subprocess.run(
        [
            "gh", "issue", "list",
            "--repo", repo,
            "--state", "open",
            "--limit", str(limit),
            "--json", "number,title,body,labels",
        ],
        capture_output=True, text=True, check=True,
    )
    raw = json.loads(result.stdout)
    issues = []
    for item in raw:
        labels = [label["name"] for label in item.get("labels", [])]
        if SKIP_LABELS & set(labels):
            log.info("  Skipping #%d (labels: %s)", item["number"], labels)
            continue
        issues.append(Issue(
            number=item["number"],
            title=item["title"],
            body=item.get("body") or "",
            labels=labels,
        ))
    log.info("Fetched %d bug issues (skipped %d non-bug)", len(issues), len(raw) - len(issues))
    return sorted(issues, key=lambda i: i.number)


def get_existing_tests(regress_dir: Path) -> set[int]:
    """Get issue numbers that already have regression tests."""
    numbers = set()
    for f in regress_dir.iterdir():
        match = re.match(r"^(\d+)", f.name)
        if match:
            numbers.add(int(match.group(1)))
    return numbers


def find_ledger() -> str:
    """Find the ledger binary."""
    result = subprocess.run(["which", "ledger"], capture_output=True, text=True)
    if result.returncode == 0:
        return result.stdout.strip()
    raise RuntimeError("Cannot find ledger binary. Ensure it's on PATH.")


def process_issue(
    issue: Issue,
    ledger_bin: str,
    repo_root: Path,
    output_dir: Path,
    model: str,
    max_budget: float,
) -> Result:
    """Process a single issue using Claude Code."""
    test_path = output_dir / f"{issue.number}.test"
    log.info("Processing #%d: %s", issue.number, issue.title)

    prompt = render_prompt(
        issue_number=issue.number,
        issue_title=issue.title,
        issue_body=issue.body,
        output_path=str(test_path),
        ledger_bin=ledger_bin,
        repo_root=str(repo_root),
    )

    try:
        cmd = [
            "claude", "-p",
            "--model", model,
            "--allowedTools", "Bash", "Read", "Write", "Glob", "Grep",
            "--permission-mode", "bypassPermissions",
            "--no-session-persistence",
            "--max-budget-usd", str(max_budget),
            prompt,
        ]

        start = time.time()
        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=600,  # 10 minute timeout per issue
            cwd=str(repo_root),
        )
        elapsed = time.time() - start
        log.info("  #%d completed in %.1fs (exit=%d)", issue.number, elapsed, proc.returncode)

        output = proc.stdout + "\n" + proc.stderr

        # Parse the RESULT_JSON from Claude's output
        status = "error"
        notes = ""
        json_match = re.search(r"RESULT_JSON:\s*(\{.*?\})", output, re.DOTALL)
        if json_match:
            try:
                result_data = json.loads(json_match.group(1))
                status = result_data.get("status", "error")
                notes = result_data.get("notes", "")
            except json.JSONDecodeError:
                notes = "Failed to parse RESULT_JSON"

        # Check if test file was created
        test_passes = False
        if test_path.exists() and test_path.stat().st_size > 0:
            test_passes = verify_test(str(test_path), ledger_bin, str(repo_root))
            if not test_passes:
                log.warning("  #%d test file exists but FAILS", issue.number)
            # Fallback: if no RESULT_JSON was parsed but test exists and passes,
            # infer status as "broken" (conservative — we wrote current behavior)
            if status == "error" and test_passes:
                status = "broken"
                notes = notes or "Test created with current behavior (no explicit status from Claude)"
        elif status != "skip":
            log.warning("  #%d no test file created", issue.number)
            if status not in ("error", "skip"):
                notes = f"Claude reported '{status}' but no test file was created"
                status = "error"

        return Result(
            issue_number=issue.number,
            issue_title=issue.title,
            status=status,
            notes=notes,
            test_file=str(test_path) if test_path.exists() else "",
            test_passes=test_passes,
            claude_output=output[-2000:] if len(output) > 2000 else output,
        )

    except subprocess.TimeoutExpired:
        log.error("  #%d TIMEOUT", issue.number)
        # Check if a test file was partially created before timeout
        test_passes = False
        if test_path.exists() and test_path.stat().st_size > 0:
            test_passes = verify_test(str(test_path), ledger_bin, str(repo_root))
        return Result(
            issue_number=issue.number,
            issue_title=issue.title,
            status="broken" if test_passes else "error",
            notes="Timed out but test passes" if test_passes else "Claude Code subprocess timed out after 600s",
            test_file=str(test_path) if test_path.exists() else "",
            test_passes=test_passes,
        )
    except Exception as e:
        log.error("  #%d ERROR: %s", issue.number, e)
        return Result(
            issue_number=issue.number,
            issue_title=issue.title,
            status="error",
            notes=str(e),
        )


def verify_test(test_path: str, ledger_bin: str, repo_root: str) -> bool:
    """Run a .test file and return True if it passes."""
    env = os.environ.copy()
    env["TZ"] = "America/Chicago"
    try:
        result = subprocess.run(
            [
                sys.executable,
                os.path.join(repo_root, "test", "RegressTests.py"),
                "--ledger", ledger_bin,
                "--sourcepath", repo_root,
                test_path,
            ],
            env=env,
            capture_output=True,
            text=True,
            timeout=60,
        )
        return result.returncode == 0
    except (subprocess.TimeoutExpired, Exception):
        return False


def categorize_results(
    results: list[Result],
    repo_root: Path,
    staging_dir: Path,
) -> dict[str, list[int]]:
    """Move test files to their final locations based on results."""
    regress_dir = repo_root / "test" / "regress"
    todo_dir = repo_root / "test" / "todo"
    todo_dir.mkdir(exist_ok=True)

    categories: dict[str, list[int]] = {
        "fixed": [],
        "broken": [],
        "skip": [],
        "error": [],
    }

    for r in results:
        categories[r.status].append(r.issue_number)

        if not r.test_file or not Path(r.test_file).exists():
            continue

        src = Path(r.test_file)
        if r.status == "fixed" and r.test_passes:
            dest = regress_dir / src.name
            shutil.copy2(src, dest)
            log.info("  #%d → test/regress/%s (FIXED)", r.issue_number, src.name)
        elif r.status == "broken" and r.test_passes:
            dest = todo_dir / src.name
            shutil.copy2(src, dest)
            log.info("  #%d → test/todo/%s (BROKEN)", r.issue_number, src.name)
        elif r.test_passes:
            # Status unclear but test passes — put in todo for review
            dest = todo_dir / src.name
            shutil.copy2(src, dest)
            log.info("  #%d → test/todo/%s (status=%s)", r.issue_number, src.name, r.status)

    return categories


def main():
    parser = argparse.ArgumentParser(
        description="Process open GitHub issues and generate regression tests"
    )
    parser.add_argument("--repo", default="ledger/ledger", help="GitHub repo")
    parser.add_argument("--parallel", type=int, default=10, help="Max parallel workers")
    parser.add_argument("--limit", type=int, default=0, help="Limit issues to process (0=all)")
    parser.add_argument("--offset", type=int, default=0, help="Skip first N issues")
    parser.add_argument("--model", default="sonnet", help="Claude model to use")
    parser.add_argument("--max-budget", type=float, default=0.50, help="Max $ per issue")
    parser.add_argument("--repo-root", type=Path, default=Path("."), help="Repo root path")
    parser.add_argument("--staging-dir", type=Path, default=None, help="Staging dir for tests")
    parser.add_argument("--report", type=Path, default=Path("issues_report.json"), help="Report output")
    parser.add_argument("--resume", action="store_true", help="Skip issues already in staging dir")
    parser.add_argument("--issues", type=str, default="", help="Comma-separated issue numbers to process")
    args = parser.parse_args()

    repo_root = args.repo_root.resolve()
    staging_dir = (args.staging_dir or repo_root / "test" / "staging").resolve()
    staging_dir.mkdir(parents=True, exist_ok=True)

    ledger_bin = find_ledger()
    log.info("Using ledger: %s", ledger_bin)

    # Fetch and filter issues
    if args.issues:
        # Process specific issues
        requested = {int(n.strip()) for n in args.issues.split(",")}
        all_issues = fetch_issues(args.repo)
        issues = [i for i in all_issues if i.number in requested]
        if not issues:
            log.error("None of the requested issues found in open issues")
            sys.exit(1)
    else:
        issues = fetch_issues(args.repo)

    # Filter out issues that already have regression tests
    existing = get_existing_tests(repo_root / "test" / "regress")
    before = len(issues)
    issues = [i for i in issues if i.number not in existing]
    if before != len(issues):
        log.info("Skipped %d issues with existing regression tests", before - len(issues))

    # Resume support: skip issues already processed in staging
    if args.resume:
        processed = {
            int(m.group(1))
            for f in staging_dir.iterdir()
            if (m := re.match(r"^(\d+)\.test$", f.name))
        }
        before = len(issues)
        issues = [i for i in issues if i.number not in processed]
        if before != len(issues):
            log.info("Resuming: skipped %d already-processed issues", before - len(issues))

    # Apply offset/limit
    if args.offset:
        issues = issues[args.offset:]
    if args.limit:
        issues = issues[:args.limit]

    log.info("Processing %d issues with %d parallel workers", len(issues), args.parallel)

    # Process issues in parallel
    results: list[Result] = []
    with ThreadPoolExecutor(max_workers=args.parallel) as executor:
        futures = {
            executor.submit(
                process_issue, issue, ledger_bin, repo_root, staging_dir,
                args.model, args.max_budget,
            ): issue
            for issue in issues
        }
        for future in as_completed(futures):
            issue = futures[future]
            try:
                result = future.result()
                results.append(result)
                log.info(
                    "  [%d/%d] #%d → %s%s",
                    len(results), len(issues),
                    result.issue_number, result.status,
                    f" ({result.notes[:60]})" if result.notes else "",
                )
            except Exception as e:
                log.error("  #%d unexpected error: %s", issue.number, e)
                results.append(Result(
                    issue_number=issue.number,
                    issue_title=issue.title,
                    status="error",
                    notes=str(e),
                ))

    results.sort(key=lambda r: r.issue_number)

    # Categorize and move files
    log.info("\n=== Categorizing results ===")
    categories = categorize_results(results, repo_root, staging_dir)

    # Write report
    report = {
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%S"),
        "ledger_binary": ledger_bin,
        "total_processed": len(results),
        "summary": {k: len(v) for k, v in categories.items()},
        "categories": categories,
        "results": [asdict(r) for r in results],
    }

    # Strip verbose claude_output from the report file (keep in results list)
    for r in report["results"]:
        r.pop("claude_output", None)

    args.report.write_text(json.dumps(report, indent=2))
    log.info("\nReport written to %s", args.report)

    # Print summary
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"  Total processed: {len(results)}")
    print(f"  Fixed (→ test/regress/): {len(categories['fixed'])}")
    print(f"  Broken (→ test/todo/):   {len(categories['broken'])}")
    print(f"  Skipped:                 {len(categories['skip'])}")
    print(f"  Errors:                  {len(categories['error'])}")

    if categories["fixed"]:
        print(f"\n  Fixed issues: {categories['fixed']}")
    if categories["broken"]:
        print(f"  Broken issues: {categories['broken']}")
    if categories["error"]:
        print(f"  Error issues: {categories['error']}")


if __name__ == "__main__":
    main()
