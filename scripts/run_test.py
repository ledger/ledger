#!/usr/bin/env python3
"""Run a single .test file against ledger and report pass/fail."""

import argparse
import os
import subprocess
import sys


def run_test(test_path: str, ledger_bin: str, repo_root: str) -> tuple[bool, str, str]:
    """Run a .test file and return (passed, stdout, stderr)."""
    env = os.environ.copy()
    env["TZ"] = "America/Chicago"

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
        timeout=120,
    )

    return result.returncode == 0, result.stdout, result.stderr


def main():
    parser = argparse.ArgumentParser(description="Run a ledger .test file")
    parser.add_argument("test_file", help="Path to .test file")
    parser.add_argument("--ledger", default="ledger", help="Path to ledger binary")
    parser.add_argument("--repo-root", default=".", help="Path to repo root")
    args = parser.parse_args()

    passed, stdout, stderr = run_test(args.test_file, args.ledger, args.repo_root)
    if stdout.strip():
        print(stdout.strip())
    if stderr.strip():
        print(stderr.strip(), file=sys.stderr)

    sys.exit(0 if passed else 1)


if __name__ == "__main__":
    main()
