# Regression Test Automation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a Python orchestrator that processes 188 open GitHub issues in parallel (10 at a time), using Claude Code to analyze each issue and generate regression tests, then categorize them as fixed or still-broken.

**Architecture:** Python orchestrator fetches issues via `gh`, spawns `claude -p` subprocesses with a carefully crafted prompt for each issue, collects the generated `.test` files, runs them against the current ledger binary, and sorts results into `test/regress/` (fixed) or `test/todo/` (still-broken).

**Tech Stack:** Python 3.10+, `gh` CLI, `claude` CLI (`-p` mode), ledger test harness

---

## File Structure

- Create: `scripts/orchestrator.py` — main orchestrator managing parallel issue processing
- Create: `scripts/prompt_template.py` — Claude Code prompt template for analyzing issues
- Create: `scripts/run_test.py` — standalone test runner that validates a `.test` file
- Create: `test/todo/` — directory for tests demonstrating still-broken issues
- Modify: `test/regress/` — add new tests for fixed issues

---

## Chunk 1: Core Infrastructure

### Task 1: Create the prompt template

**Files:**
- Create: `scripts/prompt_template.py`

- [ ] **Step 1: Write the prompt template module**

This module generates the prompt sent to each Claude Code subprocess. The prompt must:
- Include the issue number, title, and body
- Explain the `.test` file format with examples
- Instruct Claude to run ledger commands to understand current behavior
- Output the `.test` file content to a specific path

- [ ] **Step 2: Verify the template renders correctly**

Run: `python3 scripts/prompt_template.py --issue 512 --title "test" --body "test body"`
Expected: prints the rendered prompt

### Task 2: Create the test runner

**Files:**
- Create: `scripts/run_test.py`

- [ ] **Step 1: Write the test runner**

A thin wrapper around `RegressTests.py` that:
- Takes a `.test` file path
- Runs it against ledger
- Returns exit code 0 (pass) or 1 (fail)

- [ ] **Step 2: Verify it works**

Run: `python3 scripts/run_test.py test/regress/2413.test`
Expected: exit 0

### Task 3: Create the orchestrator

**Files:**
- Create: `scripts/orchestrator.py`

- [ ] **Step 1: Write the orchestrator**

The orchestrator:
1. Fetches all open bug issues via `gh` (excluding enhancement/python/build/docs labels)
2. Filters out issues that already have regression tests
3. For each issue (up to 10 parallel):
   a. Spawns `claude -p` with the rendered prompt
   b. Waits for completion
   c. Validates the generated `.test` file exists
   d. Runs the test
   e. Categorizes: pass → `test/regress/`, fail → `test/todo/`
4. Writes `issues_report.json` with full results

- [ ] **Step 2: Test with a single issue**

Run: `python3 scripts/orchestrator.py --limit 1 --parallel 1`
Expected: processes one issue, creates one test file, reports result

- [ ] **Step 3: Test with a small batch**

Run: `python3 scripts/orchestrator.py --limit 5 --parallel 5`
Expected: processes 5 issues in parallel

### Task 4: Create test/todo directory and integration

- [ ] **Step 1: Create test/todo/ directory with README**

- [ ] **Step 2: Full run**

Run: `python3 scripts/orchestrator.py --parallel 10`
Expected: processes all ~188 issues

### Task 5: Generate summary and commit

- [ ] **Step 1: Review issues_report.json**
- [ ] **Step 2: Commit all tests**
- [ ] **Step 3: Create PR referencing closed issues**
