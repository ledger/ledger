"""Prompt template for Claude Code subprocess that analyzes a GitHub issue
and generates a ledger .test regression test file."""


def render_prompt(
    issue_number: int,
    issue_title: str,
    issue_body: str,
    output_path: str,
    ledger_bin: str,
    repo_root: str,
) -> str:
    return f"""You are creating a regression test for the ledger command-line accounting tool.

## Your Task

Analyze GitHub issue #{issue_number} and create a `.test` file that demonstrates
whether the reported bug still exists in the current version of ledger.

**Issue #{issue_number}: {issue_title}**

{issue_body}

## Test File Format

Ledger `.test` files have this structure:

```
; Comment describing the test
; One or more journal entries (transactions, directives, price entries, etc.)

2024/01/01 Payee
    Expenses:Food    $10.00
    Assets:Cash

test <ledger-command-without-ledger-binary-or-file-flag>
<expected stdout output, exactly matching what ledger produces>
end test
```

Key rules:
- Journal data goes at the top of the file (before any `test` blocks)
- `test <command>` starts a test block — the command should NOT include the ledger binary path or `-f` flag (the harness adds those)
- Expected output follows the `test` line, matching ledger's stdout exactly
- `end test` closes the block
- For tests expecting specific exit codes: `test <command> -> <exit_code>`
- For tests expecting stderr output: add `__ERROR__` line, then expected stderr lines
- Multiple `test`/`end test` blocks can appear in one file
- `$FILE` is replaced with the test file path at runtime
- Comments start with `;` (in journal sections) or `#` (standalone)

## Examples of good test files

Example 1 (simple register test):
```
; Regression test for issue #2264
i 2023-06-16 06:30:00 Start of test entry
o 2023-06-16 07:00:00

test reg --format '%(format_datetime(datetime, "%H:%M"))\\n'
06:30
end test
```

Example 2 (commodity/price test):
```
; Regression test for issue #2131
commodity ACME
    format 0 ACME

P 2022/07/01 ACME $100.00
P 2022/08/01 ACME $200.00

2022/07/01 Buy ACME
    Assets:Investments     1 ACME @ $100.00
    Assets:Cash

test reg -V -M -e 2022/09/01 Assets:Investments
22-Jul-01 - 22-Jul-31           Assets:Investments          $100.00      $100.00
22-Aug-01 Commodities revalued  <Revalued>                  $100.00      $200.00
end test
```

Example 3 (CSV output test):
```
; Quoted commodity symbols should not retain quotes in CSV output
2021/01/01 Imaginary cryptocurrency tx
    Assets:Crypto   5 "l33tc0in" @ £123
    Assets:Checking

test csv
"2021/01/01","","Imaginary cryptocurrency tx","Assets:Crypto","l33tc0in","5","",""
"2021/01/01","","Imaginary cryptocurrency tx","Assets:Checking","£","-615","",""
end test
```

Example 4 (error test):
```
; Test that invalid input produces an error
2024/01/01 Bad transaction
    Expenses:Food    $10.00

test bal -> 1
__ERROR__
While parsing file "$FILE", line 2:
Error: Transaction does not balance
end test
```

## Instructions

1. First, read the issue carefully and understand what bug is being reported.

2. If the issue includes example ledger data or commands, use those as the basis for your test.
   If not, construct minimal journal data that reproduces the described scenario.

3. Run the ledger command(s) to see what the CURRENT behavior is:
   ```
   echo '<journal data>' | {ledger_bin} --args-only -f /dev/stdin <command>
   ```
   or write a temp file and run against it.

4. Compare current behavior against what the issue reports:
   - If ledger now produces the CORRECT output (the bug is fixed), write the test
     with the correct expected output.
   - If ledger still produces WRONG output (the bug persists), write the test with
     the CURRENT (wrong) output as expected output. We need the test to PASS against
     current ledger so we can track it.

5. Write the test file to: {output_path}

6. The test file MUST start with a comment:
   ```
   ; Regression test for issue #{issue_number}: {issue_title}
   ; https://github.com/ledger/ledger/issues/{issue_number}
   ```

7. After writing the file, verify it by running:
   ```
   TZ=America/Chicago python3 {repo_root}/test/RegressTests.py \\
     --ledger {ledger_bin} --sourcepath {repo_root} {output_path}
   ```
   If the test fails, read the diff output and fix the expected output to match
   what ledger actually produces. Re-run until it passes.

8. CRITICAL — You MUST end your response with EXACTLY this line (replace values):
   ```
   RESULT_JSON: {{"status": "<fixed|broken|skip>", "notes": "<brief explanation>"}}
   ```
   This MUST be the very last line of your output. Do not write anything after it.

   Status values:
   - "fixed": The issue appears to be fixed — ledger produces correct output
   - "broken": The issue still exists — the test captures current (wrong) behavior
   - "skip": Cannot create a meaningful test (e.g., issue is about build system,
     requires interactive input, is a feature request, or is too vague)

   Example: RESULT_JSON: {{"status": "broken", "notes": "Rounding error still present in --price output"}}

IMPORTANT:
- The test MUST pass when run against the current ledger binary.
- Keep journal data minimal — just enough to demonstrate the issue.
- Use simple, deterministic dates and amounts.
- Do NOT use `-f` in the `test` command — the harness adds it automatically.
- If the issue involves a crash or hang, test for the error exit code.
- If you cannot determine what the correct behavior should be from the issue,
  write the test with current behavior and mark as "broken" with a note explaining
  the ambiguity.
- If the issue is truly untestable (about REPL, build system, IDE integration, etc.),
  output status "skip" and do NOT create a test file.
"""
