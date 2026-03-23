#!/usr/bin/env python3
"""Ledger Snapshot Tool — run every command/option combo and capture output."""

from __future__ import annotations

import argparse
import hashlib
import os
import re
import signal
import subprocess
import sys
import time
from concurrent.futures import ProcessPoolExecutor, as_completed
from dataclasses import dataclass
from pathlib import Path
from typing import Any


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Combo:
    """A single command + options combination to test."""
    command: str
    options: tuple[tuple[str, ...], ...]  # sorted tuples, e.g. (("--flat",), ("--depth", "2"))
    source: str  # "curated", "mined:<filepath>:<line>"


# ---------------------------------------------------------------------------
# Filename encoding
# ---------------------------------------------------------------------------

def combo_to_filename(combo: Combo) -> str:
    """Deterministic filename for a combo's output file."""
    parts = [combo.command]
    for opt in sorted(combo.options):
        parts.append("-".join(opt).lstrip("-"))
    name = "--".join(parts) + ".txt"
    if len(name.encode("utf-8")) > 255:
        full_hash = hashlib.sha256(name.encode()).hexdigest()[:8]
        name = name[:240] + "--" + full_hash + ".txt"
    return name


def combo_to_cmdline(combo: Combo) -> list[str]:
    """Flatten a Combo's options into a command-line argument list."""
    args = [combo.command]
    for opt in combo.options:
        args.extend(opt)
    return args


# ---------------------------------------------------------------------------
# Commands and option applicability
# ---------------------------------------------------------------------------

REPORT_COMMANDS = [
    "balance", "register", "print", "csv", "xml", "emacs",
    "accounts", "payees", "commodities", "prices", "pricedb",
    "tags", "stats", "equity", "budget", "cleared",
]

ALL_KNOWN_COMMANDS = REPORT_COMMANDS + [
    "xact", "entry", "select", "source", "convert", "echo", "pricemap", "script",
]

# Short aliases that map to canonical command names
COMMAND_ALIASES = {
    "bal": "balance", "reg": "register", "p": "print",
    "eq": "equity", "pr": "print",
}

# Options exempt from coverage validation (execution-environment or debug options)
EXEMPT_OPTIONS = frozenset({
    "args-only", "debug", "trace", "verify", "verify-memory",
    "version", "help", "options", "script", "import",
    "force-pager", "no-pager", "no-color", "pager", "init-file",
    "seed", "verbose", "download", "price-db", "file", "output",
    "columns", "input-date-format", "cache",
})

# Commands grouped by capability for option applicability
_BAL_REG = {"balance", "register"}
_BAL_REG_CSV = {"balance", "register", "csv"}
_BAL_REG_CSV_BUD = {"balance", "register", "csv", "budget"}
_BAL_REG_CSV_BUD_CLR = {"balance", "register", "csv", "budget", "cleared"}
_BAL_REG_BUD = {"balance", "register", "budget"}
_BAL_REG_CSV_PRINT = {"balance", "register", "csv", "print"}
_PERIOD_CMDS = {"balance", "register", "csv", "budget", "cleared"}
_VALUATION_CMDS = {"balance", "register", "csv", "budget", "cleared", "equity"}
_LOT_CMDS = {"balance", "register", "csv", "print"}
_SORT_CMDS = {"balance", "register", "csv", "accounts", "payees", "commodities", "prices"}
_HEAD_TAIL_CMDS = {"balance", "register", "csv", "print", "accounts", "payees"}

# Map: option -> (takes_value, applicable_commands)
# Options with takes_value=True will have a default test value assigned.
OPTION_MATRIX: dict[str, tuple[bool, set[str]]] = {
    # Universal display options
    "--flat":            (False, {"balance", "budget"}),
    "--empty":           (False, {"balance", "register", "budget", "cleared"}),
    "--real":            (False, {"balance", "register", "csv", "budget", "cleared", "print", "equity"}),
    "--cleared":         (False, _BAL_REG_CSV_BUD_CLR),
    "--uncleared":       (False, _BAL_REG_CSV_BUD_CLR),
    "--pending":         (False, _BAL_REG_CSV_BUD_CLR),
    "--actual":          (False, {"balance", "register", "csv", "budget", "cleared", "print", "equity"}),
    "--current":         (False, _BAL_REG_CSV_BUD_CLR),
    "--collapse":        (False, {"register", "csv"}),
    "--subtotal":        (False, {"register", "csv"}),
    "--no-total":        (False, _BAL_REG_CSV_BUD_CLR),
    "--invert":          (False, _BAL_REG_CSV_BUD),
    "--dc":              (False, _BAL_REG_BUD),
    "--related":         (False, _BAL_REG_CSV_PRINT),
    "--generated":       (False, {"balance", "register", "csv", "print", "xml"}),
    "--depth":           (True,  {"balance", "register", "budget", "accounts"}),
    "--wide":            (False, {"register"}),
    "--average":         (False, _BAL_REG_CSV_BUD),
    "--percent":         (False, _BAL_REG_BUD),
    "--count":           (False, _BAL_REG),
    "--aux-date":        (False, {"balance", "register", "csv", "print", "cleared"}),
    "--by-payee":        (False, _BAL_REG_CSV_BUD),
    "--no-rounding":     (False, _BAL_REG_CSV_BUD_CLR),
    "--no-titles":       (False, {"balance", "budget"}),
    "--exact":           (False, _BAL_REG_CSV_BUD),
    "--raw":             (False, {"print"}),
    "--related-all":     (False, {"register"}),
    "--deviation":       (False, _BAL_REG_BUD),
    "--immediate":       (False, _BAL_REG_CSV),
    "--inject":          (True,  {"register"}),

    # Period options
    "--daily":           (False, _PERIOD_CMDS),
    "--weekly":          (False, _PERIOD_CMDS),
    "--monthly":         (False, _PERIOD_CMDS),
    "--quarterly":       (False, _PERIOD_CMDS),
    "--yearly":          (False, _PERIOD_CMDS),
    "--dow":             (False, {"balance", "register", "csv", "budget"}),

    # Valuation options
    "--market":          (False, _VALUATION_CMDS),
    "--basis":           (False, _BAL_REG_CSV_BUD_CLR),
    "--gain":            (False, _BAL_REG_CSV),
    "--quantity":        (False, _BAL_REG_CSV_BUD),
    "--price":           (False, _BAL_REG_CSV),
    "--historical":      (False, _BAL_REG_CSV),

    # Lot options
    "--lots":            (False, _LOT_CMDS),
    "--lot-prices":      (False, _LOT_CMDS),
    "--lot-dates":       (False, _LOT_CMDS),
    "--lot-notes":       (False, _LOT_CMDS),
    "--lots-actual":     (False, _BAL_REG_CSV),

    # Sort/limit options
    "--sort":            (True,  _SORT_CMDS),
    "--head":            (True,  _HEAD_TAIL_CMDS),
    "--tail":            (True,  _HEAD_TAIL_CMDS),

    # Budget options
    "--budget":          (False, _BAL_REG),
    "--add-budget":      (False, _BAL_REG),
    "--unbudgeted":      (False, _BAL_REG),
    "--forecast-while":  (True,  {"register"}),

    # Print-specific
    "--anon":            (False, {"print"}),
    "--rich-data":       (False, {"print"}),
    "--equity":          (False, {"print"}),

    # Register/tags
    "--values":          (False, {"register", "tags"}),
    "--revalued":        (False, {"register"}),
    "--revalued-only":   (False, {"register"}),
    "--collapse-if-zero": (False, {"register"}),
    "--day-break":       (False, {"register"}),

    # Display formatting
    "--group-by":        (True,  _BAL_REG),
    "--pivot":           (True,  _BAL_REG),
    "--color":           (False, _BAL_REG),
    "--bold-if":         (True,  _BAL_REG),
    "--average-lot-prices": (False, _BAL_REG),
    "--unrealized":      (False, _BAL_REG),
    "--decimal-comma":   (False, _BAL_REG_CSV_PRINT),
    "--time-colon":      (False, _BAL_REG),
    "--date-format":     (True,  {"balance", "register", "csv", "print", "cleared"}),
    "--base":            (False, _BAL_REG_CSV),
    "--primary-date":    (False, _BAL_REG_CSV_PRINT),
    "--expand-period":   (False, {"register", "balance"}),
}

# Default test values for value-bearing options
OPTION_TEST_VALUES: dict[str, str] = {
    "--depth": "2",
    "--sort": "date",
    "--head": "5",
    "--tail": "5",
    "--inject": "amount == 0",
    "--forecast-while": "date < [2015]",
    "--group-by": "payee",
    "--pivot": "payee",
    "--bold-if": "amount > 0",
    "--date-format": "%Y-%m-%d",
}


# ---------------------------------------------------------------------------
# Curated combination generation
# ---------------------------------------------------------------------------

# Multi-option combos: list of (option_tuples, applicable_commands)
MULTI_OPTION_COMBOS: list[tuple[tuple[tuple[str, ...], ...], set[str]]] = [
    # Period + Valuation
    ((("--monthly",), ("--market",)),           _PERIOD_CMDS & _VALUATION_CMDS),
    ((("--monthly",), ("--basis",)),            _PERIOD_CMDS & _BAL_REG_CSV_BUD_CLR),
    ((("--weekly",), ("--market",)),            _PERIOD_CMDS & _VALUATION_CMDS),
    ((("--quarterly",), ("--gain",)),           _PERIOD_CMDS & _BAL_REG_CSV),
    ((("--yearly",), ("--market",)),            _PERIOD_CMDS & _VALUATION_CMDS),

    # Period + Display
    ((("--monthly",), ("--empty",)),            _PERIOD_CMDS & {"balance", "register", "budget", "cleared"}),
    ((("--monthly",), ("--collapse",)),         {"register"}),
    ((("--monthly",), ("--no-total",)),         _PERIOD_CMDS & _BAL_REG_CSV_BUD_CLR),
    ((("--monthly",), ("--flat",)),             {"balance"}),
    ((("--daily",), ("--empty",)),              _PERIOD_CMDS & {"balance", "register", "budget", "cleared"}),
    ((("--weekly",), ("--subtotal",)),          {"register"}),

    # Flat + Depth
    ((("--flat",), ("--depth", "2")),           {"balance"}),
    ((("--flat",), ("--empty",)),               {"balance"}),
    ((("--flat",), ("--no-total",)),            {"balance"}),
    ((("--flat",), ("--sort", "amount")),       {"balance"}),

    # Valuation + Lot
    ((("--basis",), ("--lots",)),               _BAL_REG_CSV),
    ((("--market",), ("--lots",)),              _BAL_REG_CSV),
    ((("--gain",), ("--lots",)),                _BAL_REG_CSV),
    ((("--market",), ("--lot-prices",)),        _BAL_REG_CSV),

    # Budget combinations
    ((("--budget",), ("--empty",)),             _BAL_REG),
    ((("--budget",), ("--monthly",)),           _BAL_REG),
    ((("--budget",), ("--flat",)),              {"balance"}),
    ((("--budget",), ("--monthly",), ("--empty",)), _BAL_REG),
    ((("--add-budget",), ("--monthly",)),       _BAL_REG),
    ((("--unbudgeted",), ("--flat",)),          {"balance"}),

    # Sort + Display
    ((("--sort", "date"), ("--flat",)),         {"balance"}),
    ((("--sort", "amount"), ("--flat",)),       {"balance"}),
    ((("--subtotal",), ("--by-payee",)),        {"register"}),
    ((("--sort", "payee"), ("--subtotal",)),    {"register"}),

    # Three-option combos
    ((("--market",), ("--monthly",), ("--empty",)),          _PERIOD_CMDS & _VALUATION_CMDS & {"balance", "register", "budget", "cleared"}),
    ((("--monthly",), ("--flat",), ("--depth", "2")),        {"balance"}),
    ((("--budget",), ("--monthly",), ("--flat",)),           {"balance"}),
    ((("--gain",), ("--monthly",), ("--lots",)),             _BAL_REG_CSV),
    ((("--flat",), ("--empty",), ("--no-total",)),           {"balance"}),
    ((("--market",), ("--historical",), ("--monthly",)),     _BAL_REG_CSV),
    ((("--basis",), ("--lots",), ("--lot-prices",)),         _BAL_REG_CSV),
    ((("--monthly",), ("--by-payee",), ("--subtotal",)),     {"register"}),
    ((("--dc",), ("--flat",), ("--empty",)),                 {"balance"}),
    ((("--invert",), ("--monthly",), ("--market",)),         _BAL_REG_CSV_BUD & _PERIOD_CMDS & _VALUATION_CMDS),
]


def generate_curated_combos() -> list[Combo]:
    """Generate all curated command/option combinations."""
    combos: list[Combo] = []
    seen: set[tuple[str, tuple[tuple[str, ...], ...]]] = set()

    def _add(command: str, options: tuple[tuple[str, ...], ...], source: str = "curated") -> None:
        opts = tuple(sorted(options))
        key = (command, opts)
        if key not in seen:
            seen.add(key)
            combos.append(Combo(command=command, options=opts, source=source))

    # Bare command (no extra options) for every report command
    for cmd in REPORT_COMMANDS:
        _add(cmd, ())

    # Bare command for non-report commands too
    for cmd in ALL_KNOWN_COMMANDS:
        if cmd not in REPORT_COMMANDS:
            _add(cmd, ())

    # Single-option combos: pair each option with each applicable command
    for opt, (takes_value, applicable) in OPTION_MATRIX.items():
        for cmd in applicable:
            if takes_value:
                value = OPTION_TEST_VALUES.get(opt, "1")
                _add(cmd, ((opt, value),))
            else:
                _add(cmd, ((opt,),))

    # Multi-option combos
    for opts, applicable in MULTI_OPTION_COMBOS:
        for cmd in applicable:
            _add(cmd, opts)

    return combos


# ---------------------------------------------------------------------------
# Command line parsing for mining
# ---------------------------------------------------------------------------

# Options that take a value argument (the next token is consumed)
VALUE_OPTIONS = frozenset({
    "-f", "--file", "--init-file", "--pager", "--columns", "--output",
    "--sort", "--head", "--tail", "--depth", "--inject", "--forecast-while",
    "--group-by", "--pivot", "--bold-if", "--date-format", "--format",
    "--balance-format", "--register-format", "--csv-format", "--plot-format",
    "--pricedb-format", "--prices-format", "--limit", "--only", "--display",
    "--amount", "--total", "--account", "--payee", "--begin", "--end",
    "--period", "--period-sort", "--now", "--exchange", "--prepend-format",
    "--prepend-width", "--meta", "--price-exp", "--lot-price",
    "--lot-date", "--lot-note", "--lot-tag", "--input-date-format",
    "--recursive-aliases", "--start-of-week",
})

# Options to strip from mined commands (execution-environment options)
STRIP_OPTIONS = frozenset({
    "--args-only", "--columns", "--pager", "--init-file", "--force-pager",
    "--no-pager", "--no-color", "-f", "--file", "--output",
})

# Options whose VALUES should also be stripped (option + next arg both removed)
STRIP_VALUE_OPTIONS = STRIP_OPTIONS & VALUE_OPTIONS


def parse_ledger_cmdline(cmdline: str) -> tuple[str, tuple[tuple[str, ...], ...]] | None:
    """Parse a ledger command line into (canonical_command, sorted_option_tuples).

    Strips file args, query args, and execution-environment options.
    Returns None if the command is not recognized.
    """
    import shlex
    try:
        tokens = shlex.split(cmdline)
    except ValueError:
        return None

    # Skip leading 'ledger' or '$ledger' if present
    if tokens and tokens[0] in ("ledger", "$ledger"):
        tokens = tokens[1:]

    # First pass: find the command and collect options
    command = None
    options: list[tuple[str, ...]] = []
    i = 0
    while i < len(tokens):
        tok = tokens[i]

        if tok.startswith("-"):
            # Handle --option=value
            if "=" in tok:
                opt_name, opt_val = tok.split("=", 1)
                if opt_name in STRIP_OPTIONS:
                    i += 1
                    continue
                if opt_name in VALUE_OPTIONS:
                    options.append((opt_name, opt_val))
                else:
                    options.append((opt_name,))
                i += 1
                continue

            # Handle option that takes a value
            if tok in STRIP_OPTIONS:
                # Skip this option and its value if it takes one
                if tok in VALUE_OPTIONS and i + 1 < len(tokens):
                    i += 2
                else:
                    i += 1
                continue

            if tok in VALUE_OPTIONS:
                if i + 1 < len(tokens):
                    options.append((tok, tokens[i + 1]))
                    i += 2
                else:
                    i += 1
                continue

            # Boolean option
            options.append((tok,))
            i += 1
            continue

        # Non-option token
        if command is None:
            # This might be the command
            canonical = COMMAND_ALIASES.get(tok, tok)
            if canonical in ALL_KNOWN_COMMANDS:
                command = canonical
            # else: skip (might be a query argument before the command)
        # else: skip query arguments after the command

        i += 1

    if command is None:
        return None

    # Sort options for canonical form
    sorted_opts = tuple(sorted(options))
    return (command, sorted_opts)


# ---------------------------------------------------------------------------
# Mining from .test files
# ---------------------------------------------------------------------------

def mine_test_files(test_root: Path, known_commands: set[str]) -> list[Combo]:
    """Extract combos from 'test <command> [args]' lines in .test files."""
    combos: list[Combo] = []
    seen: set[tuple[str, tuple[tuple[str, ...], ...]]] = set()

    test_dirs = ["baseline", "regress", "manual"]
    for dirname in test_dirs:
        test_dir = test_root / dirname
        if not test_dir.is_dir():
            continue
        for test_file in sorted(test_dir.glob("*.test")):
            try:
                lines = test_file.read_text(encoding="utf-8", errors="replace").splitlines()
            except OSError:
                continue
            for lineno, line in enumerate(lines, 1):
                if not line.startswith("test ") or line.startswith("test python"):
                    continue
                # Strip 'test ' prefix
                cmd_str = line[5:].strip()
                # Strip exit code annotation '-> N'
                cmd_str = re.sub(r'\s*->\s*\d+\s*$', '', cmd_str)
                if not cmd_str:
                    continue
                # Strip $ledger prefix if present
                cmd_str = re.sub(r'^\$ledger\s+', '', cmd_str)
                # Strip --args-only and --columns if already present
                result = parse_ledger_cmdline(cmd_str)
                if result is None:
                    continue
                command, options = result
                if command not in known_commands:
                    continue
                key = (command, options)
                if key not in seen:
                    seen.add(key)
                    source = f"mined:{test_file.relative_to(test_root)}:{lineno}"
                    combos.append(Combo(command=command, options=options, source=source))

    return combos


# ---------------------------------------------------------------------------
# Mining from Texinfo manual
# ---------------------------------------------------------------------------

def mine_texinfo(texi_path: Path, known_commands: set[str]) -> list[Combo]:
    """Extract combos from ledger invocations in @smallexample blocks of ledger3.texi."""
    if not texi_path.is_file():
        return []

    combos: list[Combo] = []
    seen: set[tuple[str, tuple[tuple[str, ...], ...]]] = set()

    try:
        lines = texi_path.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError:
        return []

    for lineno, line in enumerate(lines, 1):
        stripped = line.strip()

        # Look for ledger invocations both inside and outside examples
        # Pattern: "$ ledger ..." or "ledger ..." at start of line
        match = re.match(r'^(?:@c\s+)?\$?\s*ledger\s+(.+)', stripped)
        if not match:
            continue

        cmd_str = match.group(1).strip()
        result = parse_ledger_cmdline(cmd_str)
        if result is None:
            continue
        command, options = result
        if command not in known_commands:
            continue
        key = (command, options)
        if key not in seen:
            seen.add(key)
            combos.append(Combo(command=command, options=options,
                                source=f"mined:doc/ledger3.texi:{lineno}"))

    return combos


# ---------------------------------------------------------------------------
# Mining from man page
# ---------------------------------------------------------------------------

def mine_manpage(man_path: Path, known_commands: set[str]) -> list[Combo]:
    """Extract combos from ledger invocations in the man page."""
    if not man_path.is_file():
        return []

    combos: list[Combo] = []
    seen: set[tuple[str, tuple[tuple[str, ...], ...]]] = set()

    try:
        lines = man_path.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError:
        return []

    for lineno, line in enumerate(lines, 1):
        stripped = line.strip()
        # Match patterns like: .Dl ledger reg --sort ...
        # or: .Ic ledger bal --gain
        # or: .Ic "ledger bal --gain"
        match = re.match(r'^\.(?:Dl|Ic)\s+"?(?:Li\s+)?ledger\s+(.+?)"?\s*$', stripped)
        if not match:
            # Also match plain ledger invocations
            match = re.match(r'^ledger\s+(.+)', stripped)
        if not match:
            continue

        cmd_str = match.group(1).strip()
        # Clean up troff escapes
        cmd_str = re.sub(r'\\?\[aq\]', "'", cmd_str)
        cmd_str = re.sub(r'\\?\[ti\]', "~", cmd_str)
        cmd_str = re.sub(r'\\f[A-Z]', '', cmd_str)

        result = parse_ledger_cmdline(cmd_str)
        if result is None:
            continue
        command, options = result
        if command not in known_commands:
            continue
        key = (command, options)
        if key not in seen:
            seen.add(key)
            combos.append(Combo(command=command, options=options,
                                source=f"mined:doc/ledger.1:{lineno}"))

    return combos


# ---------------------------------------------------------------------------
# Combo mining aggregator
# ---------------------------------------------------------------------------

def mine_combos(source_root: Path) -> list[Combo]:
    """Mine combos from test files and documentation."""
    all_known = set(ALL_KNOWN_COMMANDS)
    combos: list[Combo] = []
    seen: set[tuple[str, tuple[tuple[str, ...], ...]]] = set()

    def _add_unique(new_combos: list[Combo]) -> None:
        for c in new_combos:
            key = (c.command, c.options)
            if key not in seen:
                seen.add(key)
                combos.append(c)

    # Mine from test files
    test_dir = source_root / "test"
    if test_dir.is_dir():
        _add_unique(mine_test_files(test_dir, all_known))

    # Mine from Texinfo manual
    texi = source_root / "doc" / "ledger3.texi"
    _add_unique(mine_texinfo(texi, all_known))

    # Mine from man page
    manpage = source_root / "doc" / "ledger.1"
    _add_unique(mine_manpage(manpage, all_known))

    return combos


# ---------------------------------------------------------------------------
# Option discovery and coverage validation
# ---------------------------------------------------------------------------

def discover_options(ledger_binary: Path) -> set[str]:
    """Discover all options the ledger binary supports via --debug option.names."""
    try:
        result = subprocess.run(
            [str(ledger_binary), "--debug", "option.names", "parse", "true"],
            capture_output=True, timeout=10,
        )
        regex = re.compile(r'\[DEBUG\]\s+Option:\s+(.*?)_?$')
        options = set()
        for line in result.stderr.decode("utf-8", errors="replace").splitlines():
            m = regex.search(line)
            if m:
                options.add(m.group(1).replace("_", "-"))
        return options
    except (subprocess.TimeoutExpired, OSError):
        return set()


def validate_coverage(
    combos: list[Combo],
    ledger_binary: Path,
    verbose: bool,
) -> list[Combo]:
    """Check every known command and option appears in at least one combo.

    Generates fallback combos for any gaps.
    """
    covered_commands = {c.command for c in combos}
    covered_options: set[str] = set()
    for c in combos:
        for opt_tuple in c.options:
            covered_options.add(opt_tuple[0].lstrip("-"))

    fallbacks: list[Combo] = []

    # Check commands
    for cmd in ALL_KNOWN_COMMANDS:
        if cmd not in covered_commands:
            fallbacks.append(Combo(command=cmd, options=(), source="coverage-fallback"))
            if verbose:
                print(f"Coverage: adding fallback bare combo for command '{cmd}'", file=sys.stderr)

    # Check options from binary discovery
    known_options = discover_options(ledger_binary)
    # Also include options from our matrix
    for opt in OPTION_MATRIX:
        known_options.add(opt.lstrip("-"))

    for opt in sorted(known_options):
        if opt in EXEMPT_OPTIONS:
            continue
        if opt in covered_options:
            continue
        # Try to find an applicable command for this option
        matrix_key = f"--{opt}"
        if matrix_key in OPTION_MATRIX:
            takes_value, applicable = OPTION_MATRIX[matrix_key]
            cmd = sorted(applicable)[0]  # pick first alphabetically
            if takes_value:
                val = OPTION_TEST_VALUES.get(matrix_key, "1")
                fb = Combo(command=cmd, options=((matrix_key, val),), source="coverage-fallback")
            else:
                fb = Combo(command=cmd, options=((matrix_key,),), source="coverage-fallback")
        else:
            # Unknown option — pair with 'balance' as a safe default
            fb = Combo(command="balance", options=((matrix_key,),), source="coverage-fallback")
        fallbacks.append(fb)
        if verbose:
            print(f"Coverage: adding fallback combo for option '--{opt}'", file=sys.stderr)

    return combos + fallbacks


# ---------------------------------------------------------------------------
# Environment sanitization (mirrors LedgerHarness.run)
# ---------------------------------------------------------------------------

ALLOWED_ENV_VARS = frozenset({
    "PATH", "HOME", "TZ", "TZDIR", "LANG", "LC_ALL", "LC_CTYPE", "TERM",
    "TMPDIR", "TEMP", "TMP",
    "SYSTEMROOT", "COMSPEC", "MSYSTEM", "MINGW_PREFIX",
    "ASAN_OPTIONS", "LSAN_OPTIONS", "UBSAN_OPTIONS", "MSAN_OPTIONS",
    "TSAN_OPTIONS",
})

ALLOWED_ENV_PREFIXES = ("LLVM_PROFILE_", "GCOV_")


def make_sanitized_env() -> dict[str, str]:
    """Build a sanitized environment dict matching LedgerHarness conventions."""
    env = {
        k: v for k, v in os.environ.items()
        if k in ALLOWED_ENV_VARS or k.startswith(ALLOWED_ENV_PREFIXES)
    }
    env["TZ"] = "America/Chicago"
    return env


# ---------------------------------------------------------------------------
# Execution
# ---------------------------------------------------------------------------

def _safe_decode(data: bytes) -> str:
    """Decode bytes to UTF-8, replacing binary content with a placeholder."""
    try:
        return data.decode("utf-8")
    except UnicodeDecodeError:
        return f"# [Binary output: {len(data)} bytes]"


def run_combo(
    ledger_binary: Path,
    data_file: Path,
    combo: Combo,
    timeout: int,
    env: dict[str, str],
) -> tuple[Combo, int | str, str, str]:
    """Run a single combo and return (combo, exit_code, stdout, stderr).

    exit_code is an int for normal exits, or "TIMEOUT" for timeouts.
    """
    cmd = [
        str(ledger_binary), "--args-only", "--columns", "80",
        "-f", str(data_file),
    ] + combo_to_cmdline(combo)

    try:
        result = subprocess.run(
            cmd, capture_output=True, timeout=timeout, env=env,
        )
        stdout = _safe_decode(result.stdout)
        stderr = _safe_decode(result.stderr)
        return (combo, result.returncode, stdout, stderr)
    except subprocess.TimeoutExpired:
        return (combo, "TIMEOUT", "", "")


def format_output(
    combo: Combo,
    exit_code: int | str,
    stdout: str,
    stderr: str,
    data_file: Path,
    timeout: int,
) -> str:
    """Format the output for writing to a snapshot file."""
    cmd_display = " ".join(
        ["ledger", "--args-only", "--columns", "80", "-f", data_file.name]
        + combo_to_cmdline(combo)
    )
    lines = [f"# Command: {cmd_display}"]

    if exit_code == "TIMEOUT":
        lines.append(f"# Exit code: TIMEOUT ({timeout}s)")
    else:
        lines.append(f"# Exit code: {exit_code}")

    if stdout:
        lines.append(stdout.rstrip("\n"))

    lines.append("# --- stderr ---")
    if stderr:
        lines.append(stderr.rstrip("\n"))

    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# Output metadata
# ---------------------------------------------------------------------------

def write_metadata(
    output_dir: Path,
    ledger_binary: Path,
    data_file: Path,
    total: int,
    curated_count: int,
    mined_count: int,
    elapsed: float,
    workers: int,
) -> None:
    """Write _snapshot_meta.txt to the output directory."""
    meta = output_dir / "_snapshot_meta.txt"
    meta.write_text(
        f"# Ledger Snapshot Tool\n"
        f"# Date: {time.strftime('%Y-%m-%dT%H:%M:%S')}\n"
        f"# Ledger binary: {ledger_binary.resolve()}\n"
        f"# Data file: {data_file.resolve()}\n"
        f"# Combinations: {total}\n"
        f"# Curated: {curated_count}\n"
        f"# Mined: {mined_count}\n"
        f"# Elapsed: {elapsed:.1f}s\n"
        f"# Workers: {workers}\n",
        encoding="utf-8",
    )


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        prog="snapshot.py",
        description="Run every sensible ledger command/option combo and capture output.",
    )
    parser.add_argument("ledger_binary", type=Path, help="Path to the ledger executable")
    parser.add_argument("data_file", type=Path, help="Path to the journal/data file")
    parser.add_argument("output_dir", type=Path, help="Directory to write output files")
    parser.add_argument("--jobs", type=int, default=os.cpu_count(), help="Parallel workers (default: CPU count)")
    parser.add_argument("--mine-from", type=Path, default=None, help="Source tree root to mine combos from")
    parser.add_argument("--timeout", type=int, default=30, help="Per-invocation timeout in seconds (default: 30)")
    parser.add_argument("--dry-run", action="store_true", help="Print all combos without running them")
    parser.add_argument("--verbose", action="store_true", help="Print progress to stderr")
    parser.add_argument("--only-command", type=str, default=None, help="Only run combos for this command")
    parser.add_argument("--only-curated", action="store_true", help="Skip mined combos")
    parser.add_argument("--only-mined", action="store_true", help="Skip curated combos")
    return parser.parse_args(argv)


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

# Global for SIGINT handling in worker pool
_interrupted = False


def main(argv: list[str] | None = None) -> int:
    global _interrupted
    args = parse_args(argv)

    # Validate inputs
    if not args.ledger_binary.is_file():
        print(f"Error: ledger binary not found: {args.ledger_binary}", file=sys.stderr)
        return 1
    if not args.data_file.is_file():
        print(f"Error: data file not found: {args.data_file}", file=sys.stderr)
        return 1

    # Generate combinations
    curated: list[Combo] = []
    mined: list[Combo] = []

    if not args.only_mined:
        curated = generate_curated_combos()
    if not args.only_curated:
        source_root = args.mine_from
        if source_root is None:
            # Auto-detect: assume ledger binary is in <source>/build/ledger
            candidate = args.ledger_binary.resolve().parent.parent
            if (candidate / "test").is_dir():
                source_root = candidate
        if source_root is not None and source_root.is_dir():
            mined = mine_combos(source_root)
            # Deduplicate mined against curated
            curated_keys = {(c.command, c.options) for c in curated}
            mined = [c for c in mined if (c.command, c.options) not in curated_keys]

    all_combos = curated + mined

    # Apply --only-command filter
    if args.only_command:
        cmd = COMMAND_ALIASES.get(args.only_command, args.only_command)
        all_combos = [c for c in all_combos if c.command == cmd]
        if not all_combos:
            print(f"Error: no combos found for command '{args.only_command}'", file=sys.stderr)
            return 1

    # Coverage validation (unless filtering)
    if not args.only_command and not args.only_mined:
        all_combos = validate_coverage(all_combos, args.ledger_binary, args.verbose)

    if args.dry_run:
        for combo in sorted(all_combos, key=lambda c: combo_to_filename(c)):
            opts = " ".join(o for t in combo.options for o in t)
            source_tag = f"  [{combo.source}]" if args.verbose else ""
            print(f"{combo.command} {opts}{source_tag}".strip())
        print(f"\nTotal: {len(all_combos)} combinations", file=sys.stderr)
        return 0

    # Create output directory
    args.output_dir.mkdir(parents=True, exist_ok=True)

    # Run all combinations
    env = make_sanitized_env()
    total = len(all_combos)
    successes = 0
    warnings = 0
    errors = 0
    timeouts = 0

    # SIGINT handler
    original_sigint = signal.getsignal(signal.SIGINT)

    def sigint_handler(_signum: int, _frame: Any) -> None:
        global _interrupted
        _interrupted = True
        print("\nInterrupted — finishing in-progress work...", file=sys.stderr)

    signal.signal(signal.SIGINT, sigint_handler)

    t0 = time.monotonic()

    try:
        with ProcessPoolExecutor(max_workers=args.jobs) as executor:
            futures = {
                executor.submit(run_combo, args.ledger_binary, args.data_file, combo, args.timeout, env): combo
                for combo in all_combos
            }

            done_count = 0
            for future in as_completed(futures):
                if _interrupted:
                    executor.shutdown(wait=False, cancel_futures=True)
                    break

                combo, exit_code, stdout, stderr = future.result()
                done_count += 1

                # Classify result
                if exit_code == "TIMEOUT":
                    timeouts += 1
                elif exit_code != 0:
                    errors += 1
                elif stderr:
                    warnings += 1
                else:
                    successes += 1

                # Write output file
                filename = combo_to_filename(combo)
                content = format_output(combo, exit_code, stdout, stderr,
                                        args.data_file, args.timeout)
                (args.output_dir / filename).write_text(content, encoding="utf-8")

                if args.verbose:
                    opts = " ".join(o for t in combo.options for o in t)
                    print(f"[{done_count}/{total}] {combo.command} {opts}".strip(), file=sys.stderr)
    finally:
        signal.signal(signal.SIGINT, original_sigint)

    elapsed = time.monotonic() - t0

    # Write metadata
    write_metadata(args.output_dir, args.ledger_binary, args.data_file,
                   total, len(curated), len(mined), elapsed, args.jobs or os.cpu_count() or 1)

    # Summary (always to stderr)
    print(f"\n--- Snapshot Summary ---", file=sys.stderr)
    print(f"Total:    {done_count}", file=sys.stderr)
    print(f"Success:  {successes}", file=sys.stderr)
    print(f"Warnings: {warnings} (exit 0 + stderr)", file=sys.stderr)
    print(f"Errors:   {errors}", file=sys.stderr)
    print(f"Timeouts: {timeouts}", file=sys.stderr)
    print(f"Elapsed:  {elapsed:.1f}s", file=sys.stderr)
    if _interrupted:
        print(f"(interrupted after {done_count}/{total})", file=sys.stderr)

    return 0


if __name__ == "__main__":
    sys.exit(main())
