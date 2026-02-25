#!/usr/bin/env python3
"""Strip PCH-related flags from compile_commands.json for clang-tidy.

Precompiled headers (PCH) built with certain compiler wrappers (e.g., Nix's
clang-wrapper) can inject implicit flags not visible in compile_commands.json.
This causes clang-tidy to fail with:
  "predefined macro was enabled in PCH file but is currently disabled"

This script removes PCH flags and injects any implicit -isystem/-isysroot
flags that the compiler wrapper adds (e.g., Nix store paths for system headers)
so that clang-tidy can parse files without the PCH and find all system headers.
"""

import json
import re
import shlex
import subprocess
import sys
from pathlib import Path


def get_compiler_implicit_flags(compiler: str) -> list[str]:
    """Run 'compiler -v -E -x c++ /dev/null' and extract flags passed to cc1.

    Compiler wrappers like Nix's clang-wrapper inject -isystem/-isysroot paths
    for Nix store locations that are not present in compile_commands.json.
    Without them clang-tidy cannot find system headers like <wchar.h>.

    The wrapper outputs the underlying cc1 invocation to stderr when -v is
    used; we parse that line to extract the extra flags.
    """
    try:
        result = subprocess.run(
            [compiler, '-v', '-E', '-x', 'c++', '/dev/null'],
            capture_output=True,
            text=True,
            timeout=30,
        )
        output = result.stderr
    except (FileNotFoundError, subprocess.TimeoutExpired, OSError):
        return []

    # The -v output contains a line with the cc1 invocation that lists all
    # flags, including the implicit ones the wrapper adds.  Extract the line
    # that starts with a path and contains "-cc1".
    cc1_line = ''
    for line in output.splitlines():
        if ' -cc1 ' in line:
            cc1_line = line.strip()
            break
    if not cc1_line:
        return []

    # Parse the cc1 invocation tokens (the line uses shell quoting).
    try:
        tokens = shlex.split(cc1_line)
    except ValueError:
        return []

    # Extract -isystem, -isysroot, -idirafter flags
    extra_flags: list[str] = []
    seen_isystem: set[str] = set()
    i = 0
    while i < len(tokens):
        tok = tokens[i]
        if tok in ('-isystem', '-idirafter') and i + 1 < len(tokens):
            path = tokens[i + 1]
            if tok == '-idirafter' or path not in seen_isystem:
                extra_flags.extend([tok, path])
                if tok == '-isystem':
                    seen_isystem.add(path)
            i += 2
        elif tok == '-isysroot' and i + 1 < len(tokens):
            extra_flags.extend(['-isysroot', tokens[i + 1]])
            i += 2
        else:
            i += 1
    return extra_flags


def strip_pch_flags(command: str) -> str:
    """Remove PCH-related flags from a compilation command string."""
    # Remove: -Xclang -include-pch -Xclang <path.pch>
    command = re.sub(r'-Xclang\s+-include-pch\s+-Xclang\s+\S+', '', command)
    # Remove: -Xclang -include -Xclang <path>/cmake_pch*
    command = re.sub(r'-Xclang\s+-include\s+-Xclang\s+\S*cmake_pch\S*', '', command)
    # Remove standalone PCH flags
    command = command.replace('-Winvalid-pch', '')
    command = command.replace('-fpch-instantiate-templates', '')
    # Normalize whitespace
    return ' '.join(command.split())


def inject_flags(command: str, extra_flags: list[str]) -> str:
    """Append extra flags to a command, skipping duplicate -isystem paths."""
    if not extra_flags:
        return command
    existing_isystem = set(re.findall(r'-isystem\s+(\S+)', command))
    existing_isysroot = '-isysroot' in command
    new_flags = []
    i = 0
    while i < len(extra_flags) - 1:
        tok = extra_flags[i]
        if tok in ('-isystem', '-idirafter') and i + 1 < len(extra_flags):
            path = extra_flags[i + 1]
            if tok == '-idirafter' or path not in existing_isystem:
                new_flags.extend([tok, path])
                if tok == '-isystem':
                    existing_isystem.add(path)
            i += 2
        elif tok == '-isysroot' and i + 1 < len(extra_flags):
            if not existing_isysroot:
                new_flags.extend([tok, extra_flags[i + 1]])
                existing_isysroot = True
            i += 2
        else:
            i += 1
    if new_flags:
        return command + ' ' + ' '.join(new_flags)
    return command


def main() -> int:
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <input_db> <output_db>", file=sys.stderr)
        return 1

    input_path, output_path = sys.argv[1], sys.argv[2]

    with open(input_path) as f:
        entries = json.load(f)

    # Collect all unique compilers so we only probe each wrapper once.
    compiler_flags_cache: dict[str, list[str]] = {}

    filtered = []
    for entry in entries:
        # Skip PCH-generation entries (synthetic .hxx.cxx files)
        file_path = entry.get('file', '')
        if file_path.endswith('.cxx') or 'cmake_pch' in file_path:
            continue
        if 'command' in entry:
            entry = dict(entry)
            command = entry['command']
            # Extract the compiler binary (first token)
            try:
                tokens = shlex.split(command)
            except ValueError:
                tokens = []
            compiler = tokens[0] if tokens else ''
            # Get implicit flags for this compiler (cached)
            if compiler and compiler not in compiler_flags_cache:
                compiler_flags_cache[compiler] = get_compiler_implicit_flags(compiler)
            extra = compiler_flags_cache.get(compiler, [])
            command = strip_pch_flags(command)
            command = inject_flags(command, extra)
            entry['command'] = command
        filtered.append(entry)

    Path(output_path).parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, 'w') as f:
        json.dump(filtered, f, indent=2)

    n_extra = sum(
        len(v) // 2 for v in compiler_flags_cache.values()
    )
    print(
        f"strip_pch_flags: {len(entries)} -> {len(filtered)} entries"
        f" (+{n_extra} implicit flags per entry)",
        file=sys.stderr,
    )
    return 0


if __name__ == '__main__':
    sys.exit(main())
