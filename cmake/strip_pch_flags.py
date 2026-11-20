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


def get_compiler_implicit_flags(compiler: str) -> list[tuple[str, str]]:
    """Run 'compiler -v -E -x c++ /dev/null' and extract flags passed to cc1.

    Compiler wrappers like Nix's clang-wrapper inject -isystem/-isysroot paths
    for Nix store locations that are not present in compile_commands.json.
    Without them clang-tidy cannot find system headers like <wchar.h>.

    The wrapper outputs the underlying cc1 invocation to stderr when -v is
    used; we parse that line to extract the extra flags.

    Every (-isystem|-idirafter|-isysroot, path) pair is returned in cc1
    order, duplicates included; inject_flags() decides which ones a
    command lacks.
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

    return [(flag, path) for flag, path in zip(tokens, tokens[1:])
            if flag in ('-isystem', '-idirafter', '-isysroot')]


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


def inject_flags(command: str, pairs: list[tuple[str, str]]) -> str:
    """Append the implicit flag pairs a command lacks: an -isystem path
    already on the command (or added earlier) is skipped, -isysroot is
    added only when the command has none, and every -idirafter is kept."""
    existing_isystem = set(re.findall(r'-isystem\s+(\S+)', command))
    has_isysroot = '-isysroot' in command
    new_flags: list[str] = []
    for flag, path in pairs:
        if flag == '-isystem':
            if path in existing_isystem:
                continue
            existing_isystem.add(path)
        elif flag == '-isysroot':
            if has_isysroot:
                continue
            has_isysroot = True
        new_flags.extend([flag, path])
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
    compiler_flags_cache: dict[str, list[tuple[str, str]]] = {}

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

    n_extra = sum(len(v) for v in compiler_flags_cache.values())
    print(
        f"strip_pch_flags: {len(entries)} -> {len(filtered)} entries"
        f" (+{n_extra} implicit flags per entry)",
        file=sys.stderr,
    )
    return 0


if __name__ == '__main__':
    sys.exit(main())
