#!/usr/bin/env python3
# Regression test for GitHub issue #3277:
#
# The snapshot tool's guide was added at the top of the source tree as
# README-snapshot.md.  A shell completes a partial file name only as far as the
# longest common prefix of the names it matches, so with both README.md and
# README-snapshot.md present, "README<TAB>" stopped at "README" and README.md --
# the first file most people read -- could no longer be completed in one step.
# The guide now lives next to the tool it documents, as test/snapshot.md.
#
# This test lists the top of the source tree and fails unless every entry whose
# name begins with "README" also begins with "README.md", which is exactly the
# condition under which "README<TAB>" completes all the way to README.md.  An
# editor backup such as README.md~ therefore passes, while README-snapshot.md
# or a README directory fails.  Names are compared without regard to case,
# because case-insensitive completion (readline's completion-ignore-case, or the
# matcher-list many zsh configurations set) treats "readme" like "README".
# Like the shell, the test sees untracked files as well as tracked ones, so a
# local README.html exported from README.md fails it too.  It also fails if
# README.md itself is missing.

import os
import sys


def main():
    if len(sys.argv) != 2:
        print("usage: 3277.py <path-to-source-tree>", file=sys.stderr)
        return 2

    source = sys.argv[1]

    if not os.path.isfile(os.path.join(source, "README.md")):
        print(f"FAIL: README.md not found at the top of {source}")
        return 1

    candidates = sorted(
        name for name in os.listdir(source) if name.lower().startswith("readme")
    )
    blocking = [
        name for name in candidates if not name.lower().startswith("readme.md")
    ]

    if blocking:
        print(
            f"FAIL: \"README<TAB>\" at the top of {source} no longer completes"
            " to README.md,"
        )
        print("because these entries also begin with README (ignoring case):")
        for name in blocking:
            print(f"    {name}")
        print("Give them names that do not begin with README, or move them.")
        return 1

    print(f"OK: \"README<TAB>\" completes to README.md ({', '.join(candidates)})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
