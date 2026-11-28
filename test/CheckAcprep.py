#!/usr/bin/env python3
# CheckAcprep.py - keep acprep's help text, options and package lists honest.
#
# Usage: CheckAcprep.py <path-to-source-tree>
#
# acprep is the documented entry point for installing dependencies and
# configuring a build, but nothing else exercises its help text or its
# per-distribution package lists.  This check fails when:
#
#   1. `acprep help` lists a build flavor or phase that has no
#      setup_flavor_<name> or phase_<name> method, or omits one that exists;
#   2. the help still sends readers to tools the build no longer uses;
#   3. an option stores a value that acprep never reads;
#   4. importing acprep runs its command line (the check needs the module);
#   5. for a supported Ubuntu release, BoostInfo's Boost packages differ from
#      the components CMakeLists.txt's find_package(Boost ... REQUIRED ...)
#      names, or --python does not add exactly libboost-python-dev;
#   6. --boost=DIR does not reach CMake as -DBoost_ROOT=DIR, the spelling
#      find_package(Boost ... CONFIG) reads on every supported CMake.
#
# The version that `acprep info` reports is checked by CheckAcprepInfo.py.

import importlib.machinery
import importlib.util
import os
import pathlib
import re
import subprocess
import sys
import tempfile

failures = []


def fail(message):
    failures.append(message)
    print('FAIL: ' + message)


def help_entries(text, start, stop):
    section = text.split(start, 1)[1].split(stop, 1)[0]
    return set(re.findall(r'^  ([a-z_]+) {2,}\S', section, re.MULTILINE))


def check_boost_root(source, acprep_path):
    # find_package reads <PackageName>_ROOT (Boost_ROOT) under policy CMP0074,
    # NEW since CMake 3.12.  It reads the upper-case BOOST_ROOT only under
    # CMP0144, which needs CMake 3.27, so -DBOOST_ROOT selects nothing on the
    # 3.16.2 floor and is ignored with a warning on newer CMake.  A stub cmake
    # records the arguments acprep's config phase would pass.
    with tempfile.TemporaryDirectory() as tmp:
        tmp = pathlib.Path(tmp)
        (tmp / 'bin').mkdir()
        stub = tmp / 'bin' / 'cmake'
        stub.write_text('#!/bin/sh\nprintf \'%s\\n\' "$@" > "$CHECK_ACPREP_ARGS"\n')
        stub.chmod(0o755)
        args_file = tmp / 'cmake-args'
        boost = str(tmp / 'my boost')
        env = dict(os.environ, HOME=str(tmp), CHECK_ACPREP_ARGS=str(args_file),
                   PATH=str(tmp / 'bin') + os.pathsep + os.environ.get('PATH', ''))
        result = subprocess.run(
            [sys.executable, '-B', str(acprep_path), '--no-git',
             '--boost=' + boost, '--output=' + str(tmp / 'build'), 'config'],
            cwd=str(source), env=env, capture_output=True, text=True)
        args = args_file.read_text().splitlines() if args_file.exists() else []
        if result.returncode != 0 or not args:
            fail('acprep --boost=DIR config did not run cmake (exit %d): %s'
                 % (result.returncode, result.stderr))
        elif '-DBoost_ROOT=' + boost not in args or \
                any(a.startswith('-DBOOST_ROOT=') for a in args):
            fail('acprep --boost=DIR passes %s, expected -DBoost_ROOT=DIR'
                 % [a for a in args if a.upper().startswith('-DBOOST_ROOT=')])


def main():
    if len(sys.argv) != 2:
        print('usage: CheckAcprep.py <path-to-source-tree>', file=sys.stderr)
        return 2
    source = pathlib.Path(sys.argv[1]).resolve()
    acprep_path = source / 'acprep'
    code = acprep_path.read_text()

    with tempfile.TemporaryDirectory() as cwd:
        # HOME points at the empty directory so that a ~/Products or a
        # missing HOME on the test machine cannot change what acprep does.
        result = subprocess.run([sys.executable, str(acprep_path), 'help'],
                                cwd=cwd, env=dict(os.environ, HOME=cwd),
                                capture_output=True, text=True)
    if result.returncode != 0:
        fail('acprep help exited %d: %s' % (result.returncode, result.stderr))
        return 1
    text = result.stdout

    for kind, method, start, stop in [
            ('flavor', 'setup_flavor_', 'optional build FLAVOR',
             'optional build PHASE'),
            ('phase', 'phase_', 'optional build PHASE', 'NOTE:')]:
        listed = help_entries(text, start, stop)
        defined = set(re.findall(r'^    def ' + method + r'(\w+)\(',
                                 code, re.MULTILINE))
        for name in sorted(listed - defined):
            fail('help lists %s %r but acprep has no %s%s'
                 % (kind, name, method, name))
        for name in sorted(defined - listed):
            fail('acprep defines %s%s but help does not list %s %r'
                 % (method, name, kind, name))

    for stale in ['autoconf', 'shark']:
        if stale in text:
            fail('help still mentions %r' % stale)

    for dest in re.findall(r'dest=["\'](\w+)["\']', code):
        if not re.search(r'options\.' + dest + r'\b', code):
            fail('an option stores options.%s, which acprep never reads'
                 % dest)

    sys.dont_write_bytecode = True
    loader = importlib.machinery.SourceFileLoader('acprep', str(acprep_path))
    module = importlib.util.module_from_spec(
        importlib.util.spec_from_loader('acprep', loader))
    saved_argv = sys.argv
    sys.argv = ['acprep', 'help']
    try:
        loader.exec_module(module)
    except SystemExit:
        fail('importing acprep ran its command line')
        return 1
    finally:
        sys.argv = saved_argv

    cmake = (source / 'CMakeLists.txt').read_text()
    required = re.search(r'find_package\(Boost\s+\S+\s+REQUIRED\s+([\w\s]+?)\s+\$',
                         cmake).group(1).split()
    expected = {'libboost-%s-dev' % c.replace('_', '-') for c in required}
    extras = {'libboost-dev', 'libboost-test-dev', 'libboost-python-dev'}
    for release in module.UBUNTU_RELEASES:
        for python in (False, True):
            packages = module.BoostInfo().dependencies('ubuntu-' + release,
                                                       python)
            components = {p for p in packages
                          if p.startswith('libboost-')} - extras
            if components != expected:
                fail('ubuntu-%s: Boost component packages %s, expected %s'
                     % (release, sorted(components), sorted(expected)))
            if ('libboost-python-dev' in packages) != python:
                fail('ubuntu-%s: libboost-python-dev %s with python=%s'
                     % (release, 'present' if not python else 'missing',
                        python))

    check_boost_root(source, acprep_path)

    if failures:
        return 1
    print('OK')
    return 0


if __name__ == '__main__':
    sys.exit(main())
