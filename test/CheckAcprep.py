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
#      names, or --python does not add exactly libboost-python-dev.
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

    if failures:
        return 1
    print('OK')
    return 0


if __name__ == '__main__':
    sys.exit(main())
