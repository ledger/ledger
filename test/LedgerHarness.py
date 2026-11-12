#!/usr/bin/env python3

import argparse
import io
import pathlib
import shlex
import signal
import sys
import os
import subprocess

from subprocess import Popen, PIPE


class LedgerHarness:
    @staticmethod
    def parser():
      parser = argparse.ArgumentParser(add_help=False)
      parser.add_argument('-l', '--ledger', type=pathlib.Path, required=True)
      parser.add_argument('-s', '--sourcepath', type=pathlib.Path, required=True)
      parser.add_argument('--verify', action='store_true')
      parser.add_argument('--gmalloc', action='store_true')
      parser.add_argument('--python', action='store_true')
      parser.add_argument('--timeout', type=float, default=60,
                          help='seconds one ledger command may run before '
                               'it is killed and counted as a failure '
                               '(default 60; a slow run, such as one under '
                               '--gmalloc, may need more)')
      return parser

    def __init__(self, ledger, sourcepath, verify=False, gmalloc=False, python=False,
                 timeout=60):
        if not ledger.is_file():
            print(f"Cannot find ledger at '{ledger}'", file=sys.stderr)
            sys.exit(1)
        if not sourcepath.is_dir():
            print(f"Cannot find source path at '{sourcepath}'", file=sys.stderr)
            sys.exit(1)

        self.ledger     = ledger.resolve()
        self.sourcepath = sourcepath.resolve()
        self.skipped    = 0
        self.succeeded  = 0
        self.failed     = 0
        self.verify     = verify
        self.gmalloc    = gmalloc
        self.python     = python
        self.failures   = []
        self.timeout    = timeout

    def run(self, command, verify=None, gmalloc=None, columns=True, now=None):
        ALLOWED_ENV_VARS = {
            'PATH', 'HOME', 'TZ', 'TZDIR', 'LANG', 'LC_ALL', 'LC_CTYPE', 'TERM',
            'TMPDIR', 'TEMP', 'TMP',
            'SYSTEMROOT', 'COMSPEC', 'MSYSTEM', 'MINGW_PREFIX',
            'ASAN_OPTIONS', 'LSAN_OPTIONS', 'UBSAN_OPTIONS', 'MSAN_OPTIONS',
            'TSAN_OPTIONS',
        }
        ALLOWED_ENV_PREFIXES = ('LLVM_PROFILE_', 'GCOV_')
        env = {k: v for k, v in os.environ.items()
               if k in ALLOWED_ENV_VARS or k.startswith(ALLOWED_ENV_PREFIXES)}
        env['TZ'] = 'America/Chicago'

        if (gmalloc is not None and gmalloc) or \
           (gmalloc is None and self.gmalloc):
            env['MallocGuardEdges']      = '1'
            env['MallocScribble']        = '1'
            env['MallocPreScribble']     = '1'
            env['MallocCheckHeapStart']  = '1000'
            env['MallocCheckHeapEach']   = '10000'
            env['DYLD_INSERT_LIBRARIES'] = '/usr/lib/libgmalloc.dylib'
            env['MALLOC_PROTECT_BEFORE'] = '1'
            env['MALLOC_FILL_SPACE']     = '1'
            env['MALLOC_STRICT_SIZE']    = '1'

        cmd = [str(self.ledger), '--args-only']
        if (verify is not None and verify) or \
           (verify is None and self.verify):
            cmd.append('--verify')
        if columns:
            cmd.append('--columns=80')
        if now:
            cmd.append(f'--now={now}')
        command = command.replace('$ledger', shlex.join(cmd))

        valgrind = '/usr/bin/valgrind'
        if not os.path.isfile(valgrind):
            valgrind = '/opt/local/bin/valgrind'

        if os.path.isfile(valgrind) and '--verify' in cmd:
            command = shlex.join([valgrind, '-q']) + ' ' + command

        ismsys2 = 'MSYSTEM' in os.environ
        if ismsys2:
            # If we are running under msys2, use bash to execute the test commands
            bash_path = os.environ['MINGW_PREFIX'] + '/../usr/bin/bash.exe'
            command = shlex.join([bash_path, '-c', command])

        # Its own session, so that a timeout can kill the shell together with
        # every process of a pipeline it started (see communicate).  It also
        # leaves the terminal's and ctest's process group, so a Ctrl-C or a
        # ctest kill of the harness no longer reaches a running command; the
        # timeout bounds how long such a command outlives the harness.
        return Popen(command, shell=not ismsys2, close_fds=not ismsys2,
                     env=env, stdin=PIPE, stdout=PIPE, stderr=PIPE,
                     cwd=self.sourcepath, start_new_session=True)

    def lines(self, data):
        """Decode DATA into lines, dropping the lines Guard Malloc adds."""
        return [line.decode('utf-8') for line in io.BytesIO(data).readlines()
                if not line.startswith(b'GuardMalloc')]

    def readlines(self, fd):
        return self.lines(fd.read())

    def communicate(self, process, input=None):
        """Send INPUT to PROCESS, then return its (stdout, stderr) bytes once
        it exits, or None if it is still running after self.timeout
        seconds; then it and every process it started are killed."""
        try:
            return process.communicate(input=input, timeout=self.timeout)
        except subprocess.TimeoutExpired:
            if os.name == 'posix':
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                # Every holder of the pipes is dead: drain them and reap.
                process.communicate()
            else:
                # No process group to kill: a surviving pipeline stage may
                # hold the pipes open, so reap without draining them.
                process.kill()
                process.wait()
            return None

    def wait(self, process, msg='Ledger invocation failed:', input=None):
        """Send INPUT to PROCESS and return its stdout bytes, or report MSG
        as a failure and return None if it fails or times out."""
        result = self.communicate(process, input)
        if result is None:
            print(f'{msg} (timed out after {self.timeout}s)')
            self.failure()
            return None
        if process.returncode != 0:
            print(msg)
            print(result[1].decode('utf-8', errors='replace'))
            self.failure()
            return None
        return result[0]

    def success(self):
        sys.stdout.write(".")
        sys.stdout.flush()
        self.succeeded += 1

    def skip(self):
        sys.stdout.write("S")
        sys.stdout.flush()
        self.skipped += 1

    def failure(self, name=None):
        sys.stdout.write("E")
        if name:
            sys.stdout.write("[%s]" % name)
            self.failures.append(name)
        sys.stdout.flush()
        self.failed += 1

    def exit(self):
        print()
        if self.succeeded > 0:
            print(f"OK ({self.succeeded})")
        if self.skipped > 0:
            print(f"SKIPPED ({self.skipped})")
        if self.failed > 0:
            print(f"FAILED ({self.failed})")
            if self.failures:
                print("Failed tests:")
                for name in self.failures:
                    print(f"  - {name}")
        print()
        sys.exit(0 if self.failed == 0 else 1)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(prog='LedgerHarness', parents=[LedgerHarness.parser()])
    args = LedgerHarness.parser().parse_args()
    harness = LedgerHarness(args.ledger, args.sourcepath, args.verify, args.gmalloc, args.python)
    proc = harness.run('$ledger -f test/input/sample.dat reg')
    print('STDOUT:')
    print(proc.stdout.read())
    print('STDERR:')
    print(proc.stderr.read())
    harness.success()
    harness.exit()
