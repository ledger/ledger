#!/usr/bin/env python3

import argparse
import pathlib
import shlex
import sys
import os
import re

from subprocess import Popen, PIPE

import types
if sys.version_info.major == 2:
    import copy_reg as copyreg
else:
    import copyreg

def _pickle_method(method):
    func_name = method.im_func.__name__
    obj = method.im_self
    cls = method.im_class
    return _unpickle_method, (func_name, obj, cls)

def _unpickle_method(func_name, obj, cls):
    for cls in cls.mro():
        try:
            func = cls.__dict__[func_name]
        except KeyError:
            pass
        else:
            break
    return func.__get__(obj, cls)

copyreg.pickle(types.MethodType, _pickle_method, _unpickle_method)

class LedgerHarness:
    ledger     = None
    sourcepath = None
    skipped    = 0
    succeeded  = 0
    failed     = 0
    verify     = False
    gmalloc    = False
    python     = False

    @staticmethod
    def parser():
      parser = argparse.ArgumentParser(add_help=False)
      parser.add_argument('-l', '--ledger', type=pathlib.Path, required=True)
      parser.add_argument('-s', '--sourcepath', type=pathlib.Path, required=True)
      parser.add_argument('--verify', action='store_true')
      parser.add_argument('--gmalloc', action='store_true')
      parser.add_argument('--python', action='store_true')
      return parser

    def __init__(self, ledger, sourcepath, verify=False, gmalloc=False, python=False):
        if not ledger.is_file():
            print(f"Cannot find ledger at '{ledger}'", file=sys.stderr)
            sys.exit(1)
        if not sourcepath.is_dir():
            print(f"Cannot find source path at '{sourcepath}'", file=sys.stderr)
            sys.exit(1)

        self.ledger     = ledger.resolve()
        self.sourcepath = sourcepath.resolve()
        self.verify     = verify
        self.gmalloc    = gmalloc
        self.python     = python

    def run(self, command, verify=None, gmalloc=None, columns=True):
        env = os.environ.copy()

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
        command = command.replace('$ledger', shlex.join(cmd))

        valgrind = '/usr/bin/valgrind'
        if not os.path.isfile(valgrind):
            valgrind = '/opt/local/bin/valgrind'

        if os.path.isfile(valgrind) and '--verify' in cmd:
            command = shlex.join([valgrind, '-q', command])

        ismsys2 = 'MSYSTEM' in os.environ
        if ismsys2:
            # If we are running under msys2, use bash to execute the test commands
            bash_path = os.environ['MINGW_PREFIX'] + '/../usr/bin/bash.exe'
            command = shlex.join([bash_path, '-c', command])

        return Popen(command, shell=not ismsys2, close_fds=not ismsys2,
                     env=env, stdin=PIPE, stdout=PIPE, stderr=PIPE,
                     cwd=self.sourcepath)

    def read(self, fd):
        text = ""
        text_data = os.read(fd.fileno(), 8192)
        while text_data:
            if text_data:
                text += text_data
            text_data = os.read(fd.fileno(), 8192)
        return text

    def readlines(self, fd):
        lines = []
        for line in fd.readlines():
            line = line.decode('utf-8')
            if not line.startswith('GuardMalloc'):
                lines.append(line)
        return lines

    def wait(self, process, msg='Ledger invocation failed:'):
        if process.wait() != 0:
            print(msg)
            print(process.stderr.read())
            self.failure()
            return False
        return True

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
        print()

        sys.exit(self.failed)

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
