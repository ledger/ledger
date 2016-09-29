#!/usr/bin/env python

import sys
import os
import re

from subprocess import Popen, PIPE

import copy_reg
import types

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

copy_reg.pickle(types.MethodType, _pickle_method, _unpickle_method)

class LedgerHarness:
    ledger     = None
    sourcepath = None
    succeeded  = 0
    failed     = 0
    verify     = False
    gmalloc    = False
    python     = False

    def __init__(self, argv):
        if not os.path.isfile(argv[1]):
            print "Cannot find ledger at '%s'" % argv[1]
            sys.exit(1)
        if not os.path.isdir(argv[2]):
            print "Cannot find source path at '%s'" % argv[2]
            sys.exit(1)

        self.ledger     = os.path.abspath(argv[1])
        self.sourcepath = os.path.abspath(argv[2])
        self.succeeded  = 0
        self.failed     = 0
        self.verify     = '--verify' in argv
        self.gmalloc    = '--gmalloc' in argv
        self.python     = '--python' in argv

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

        if (verify is not None and verify) or \
           (verify is None and self.verify):
            insert = ' --verify'
        else:
            insert = ''

        if columns:
            insert += ' --columns=80'

        command = re.sub('\$ledger', '%s%s %s' % \
                         (self.ledger, insert, '--args-only'), command)

        valgrind = '/usr/bin/valgrind'
        if not os.path.isfile(valgrind):
            valgrind = '/opt/local/bin/valgrind'

        if os.path.isfile(valgrind) and '--verify' in insert:
            command = valgrind + ' -q ' + command

        # If we are running under msys2, use bash to execute the test commands
        if 'MSYSTEM' in os.environ:
            bash_path = os.environ['MINGW_PREFIX'] + '/../usr/bin/bash.exe'
            return Popen([bash_path, '-c', command], shell=False,
                         close_fds=False, env=env, stdin=PIPE, stdout=PIPE,
                         stderr=PIPE, cwd=self.sourcepath)

        return Popen(command, shell=True, close_fds=True, env=env,
                     stdin=PIPE, stdout=PIPE, stderr=PIPE, 
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
            if not line.startswith("GuardMalloc"):
                lines.append(line)
        return lines

    def wait(self, process, msg='Ledger invocation failed:'):
        if process.wait() != 0:
            print msg
            print process.stderr.read()
            self.failure()
            return False
        return True

    def success(self):
        sys.stdout.write(".")
        sys.stdout.flush()
        self.succeeded += 1

    def failure(self, name=None):
        sys.stdout.write("E")
        if name:
            sys.stdout.write("[%s]" % name)
        sys.stdout.flush()
        self.failed += 1

    def exit(self):
        print
        if self.succeeded > 0:
            print "OK (%d) " % self.succeeded,
        if self.failed > 0:
            print "FAILED (%d)" % self.failed,
        print

        sys.exit(self.failed)

if __name__ == '__main__':
    harness = LedgerHarness(sys.argv)
    proc = harness.run('$ledger -f doc/sample.dat reg')
    print 'STDOUT:'
    print proc.stdout.read()
    print 'STDERR:'
    print proc.stderr.read()
    harness.success()
    harness.exit()
