#!/usr/bin/env python3
"""Benchmark Ledger (and, optionally, other binaries) on hledger-shaped journals.

    bench.py [--sizes 1k,10k,100k] [--commands "bal --flat,print,reg"]
             [--runs 3] [--journals DIR] [--timeout SECS] [--mem-limit-gb N]
             [--markdown] LABEL=BINARY [LABEL=BINARY ...]

For every (label, size, command) the command is run `--runs` times and the
run with the least user+sys CPU time is reported, together with its wall
time and maximum resident set size.  CPU time is the primary metric because
wall time on a shared machine mostly measures the other tenants.

Journals are expected at DIR/<size>.journal (default: ./bench-journals,
created on demand with generate_journal.py, which reproduces hledger's
tools/generatejournal.hs byte for byte, so the sizes match the tables on
https://hledger.org/PERFORMANCE.html).

Binaries whose label starts with "hledger" are run as hledger (`balance`,
`print`, `register`, no `--args-only`); everything else is run as Ledger
with `--args-only` so that ~/.ledgerrc cannot interfere.

Example:

    tools/bench/bench.py --sizes 10k,100k --markdown \\
        main=./build/ledger v3.4.1=/nix/store/...-ledger-3.4.1/bin/ledger
"""

import argparse
import os
import resource
import subprocess
import sys
import tempfile
import threading
import time

HERE = os.path.dirname(os.path.abspath(__file__))

SIZE_TXNS = {'1k': 1000, '10k': 10000, '100k': 100000, '1M': 1000000}

# Ledger command -> hledger equivalent, as benchmarked by hledger's author.
HLEDGER_COMMANDS = {'bal --flat': 'balance', 'balance --flat': 'balance',
                    'bal': 'balance', 'print': 'print', 'reg': 'register',
                    'register': 'register', 'source': 'check'}


def ensure_journal(journals_dir, size):
    path = os.path.join(journals_dir, size + '.journal')
    if not os.path.exists(path):
        if size not in SIZE_TXNS:
            sys.exit('unknown size %r (known: %s)' % (size, ', '.join(SIZE_TXNS)))
        os.makedirs(journals_dir, exist_ok=True)
        with open(path + '.tmp', 'w') as out:
            subprocess.check_call([sys.executable, os.path.join(HERE, 'generate_journal.py'),
                                   str(SIZE_TXNS[size]), '1000', '10'], stdout=out)
        os.rename(path + '.tmp', path)
    return path


def maxrss_bytes(ru):
    # ru_maxrss is bytes on macOS and kilobytes on Linux.
    return ru.ru_maxrss if sys.platform == 'darwin' else ru.ru_maxrss * 1024


def run_once(argv, timeout, mem_limit_gb):
    """Run one command; return its own wall time, CPU time and max RSS.

    wait4() reports the resource usage of exactly this child.  The tempting
    alternative, getrusage(RUSAGE_CHILDREN), accumulates CPU time over all
    children and keeps the largest RSS of any child reaped so far, which
    would make every row after a 4 GB run report 4 GB.
    """
    def limit():
        if mem_limit_gb and sys.platform != 'darwin':
            lim = int(mem_limit_gb * 1024 ** 3)
            resource.setrlimit(resource.RLIMIT_AS, (lim, lim))

    with tempfile.TemporaryFile() as err:
        t0 = time.perf_counter()
        proc = subprocess.Popen(argv, stdout=subprocess.DEVNULL, stderr=err, preexec_fn=limit)
        watchdog = threading.Timer(timeout, proc.kill)
        watchdog.start()
        try:
            _, wait_status, ru = os.wait4(proc.pid, 0)
        finally:
            timed_out = not watchdog.is_alive()
            watchdog.cancel()
        wall = time.perf_counter() - t0
        proc.returncode = os.waitstatus_to_exitcode(wait_status)  # mark the child as reaped
        err.seek(0)
        err_lines = err.read().decode(errors='replace').strip().splitlines()

    status = 'timeout' if timed_out else proc.returncode
    return {
        'status': status,
        'error': err_lines[-1] if err_lines else '',
        'wall': wall,
        'user': ru.ru_utime,
        'sys': ru.ru_stime,
        'maxrss': maxrss_bytes(ru),
    }


def measure(argv, runs, timeout, mem_limit_gb):
    best = None
    for _ in range(runs):
        r = run_once(argv, timeout, mem_limit_gb)
        if r['status'] != 0:
            return r
        if best is None or r['user'] + r['sys'] < best['user'] + best['sys']:
            best = r
    return best


def fmt_row(label, size, command, r):
    if r['status'] != 0:
        what = 'timeout' if r['status'] == 'timeout' else 'exit %s %s' % (r['status'], r['error'])
        return (label, size, command, 'FAILED: ' + what, '', '', '')
    return (label, size, command, '%.2f' % r['user'], '%.2f' % r['sys'], '%.2f' % r['wall'],
            '%.0f' % (r['maxrss'] / 2 ** 20))


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--sizes', default='1k,10k,100k')
    ap.add_argument('--commands', default='source,bal --flat,print,reg')
    ap.add_argument('--runs', type=int, default=3)
    ap.add_argument('--journals', default=os.path.join(os.getcwd(), 'bench-journals'))
    ap.add_argument('--timeout', type=float, default=900)
    ap.add_argument('--mem-limit-gb', type=float, default=0,
                    help='RLIMIT_AS for each run (Linux only); quadratic cases then fail fast')
    ap.add_argument('--markdown', action='store_true')
    ap.add_argument('binaries', nargs='+', metavar='LABEL=BINARY')
    args = ap.parse_args()

    binaries = []
    for spec in args.binaries:
        if '=' not in spec:
            sys.exit('expected LABEL=BINARY, got %r' % spec)
        binaries.append(tuple(spec.split('=', 1)))

    sizes = [s.strip() for s in args.sizes.split(',') if s.strip()]
    commands = [c.strip() for c in args.commands.split(',') if c.strip()]

    rows = []
    for size in sizes:
        journal = ensure_journal(args.journals, size)
        for command in commands:
            for label, binary in binaries:
                if label.startswith('hledger'):
                    hcmd = HLEDGER_COMMANDS.get(command) or command
                    argv = [binary, '-f', journal] + hcmd.split()
                else:
                    argv = [binary, '--args-only', '-f', journal] + command.split()
                r = measure(argv, args.runs, args.timeout, args.mem_limit_gb)
                row = fmt_row(label, size, command, r)
                rows.append(row)
                print('\t'.join(row), file=sys.stderr, flush=True)

    header = ('tool', 'size', 'command', 'user_s', 'sys_s', 'wall_s', 'maxrss_MB')
    if args.markdown:
        print('| ' + ' | '.join(header) + ' |')
        print('|' + '---|' * len(header))
        for row in rows:
            print('| ' + ' | '.join(row) + ' |')
    else:
        widths = [max(len(str(x)) for x in col) for col in zip(header, *rows)]
        for row in (header,) + tuple(rows):
            print('  '.join(str(x).ljust(w) for x, w in zip(row, widths)).rstrip())


if __name__ == '__main__':
    main()
