#!/usr/bin/env python3
"""Generate hledger's synthetic benchmark journals, byte for byte.

This is a port of hledger's tools/generatejournal.hs (the generator behind the
journals on https://hledger.org/PERFORMANCE.html), so that Ledger's benchmark
inputs can be reproduced without a Haskell toolchain and compared with
hledger's published numbers.

    generate_journal.py NUMTXNS NUMACCTS ACCTDEPTH [--start=YYYY-MM-DD] [--days=N]

Prints NUMTXNS transactions, one per day from 2000-01-01 (or --start), using
NUMACCTS account names nested up to ACCTDEPTH levels, followed by NUMTXNS `P`
market-price directives.  --days=N spreads the transactions evenly over N days
instead of one per day.  The --chinese/--mixed account-name variants of the
original are not implemented.

Journal shape (all of it matters for Ledger's performance):

- transaction n has amount n, in commodity A..Z cycling with n;
- n % 3 == 1 buys at a per-unit cost (`1 A @ 0.71 B`), n % 3 == 2 at a total
  cost (`2 B @@ 2 C`), n % 3 == 0 has no cost, so two thirds of the postings
  become lots in Ledger;
- the P directives are all for A in B, one per day, walking a triangle wave of
  rates between 0.70 and 1.30.
"""

import sys
from datetime import date, timedelta
from decimal import Decimal


def show_decimal(d):
    """Mimic Haskell's `show` for Data.Decimal values.

    Every Decimal the generator builds comes out of fromRational or (*), both
    of which normalise away trailing fractional zeros, so 0.70 prints as 0.8
    after adding 0.1, and -10 * 0.8 prints as -8.
    """
    return format(d.normalize(), 'f')


def hex_names():
    n = 1
    while True:
        yield format(n, 'x')
        n += 1


def unique_account_names(depth):
    """Chunks of `depth` fresh hex names, each chunk nested ("a", "a:b", "a:b:c")."""
    names = hex_names()
    while True:
        chunk = [next(names) for _ in range(depth)]
        prefix = ''
        for name in chunk:
            prefix = name if not prefix else prefix + ':' + name
            yield prefix


def pairs(seq):
    seq = list(seq)
    out = []
    i = 0
    while i < len(seq):
        if i + 1 < len(seq):
            out.append((seq[i], seq[i + 1]))
        else:
            out.append((seq[i], seq[i]))
        i += 2
    return out


def show_txn(txnno, day, acct1, acct2, comm, pricecomm):
    amt = txnno
    rate = Decimal('0.70') + Decimal('0.01') * (txnno % 60)
    if txnno % 3 == 0:
        amt2, amt2comm, pricesymbol = show_decimal(Decimal(-amt)), comm, ''
    elif txnno % 3 == 1:
        amt2 = show_decimal(Decimal(-amt) * rate)
        amt2comm, pricesymbol = pricecomm, ' @ %s %s' % (show_decimal(rate), pricecomm)
    else:
        amt2 = show_decimal(Decimal(-amt))
        amt2comm, pricesymbol = pricecomm, ' @@ %s %s' % (amt, pricecomm)
    return '%s transaction %d\n  %-40s  %2d %s%s\n  %-40s  %s %s\n\n' % (
        day.isoformat(), txnno, acct1, amt, comm, pricesymbol, acct2, amt2, amt2comm)


def rates_wave():
    # Haskell: rates = [0.70, 0.71 .. 1.3] :: [Double]; printed with %.2f.
    up = [0.70 + 0.01 * i for i in range(61)]
    wave = up + list(reversed(up))[1:-1]
    while True:
        for r in wave:
            yield r


def main(argv):
    opts = [a for a in argv if a.startswith('-')]
    args = [a for a in argv if not a.startswith('-')]
    if len(args) != 3:
        sys.exit(__doc__)
    numtxns, numaccts, acctdepth = (int(a) for a in args)

    start = date(2000, 1, 1)
    days = None
    for o in opts:
        if o.startswith('--start='):
            start = date.fromisoformat(o[len('--start='):])
        elif o.startswith('--days='):
            days = int(o[len('--days='):])

    if days is None:
        dates = [start + timedelta(days=i) for i in range(numtxns)]
    else:
        dates = [start + timedelta(days=(i * days) // numtxns) for i in range(numtxns)]

    names = unique_account_names(acctdepth)
    accts = [next(names) for _ in range(numaccts)]
    acct_pairs = pairs(accts)
    comms = [chr(ord('A') + i) for i in range(26)]

    out = sys.stdout
    for i in range(numtxns):
        n = i + 1
        a, b = acct_pairs[i % len(acct_pairs)]
        out.write(show_txn(n, dates[i], a, b, comms[i % 26], comms[(i + 1) % 26]))
    for d, rate in zip(dates, rates_wave()):
        out.write('P %s A  %.2f B\n' % (d.isoformat(), rate))


if __name__ == '__main__':
    main(sys.argv[1:])
