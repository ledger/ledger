# Benchmarking and profiling Ledger

This directory holds the tools behind Ledger's comparison with hledger
(https://hledger.org/PERFORMANCE.html#other-apps) and the profiling
recipes used to find and fix the slow paths.  `benchmark/` next door is the
older PGO-oriented workflow that CI's `perf.yml` drives; the two will be
merged once this one has settled.

## Journals

`generate_journal.py` is a byte-for-byte port of hledger's
`tools/generatejournal.hs`, so the inputs are the same files the hledger
tables were produced from:

    tools/bench/generate_journal.py 100000 1000 10 > 100k.journal

One transaction per day from 2000-01-01, 26 commodities, 1000 accounts
nested to depth 10, two thirds of the postings carrying a cost (`@` or
`@@`), and one `P` directive per transaction.  Every costed posting is a
distinct lot in Ledger, which is what made these journals pathological for
3.4.1.

## Checking that output is unchanged

    tools/bench/difftest.sh ./build-before/ledger ./build/ledger bench-journals/10k.journal test/input/*.dat

runs both binaries over a matrix of reports (balance in several modes,
register, print, csv, xml, equity, prices, with lots, basis, market value
and periods) and lists every command whose output or exit status differs.
Byte-identical output is the acceptance criterion for a performance
change, so run it, with the pre-change binary as the reference, before
measuring anything.

## Measuring

    tools/bench/bench.py --sizes 1k,10k,100k --markdown \
        main=./build/ledger v3.4.1=$(nix build --no-link --print-out-paths nixpkgs#ledger)/bin/ledger \
        hledger=$(nix build --no-link --print-out-paths nixpkgs#hledger)/bin/hledger

reports the best user CPU time of `--runs` runs, the wall time and the
maximum RSS of every (binary, size, command).  Compare CPU time, not wall
time, on a shared machine.  `--mem-limit-gb N` (Linux) makes quadratic
cases fail fast instead of swapping.  Labels starting with `hledger` run
the hledger equivalents (`balance`, `print`, `register`).  Journals are
generated on demand into `--journals` (default `./bench-journals`).

Always pass `--args-only` when timing Ledger by hand so that `~/.ledgerrc`
cannot add files or options; `bench.py` does.

## Profiling builds

    cmake -B build-prof $cmakeFlags -DCMAKE_BUILD_TYPE=RelWithDebInfo \
          -DCMAKE_CXX_FLAGS=-fno-omit-frame-pointer

(`$cmakeFlags` comes from the dev shell and selects a Debug build; the
options after it override it, so keep that order.)

`-O2 -g` with frame pointers gives usable call graphs on both platforms
without changing the inlining decisions much relative to Release.  The code
lives in `libledger`, so make sure the profiler sees its symbols (both
`sample` and `perf` do when the library is not stripped).

The `perf` dev shell (`nix develop .#perf`) adds the tools below to the
default shell.

## Recipes

### macOS

`sample` ships with the OS and needs no setup:

    ./build-prof/ledger --args-only -f 100k.journal reg > /dev/null & pid=$!
    sleep 2; sample $pid 10 -mayDie -file reg.sample.txt; wait

The call graph in `reg.sample.txt` attributes wall time (including time
the thread was descheduled), which is fine for finding hot paths.  For
precise CPU time use Instruments from the command line:

    xcrun xctrace record --template 'Time Profiler' --output reg.trace \
        --launch -- ./build-prof/ledger --args-only -f 100k.journal reg

and open `reg.trace` in Instruments, or export with
`xctrace export --input reg.trace --xpath '/trace-toc/run[@number="1"]/data/table[@schema="time-profile"]'`.
The Allocations template (`--template Allocations`) is the memory
equivalent.  `samply record ./build-prof/ledger ...` records a profile and
opens it in the Firefox Profiler.

### Linux

    perf record -F 499 --call-graph fp -o reg.data ./build-prof/ledger --args-only -f 100k.journal reg > /dev/null
    perf report -i reg.data --stdio --no-children --percent-limit 0.5   # self time
    perf report -i reg.data --stdio --children -g none                   # inclusive
    perf script -i reg.data | inferno-collapse-perf | inferno-flamegraph > reg.svg

`--call-graph fp` relies on the frame pointers of the profiling build;
`--call-graph dwarf` works on a plain Release build at a higher recording
cost.  `kernel.perf_event_paranoid` up to 2 (the NixOS default) allows
sampling your own processes.  For allocations:

    heaptrack ./build-prof/ledger --args-only -f 10k.journal reg > /dev/null
    heaptrack --analyze heaptrack.ledger.*.zst | head -60

`valgrind --tool=callgrind` gives exact instruction counts when a sampling
profile is too noisy (expect a 50x slowdown), and `valgrind --tool=dhat`
reports allocation sizes and lifetimes.

### Either

    hyperfine -N -w 1 -r 5 './build/ledger --args-only -f 10k.journal bal --flat'

for wall-clock A/B on an idle machine; `bench.py` for CPU time and RSS.
On a loaded machine prefer instruction counts: `/usr/bin/time -l` prints
"instructions retired" on macOS, and `perf stat -e instructions:u` on
Linux; they barely move with load while wall time and even CPU time do.
Ledger's own phase timers are printed with `--trace 5` (parsing,
per-report phases).
