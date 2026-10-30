#!/usr/bin/env bash
# Differential output test: run two ledger binaries over a matrix of reports
# on the given journals and report every command whose output differs.
#
#     tools/bench/difftest.sh REFERENCE_LEDGER CANDIDATE_LEDGER JOURNAL...
#
# Byte-identical output is the acceptance criterion for a performance change,
# so this is run with the pre-change binary as the reference after every
# optimisation, on the hledger-shaped journals and on test/input/*.dat.
# Exit status is the number of differing commands.  Set DIFFTEST_SHOW=1 to
# print the first differing lines of each mismatch.

set -u

if [ $# -lt 3 ]; then
    sed -n '2,12p' "$0"
    exit 2
fi

ref=$1
cand=$2
shift 2

commands=(
    "bal"
    "bal --flat"
    "bal --flat --no-total"
    "bal --lots"
    "bal --lot-prices"
    "bal -B"
    "bal -V"
    "bal -X B"
    "bal -H"
    "bal --depth 2"
    "bal -M"
    "reg"
    "reg --lots"
    "reg -B"
    "reg -V"
    "reg -M"
    "reg --sort total"
    "reg --format %(top_amount(total))\n"
    "print"
    "print --lots"
    "csv"
    "xml"
    "equity"
    "stats --now 2030/06/15"
    "prices"
    "pricedb"
    "commodities"
    "accounts"
)

# XML ids and refs are object addresses and differ between any two runs;
# the test suite masks them the same way.
mask() { sed -E 's/ (id|ref)="[0-9a-f]+"/ \1="XXXX"/g'; }

failures=0
total=0
for journal in "$@"; do
    for cmd in "${commands[@]}"; do
        total=$((total + 1))
        # shellcheck disable=SC2086  # the command string is meant to be split
        ref_out=$("$ref" --args-only -f "$journal" $cmd 2>&1 | mask; echo "exit=${PIPESTATUS[0]}")
        # shellcheck disable=SC2086
        cand_out=$("$cand" --args-only -f "$journal" $cmd 2>&1 | mask; echo "exit=${PIPESTATUS[0]}")
        if [ "$ref_out" != "$cand_out" ]; then
            failures=$((failures + 1))
            printf 'DIFF  %-28s %s\n' "$cmd" "$journal"
            if [ "${DIFFTEST_SHOW:-0}" = 1 ]; then
                diff <(printf '%s\n' "$ref_out") <(printf '%s\n' "$cand_out") | head -12
            fi
        fi
    done
done

printf '%d of %d commands differ\n' "$failures" "$total"
exit "$failures"
