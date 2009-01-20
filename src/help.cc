/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "help.h"

namespace ledger {

void help(std::ostream& out)
{
  out << "usage: ledger [options] COMMAND [ACCT REGEX]... [-- [PAYEE REGEX]...]\n\n\
Use -H to see all the help text on one page, or:\n\
      --help-calc        calculation options\n\
      --help-disp        display options\n\
      --help-comm        commodity options\n\n\
Basic options:\n\
  -h, --help             display this help text\n\
  -v, --version          show version information\n\
  -f, --file FILE        read ledger data from FILE\n\
  -o, --output FILE      write output to FILE\n\
  -i, --init-file FILE   initialize ledger using FILE (default: ~/.ledgerrc)\n\
      --cache FILE       use FILE as a binary cache when --file is not used\n\
      --no-cache         don't use a cache, even if it would be appropriate\n\
  -a, --account NAME     use NAME for the default account (useful with QIF)\n\n\
Commands:\n\
  balance  [REGEXP]...   show balance totals for matching accounts\n\
  register [REGEXP]...   show register of matching transactions\n\
  print    [REGEXP]...   print all matching entries\n\
  xml      [REGEXP]...   print matching entries in XML format\n\
  equity   [REGEXP]...   output equity entries for matching accounts\n\
  prices   [REGEXP]...   display price history for matching commodities\n\
  entry DATE PAYEE AMT   output a derived entry, based on the arguments\n";
}

void calc_help(std::ostream& out)
{
  out << "Options to control how a report is calculated:\n\
  -c, --current          show only current and past entries (not future)\n\
  -b, --begin DATE       set report begin date\n\
  -e, --end DATE         set report end date\n\
  -p, --period STR       report using the given period\n\
      --period-sort EXPR sort each report period's entries by EXPR\n\
  -C, --cleared          consider only cleared transactions\n\
  -U, --uncleared        consider only uncleared transactions\n\
  -R, --real             consider only real (non-virtual) transactions\n\
  -L, --actual           consider only actual (non-automated) transactions\n\
  -r, --related          calculate report using related transactions\n\
      --budget           generate budget entries based on periodic entries\n\
      --add-budget       show all transactions plus the budget\n\
      --unbudgeted       show only unbudgeted transactions\n\
      --forecast EXPR    generate forecast entries while EXPR is true\n\
  -l, --limit EXPR       calculate only transactions matching EXPR\n\
  -t, --amount EXPR      use EXPR to calculate the displayed amount\n\
  -T, --total EXPR       use EXPR to calculate the displayed total\n";
}

void disp_help(std::ostream& out)
{
  out << "Output to control how report results are displayed:\n\
  -n, --collapse         register: collapse entries; balance: no grand total\n\
  -s, --subtotal         balance: show sub-accounts; other: show subtotals\n\
  -P, --by-payee         show summarized totals by payee\n\
  -x, --comm-as-payee    set commodity name as the payee, for reporting\n\
  -E, --empty            balance: show accounts with zero balance\n\
  -W, --weekly           show weekly sub-totals\n\
  -M, --monthly          show monthly sub-totals\n\
  -Y, --yearly           show yearly sub-totals\n\
      --dow              show a days-of-the-week report\n\
  -S, --sort EXPR        sort report according to the value expression EXPR\n\
  -w, --wide             for the default register report, use 132 columns\n\
      --head COUNT       show only the first COUNT entries (negative inverts)\n\
      --tail COUNT       show only the last COUNT entries (negative inverts)\n\
      --pager PAGER      send all output through the given PAGER program\n\
  -A, --average          report average transaction amount\n\
  -D, --deviation        report deviation from the average\n\
  -%, --percentage       report balance totals as a percentile of the parent\n\
      --totals           in the \"xml\" report, include running total\n\
  -j, --amount-data      print only raw amount data (useful for scripting)\n\
  -J, --total-data       print only raw total data\n\
  -d, --display EXPR     display only transactions matching EXPR\n\
  -y, --date-format STR  use STR as the date format (default: %Y/%m/%d)\n\
  -F, --format STR       use STR as the format; for each report type, use:\n\
      --balance-format      --register-format       --print-format\n\
      --plot-amount-format  --plot-total-format     --equity-format\n\
      --prices-format       --wide-register-format\n";
}

void comm_help(std::ostream& out)
{
  out << "Options to control how commodity values are determined:\n\
      --price-db FILE    sets the price database to FILE (def: ~/.pricedb)\n\
  -Z, --price-exp MINS   download quotes only if newer than MINS (def: 1440)\n\
  -Q, --download         download price information when needed\n\
  -O, --quantity         report commodity totals (this is the default)\n\
  -B, --basis            report cost basis of commodities\n\
  -V, --market           report last known market value\n\
  -g, --performance      report gain/loss for each displayed transaction\n\
  -G, --gain             report net gain/loss\n";
}

void full_help(std::ostream& out)
{
  out << "usage: ledger [options] COMMAND [ACCT REGEX]... [-- [PAYEE REGEX]...]\n\n\
Basic options:\n\
  -H, --full-help        display this help text\n\
  -h, --help             display summarized help text\n\
  -v, --version          show version information\n\
  -f, --file FILE        read ledger data from FILE\n\
  -o, --output FILE      write output to FILE\n\
  -i, --init-file FILE   initialize ledger using FILE (default: ~/.ledgerrc)\n\
      --cache FILE       use FILE as a binary cache when --file is not used\n\
      --no-cache         don't use a cache, even if it would be appropriate\n\
  -a, --account NAME     use NAME for the default account (useful with QIF)\n\n\
Report filtering:\n\
  -c, --current          show only current and past entries (not future)\n\
  -b, --begin DATE       set report begin date\n\
  -e, --end DATE         set report end date\n\
  -p, --period STR       report using the given period\n\
      --period-sort EXPR sort each report period's entries by EXPR\n\
  -C, --cleared          consider only cleared transactions\n\
  -U, --uncleared        consider only uncleared transactions\n\
  -R, --real             consider only real (non-virtual) transactions\n\
  -L, --actual           consider only actual (non-automated) transactions\n\
  -r, --related          calculate report using related transactions\n\
      --budget           generate budget entries based on periodic entries\n\
      --add-budget       show all transactions plus the budget\n\
      --unbudgeted       show only unbudgeted transactions\n\
      --forecast EXPR    generate forecast entries while EXPR is true\n\
  -l, --limit EXPR       calculate only transactions matching EXPR\n\
  -t, --amount EXPR      use EXPR to calculate the displayed amount\n\
  -T, --total EXPR       use EXPR to calculate the displayed total\n\n\
Output customization:\n\
  -n, --collapse         register: collapse entries; balance: no grand total\n\
  -s, --subtotal         balance: show sub-accounts; other: show subtotals\n\
  -P, --by-payee         show summarized totals by payee\n\
  -x, --comm-as-payee    set commodity name as the payee, for reporting\n\
  -E, --empty            balance: show accounts with zero balance\n\
  -W, --weekly           show weekly sub-totals\n\
  -M, --monthly          show monthly sub-totals\n\
  -Y, --yearly           show yearly sub-totals\n\
      --dow              show a days-of-the-week report\n\
  -S, --sort EXPR        sort report according to the value expression EXPR\n\
  -w, --wide             for the default register report, use 132 columns\n\
      --head COUNT       show only the first COUNT entries (negative inverts)\n\
      --tail COUNT       show only the last COUNT entries (negative inverts)\n\
      --pager PAGER      send all output through the given PAGER program\n\
  -A, --average          report average transaction amount\n\
  -D, --deviation        report deviation from the average\n\
  -%, --percentage       report balance totals as a percentile of the parent\n\
      --totals           in the \"xml\" report, include running total\n\
  -j, --amount-data      print only raw amount data (useful for scripting)\n\
  -J, --total-data       print only raw total data\n\
  -d, --display EXPR     display only transactions matching EXPR\n\
  -y, --date-format STR  use STR as the date format (default: %Y/%m/%d)\n\
  -F, --format STR       use STR as the format; for each report type, use:\n\
      --balance-format      --register-format       --print-format\n\
      --plot-amount-format  --plot-total-format     --equity-format\n\
      --prices-format       --wide-register-format\n\n\
Commodity reporting:\n\
      --price-db FILE    sets the price database to FILE (def: ~/.pricedb)\n\
  -L, --price-exp MINS   download quotes only if newer than MINS (def: 1440)\n\
  -Q, --download         download price information when needed\n\
  -O, --quantity         report commodity totals (this is the default)\n\
  -B, --basis            report cost basis of commodities\n\
  -V, --market           report last known market value\n\
  -g, --performance      report gain/loss for each displayed transaction\n\
  -G, --gain             report net gain/loss\n\n\
Commands:\n\
  balance  [REGEXP]...   show balance totals for matching accounts\n\
  register [REGEXP]...   show register of matching transactions\n\
  print    [REGEXP]...   print all matching entries\n\
  xml      [REGEXP]...   print matching entries in XML format\n\
  equity   [REGEXP]...   output equity entries for matching accounts\n\
  prices   [REGEXP]...   display price history for matching commodities\n\
  entry DATE PAYEE AMT   output a derived entry, based on the arguments\n";
}

} // namespace ledger
