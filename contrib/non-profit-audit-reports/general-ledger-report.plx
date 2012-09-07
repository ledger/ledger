#!/usr/bin/perl
# general-ledger-report.plx                                    -*- Perl -*-
#
#    Script to generate a General Ledger report that accountants like
#    using Ledger.
#
# Copyright (C) 2011, Bradley M. Kuhn
#
# This program gives you software freedom; you can copy, modify, convey,
# and/or redistribute it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program in a file called 'GPLv3'.  If not, write to the:
#    Free Software Foundation, Inc., 51 Franklin St, Fifth Floor
#                                    Boston, MA 02110-1301, USA.

use strict;
use warnings;

use Math::BigFloat;
use Date::Manip;

my $LEDGER_CMD = "/usr/bin/ledger";

my $ACCT_WIDTH = 75;

sub ParseNumber($) {
  $_[0] =~ s/,//g;
  return Math::BigFloat->new($_[0]);
}

Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");

if (@ARGV < 2) {
  print STDERR "usage: $0 <BEGIN_DATE> <END_DATE> <OTHER_LEDGER_OPTS>\n";
  exit 1;
}

my($beginDate, $endDate, @otherLedgerOpts) = @ARGV;

my(@chartOfAccountsOpts) = ('--wide-register-format', "%150A\n",  '-w', '-s',
                            '-b', $beginDate, '-e', $endDate, @otherLedgerOpts, 'reg');

open(CHART_DATA, "-|", $LEDGER_CMD, @chartOfAccountsOpts)
  or die "Unable to run $LEDGER_CMD @chartOfAccountsOpts: $!";

open(CHART_OUTPUT, ">", "chart-of-accounts.txt") or die "unable to write chart-of-accounts.txt: $!";

my @accounts;
while (my $line = <CHART_DATA>) {
  chomp $line;
  $line =~ s/^\s*//;   $line =~ s/\s*$//;
  push(@accounts, $line);

}
close(CHART_DATA); die "error reading ledger output for chart of accounts: $!" unless $? == 0;

open(CHART_OUTPUT, ">", "chart-of-accounts.txt") or die "unable to write chart-of-accounts.txt: $!";

my @sortedAccounts;
foreach my $acct (
                  # Proper sorting for a chart of accounts
                  sort {
                    if ($a =~ /^Assets/ and $b !~ /^Assets/) {
                      return -1;
                    } elsif ($a =~ /^Liabilities/ and $b !~ /^Liabilitie/) {
                      return -1;
                    } else {
                      return $a cmp $b;
                    }
                    } @accounts) {
  print CHART_OUTPUT "$acct\n";
  push(@sortedAccounts, $acct);
}
close(CHART_OUTPUT); die "error writing to chart-of-accounts.txt: $!" unless $? == 0;

my $formattedEndDate = new Date::Manip::Date;
die "badly formatted end date, $endDate" if $formattedEndDate->parse($endDate);
my $oneDayLess = new Date::Manip::Delta;
die "bad one day less" if $oneDayLess->parse("- 1 day");
$formattedEndDate = $formattedEndDate->calc($oneDayLess);
$formattedEndDate = $formattedEndDate->printf("%Y/%m/%d");

open(GL_TEXT_OUT, ">", "general-ledger.txt") or die "unable to write general-ledger.txt: $!";
open(GL_CSV_OUT, ">", "general-ledger.csv") or die "unable to write general-ledger.csv: $!";

foreach my $acct (@sortedAccounts) {
  print GL_TEXT_OUT "\n\nACCOUNT: $acct\nFROM:    $beginDate TO $formattedEndDate\n\n";
  my @acctLedgerOpts = ('--wide-register-format',
                        "%D  %-.10C   %-.80P  %-.80N  %18t  %18T\n", '-w', '--sort', 'd',
                        '-b', $beginDate, '-e', $endDate, @otherLedgerOpts, 'reg', $acct);
  open(GL_TEXT_DATA, "-|", $LEDGER_CMD, @acctLedgerOpts)
    or die "Unable to run $LEDGER_CMD @acctLedgerOpts: $!";

  foreach my $line (<GL_TEXT_DATA>) {
    print GL_TEXT_OUT $line;
  }
  close(GL_TEXT_DATA); die "error reading ledger output for chart of accounts: $!" unless $? == 0;

  print GL_CSV_OUT "\n\"ACCOUNT:\",\"$acct\"\n\"PERIOD START:\",\"$beginDate\"\n\"PERIOD END:\",\"$formattedEndDate\"\n";
  print GL_CSV_OUT '"DATE","CHECK NUM","NAME","MEMO","TRANSACTION AMT","RUNNING TOTAL"', "\n";
  @acctLedgerOpts = ('--wide-register-format',
                     '"%D","%C","%P","%N","%t","%T"\n', '-w', '--sort', 'd',
                        '-b', $beginDate, '-e', $endDate, @otherLedgerOpts, 'reg', $acct);
  open(GL_CSV_DATA, "-|", $LEDGER_CMD, @acctLedgerOpts)
    or die "Unable to run $LEDGER_CMD @acctLedgerOpts: $!";

  foreach my $line (<GL_CSV_DATA>) {
    print GL_CSV_OUT $line;
  }
  close(GL_CSV_DATA); die "error reading ledger output for chart of accounts: $!" unless $? == 0;
}
close(GL_TEXT_OUT); die "error writing to general-ledger.txt: $!" unless $? == 0;
close(GL_CSV_OUT); die "error writing to general-ledger.csv: $!" unless $? == 0;
###############################################################################
#
# Local variables:
# compile-command: "perl -c general-ledger-report.plx"
# End:

