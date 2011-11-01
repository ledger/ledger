#!/usr/bin/perl
# fund-report.plx                                    -*- Perl -*-
#
#    Script to generate a Trial Balance report for a ledger.
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

my $LEDGER_CMD = "/usr/bin/ledger";

my $ACCT_WIDTH = 70;

sub ParseNumber($) {
  $_[0] =~ s/,//g;
  return Math::BigFloat->new($_[0]);
}
Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");

if (@ARGV < 2) {
  print STDERR "usage: $0 <START_DATE> <END_DATE> <LEDGER_OPTIONS>\n";
  exit 1;
}

my($startDate, $endDate, @mainLedgerOptions) = @ARGV;

# First, get fund list from ending balance
my(@ledgerOptions) = (@mainLedgerOptions,
                      '--wide-register-format', "%-.70A %22.108t\n",  '-w', '-s',
                      '-e', $endDate, 'reg', '^Funds:Restricted:');


my %funds;

open(LEDGER_FUNDS, "-|", $LEDGER_CMD, @ledgerOptions)
  or die "Unable to run $LEDGER_CMD for funds: $!";

while (my $fundLine = <LEDGER_FUNDS>) {
  die "Unable to parse output line from funds command: $fundLine"
    unless $fundLine =~ /^\s*([^\$]+)\s+\$\s*\s*([\d\.\,]+)/;
  my($account, $amount) = ($1, $2);
  $amount = ParseNumber($amount);
  $account =~ s/^\s*Funds:Restricted://;    $account =~ s/\s+$//;
  $funds{$account}{ending} = $amount;
}
close LEDGER_FUNDS;

# First, get fund list from ending balance
@ledgerOptions = (@mainLedgerOptions,
                  '--wide-register-format', "%-.70A %22.108t\n",  '-w', '-s',
                  '-e', $startDate, 'reg', '^Funds:Restricted:');

open(LEDGER_FUNDS, "-|", $LEDGER_CMD, @ledgerOptions)
  or die "Unable to run $LEDGER_CMD for funds: $!";

while (my $fundLine = <LEDGER_FUNDS>) {
  die "Unable to parse output line from funds command: $fundLine"
    unless $fundLine =~ /^\s*([^\$]+)\s+\$\s*\s*([\d\.\,]+)/;
  my($account, $amount) = ($1, $2);
  $amount = ParseNumber($amount);
  $account =~ s/^\s*Funds:Restricted://;    $account =~ s/\s+$//;
  $funds{$account}{starting} = $amount;
}
close LEDGER_FUNDS;


foreach my $fund (keys %funds) {
  $funds{$fund}{starting} = $ZERO if not defined $funds{$fund}{starting};
}

@ledgerOptions = (@mainLedgerOptions,
                  '--wide-register-format', "%-.70A %22.108t\n",  '-w', '-s',
                  '-b', $startDate, '-e', $endDate, 'reg');

foreach my $type ('Income', 'Expenses') {
  foreach my $fund (keys %funds) {
    open(LEDGER_INCOME, "-|", $LEDGER_CMD, @ledgerOptions, "^${type}:$fund")
      or die "Unable to run $LEDGER_CMD for funds: $!";
      $funds{$fund}{$type} = $ZERO;
    while (my $line = <LEDGER_INCOME>) {
      die "Unable to parse output line from $type line command: $line"
        unless $line =~ /^\s*([^\$]+)\s+\$\s*\s*([\-\d\.\,]+)/;
      my($account, $amount) = ($1, $2);
      $amount = ParseNumber($amount);
      $funds{$fund}{$type} += $amount;
    }
    close LEDGER_INCOME;
  }
}

my $format = "%-${ACCT_WIDTH}.${ACCT_WIDTH}s       \$%11.2f       \$%11.2f\n";
my($totDeb, $totCred) = ($ZERO, $ZERO);

foreach my $fund (sort keys %funds) {
  print "Fund: $fund\n";
  print "      Balance as of $startDate: ", sprintf("\$%11.2f\n\n", $funds{$fund}{starting});
  print "      Income during period:     ", sprintf("\$%11.2f\n", $funds{$fund}{Income});
  print "      Expenses during period:     ", sprintf("\$%11.2f\n", $funds{$fund}{Expenses});
  print "      Balance as of $endDate: ", sprintf("\$%11.2f\n", $funds{$fund}{ending});
  print "\n\n";
}
###############################################################################
#
# Local variables:
# compile-command: "perl -c fund-report.plx"
# End:

