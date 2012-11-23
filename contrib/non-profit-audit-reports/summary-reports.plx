#!/usr/bin/perl
# fund-report.plx                                    -*- Perl -*-
#
#    Script to generate end-of-year summary reports.
#
# Copyright (C) 2011, 2012, Bradley M. Kuhn
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

my $VERBOSE = 0;
my $DEBUG = 0;

my $LEDGER_BIN = "/usr/local/bin/ledger";

my $ACCT_WIDTH = 70;

sub Commify ($) {
    my $text = reverse $_[0];
    $text =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
    return scalar reverse $text;
}

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
                      '-V', '-X', '$', '-F', "%-.70A %22.108t\n", '-S', 'T', '-s',
                      'd', 'T', '-e', $endDate, 'bal', '/^Assets/');

my %reportFields =
  ('Cash' => { args => [ '-e', $endDate, 'bal', '/^Assets/' ] },
   'Accounts Receivable' => {args => [ '-e', $endDate, 'bal', '/^Accrued:Accounts Receivable/' ]},
   'Loans Receivable' => {args => [ '-e', $endDate, 'bal', '/^Accrued:Loans Receivable/' ]},
   'Accounts Payable' => {args => [ '-e', $endDate, 'bal', '/^Accrued.*Accounts Payable/' ]},
   'Accrued Expenses' => {args => [ '-e', $endDate, 'bal', '/^Accrued.*Expenses/' ]},
   'Unearned Income, Conference Registration' => {args => [ '-e', $endDate, 'bal',
                                                   '/^Unearned Income.*Conf.*Reg/' ]},
   'Unearned Income, Other' => {args => [ '-e', $endDate, 'bal', '/^Unearned Income/', 'and', 'not',
                                                   '/^Unearned Income.*Conf.*Reg/' ]},
   'Unrestricted Net Assets' => {args => [ '-e', $endDate, 'bal', '/^(Income|Expenses):Conservancy/' ]},
   'Temporarily Restricted Net Assets' =>  {args => [ '-e', $endDate, 'bal', '/^(Income|Expenses)/',
                              'and', 'not', '/^(Unearned Income|(Income|Expenses):Conservancy)/' ]},
   'Total Net Assets' =>  {args => [ '-e', $endDate, 'bal', '/^(Income|Expenses)/' ]},

);
foreach my $item (keys %reportFields) {
  my(@fullCommand) = ($LEDGER_BIN, @mainLedgerOptions,
                      '-V', '-X', '$', '-S', 'T', '-s', '-d', 'T', @{$reportFields{$item}{args}});
  open(FILE, "-|", @fullCommand)
    or die "unable to run command ledger command: @fullCommand: $!";

  my $foundBalance;
  my $seenTotalLine = 0;

  print STDERR ($VERBOSE ? "Running: @fullCommand\n" : ".");
  print STDERR "    Output of @fullCommand\n" if $DEBUG;

  while (my $line = <FILE>) {
    print STDERR $line if ($DEBUG);

    $seenTotalLine = 1 if $line =~ /^\s*\-+\s*/;   # Skip lines until the total line
    $foundBalance = $1
      if (not $seenTotalLine and $line =~ /^\s*[^0-9\-]+\s*([\-\d,\.]+)\s+/);

    if ($line =~ /^\s*\$\s*([\-\d,\.]+)\s*$/) {
      $foundBalance = $1;
      last;
    }
  }
  close FILE;
  die "problem running ledger command: @fullCommand: $!" unless ($? == 0);
  if (not defined $foundBalance) {
    $foundBalance = $ZERO;
  } else {
    $foundBalance =~ s/,//g;
    $foundBalance = Math::BigFloat->new($foundBalance);
  }
  $foundBalance = $ZERO if not defined $foundBalance;
  $reportFields{$item}{total} = abs($foundBalance);
  print STDERR  "$item: $reportFields{$item}{total}\n" if $VERBOSE;
}

die "Cash+accounts receivable total does not equal net assets and liabilities total"
  if ( ($reportFields{'Cash'}{total} + $reportFields{'Accounts Receivable'}{total}) !=
      ($reportFields{'Accounts Payable'}{total} +
       $reportFields{'Accrued Expenses'}{total} +
       $reportFields{'Unearned Income, Conference Registration'}{total} +
       $reportFields{'Unearned Income, Other'}{total} +
       $reportFields{'Total Net Assets'}{total}));

die "Total net assets doesn't equal sum of restricted and unrestricted ones!"
  if ($reportFields{'Total Net Assets'}{total} !=
      $reportFields{'Unrestricted Net Assets'}{total} +
      $reportFields{'Temporarily Restricted Net Assets'}{total});

open(ASSETS, ">", "assets-and-liabilities.txt")
  or die "unable to open assets-and-liabilities.txt for writing: $!";

print ASSETS "ASSETS\n\n";

my $formatStr      = "   %-42s \$%18s\n";
my $formatStrTotal = "%-45s \$%12s\n";
my $tot = $ZERO;
foreach my $item ('Cash', 'Accounts Receivable') {
  next if $reportFields{$item}{total} == $ZERO;
  print ASSETS sprintf($formatStr, "$item:", Commify($reportFields{$item}{total}));
  $tot += $reportFields{$item}{total};
}
print ASSETS "\n", sprintf($formatStrTotal, "TOTAL ASSETS", Commify($tot)), "\n\nLIABILITIES\n\n";

my $totLiabilities = $ZERO;
foreach my $item ('Accounts Payable', 'Accrued Expenses',
                  'Unearned Income, Conference Registration', 'Unearned Income, Other') {
  next if $reportFields{$item}{total} == $ZERO;
  print ASSETS sprintf($formatStr, "$item:", Commify($reportFields{$item}{total}));
  $totLiabilities += $reportFields{$item}{total};
}
print ASSETS "\n", sprintf($formatStr, "TOTAL LIABILTIES", Commify($totLiabilities)),
  "\n\nNET ASSETS\n\n";

my $totNetAssets = $ZERO;
foreach my $item ('Unrestricted Net Assets', 'Temporarily Restricted Net Assets') {
  next if $reportFields{$item}{total} == $ZERO;
  print ASSETS sprintf($formatStr, "$item:", Commify($reportFields{$item}{total}));
  $totNetAssets += $reportFields{$item}{total};
}
print ASSETS "\n", sprintf($formatStr, "TOTAL NET ASSETS", Commify($totNetAssets)), "\n\n",
             sprintf($formatStrTotal, "TOTAL LIABILITIES AND NET ASSETS", 
                     Commify($totNetAssets + $totLiabilities));

close ASSETS;
print STDERR "\n";
die "unable to write to Assets-and-liabilities.txt: $!" unless ($? == 0);

###############################################################################
#
# Local variables:
# compile-command: "perl -c summary-reports.plx"
# End:

