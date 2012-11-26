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
use Date::Manip;

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
my $ONE_PENNY =  Math::BigFloat->new("0.01");

if (@ARGV < 2) {
  print STDERR "usage: $0 <START_DATE> <END_DATE> <LEDGER_OPTIONS>\n";
  exit 1;
}
my($startDate, $endDate, @mainLedgerOptions) = @ARGV;

my $err;
my $formattedEndDate = UnixDate(DateCalc(ParseDate($endDate), ParseDateDelta("- 1 day"), \$err),
                                "%B %e, %Y");
die "Date calculation error on $endDate" if ($err);
my $formattedStartDate = UnixDate(ParseDate($startDate), "%B %e, %Y");
die "Date calculation error on $startDate" if ($err);

my %reportFields =
  ('Cash' => { args => [ '-e', $endDate, 'bal', '/^Assets/' ] },
   'Accounts Receivable' => {args => [ '-e', $endDate, 'bal', '/^Accrued:Accounts Receivable/' ]},
   'Loans Receivable' => {args => [ '-e', $endDate, 'bal', '/^Accrued:Loans Receivable/' ]},
   'Accounts Payable' => {args => [ '-e', $endDate, 'bal', '/^Accrued.*Accounts Payable/' ]},
   'Accrued Expenses' => {args => [ '-e', $endDate, 'bal', '/^Accrued.*Expenses/' ]},
   'Liabilities, Credit Cards' => {args => [ '-e', $endDate, 'bal', '/^Liabilities:Credit Card/' ]},
   'Liabilities, Other' => {args => [ '-e', $endDate, 'bal', '/^Liabilities/',
                                    'and', 'not', '/^Liabilities:Credit Card/']},
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

open(BALANCE_SHEET, ">", "balance-sheet.txt")
  or die "unable to open balance-sheet.txt for writing: $!";

print BALANCE_SHEET "                          BALANCE SHEET\n",
                    "                          Ending ", $formattedEndDate, "\n",
                  "\n\nASSETS\n\n";

my $formatStr      = "   %-42s \$%13s\n";
my $formatStrTotal = "%-45s \$%13s\n";
my $tot = $ZERO;
foreach my $item ('Cash', 'Accounts Receivable', 'Loans Receivable') {
  next if $reportFields{$item}{total} == $ZERO;
  print BALANCE_SHEET sprintf($formatStr, "$item:", Commify($reportFields{$item}{total}));
  $tot += $reportFields{$item}{total};
}
print BALANCE_SHEET "\n", sprintf($formatStrTotal, "TOTAL ASSETS", Commify($tot)), "\n\nLIABILITIES\n\n";

my $totLiabilities = $ZERO;
foreach my $item ('Accounts Payable', 'Accrued Expenses',
                  'Liabilities, Credit Cards', 'Liabilities, Other',
                  'Unearned Income, Conference Registration', 'Unearned Income, Other') {
  next if $reportFields{$item}{total} == $ZERO;
  print BALANCE_SHEET sprintf($formatStr, "$item:", Commify($reportFields{$item}{total}));
  $totLiabilities += $reportFields{$item}{total};
}
print BALANCE_SHEET "\n", sprintf($formatStr, "TOTAL LIABILTIES", Commify($totLiabilities)),
  "\n\nNET ASSETS\n\n";

my $totNetAssets = $ZERO;
foreach my $item ('Unrestricted Net Assets', 'Temporarily Restricted Net Assets') {
  next if $reportFields{$item}{total} == $ZERO;
  print BALANCE_SHEET sprintf($formatStr, "$item:", Commify($reportFields{$item}{total}));
  $totNetAssets += $reportFields{$item}{total};
}
print BALANCE_SHEET "\n", sprintf($formatStr, "TOTAL NET ASSETS", Commify($totNetAssets)), "\n\n",
             sprintf($formatStrTotal, "TOTAL LIABILITIES AND NET ASSETS", 
                     Commify($totNetAssets + $totLiabilities));

close BALANCE_SHEET;
print STDERR "\n";
die "unable to write to balance-sheet.txt: $!" unless ($? == 0);

die "Cash+accounts receivable total does not equal net assets and liabilities total"
  if (abs( ($reportFields{'Cash'}{total} + $reportFields{'Accounts Receivable'}{total}
       + $reportFields{'Loans Receivable'}{total})) -
      abs($reportFields{'Accounts Payable'}{total} +
       $reportFields{'Accrued Expenses'}{total} +
       $reportFields{'Unearned Income, Conference Registration'}{total} +
       $reportFields{'Unearned Income, Other'}{total} +
       $reportFields{'Liabilities, Credit Cards'}{total} +
       $reportFields{'Liabilities, Other'}{total} +
       $reportFields{'Total Net Assets'}{total}) > $ONE_PENNY);

die "Total net assets doesn't equal sum of restricted and unrestricted ones!"
  if (abs($reportFields{'Total Net Assets'}{total}) -
      abs($reportFields{'Unrestricted Net Assets'}{total} +
      $reportFields{'Temporarily Restricted Net Assets'}{total}) > $ONE_PENNY);


my %incomeGroups = ('INTEREST INCOME' => { args => ['/^Income.*Interest/' ] },
                    'DONATIONS' => { args => [ '/^Income.*Donation/' ] },
                    'BOOK ROYALTIES & AFFILIATE PROGRAMS' =>
                    { args => [ '/^Income.*(Royalt|Affilate)/' ] },
                    'CONFERENCES, REGISTRATION' => {args => [ '/^Income.*Conf.*Reg/' ] },
                    'CONFERENCES, RELATED BUSINESS INCOME' => { args => [ '/^Income.*(Booth|RBI)/'] },
                    'LICENSE ENFORCEMENT' => { args => [ '/^Income.*Enforce/' ]},
                    'TRADEMARKS' => {args => [ '/^Income.*Trademark/' ]},
                    'ADVERSITING' => {args => [ '/^Income.*Advertising/' ]});

my @otherArgs;
foreach my $type (keys %incomeGroups) {
  @otherArgs = ("/^Income/") if @otherArgs == 0;
  push(@otherArgs, 'and', 'not', @{$incomeGroups{$type}{args}});
}
$incomeGroups{"OTHER"}{args} = \@otherArgs;
$incomeGroups{"TOTAL"}{args} = ['/^Income/'];

open(INCOME, ">", "income.txt") or die "unable to open income.txt for writing: $!";

foreach my $type (keys %incomeGroups) {
  my(@fullCommand) = ($LEDGER_BIN, @mainLedgerOptions, '-V', '-X', '$',
                      '-b', $startDate, '-e', $endDate,
                      '-F', '%-.80A   %22.108t\n', '-s',
                      'reg', @{$incomeGroups{$type}{args}});

  open(FILE, "-|", @fullCommand)
    or die "unable to run command ledger command: @fullCommand: $!";

  print STDERR ($VERBOSE ? "Running: @fullCommand\n" : ".");

  $incomeGroups{$type}{total} = $ZERO;
  $incomeGroups{$type}{output} = "";

  foreach my $line (<FILE>) {
    die "Unable to parse output line from second funds command: $line"
      unless $line =~ /^\s*([^\$]+)\s+\$\s*([\-\d\.\,]+)/;
    my($account, $amount) = ($1, $2);
    $amount = ParseNumber($amount);
    $account =~ s/\s+$//;
    next if $account =~ /\<Adjustment\>/ and (abs($amount) <= 0.02);
    die "Weird account found, $account with amount of $amount in income command\n"
      unless $account =~ s/^\s*Income://;

    $incomeGroups{$type}{total} += $amount;
    $incomeGroups{$type}{output} .= "    $line";
  }
}
print INCOME "                           INCOME\n",
             "           Between $formattedStartDate and $formattedEndDate\n\n";


my $overallTotal = $ZERO;

$formatStrTotal = "%-90s    \$%14s\n";
foreach my $type ('DONATIONS', 'LICENSE ENFORCEMENT',
                  'CONFERENCES, REGISTRATION', 'CONFERENCES, RELATED BUSINESS INCOME',
                  'BOOK ROYALTIES & AFFILIATE PROGRAMS', 'ADVERSITING',
                  'TRADEMARKS', 'INTEREST INCOME', 'OTHER') {
  next if ($incomeGroups{$type}{output} =~ /^\s*$/ and $incomeGroups{$type}{total} == $ZERO);
  print INCOME "\n$type\n",
               $incomeGroups{$type}{output}, "\n",
               sprintf($formatStrTotal, "TOTAL $type:", Commify($incomeGroups{$type}{total}));
  $overallTotal += $incomeGroups{$type}{total};
}
print INCOME "\n\n\n", sprintf($formatStrTotal, "OVERALL TOTAL:", Commify($overallTotal));

close INCOME;    die "unable to write to income.txt: $!" unless ($? == 0);

die "calculated total of $overallTotal does equal $incomeGroups{TOTAL}{total}"
  if (abs($overallTotal) - abs($incomeGroups{TOTAL}{total}) > $ONE_PENNY);

print STDERR "\n";

my %expenseGroups = ('BANKING FEES' => { regex => '^Expenses.*(Banking Fees|Currency Conversion)'  },
                    'COMPUTING, HOSTING AND EQUIPMENT' => { regex =>  '^Expenses.*(Hosting|Computer Equipment)'  },
                    'CONFERENCES' => { regex =>  '^Expenses.*(Conferences|Sprint)'  },
                    'DEVELOPER MENTORING' => {regex =>  '^Expenses.*Mentor'  },
                    'LICENSE ENFORCEMENT' => { regex =>  '^Expenses.*Enforce' },
                    'ACCOUNTING' => { regex =>  '^Expenses.*(Accounting|Annual Audit)' },
                    'PAYROLL' => { regex =>  '^Expenses.*Payroll' },
                    'OFFICE' => { regex =>  '^Expenses.*(Office|Phones)' },
                    'RENT' => { regex =>  '^Expenses.*Rent' },
                    'SOFTWARE DEVELOPMENT' => { regex =>  '^Expenses.*Development' },
                    'OTHER PROGRAM ACTIVITY' => {regex =>  '^Expenses.*Gould' },
                    'ADVOCACY AND PROMOTION' => {regex =>  '^Expenses.*(Slipstream|Advocacy Merchandise|Promotional)' },
                    'ADVERSITING' => {regex =>  '^Expenses.*Advertising' });

foreach my $type (keys %expenseGroups, 'TRAVEL') {
  $expenseGroups{$type}{total} = $ZERO;
  $expenseGroups{$type}{output} = "";
}

open(EXPENSE, ">", "expense.txt") or die "unable to open expense.txt for writing: $!";

my(@fullCommand) = ($LEDGER_BIN, @mainLedgerOptions, '-V', '-X', '$',
                    '-b', $startDate, '-e', $endDate,
                    '-F', '%-.80A   %22.108t\n', '-s',
                    'reg', '/^Expenses/');

open(FILE, "-|", @fullCommand)
  or die "unable to run command ledger command: @fullCommand: $!";

print STDERR ($VERBOSE ? "Running: @fullCommand\n" : ".");

my $firstTotal = $ZERO;
foreach my $line (<FILE>) {
  die "Unable to parse output line from second funds command: $line"
    unless $line =~ /^\s*([^\$]+)\s+\$\s*([\-\d\.\,]+)/;
  my($account, $amount) = ($1, $2);
  $amount = ParseNumber($amount);
  $account =~ s/\s+$//;
  next if $account =~ /\<Adjustment\>/ and (abs($amount) <= 0.02);
  die "Weird account found, $account, with amount of $amount in expenses command\n"
    unless $account =~ /^\s*Expenses:/;

  my $taken = 0;
  # Note: Prioritize to put things under conference expenses if they were for a conference. 
  foreach my $type ('CONFERENCES', keys %expenseGroups) {
    last if $taken;
    next if $type eq 'TRAVEL' or $type eq 'OTHER';
    next unless $line =~ /$expenseGroups{$type}{regex}/;
    $taken = 1;
    $expenseGroups{$type}{total} += $amount;
    $expenseGroups{$type}{output} .= "    $line";
  }
  if (not $taken) {
    if ($account =~ /Travel/) {
      $expenseGroups{'TRAVEL'}{total} += $amount;
      $expenseGroups{'TRAVEL'}{output} .= "    $line";
    } else {
      $expenseGroups{'OTHER'}{total} += $amount;
      $expenseGroups{'OTHER'}{output} .= "    $line";
    }
  }
  $firstTotal += $amount;
}
print EXPENSE "                           EXPENSES\n",
             "           Between $formattedStartDate and $formattedEndDate\n\n";
$overallTotal = $ZERO;
$formatStrTotal = "%-90s    \$%14s\n";

my %verifyAllGroups;
foreach my $key (keys %expenseGroups) {
  $verifyAllGroups{$key} = 1;
}
foreach my $type ('PAYROLL', 'SOFTWARE DEVELOPMENT', 'LICENSE ENFORCEMENT', 'CONFERENCES',
                  'DEVELOPER MENTORING', 'TRAVEL', 'BANKING FEES', 'ADVOCACY AND PROMOTION',
                  'COMPUTING, HOSTING AND EQUIPMENT', 'ACCOUNTING',
                  'OFFICE', 'RENT', 'ADVERSITING', 'OTHER PROGRAM ACTIVITY', 'OTHER') {
  delete $verifyAllGroups{$type};

  die "$type is not defined!" if not defined $expenseGroups{$type};
  next if ($expenseGroups{$type}{output} =~ /^\s*$/ and $expenseGroups{$type}{total} == $ZERO);

  print EXPENSE "\n$type\n",
               $expenseGroups{$type}{output}, "\n",
               sprintf($formatStrTotal, "TOTAL $type:", Commify($expenseGroups{$type}{total}));
  $overallTotal += $expenseGroups{$type}{total};
}

print EXPENSE "\n\n\n", sprintf($formatStrTotal, "OVERALL TOTAL:", Commify($overallTotal));

close EXPENSE;    die "unable to write to expense.txt: $!" unless ($? == 0);

die "GROUPS NOT INCLUDED : ", join(keys(%verifyAllGroups), ", "), "\n"
  unless (keys %verifyAllGroups == 0);

die "calculated total of $overallTotal does equal $firstTotal"
  if (abs($overallTotal) - abs($firstTotal) > $ONE_PENNY);

print STDERR "\n";

###############################################################################
#
# Local variables:
# compile-command: "perl -c summary-reports.plx"
# End:
