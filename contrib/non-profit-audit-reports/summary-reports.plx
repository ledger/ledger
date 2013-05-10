#!/usr/bin/perl
# fund-report.plx                                    -*- Perl -*-
#
#    Script to generate end-of-year summary reports.
#
# Copyright (C) 2011, 2012, 2013, Bradley M. Kuhn
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

sub preferredAccountSorting ($$) {
  if ($_[0] =~ /^Assets/ and $_[1] !~ /^Assets/) {
    return -1;
  } elsif ($_[1] =~ /^Assets/ and $_[0] !~ /^Assets/) {
    return 1;
  } elsif ($_[0] =~ /^Liabilities/ and $_[1] !~ /^(Assets|Liabilities)/) {
    return -1;
  } elsif ($_[1] =~ /^Liabilities/ and $_[0] !~ /^(Assets|Liabilities)/) {
    return 1;
  } elsif ($_[0] =~ /^(Accrued:[^:]+Receivable)/ and $_[1] !~ /^(Assets|Liabilities|Accrued:[^:]+Receivable)/) {
    return -1;
  } elsif ($_[1] =~ /^(Accrued:[^:]+Receivable)/ and $_[0] !~ /^(Assets|Liabilities|Accrued:[^:]+Receivable)/) {
    return 1;
  } elsif ($_[0] =~ /^(Accrued)/ and $_[1] !~ /^(Assets|Liabilities|Accrued)/) {
    return -1;
  } elsif ($_[1] =~ /^(Accrued)/ and $_[0] !~ /^(Assets|Liabilities|Accrued)/) {
    return 1;
  } elsif ($_[0] =~ /^(Unearned Income)/ and $_[1] !~ /^(Assets|Liabilities|Accrued|Unearned Income)/) {
    return -1;
  } elsif ($_[1] =~ /^(Unearned Income)/ and $_[0] !~ /^(Assets|Liabilities|Accrued|Unearned Income)/) {
    return 1;
  } elsif ($_[0] =~ /^Income/ and $_[1] !~ /^(Assets|Liabilities|Accrued|Unearned Income|Income)/) {
    return -1;
  } elsif ($_[1] =~ /^Income/ and $_[0] !~ /^(Assets|Liabilities|Accrued|Unearned Income|Income)/) {
    return 1;
  } elsif ($_[0] =~ /^Expense/ and $_[1] !~ /^(Assets|Liabilities|Accrued|Income|Unearned Income|Expense)/) {
    return -1;
  } elsif ($_[1] =~ /^Expense/ and $_[0] !~ /^(Assets|Liabilities|Accrued|Income|Unearned Income|Expense)/) {
    return 1;
  } else {
    return $_[0] cmp $_[1];
  }
}

sub ParseNumber($) {
  $_[0] =~ s/,//g;
  return Math::BigFloat->new($_[0]);
}
Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");
my $ONE_PENNY =  Math::BigFloat->new("0.01");
my $TWO_CENTS =  Math::BigFloat->new("0.02");

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
                                                   '/^Unearned Income.*Reg/' ]},
   'Unearned Income, Other' => {args => [ '-e', $endDate, 'bal', '/^Unearned Income/', 'and', 'not',
                                                   '/^Unearned Income.*Reg/' ]},
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

open(BALANCE_SHEET, ">", "balance-sheet.csv")
  or die "unable to open balance-sheet.csv for writing: $!";

print BALANCE_SHEET "\"BALANCE SHEET\"\n",
                    "\"Ending\",\"", $formattedEndDate, "\"\n",
                  "\n\n\"ASSETS\"\n\n";

my $formatStr      = "\"\",\"%-42s\",\"\$%13s\"\n";
my $formatStrTotal = "\"\",\"%-45s\",\"\$%13s\"\n";
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
die "unable to write to balance-sheet.csv: $!" unless ($? == 0);

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
                    'CONFERENCES, REGISTRATION' => {args => [ '/^Income.*Reg/' ] },
                    'CONFERENCES, RELATED BUSINESS INCOME' => { args => [ '/^Income.*(Conferences?:.*Sponsor|Booth|RBI)/'] },
                    'LICENSE COMPLIANCE' => { args => [ '/^Income.*(Enforce|Compliance)/' ]},
                    'TRADEMARKS' => {args => [ '/^Income.*Trademark/' ]},
                    'ADVERSITING' => {args => [ '/^Income.*Advertising/' ]});

my @otherArgs;
foreach my $type (keys %incomeGroups) {
  @otherArgs = ("/^Income/") if @otherArgs == 0;
  push(@otherArgs, 'and', 'not', @{$incomeGroups{$type}{args}});
}
$incomeGroups{"OTHER"}{args} = \@otherArgs;
$incomeGroups{"TOTAL"}{args} = ['/^Income/'];

open(INCOME, ">", "income.csv") or die "unable to open income.csv for writing: $!";

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
    next if $account =~ /\<Adjustment\>/ and (abs($amount) <= $TWO_CENTS);
    die "Weird account found, $account with amount of $amount in income command\n"
      unless $account =~ /^\s*Income:/;

    $incomeGroups{$type}{total} += $amount;
    $incomeGroups{$type}{output} .= "\"$account\",\"\$$amount\"\n";
  }
}
print INCOME "\"INCOME\",",
             "\"STARTING:\",\"$formattedStartDate\",\"ENDING:\",\"$formattedEndDate\"\n\n";


my $overallTotal = $ZERO;

$formatStrTotal = "\"%-90s\",\"\$%14s\"\n";
foreach my $type ('DONATIONS', 'LICENSE COMPLIANCE',
                  'CONFERENCES, REGISTRATION', 'CONFERENCES, RELATED BUSINESS INCOME',
                  'BOOK ROYALTIES & AFFILIATE PROGRAMS', 'ADVERSITING',
                  'TRADEMARKS', 'INTEREST INCOME', 'OTHER') {
  next if ($incomeGroups{$type}{output} =~ /^\s*$/ and $incomeGroups{$type}{total} == $ZERO);
  print INCOME "\n\"$type\"\n",
               $incomeGroups{$type}{output}, "\n",
               sprintf($formatStrTotal, "TOTAL $type:", Commify($incomeGroups{$type}{total}));
  $overallTotal += $incomeGroups{$type}{total};
}
print INCOME "\n\n\n", sprintf($formatStrTotal, "OVERALL TOTAL:", Commify($overallTotal));

close INCOME;    die "unable to write to income.csv: $!" unless ($? == 0);

die "calculated total of $overallTotal does equal $incomeGroups{TOTAL}{total}"
  if (abs($overallTotal) - abs($incomeGroups{TOTAL}{total}) > $ONE_PENNY);

print STDERR "\n";

my %expenseGroups = ('BANKING FEES' => { regex => '^Expenses.*(Banking Fees|Currency Conversion)'  },
                    'COMPUTING, HOSTING AND EQUIPMENT' => { regex =>  '^Expenses.*(Hosting|Computer Equipment)'  },
                    'CONFERENCES' => { regex =>  '^Expenses.*(Conferences|Sprint)'  },
                    'DEVELOPER MENTORING' => {regex =>  '^Expenses.*Mentor'  },
                    'LICENSE COMPLIANCE' => { regex =>  '^Expenses.*(Enforce|Compliance)' },
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

open(EXPENSE, ">", "expense.csv") or die "unable to open expense.csv for writing: $!";

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
  next if $account =~ /\<Adjustment\>/ and (abs($amount) <= $TWO_CENTS);
  die "Weird account found, $account, with amount of $amount in expenses command\n"
    unless $account =~ /^\s*Expenses:/;

  my $outputLine = "\"$account\",\"\$$amount\"\n";
  my $taken = 0;
  # Note: Prioritize to put things under conference expenses if they were for a conference. 
  foreach my $type ('CONFERENCES', keys %expenseGroups) {
    last if $taken;
    next if $type eq 'TRAVEL' or $type eq 'OTHER';
    next unless $line =~ /$expenseGroups{$type}{regex}/;
    $taken = 1;
    $expenseGroups{$type}{total} += $amount;
    $expenseGroups{$type}{output} .= $outputLine;
  }
  if (not $taken) {
    if ($account =~ /Travel/) {
      $expenseGroups{'TRAVEL'}{total} += $amount;
      $expenseGroups{'TRAVEL'}{output} .= $outputLine;
    } else {
      $expenseGroups{'OTHER'}{total} += $amount;
      $expenseGroups{'OTHER'}{output} .= $outputLine;
    }
  }
  $firstTotal += $amount;
}
print EXPENSE "\"EXPENSES\",",
             "\"STARTING:\",\"$formattedStartDate\",\"ENDING:\",\"$formattedEndDate\"\n\n";
$overallTotal = $ZERO;
$formatStrTotal = "\"%-90s\",\"\$%14s\"\n";

my %verifyAllGroups;
foreach my $key (keys %expenseGroups) {
  $verifyAllGroups{$key} = 1;
}
foreach my $type ('PAYROLL', 'SOFTWARE DEVELOPMENT', 'LICENSE COMPLIANCE', 'CONFERENCES',
                  'DEVELOPER MENTORING', 'TRAVEL', 'BANKING FEES', 'ADVOCACY AND PROMOTION',
                  'COMPUTING, HOSTING AND EQUIPMENT', 'ACCOUNTING',
                  'OFFICE', 'RENT', 'ADVERSITING', 'OTHER PROGRAM ACTIVITY', 'OTHER') {
  delete $verifyAllGroups{$type};

  die "$type is not defined!" if not defined $expenseGroups{$type};
  next if ($expenseGroups{$type}{output} =~ /^\s*$/ and $expenseGroups{$type}{total} == $ZERO);

  print EXPENSE "\n\"$type\"\n",
               $expenseGroups{$type}{output}, "\n",
               sprintf($formatStrTotal, "TOTAL $type:", Commify($expenseGroups{$type}{total}));
  $overallTotal += $expenseGroups{$type}{total};
}

print EXPENSE "\n\n\n", sprintf($formatStrTotal, "OVERALL TOTAL:", Commify($overallTotal));

close EXPENSE;    die "unable to write to expense.csv: $!" unless ($? == 0);

die "GROUPS NOT INCLUDED : ", join(keys(%verifyAllGroups), ", "), "\n"
  unless (keys %verifyAllGroups == 0);

die "calculated total of $overallTotal does *not* equal $firstTotal"
  if (abs($overallTotal) - abs($firstTotal) > $ONE_PENNY);

print STDERR "\n";

open(TRIAL, ">", "trial-balance.csv") or die "unable to open accrued.txt for writing: $!";

print TRIAL "\"TRIAL BALANCE REPORT\",\"ENDING: $formattedEndDate\"\n\n",
             "\"ACCOUNT\",\"BALANCE AT $formattedStartDate\",\"CHANGE DURING PERIOD\",\"BALANCE AT $formattedEndDate\"\n\n";

my %commands = (
                'totalEndFY' => [ $LEDGER_BIN, @mainLedgerOptions, '-V', '-X', '$',
                                              '-e', $endDate, '-F', '%-.80A   %22.108t\n', '-s',
                                              'reg' ],
                'amountInYear' =>  [ $LEDGER_BIN, @mainLedgerOptions, '-V', '-X', '$',
                                     '-b', $startDate, '-e', $endDate, '-F', '%-.80A   %22.108t\n',
                                     '-s', 'reg' ],
                'totalBeginFY' =>  [ $LEDGER_BIN, @mainLedgerOptions, '-V', '-X', '$',
                                     '-e', $startDate, '-F', '%-.80A   %22.108t\n',
                                     '-s', 'reg' ]);

my %trialBalanceData;
my %fullAccountList;

foreach my $id (keys %commands) {
  my(@command) = @{$commands{$id}};

  open(FILE, "-|", @command)
  or die "unable to run command ledger command: @command: $!";

  print STDERR ($VERBOSE ? "Running: @command\n" : ".");

  foreach my $line (<FILE>) {
    die "Unable to parse output line from trial balance $id command: $line"
      unless $line =~ /^\s*([^\$]+)\s+\$\s*([\-\d\.\,]+)/;
    my($account, $amount) = ($1, $2);
    $amount = ParseNumber($amount);
    $account =~ s/\s+$//;
    next if $account =~ /\<Adjustment\>/ and (abs($amount) <= $TWO_CENTS);
    next if $account =~ /^Equity:/;   # Stupid auto-account made by ledger.
    $trialBalanceData{$id}{$account} = $amount;
    $fullAccountList{$account} = $id;
  }
  close FILE;
  die "unable to run trial balance ledger command, @command: $!" unless ($? == 0);
}

my $curOn = 'Assets';

foreach my $account (sort preferredAccountSorting keys %fullAccountList) {
  # Blank lines right
  if ($account !~ /^$curOn/) {
    print TRIAL "pagebreak\n";
    $curOn = $account;
    if ($curOn =~ /(Accrued:[^:]+):.*$/) {
      $curOn = $1;
    } else {
      $curOn =~ s/^([^:]+):.*$/$1/;
    }
  }
  if ($account =~ /^Assets|Liabilities|Accrued|Unearned Income/) {
    foreach my $id (qw/totalBeginFY totalEndFY amountInYear/) {
      $trialBalanceData{$id}{$account} = $ZERO
      unless defined $trialBalanceData{$id}{$account};
    }
    print TRIAL "\"$account\",\"\$$trialBalanceData{totalBeginFY}{$account}\",",
     "\"\$$trialBalanceData{amountInYear}{$account}\",\"\$$trialBalanceData{totalEndFY}{$account}\"\n"
       unless $trialBalanceData{totalBeginFY}{$account} == $ZERO and
         $trialBalanceData{amountInYear}{$account} == $ZERO and
           $trialBalanceData{totalEndFY}{$account} == $ZERO;
  } else {
    print TRIAL "\"$account\",\"\",\"\$$trialBalanceData{amountInYear}{$account}\",\"\"\n"
      if defined $trialBalanceData{amountInYear}{$account} and
        $trialBalanceData{amountInYear}{$account} != $ZERO;
  }
}
close TRIAL;
die "unable to write trial-balance.csv: $!" unless ($? == 0);

###############################################################################
#
# Local variables:
# compile-command: "perl -c summary-reports.plx"
# End:
