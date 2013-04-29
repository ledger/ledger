#!/usr/bin/perl
# cash-receipts-and-disbursments-journals                                    -*- Perl -*-
#
#    Script to generate a cash receipts and disbursement joural reports
#    using Ledger.
#
#    Accountants sometimes ask for a report called the "cash receipts and
#    disbursements journals".  From a programmer's perspective, these are two
#    reports that have the following properties:
#
#       * Receipts: "a list of all transactions in the period where funds
#                    enter a cash account (i.e., the amount reconciled
#                    against the cash account is > 0"
#
#       * Disbursements: "a list of all transactions in the period where
#                         funds leave a cash account (i.e., the amount
#                         reconciled against the cash account is < 0)
#
# Copyright (C) 2011, 2012, 2013 Bradley M. Kuhn
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
use File::Temp qw/tempfile/;

my $LEDGER_CMD = "/usr/local/bin/ledger";

my $ACCT_WIDTH = 75;

sub ParseNumber($) {
  $_[0] =~ s/,//g;
  return Math::BigFloat->new($_[0]);
}

sub LedgerAcctToFilename($) {
  my $x = $_[0];
  $x =~ s/ /-/g;
  $x =~ s/:/-/g;
  return $x;
}

Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");

if (@ARGV < 2) {
  print STDERR "usage: $0 <BEGIN_DATE> <END_DATE> <OTHER_LEDGER_OPTS>\n";
  exit 1;
}

my($beginDate, $endDate, @otherLedgerOpts) = @ARGV;

my(@chartOfAccountsOpts) = ('-V', '-F', "%150A\n",  '-w', '-s',
                            '-b', $beginDate, '-e', $endDate, @otherLedgerOpts, 'reg');

open(CHART_DATA, "-|", $LEDGER_CMD, @chartOfAccountsOpts)
  or die "Unable to run $LEDGER_CMD @chartOfAccountsOpts: $!";

my @accounts;
while (my $line = <CHART_DATA>) {
  chomp $line;
  next if $line =~ /^\s*\<\s*Adjustment\s*\>\s*$/;
  next if $line =~ /^Equity:/;   # Stupid auto-account made by ledger.
  $line =~ s/^\s*//;   $line =~ s/\s*$//;
  push(@accounts, $line);

}
close(CHART_DATA); die "error reading ledger output for chart of accounts: $!" unless $? == 0;

my $formattedEndDate = new Date::Manip::Date;
die "badly formatted end date, $endDate" if $formattedEndDate->parse($endDate);
my $oneDayLess = new Date::Manip::Delta;
die "bad one day less" if $oneDayLess->parse("- 1 day");
$formattedEndDate = $formattedEndDate->calc($oneDayLess);
$formattedEndDate = $formattedEndDate->printf("%Y/%m/%d");

foreach my $typeData ({ name => 'disbursements', query => 'a<=0' },
                      { name => 'receipts', query => 'a>0' }) {
  my $fileNameBase = $typeData->{name};

  open(CSV_OUT, ">", "$fileNameBase.csv") or die "unable to open $fileNameBase.csv: $!";

  foreach my $acct (sort { $a cmp $b } @accounts) {
    next unless ($acct =~ /^(?:Assets|Liabilities)/);

    my @entryLedgerOpts = ('-l', $typeData->{query},
                           '-b', $beginDate, '-e', $endDate, @otherLedgerOpts, 'print', $acct);

    open(ENTRY_DATA, "-|", $LEDGER_CMD, @entryLedgerOpts)
      or die "Unable to run $LEDGER_CMD @entryLedgerOpts: $!";

    my($tempFH, $tempFile) = tempfile("cashreportsXXXXXXXX", TMPDIR => 1);

    while (my $line = <ENTRY_DATA>) { print $tempFH $line; }
    close(ENTRY_DATA); die "Error reading ledger output for entries: $!" unless $? == 0;
    $tempFH->close() or die "Error writing ledger output for entries to temp file, $tempFile: $!";

    goto SKIP_REGISTER_COMMANDS if (-z $tempFile);

    print CSV_OUT "\"ACCOUNT:\",\"$acct\"\n\"PERIOD START:\",\"$beginDate\"\n\"PERIOD END:\",\"$formattedEndDate\"\n";
    print CSV_OUT '"DATE","CHECK NUM","NAME","ACCOUNT","AMOUNT"';

    my $formatString = '\n"%(date)","%C","%P","%A","%t"';
    my $tagStrings = "";
    foreach my $tagField (qw/Receipt Invoice Statement Contract PurchaseOrder Approval Check IncomeDistributionAnalysis CurrencyRate/) {
      print CSV_OUT ',"', $tagField, '"';
      $tagStrings .= ',"link:%(tag(\'' . $tagField . '\'))"';
    }
    $formatString .= $tagStrings . '\n%/"","","","%A","%t"' . $tagStrings . '\n';

    # I thought '--sort', 'd', '--sort-xact', 'a',  should
    # have worked below for a good sort.  Then I tried
    # rather than '--sort', "d,n,a", which didn't work either.
    #  I opened a bug: http://bugs.ledger-cli.org/show_bug.cgi?id=901

    my @csvRegLedgerOpts = ('-f', $tempFile, '-V', '-F', $formatString, '-w', '--sort', 'd',
                            '-b', $beginDate, '-e', $endDate, 'reg');

    open(CSV_DATA, "-|", $LEDGER_CMD, @csvRegLedgerOpts)
      or die "unable to run ledger command for $fileNameBase.csv: $!";

    my($curDepositDate, $curDepositTotal);

    while (my $line = <CSV_DATA>) {
      $line =~ s/"link:"/""/g;

      # Skip lines that have Adjustment or Equity: in them.
      next if $line =~
         /^\s*"[^"]*","[^"]*","[^"]*","(\s*\<\s*Adjustment\s*\>\s*|Equity:)/;

      #  Note that we don't do our usual "$TWO_CENTS" check on Adjustment
      #  here.  That's by design: if we consistently ignore Adjustements in
      #  the same way, it might have the appearance that a Superman
      #  III/Office Space -style movement of funds is going on.  By just
      #  straight "ignoring" them here, and not doing the TWO_CENTS test, it
      #  helps to assure that.

      # However, it's worth noting that the ignoring of "Adjustment" in these
      # scripts is not that meaningful and doesn't indicate as Superman
      # III/Office Space -style scheme, because such a scheme would also have
      # to be implemented in the main Ledger codebase.


      my $date = $line;  chomp $date;
      $date =~ s/^\s*"([^"]*)"\s*,.*$/$1/;
      if (defined $date and $date !~ /^\s*$/ and
          defined $curDepositDate and ($date ne $curDepositDate or
          ($date eq $curDepositDate and $line !~ /DEPOSIT[\s\-]+BRANCH/))) {
        print CSV_OUT "\"$curDepositDate\",\"SUBTOTAL\",\"BRANCH DEPOSIT TOTAL:\",\"\",\"\$$curDepositTotal\"\n\n";
        $curDepositTotal = $curDepositDate = undef;
      }
      if ($line =~ /DEPOSIT[\s\-]+BRANCH/) {
        if (not defined $curDepositDate) {
          $curDepositDate = $line; chomp $curDepositDate;
          $curDepositDate =~ s/^\s*"([^"]+)"\s*,.*$/$1/;
        }
      }
      # This is a bit of a hack because I can't ssume that the line with the
      # description on it has the account name in it.
      if (defined $curDepositDate and $line =~ /$acct/) {
        my $amt = $line;
        chomp $amt;
        $amt =~ s/^\s*"[^"]*","[^"]*","[^"]*","[^"]*","\$\s*([^"]*)".*$/$1/;
        $amt =~ s/,//g;

        $curDepositTotal = 0.0 unless defined $curDepositTotal;
        $curDepositTotal += $amt;
      }
      print CSV_OUT $line;
    }
    # Catch potential last Deposit subtotal
    print CSV_OUT "\n\"$curDepositDate\",\"SUBTOTAL\",\"BRANCH DEPOSIT TOTAL:\",\"\",\"\$$curDepositTotal\"\n\n"
      if (defined $curDepositDate);

    close(CSV_DATA); die "Error read from csv ledger command $!" unless $? == 0;
    print CSV_OUT "pagebreak\n";
  SKIP_REGISTER_COMMANDS:
    unlink($tempFile);
  }
  close(CSV_OUT); die "Error read write csv out to $fileNameBase.csv: $!" unless $? == 0;
}
###############################################################################
#
# Local variables:
# compile-command: "perl -c cash-receipts-and-disbursments-journals.plx"
# End:

