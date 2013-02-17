#!/usr/bin/perl

use strict;
use warnings;

use Math::BigFloat;
use Date::Manip;
use Data::PowerSet;

Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");
my $ONE_HUNDRED =  Math::BigFloat->new("100.00");

my $VERBOSE = 1;
my $DEBUG = 0;

my $LEDGER_BIN = "/usr/local/bin/ledger";

######################################################################
sub BruteForceSubSetSumSolver ($$$) {
  my($numberList, $totalSought, $extractNumber) = @_;

  my($P, $N) = (0, 0);
  my $size = scalar(@{$numberList});
  my %Q;
  my(@L) =
     map { { val => &$extractNumber($_), obj => $_ } } @{$numberList};

  my $powerset = Data::PowerSet->new(@L);

  while (my $set = $powerset->next) {
    my $total = $ZERO;
    foreach my $ee (@{$set}) {
      $total += $ee->{val};
    }
    if ($totalSought == $total) {
      my(@list) = map { $_->{obj} } @{$set};
      return (1, \@list);
    }
  }
  return (0, []);
}
######################################################################
sub DynamicProgrammingSubSetSumSolver ($$$) {
  my($numberList, $totalSought, $extractNumber) = @_;

  my($P, $N) = (0, 0);
  my $size = scalar(@{$numberList});
  my %Q;
  my(@L) =
     map { { val => &$extractNumber($_), obj => $_ } } @{$numberList};

  print STDERR "  TotalSought:", $totalSought if $VERBOSE;
  print STDERR "  L in this iteration:\n     [" if $VERBOSE;

  foreach my $ee (@L) {
    if ($ee->{val} < 0) {
      $N += $ee->{val}
    } else {
      $P += $ee->{val};
    }
    print STDERR $ee->{val}, ", " if $VERBOSE;
  }
  print STDERR "]\n    P = $P, N = $N\n" if ($VERBOSE);

  for (my $ii = 0 ; $ii <= $size ; $ii++ ) {
    $Q{$ii}{0}{value} = 1;
    $Q{$ii}{0}{list} = [];
  }
  for (my $jj = $N; $jj <= $P ; $jj++) {
    $Q{0}{$jj}{value} = ($L[0]{val} == $jj);
    $Q{0}{$jj}{list} = $Q{0}{$jj}{value} ? [ $L[0]{obj} ] : [];
  }
  for (my $ii = 1; $ii <= $size ; $ii++ ) {
    for (my $jj = $N; $jj <= $P ; $jj++) {
      if ($Q{$ii-1}{$jj}{value}) {
        $Q{$ii}{$jj}{value} = 1;

        $Q{$ii}{$jj}{list} = [] unless defined $Q{$ii}{$jj}{list};
        push(@{$Q{$ii}{$jj}{list}}, @{$Q{$ii-1}{$jj}{list}});

      } elsif ($L[$ii]{val} == $jj) {
        $Q{$ii}{$jj}{value} = 1;

        $Q{$ii}{$jj}{list} = [] unless defined $Q{$ii}{$jj}{list};
        push(@{$Q{$ii}{$jj}{list}}, $jj);
      } elsif ($Q{$ii-1}{$jj - $L[$ii]{val}}{value}) {
        $Q{$ii}{$jj}{value} = 1;
        $Q{$ii}{$jj}{list} = [] unless defined $Q{$ii}{$jj}{list};
        push(@{$Q{$ii}{$jj}{list}}, $L[$ii]{obj}, @{$Q{$ii-1}{$jj - $L[$ii]{val}}{list}});
      } else {
        $Q{$ii}{$jj}{value} = 0;
        $Q{$ii}{$jj}{list} = [];
      }
    }
  }
  foreach (my $ii = 0; $ii <= $size; $ii++) {
    foreach (my $jj = $N; $jj <= $P; $jj++) {
      print "Q($ii, $jj) == $Q{$ii}{$jj}{value} with List of ", join(", ", @{$Q{$ii}{$jj}{list}}), "\n";
    }
  }
  return [ $Q{$size}{$totalSought}{value}, \@{$Q{$size}{$totalSought}{list}}];
}
######################################################################
sub Commify ($) {
    my $text = reverse $_[0];
    $text =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
    return scalar reverse $text;
}
######################################################################
sub ParseNumber($) {
  $_[0] =~ s/,//g;
  return Math::BigFloat->new($_[0]);
}
######################################################################
sub ConvertTwoDigitPrecisionToInteger ($) {
  return sprintf("%d", $_[0] * $ONE_HUNDRED);
}
######################################################################
sub ConvertTwoDigitPrecisionToIntegerInEntry ($) {
  return ConvertTwoDigitPrecisionToInteger($_[0]->{amount});
}
######################################################################
my $firstArg = shift @ARGV;

my $solver = \&BruteForceSubSetSumSolver;

if (@ARGV < 7) {
  print STDERR "usage: $0 [-d] <TITLE> <ACCOUNT_REGEX> <END_DATE> <START_SEARCH_FROM_DATE> <END_SEARCH_TO_DATE> <BANK_STATEMENT_BALANCE> <LEDGER_OPTIONS>\n";
  exit 1;
}
if ($firstArg eq '-d') {
  $solver = \&DynamicProgrammingSubSetSumSolver;
} else {
  unshift(@ARGV, $firstArg);
}
my($title, $account, $endDate, $startSearchFromDate, $endSearchToDate, $bankBalance, @mainLedgerOptions) = @ARGV;

$bankBalance = ParseNumber($bankBalance);

my(@fullCommand) = ($LEDGER_BIN, @mainLedgerOptions, '-V', '-X', '$',
                    '-e', $endDate, '-F', '%t\n', 'bal', "/$account/");

open(FILE, "-|", @fullCommand) or die "unable to run command ledger command: @fullCommand: $!";

my $total;
foreach my $line (<FILE>) {
  chomp $line;
  die "Unable to parse output line from: \"$line\""
    unless $line =~ /^\s*\$\s*([\-\d\.\,]+)\s*$/  and not defined $total;
  $total = $1;
  $total = ParseNumber($total);
}
close FILE;
if (not defined $total or $? != 0) {
  die "unable to run ledger @fullCommand: $!";
}
my $differenceSought = $total - $bankBalance;

my $err;
my $formattedEndDate = UnixDate(DateCalc(ParseDate($endDate), ParseDateDelta("- 1 day"), \$err),
                                "%Y-%m-%d");
die "Date calculation error on $endDate" if ($err);

my $earliestStartDate = DateCalc(ParseDate($endDate), ParseDateDelta("- 1 month"), \$err);

die "Date calculation error on $endDate" if ($err);

my $startDate = ParseDate($startSearchFromDate);

my @solution;
while ($startDate ge $earliestStartDate) {
  $startDate = DateCalc(ParseDate($startDate), ParseDateDelta("- 1 day"), \$err);
  die "Date calculation error on $endDate" if ($err);

  my $formattedStartDate = UnixDate($startDate, "%Y-%m-%d");

  print STDERR "Testing $formattedStartDate through $endSearchToDate for a total of ", Commify($differenceSought), ": \n"
    if $VERBOSE;

  my(@fullCommand) = ($LEDGER_BIN, @mainLedgerOptions, '-V', '-X', '$',
                      '-b', $formattedStartDate, '-e', $endSearchToDate,
                      '-F', '"%(date)","%C","%P","%t"\n',
                      'reg', "/$account/");

  open(FILE, "-|", @fullCommand)
    or die "unable to run command ledger command: @fullCommand: $!";

  my @entries;

  foreach my $line (<FILE>) {
    die "Unable to parse output line from: $line"
      unless $line =~ /^\s*"([^"]*)","([^"]*)","([^"]*)","([^"]*)"\s*$/;
    my($date, $checkNum, $payee, $amount) = ($1, $2, $3, $4);
    die "$amount is not a valid amount"
      unless $amount =~ s/\s*\$\s*([\-\d\.\,]+)\s*$/$1/;
    $amount = ParseNumber($amount);

    push(@entries, { date => $date, checkNum => $checkNum, 
                     payee => $payee, amount => $amount });
  }
  close FILE;
  die "unable to properly run ledger command: @fullCommand: $!" unless ($? == 0);

  @solution = $solver->(\@entries,
                        ConvertTwoDigitPrecisionToInteger($differenceSought),
                        \&ConvertTwoDigitPrecisionToIntegerInEntry);
  if ($VERBOSE) {
    if ($solution[0]) {
      use Data::Dumper;
      print STDERR "Solution for $formattedStartDate to $formattedEndDate, $differenceSought: \n",
        Data::Dumper->Dump(\@solution);
    } else {
      print STDERR "No Solution Found. :(\n";
    }
  }
  last if ($solution[0]);
}
if ($solution[0]) {
  print "\"title:$formattedEndDate: $title\"\n\"BANK RECONCILATION: $account\",\"ENDING\",\"$formattedEndDate\"\n";
  print "\n\n\"DATE\",\"CHECK NUM\",\"PAYEE\",\"AMOUNT\"\n\n";
  print "\"$formattedEndDate\",\"\",\"BANK ACCOUNT BALANCE\",\"\$$bankBalance\"\n\n";
  foreach my $ee (sort { $a->{date} cmp $b->{date} } @{$solution[1]}) {
    print "\"$ee->{date}\",\"$ee->{checkNum}\",\"$ee->{payee}\",\"\$$ee->{amount}\"\n";
  }
    print "\n\"$formattedEndDate\",\"\",\"OUR ACCOUNT BALANCE\",\"\$$total\"\n\n";
}
###############################################################################
#
# Local variables:
# compile-command: "perl -c bank-reconcilation.plx"
# End:
