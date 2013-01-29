#!/usr/bin/perl

use warnings;
use strict;
use Getopt::Long;    # Options processing
use Smart::Comments -ENV, "###"; # Ignore my dividers, and use
                                 # Smart_Comments=1 to activate
use Cwd;
use File::Basename;
use 5.10.0;
use POSIX qw(strftime);
use Date::Calc qw(Add_Delta_Days);

use Template;
my $TT = Template->new( { POST_CHOMP => 1 } );

######################################################################
# TODO
#
# DONE Meal summaries are broken for multi-week reports

######################################################################
# Options

# Is this an internal report?
my $ExpenseReportCode = undef;
my $Internal = undef;
my $SuppressMeals = 0;
my $ViewAfter = 0;
my $ImageDir = ".";
my $Anonymize = 0;
my $Help = undef;

GetOptions( 'c'      => \$Internal,
            'm'      => \$SuppressMeals,
		    'v'      => \$ViewAfter,
            'a'      => \$Anonymize,
            'I'      => \$ImageDir,
            'e:s'    => \$ExpenseReportCode,
            'h|help' => \$Help
          );

# Help

defined $Help && do {
    print <<EOF;
Usage: GenerateLatexExpenseReport.pl [OPTION] -e ERCode

Options:
 -c                       Internal report
 -m                       Suppress meals
 -v                       View PDF on completion
 -a                       Anonymous, omit header/footer
 -I                       Image directory
 -e                       ER Code (AISER0001)

EOF
    exit -1;
};

die "Pass -e <ExpenseReportCode>" unless defined $ExpenseReportCode;

######################################################################
# Report items

my @ItemizedExpenses;
my $ItemizedTotal = 0.00;

my @ItemizedReceipts;

my @MealsReport;

######################################################################
# Gather required data about this expense report from the directory name
# ie: ./AISER0015 20090419-20090425 AGIL2078 Shands HACMP/
#
# ExpenseReportCode = AISER0015
# DateRange         = 20090419-20090425
# ProjectCode       = AGIL2078
# Description       = Shands HACMP


######################################################################
# Remaining options

# Where is the ledger binary?
my $LedgerBin = "./ledger -f ./.ledger -V";

# -E     Show empty accounts
# -S d   Sort by date
# -V     Convert mileage to $
my $LedgerOpts = "--no-color -S d";

my $LedgerAcct = "^Dest:Projects";

my $LedgerCriteria = "%" . "ER=$ExpenseReportCode";

# Internal report?

if ( $Internal ) {

  # No mileage on an internal report
  # $LedgerCriteria .= "&!/Mileage/"; # This shouldn't matter, just don't put metadata for ER on mileage
  $LedgerAcct = "^Dest:Internal";

}

my $CmdLine = "$LedgerBin reg $LedgerOpts -E \"$LedgerCriteria\" and ^Stub "
  . "--format \"%(tag('ER'))~%(tag('PROJECT'))~%(tag('NOTE'))\n\"";
### $CmdLine

my @TempLine = `$CmdLine`;

# Match all remaining items
$TempLine[0] =~ m,^(?<Er>.*?)~
                  (?<Project>.*?)~
                  (?<Note>.*?)\s*$,x;

my $ProjectCode= $+{'Project'};
my $Description= $+{'Note'};

### $ExpenseReportCode
### $ProjectCode
### $Description
### $LedgerAcct
### $Internal
### $Anonymize
### $LedgerAcct
### $LedgerOpts
### $LedgerCriteria


######################################################################
# Pull main ledger report of line items for the expense report
# Using ~ as a delimiter
#
# Example:
# '2009/04/25~AR:Projects:AGIL2078:PersMealsLunch~:AISER0015: PILOT         00004259 MIDWAY, FL~ 8.68~Receipts/AGIL2078/20090425_Pilot_8_68_21204.jpg\n'
#
#./ledger  --no-color reg %ER=AISER0040 and ^Projects -y "%Y/%m/%d" -V --format "%(date)~%(account)~%(payee)~%(amount)~%(tag('NOTE'))\n"

$CmdLine = "$LedgerBin reg $LedgerOpts \"$LedgerCriteria\" and \"$LedgerAcct\" "
  . "-y \"%Y/%m/%d\" "
  . "--format \"%(date)~%(tag('CATEGORY'))~%(payee)~%(display_amount)~%(tag('NOTE'))~%(tag('RECEIPT'))\\n\"";
### $CmdLine
my @MainReport = `$CmdLine`;


### MainReport: @MainReport

# Remove any project codes and linefeeds
#map { chomp(); s/(:AISER[0-9][0-9][0-9][0-9])+://g; } @MainReport; # No need, thats now metadata

foreach my $line (@MainReport) {       ### Processing Main Report... done

    # Remove bad chars (#&)
    $line =~ tr/#&//d;

    # Match all remaining items
    $line =~ m,^(?<Date>[0-9]{4}/[0-9]{2}/[0-9]{2})~
			(?<Category>.*?)~
			(?<Vendor>.*?)~
			(?<Amount>.*?)~
			(?<Note>.*?)~
            (?<Receipts>.*?)\s*$,x;
    my %Record = %+;

    $Record{'Amount'}=~tr/$,//d;

    foreach (keys %Record) {
      $Record{$_} =~ s/^\s+//g;
    }


    # Grab images from <<file:///dir/filename.jpg>>
    my $ImageList = $Record{'Receipts'};
    $ImageList //= '';
    my @Images = split( /,/, $ImageList );

    # Cleanup
    # Take last word of account name as category
    $Record{'Category'} = ( split( /:/, $Record{'Category'} ) )[-1];

	# If no images, italicise the line item.
	$Record{'Italics'} = 1;

	# Test images
	foreach my $Image (@Images) {

	  # Turn off italics because there is an image
	  $Record{'Italics'} = 0;

	  if (! -r $ImageDir . "/" . $Image) {
		print STDERR "Missing $ImageDir/$Image\n";
	  }
	}

    # Add to itemized expenses to be printed
    push( @ItemizedExpenses, \%Record );
    $ItemizedTotal += $Record{'Amount'};

    # Add to itemized reciepts for printing
    push( @ItemizedReceipts, { 'Vendor' => $Record{'Vendor'},
                               'Amount' => $Record{'Amount'},
                               'Date' => $Record{'Date'},
                               'Images' => \@Images } )
      if $ImageList;

}

### @ItemizedExpenses

######################################################################
# Meals report

# Summarize total spent on meals by day
$CmdLine = "$LedgerBin reg $LedgerOpts "
  . "\"$LedgerCriteria\" and \"$LedgerAcct\" and \%CATEGORY=Meals "
  . "-D -n "
  . "--format \"%(account)~%(payee)~%(display_amount)~%(total)\n\" "
  . "| grep -v '<None>'";

### $CmdLine
my @MealsOutput = `$CmdLine`;

### @MealsOutput

foreach my $line (@MealsOutput) {

    # Match all remaining items
  $line =~ m,^(?<Account>.*?)~
             (?<DOW>.*?)~\$
             (?<Amount>\s*[0-9]+\.[0-9]+)~\$
             (?<RunningTotal>.*?)\s*$,x;
  my %TRecord=%+;
  $TRecord{'Account'}=~s/^Projects://g;
  $TRecord{'DOW'}=~s/^- //g;

  # Add to itemized expenses to be printed
  push( @MealsReport, \%TRecord );

}

######################################################################
# Total by category

$CmdLine = "$LedgerBin bal $LedgerOpts "
  . "\"$LedgerCriteria\" and \"$LedgerAcct\" '--account=tag(\"CATEGORY\")' "
  . "--format \"%(account)~%(display_total)\\n\"";

### $CmdLine
my @CategoryOutput = `$CmdLine`;

### @CategoryOutput

my @CategoryReport;

foreach my $line (@CategoryOutput) {

  chomp $line;
  $line =~ tr/\$,//d;

    # Match all remaining items
  my @Temp = split(/~/,$line);

  my %TRecord= ( 'Category' => $Temp[0],
                 'Amount'   => $Temp[1]);

  if ($TRecord{'Category'} eq '') {
    $TRecord{'Category'} = '\\hline \\bf Total';
  }

  # Cleanup
  # Take last word of account name as category
  $TRecord{'Category'} = ( split( /:/, $TRecord{'Category'} ) )[0];

  # Add to itemized expenses to be printed
  push( @CategoryReport, \%TRecord );

}

### @CategoryReport


######################################################################
#  Output
######################################################################

my $TTVars = {
    'Internal'         => $Internal,
    'SuppressMeals'    => $SuppressMeals,
    'ExpenseReportCode' => $ExpenseReportCode,
    'ProjectCode'      => $ProjectCode,
    'Description'      => $Description,
    'ItemizedExpenses' => \@ItemizedExpenses,
    'ItemizedTotal'    => $ItemizedTotal,
    'MealsReport'      => \@MealsReport,
    'CategoryReport'   => \@CategoryReport,
    'ItemizedReceipts' => \@ItemizedReceipts,
    'ImageDir'         => $ImageDir,
    'Anonymize'        => $Anonymize
};

#### $TTVars

my $LatexTemplate = <<EOF;
[% USE format %][% ToDollars = format('\\\$%.2f') %]
%%%%%%%%%%% Header

\\documentclass[10pt,letterpaper]{article}
\\usepackage[letterpaper,includeheadfoot,top=0.5in,bottom=0.5in,left=0.75in,right=0.75in]{geometry}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[scaled]{helvet}
\\renewcommand*\\familydefault{\\sfdefault}
\\usepackage{lastpage}
\\usepackage{fancyhdr}
\\usepackage{graphicx}
\\usepackage{multicol}
\\usepackage[colorlinks,linkcolor=blue]{hyperref}
\\pagestyle{fancy}
\\renewcommand{\\headrulewidth}{1pt}
\\renewcommand{\\footrulewidth}{0.5pt}
\\geometry{headheight=48pt}


\\begin{document}

%%%%%%%%%% Itemized table

\\section{Itemized Expenses}
[% FOREACH Expense IN ItemizedExpenses %]
[% IF loop.first() or ( ( loop.count mod 20 ) == 0 ) %]
\\begin{tabular}{|l|l|p{2in}|r|p{2in}|}
\\hline
\\hline
\\bf Date & \\bf Category & \\bf Expense & \\bf Amount & \\bf Notes \\\\
\\hline \\hline
[% END %]
[% IF Expense.Italics %]\\it[% END %] [% Expense.Date %] & [% IF Expense.Italics %]\\it[% END %] [% Expense.Category %] & [% IF Expense.Italics %]\\it[% END %] [% Expense.Vendor %] & [% IF Expense.Italics %]\\it[% END %] [% ToDollars(Expense.Amount) %] & [% IF Expense.Italics %]\\it[% END %] [% Expense.Note %] \\\\ \\hline
[% IF loop.last() %]
\\hline
 & & \\bf Total & \\bf [% ToDollars(ItemizedTotal) %] &  \\\\
[% END %]
[% IF ( ( (loop.count + 1) mod 20 ) == 0 ) or loop.last() %]
\\hline
\\hline
\\end{tabular}
[% IF ( ( (loop.count + 1) mod 20 ) == 0 ) %]\\newline {\\it Continued on next page...}[% END %]
\\newpage
[% END %]
[% END %]

[% IF ! Internal %][% IF ! SuppressMeals %]
%%%%%%%%%% Meals summary table

\\section{Meals Summary By Day}

\\begin{tabular}{|l|l|p{2in}|p{2in}|}
\\hline
\\hline
\\bf DOW & \\bf Daily Total & \\bf Running Total \\\\
\\hline \\hline
[% FOREACH Meal IN MealsReport %]
[% Meal.DOW %] & [% ToDollars(Meal.Amount) %] & [% ToDollars(Meal.RunningTotal) %] \\\\ \\hline
[% END %]
\\hline
\\hline
\\end{tabular}
[% END %][% END %]

%%%%%%%%%% Category summary

\\section{Expenses Summary}

\\begin{tabular}{|l|l|}
\\hline
\\hline
\\bf Category & \\bf Total \\\\
\\hline \\hline
[% FOREACH Category IN CategoryReport %]
[% Category.Category %] & [% ToDollars(Category.Amount) %] \\\\ \\hline
[% END %]
\\hline
\\hline
\\end{tabular}


%%%%%%%%%% Begin receipts

\\section{Scanned Receipts}

[% FOREACH Receipt IN ItemizedReceipts %]
\\subsection{[% Receipt.Date %] [% Receipt.Vendor %]: [% ToDollars(Receipt.Amount) %]}
[% FOREACH Image IN Receipt.Images %]
\\includegraphics[angle=90,width=\\textwidth,keepaspectratio]{[% ImageDir %]/[% Image %]} \\\\
[% END %]

[% END %]


%%%%%%%%%% Footer

\\end{document}
EOF

my $LatexFN = $ExpenseReportCode . "-" . $ProjectCode . ".tex";
### $LatexFN

$TT->process( \$LatexTemplate, $TTVars, "./tmp/" . $LatexFN ) || do {
  my $error = $TT->error();
  print "error type: ", $error->type(), "\n";
  print "error info: ", $error->info(), "\n";
  die $error;
};


my $LatexOutput = `pdflatex -interaction batchmode -output-directory ./tmp "$LatexFN"`;
### $LatexOutput

   $LatexOutput = `pdflatex -interaction batchmode -output-directory ./tmp "$LatexFN"`;
### $LatexOutput

if ($ViewAfter) {
  my $ViewFN = $LatexFN;
  $ViewFN =~ s/\.tex$/.pdf/;
  `acroread "./tmp/$ViewFN"`;
}

