#! /usr/bin/perl

$report_name = './report.txt';

# Create the report file
die if !open(REPORT, ">$report_name");

# First, build everything
print REPORT "==================Make output==================\n";
close($report_name);

`make >> $report_name`;
die if !open(REPORT, ">>$report_name");
print REPORT "==================End of Make output==================\n";
print REPORT "\n";
# Now, run individual tests and create the report
print REPORT "==================Smoke Test ==================\n";
close($report_name);
chdir 'smoke_test';
`./smoketest >> ../$report_name`;
chdir '..';
die if !open(REPORT, ">>$report_name");
print REPORT "==================End of smoke test==================\n";
print REPORT "\n";
print REPORT "==================Regression Test ==================\n"; 
close($report_name);
chdir 'regression_tests';
`./regressiontest >> ../$report_name`;
chdir '..';
die if !open(REPORT, ">>$report_name");
print REPORT "==================End of regression test==================\n";
print REPORT "\n";
print REPORT "==================Negative Test ==================\n"; 
close($report_name);
chdir 'negative';
`./negative ../../test_data/negative/utf8_invalid.txt >> ../$report_name`;
chdir '..';
die if !open(REPORT, ">>$report_name");
print REPORT "==================End of negative test==================\n";
print REPORT "\n";
print REPORT "==================utf8reader runs ==================\n"; 
close($report_name);
chdir 'utf8reader';
`./utf8reader ../../test_data/utf8samples/quickbrown.txt >> ../$report_name`;
`./utf8reader ../../test_data/utf8samples/Unicode_transcriptions.html >> ../$report_name`;
`./utf8reader ../../test_data/utf8samples/UTF-8-demo.txt >> ../$report_name`;
chdir '..';
die if !open(REPORT, ">>$report_name");
print REPORT "==================End of utf8reader runs==================\n";
print REPORT "\n";
