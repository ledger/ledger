#! /usr/bin/perl

$release_files = 'source/utf8.h  source/utf8/core.h source/utf8/checked.h source/utf8/unchecked.h doc/utf8cpp.html doc/ReleaseNotes';

# First get the latest version
`svn update`;

# Then construct the name of the zip file
$argc = @ARGV;
if ($argc > 0) {
    $zip_name = $ARGV[0];
}
else {
    $zip_name = "utf8";
}

# Zip the files to an archive
`zip $zip_name $release_files`; 
