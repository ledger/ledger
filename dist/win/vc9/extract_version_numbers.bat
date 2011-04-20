set target_file=config.h
set /p version_file_contents=<..\..\..\version.m4
set ledger_version_nr=%version_file_contents:m4_define([VERSION_NUMBER], [=%
set ledger_version_nr=%ledger_version_nr:])=%
echo #pragma once> %target_file%
echo #define PACKAGE_VERSION "%ledger_version_nr%">> %target_file%
echo #define VERSION "%ledger_version_nr%">> %target_file%
