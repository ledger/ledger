@echo off

rem Call the script that will read and parse version.m4, and store the version
rem number in the ledger_version_nr environment variable.
call ..\vc9\extract_version_numbers.bat
del config.h
rem Ledgers uses x.y.z-date format, but MSI needs x.y.z.b numbering. Can be
rem solved with simple string replace.
set ledger_version_nr=%ledger_version_nr:-=.%

rmdir /S /Q content
mkdir content
rem Now, gather all the files we need in a directory
copy ..\vc9\Release\ledger.exe content
copy ..\..\..\doc\ledger.pdf content
copy ..\..\..\doc\LICENSE.rtf content
copy Calculator_16x16.ico content
copy ledger.wxs content

cd content
rem Finally, call the WiX compiler & linker
"%WIX%\bin\candle.exe" ledger.wxs
"%WIX%\bin\light.exe" -ext WixUIExtension ledger.wixobj

rem Hopefully everything went well, copy the result back
copy ledger.msi ..\ledger-%ledger_version_nr%.msi

cd ..
