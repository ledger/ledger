#!/bin/zsh

git reset --hard

rm -f src/system.hh.gch ledger

for file in *(.) contrib/*(.) doc/*(.) plan/*(.) python/*(.) src/*(.) test/*(.) test/*/*(.) tools/*(.)
do
    echo Renaming items in $file ...

    perl -i -pe 's/xact/fazole/g;' $file 2> /dev/null
    perl -i -pe 's/XACT/FAZOLE/g;' $file 2> /dev/null
    perl -i -pe 's/Xact/Fazole/g;' $file 2> /dev/null
    perl -i -pe 's/xacts/fazoles/g;' $file 2> /dev/null
    perl -i -pe 's/XACTS/FAZOLES/g;' $file 2> /dev/null
    perl -i -pe 's/Xacts/Fazoles/g;' $file 2> /dev/null

    perl -i -pe 's/transaction/brazole/g;' $file 2> /dev/null
    perl -i -pe 's/TRANSACTION/BRAZOLE/g;' $file 2> /dev/null
    perl -i -pe 's/Transaction/Brazole/g;' $file 2> /dev/null
    perl -i -pe 's/transactions/brazoles/g;' $file 2> /dev/null
    perl -i -pe 's/TRANSACTIONS/BRAZOLES/g;' $file 2> /dev/null
    perl -i -pe 's/Transactions/Brazoles/g;' $file 2> /dev/null

    perl -i -pe 's/entry/xact/g;' $file 2> /dev/null
    perl -i -pe 's/ENTRY/XACT/g;' $file 2> /dev/null
    perl -i -pe 's/Entry/Xact/g;' $file 2> /dev/null
    perl -i -pe 's/entries/xacts/g;' $file 2> /dev/null
    perl -i -pe 's/ENTRIES/XACTS/g;' $file 2> /dev/null
    perl -i -pe 's/Entries/Xacts/g;' $file 2> /dev/null
    perl -i -pe 's/entrys/xacts/g;' $file 2> /dev/null
    perl -i -pe 's/ENTRYS/XACTS/g;' $file 2> /dev/null
    perl -i -pe 's/Entrys/Xacts/g;' $file 2> /dev/null

    perl -i -pe 's/fazoles/posts/g;' $file 2> /dev/null
    perl -i -pe 's/FAZOLES/POSTS/g;' $file 2> /dev/null
    perl -i -pe 's/Fazoles/Posts/g;' $file 2> /dev/null
    perl -i -pe 's/fazole/post/g;' $file 2> /dev/null
    perl -i -pe 's/FAZOLE/POST/g;' $file 2> /dev/null
    perl -i -pe 's/Fazole/Post/g;' $file 2> /dev/null

    perl -i -pe 's/brazoles/postings/g;' $file 2> /dev/null
    perl -i -pe 's/BRAZOLES/POSTINGS/g;' $file 2> /dev/null
    perl -i -pe 's/Brazoles/Postings/g;' $file 2> /dev/null
    perl -i -pe 's/brazole/posting/g;' $file 2> /dev/null
    perl -i -pe 's/BRAZOLE/POSTING/g;' $file 2> /dev/null
    perl -i -pe 's/Brazole/Posting/g;' $file 2> /dev/null

    perl -i -pe 's/\@dirxact/\@direntry/g;' $file 2> /dev/null
    perl -i -pe 's/\@end dirxact/\@end direntry/g;' $file 2> /dev/null
done

mv src/xact.h src/fazole.h
mv src/xact.cc src/fazole.cc
mv src/entry.h src/xact.h
mv src/entry.cc src/xact.cc
mv src/fazole.h src/post.h
mv src/fazole.cc src/post.cc

mv src/py_xact.py src/fazole.py
mv src/py_entry.py src/py_xact.py
mv src/fazole.py src/py_post.py

ln -sf ~/Products/ledger/ledger .

tools/myacprep

