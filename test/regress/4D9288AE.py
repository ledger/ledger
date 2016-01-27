from __future__ import print_function

import ledger

for post in ledger.read_journal("test/regress/4D9288AE.dat").query("^expenses:"):
    print(post.cost)
