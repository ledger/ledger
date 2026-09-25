from lpy import core

for post in core.read_journal("test/regress/4D9288AE.dat").query("^expenses:"):
    print(post.cost)
