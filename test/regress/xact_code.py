from lpy import core

for post in core.read_journal('test/regress/xact_code.dat').query('expenses'):
  print(post.xact.code)
