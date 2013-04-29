import ledger

for post in ledger.read_journal('test/regress/xact_code.dat').query('expenses'):
  print post.xact.code
