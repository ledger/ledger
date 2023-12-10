#!/usr/bin/env python3

import urllib, string, sys, os

def download(sym):
   url	 = "http://uk.old.finance.yahoo.com/d/quotes.csv?s="
   url += sym + "&f=sl1d1t1c1ohgv&e=.csv"
   f = urllib.urlopen(url, proxies={})
   info = f.read()
   f.close()
   fields = string.split(info, ',')
   result = float(fields[1])/100
   return result

if len(sys.argv) == 1:
  print(f'USAGE: {os.path.basename(__file__)} SYMBOL', file=sys.stderr)
  sys.exit(-1)
sym = sys.argv[1]
sym = sym.replace('_', '.')
if sym == '£':
   print('£1.00')
else:
   try: print(f'£ {str(download(sym))}')
   except: pass
