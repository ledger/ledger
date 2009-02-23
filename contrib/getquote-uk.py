#!/usr/bin/env python
# -*- coding: utf-8 -*-

import urllib, string, sys

def download(sym):
   url	 = "http://uk.old.finance.yahoo.com/d/quotes.csv?s="
   url += sym + "&f=sl1d1t1c1ohgv&e=.csv"
   f = urllib.urlopen(url, proxies={})
   info = f.read()
   f.close()
   fields = string.split(info, ',')
   result = float(fields[1])/100
   return result


sym = sys.argv[1]
sym = sym.replace('_', '.')
if sym == '£':
   print '£1.00'
else:
   try: print "£" +str(download(sym))
   except: pass
