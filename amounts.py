from amounts import *

x = Amount ("$123.45")
print x
x = x * 2
print x

y = Amount ("$1000.45")
print x + y

y = Amount ("$1000.45")
print x * y

z = Value ("$1000.45")
#print z + Value(y)

z += Value(x)
print z

bal = Balance (x)
bal += Amount ("100.8 DM")

print "balance:", bal
for amt in bal:
    print "  amount:", amt
