from amounts import *

x = Amount ("$123.45")
print x
x = x * 2
print x

y = Amount ("$1000.45")
print x + y

z = Value ("$1000.45")
print y + z

z += x
print z
