#!/usr/bin/env python

from ledger import *

class FooOption(Option):
    def __init__(self):
	Option.__init__(self, "foo", False)

    def check(self, source):
	if source == OptionSource.CommandLine:
	    print "It's from the command line!"
	    return True
	return False

    def select(self, report):
	print "This is the foo option."

def anonymous(arg):
    print "Hello there:", arg
    return arg

options["foo"] = FooOption()
