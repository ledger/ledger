#!/usr/bin/env python

from distutils.core import setup, Extension

import os

defines = [('PYTHON_MODULE', 1)]
libs    = os.environ["PYLIBS"].split()

setup(name         = "Ledger",
      version      = "2.7",
      description  = "Ledger Accounting Library",
      author       = "John Wiegley",
      author_email = "johnw@newartisans.com",
      url          = "http://www.newartisans.com/software/ledger.html",
      ext_modules  = [
	Extension("ledger", [os.environ["SRCDIR"] + '/python/pyledger.cc'],
		  define_macros = defines, libraries = libs)])
