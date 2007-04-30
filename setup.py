#!/usr/bin/env python

from distutils.core import setup, Extension

import os

defines = [('PYTHON_MODULE', 1)]
libs    = os.environ["PYLIBS"].split()

setup(name         = "Ledger",
      version      = "3.0",
      description  = "Ledger Accounting Library",
      author       = "John Wiegley",
      author_email = "johnw@newartisans.com",
      url          = "http://johnwiegley.com/",
      ext_modules  = [
	Extension("ledger",
                  [os.path.join(os.environ['SRCDIR'], "src", "pyledger.cc")],
		  define_macros = defines, libraries = libs)])
