#!/usr/bin/env python

from distutils.core import setup, Extension

import os
import string

defines = [('PYTHON_MODULE', 1)]
if os.environ.has_key("DEBUG_LEVEL"):
    defines.extend ([('DEBUG_LEVEL', os.environ["DEBUG_LEVEL"])])

libs = os.environ["PYLIBS"].split()

setup(name         = "Ledger",
      version      = "3.0",
      description  = "Ledger Accounting Library",
      author       = "John Wiegley",
      author_email = "johnw@newartisans.com",
      url          = "http://johnwiegley.com/",
      ext_modules  = [
	Extension("ledger", ["pyledger.cc"],
		  define_macros = defines, libraries = libs)])
