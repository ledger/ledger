#!/usr/bin/env python

from distutils.core import setup, Extension

#setup(name         = "Amounts",
#      version      = "2.0b",
#      description  = "Amounts Library",
#      author       = "John Wiegley",
#      author_email = "johnw@newartisans.com",
#      url          = "http://www.newartisans.com/johnw/",
#      ext_modules  = [
#    Extension("amounts", ["pyamounts.cc"],
#	      define_macros = [('PYTHON_MODULE', None)],
#	      libraries     = ["ledger_bpy", "boost_python", "gmp"])])

setup(name         = "Ledger",
      version      = "2.0b",
      description  = "Ledger Accounting Tool",
      author       = "John Wiegley",
      author_email = "johnw@newartisans.com",
      url          = "http://www.newartisans.com/johnw/",
      ext_modules  = [
    Extension("ledger", ["pyledger.cc"],
	      define_macros = [('PYTHON_MODULE', None)],
	      libraries     = ["ledger_bpy", "boost_python", "gmp",
			       "pcre", "xmlparse", "xmltok"])])
