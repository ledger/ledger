#!/usr/bin/env python

from distutils.core import setup, Extension

import os

libs = ["ledger", "boost_python", "gmp", "pcre"]

if os.environ.has_key ("HAVE_XMLPARSE") and\
   os.environ["HAVE_XMLPARSE"] == "true":
    libs.extend (["xmlparse", "xmltok"])

setup(name         = "Ledger",
      version      = "2.0b",
      description  = "Ledger Accounting Tool",
      author       = "John Wiegley",
      author_email = "johnw@newartisans.com",
      url          = "http://www.newartisans.com/johnw/",
      ext_modules  = [
    Extension("ledger", ["pyledger.cc"],
	      define_macros = [('PYTHON_MODULE', 1)],
	      libraries     = libs)])
