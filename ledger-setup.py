#!/usr/bin/env python

from distutils.core import setup, Extension

import os

libs = ["ledger", "amounts", "boost_python", "gmp", "pcre"]

if os.environ.has_key ("HAVE_EXPAT") and\
   os.environ["HAVE_EXPAT"] == "true":
    libs.extend (["expat"])

if os.environ.has_key ("HAVE_XMLPARSE") and\
   os.environ["HAVE_XMLPARSE"] == "true":
    libs.extend (["xmlparse", "xmltok"])

if os.environ.has_key ("HAVE_LIBOFX") and\
   os.environ["HAVE_LIBOFX"] == "true":
    libs.extend (["ofx"])

setup(name         = "Ledger",
      version      = "3.0",
      description  = "Ledger Accounting Library",
      author       = "John Wiegley",
      author_email = "johnw@newartisans.com",
      url          = "http://johnwiegley.com/",
      ext_modules  = [
    Extension("ledger", ["pyledger.cc"],
	      define_macros = [('PYTHON_MODULE', 1)],
	      libraries     = libs)])
