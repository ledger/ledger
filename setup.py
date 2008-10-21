#!/usr/bin/env python

from distutils.core import setup, Extension

import os

defines = [('PYTHON_MODULE', 1)]
libs    = ["amounts", "boost_python", "gmp"]

setup(name         = "Amounts",
      version      = "2.6.2",
      description  = "Amounts and Commodities Library",
      author       = "John Wiegley",
      author_email = "johnw@newartisans.com",
      url          = "http://www.newartisans.com/johnw/",
      ext_modules  = [
    Extension("amounts", [os.path.join(os.environ['SRCDIR'], "amounts.cc")],
	      define_macros = [('PYTHON_MODULE', 1)],
	      libraries     = libs)])
