#!/usr/bin/env python

from distutils.core import setup, Extension

import os

libs = ["amounts", "boost_python", "gmp"]

setup(name         = "Amounts",
      version      = "3.0",
      description  = "Amounts and Commodities Library",
      author       = "John Wiegley",
      author_email = "johnw@newartisans.com",
      url          = "http://johnwiegley.com/",
      ext_modules  = [
    Extension("amounts", ["amounts.cc"],
	      define_macros = [('PYTHON_MODULE', 1)],
	      libraries     = libs)])
