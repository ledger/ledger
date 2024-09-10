#!/usr/bin/env python3

import sys
import re
import os
import argparse

from os.path import *
from subprocess import Popen, PIPE

from CheckOptions import CheckOptions

class CheckTexinfo (CheckOptions):
  def __init__(self, args):
    CheckOptions.__init__(self, args)
    self.option_pattern = '^@item\s+--([-A-Za-z]+)'
    self.function_pattern = '^@defun\s+([-A-Za-z_]+)'
    self.source_file = join(self.source, 'doc', 'ledger3.texi')
    self.source_type = 'texinfo'


  def find_functions(self, filename):
    functions = set()
    state_normal = 0
    state_function = 1
    state = state_normal
    function = None
    fun_doc = str()
    fun_example = False
    item_regex = re.compile(self.function_pattern)
    itemx_regex = re.compile(r'^@defunx')
    example_regex = re.compile(r'^@smallexample\s+@c\s+command:')
    fix_regex = re.compile(r'FIX')
    comment_regex = re.compile(r'^\s*@c')
    for line in open(filename):
        line = line.strip()
        if state == state_normal:
            match = item_regex.match(line)
            if match:
                state = state_function
                function = match.group(1)
        elif state == state_function:
            if line == '@end defun':
                if function and fun_example and len(fun_doc) and not fix_regex.search(fun_doc):
                    functions.add(function)
                state = state_normal
                fun_example = None
                fun_doc = str()
            elif itemx_regex.match(line):
                continue
            elif example_regex.match(line):
                fun_example = True
            elif not comment_regex.match(line):
               fun_doc += line
    return functions

  def find_options(self, filename):
    options = set()
    state_normal = 0
    state_option_table = 1
    state = state_normal
    option = None
    opt_doc = str()
    item_regex = re.compile(self.option_pattern)
    itemx_regex = re.compile(r'^@itemx')
    fix_regex = re.compile(r'FIX')
    comment_regex = re.compile(r'^\s*@c')
    for line in open(filename):
      line = line.strip()
      if state == state_normal:
        if line == '@ftable @option':
            state = state_option_table
      elif state == state_option_table:
          if line == '@end ftable':
              if option and len(opt_doc) and not fix_regex.search(opt_doc):
                  options.add(option)
              state = state_normal
              option = None
              continue
          match = item_regex.match(line)
          if match:
              if option and len(opt_doc) and not fix_regex.search(opt_doc):
                  options.add(option)
              option = match.group(1)
              opt_doc = str()
          elif itemx_regex.match(line):
              continue
          elif not comment_regex.match(line):
              opt_doc += line
    return options

if __name__ == "__main__":
  args = argparse.ArgumentParser(prog='CheckTexinfo',
                                 description='Check that ledger options are documented in the texinfo manual',
                                 parents=[CheckOptions.parser()]).parse_args()
  script = CheckTexinfo(args)
  status = script.main()
  sys.exit(status)
