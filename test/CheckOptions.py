#!/usr/bin/env python3

import re
import os
import sys
import shlex
import pathlib
import argparse
import subprocess

from os.path import *
from subprocess import Popen, PIPE

class CheckOptions (object):
  @staticmethod
  def parser():
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument('-l', '--ledger', type=pathlib.Path, required=True,
        help='the path to the ledger executable to test with')
    parser.add_argument('-s', '--source', type=pathlib.Path, required=True,
        help='the path to the top level ledger source directory')
    return parser

  def __init__(self, args):
    self.option_pattern = None
    self.source_file = None
    self.sep = "\n  --"

    self.ledger     = os.path.realpath(args.ledger)
    self.source     = os.path.realpath(args.source)

    self.missing_options = set()
    self.unknown_options = set()
    self.missing_functions = set()
    self.unknown_functions = set()

  def find_pattern(self, filename, pattern):
    regex = re.compile(pattern)
    return {match.group(1) for match in {regex.match(line) for line in open(filename)} if match}

  def find_options(self, filename):
    return self.find_pattern(filename, self.option_pattern)

  def find_functions(self, filename):
    return self.find_pattern(filename, self.function_pattern)

  def find_alternates(self):
    command = shlex.split('grep --no-filename OPT_ALT')
    for source_file in ['session', 'report']:
      command.append(os.path.join(self.source, 'src', '%s.cc' % source_file))
    try:
      output = subprocess.check_output(command, universal_newlines=True).split('\n')
    except subprocess.CalledProcessError:
      output = ''

    regex = re.compile(r'OPT_ALT\([^,]*,\s*([^)]+?)_?\)');
    alternates = {match.group(1).replace('_', '-') for match in {regex.search(line) for line in output} if match}
    return alternates

  def ledger_options(self):
    pipe   = Popen('%s --debug option.names parse true' %
        self.ledger, shell=True, stdout=PIPE, stderr=PIPE)
    regex = re.compile(r'\[DEBUG\]\s+Option:\s+(.*?)_?$')
    ledger_options = {match.group(1).replace('_', '-') for match in {regex.search(line.decode()) for line in pipe.stderr.readlines()} if match}
    return ledger_options

  def ledger_functions(self):
    command = shlex.split('grep --no-filename fn_ %s' % (os.path.join(self.source, 'src', 'report.h')))
    try:
      output = subprocess.check_output(command, universal_newlines=True).split('\n');
    except subprocess.CalledProcessError:
      output = ''

    regex = re.compile(r'fn_([^(]+)\(');
    functions = {match.group(1) for match in {regex.search(line) for line in output} if match}
    return functions

  def main(self):
    options = self.find_options(self.source_file)
    for option in self.ledger_options():
      if option not in options:
          self.missing_options.add(option)
      else:
          options.remove(option)
    known_alternates = self.find_alternates()
    self.unknown_options = options - known_alternates
    self.missing_options -= {
        # The option --explicit is a no-op as of March 2020 and is
        # therefore intentionally undocumented.
        # For details see commit 43b07fbab3b4c144eca4a771524e59c531ffa074
        'explicit'
        }

    functions = self.find_functions(self.source_file)
    for function in self.ledger_functions():
      if function not in functions:
          self.missing_functions.add(function)
      else:
          functions.remove(function)
    known_functions = {'tag', 'has_tag', 'meta', 'has_meta'}
    self.unknown_functions = functions - known_functions

    if len(self.missing_options):
      print("Missing %s option entries for:%s%s\n" % (self.source_type, self.sep, self.sep.join(sorted(list(self.missing_options)))))
    if len(self.unknown_options):
      print("%s entry for unknown options:%s%s\n" % (self.source_type, self.sep, self.sep.join(sorted(list(self.unknown_options)))))
    if len(self.missing_functions):
      print("Missing %s function entries for:%s%s\n" % (self.source_type, '\n  ', '\n  '.join(sorted(list(self.missing_functions)))))
    if len(self.unknown_functions):
      print("%s entry for unknown functions:%s%s\n" % (self.source_type, '\n  ', '\n  '.join(sorted(list(self.unknown_functions)))))
    errors = len(self.missing_options) + len(self.unknown_options) + len(self.missing_functions) + len(self.unknown_functions)
    return errors
