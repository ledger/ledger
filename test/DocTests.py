#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import re
import sys
import hashlib
import argparse
import subprocess

from difflib import unified_diff

class DocTests:
  def __init__(self, args):
    scriptpath      = os.path.dirname(os.path.realpath(__file__))
    self.ledger     = os.path.abspath(args.ledger)
    self.sourcepath = os.path.abspath(args.file)
    self.verbose    = args.verbose

    self.examples      = dict()
    self.test_files    = list()
    self.testin_token  = 'command'
    self.testout_token = 'output'
    self.testdat_token = 'input'
    self.validate_token = 'validate'
    self.testwithdat_token = 'with_input'

  def read_example(self):
    endexample = re.compile(r'^@end\s+smallexample\s*$')
    example = str()
    while True:
      line = self.file.readline()
      self.current_line += 1
      if len(line) <= 0 or endexample.match(line): break
      example += line.replace("@@","@").replace("@{","{").replace("@}","}")
    return example

  def test_id(self, example):
    return hashlib.sha1(example.rstrip()).hexdigest()[0:7].upper()

  def find_examples(self):
    startexample = re.compile(r'^@smallexample\s+@c\s+(%s|%s|%s)(?::([\dA-Fa-f]+|validate))?(?:,(.*))?'
        % (self.testin_token, self.testout_token, self.testdat_token))
    while True:
      line = self.file.readline()
      self.current_line += 1
      if len(line) <= 0: break

      startmatch = startexample.match(line)
      if (startmatch):
        test_begin_pos = self.file.tell()
        test_begin_line = self.current_line
        test_kind = startmatch.group(1)
        test_id = startmatch.group(2)
        test_options = dict()
        for pair in re.split(r',\s*', str(startmatch.group(3))):
          kv = re.split(r':\s*', pair, 2)
          try:
            test_options[kv[0]] = kv[1]
          except IndexError:
            pass
        example = self.read_example()
        test_end_pos = self.file.tell()
        test_end_line = self.current_line

        if not test_id:
          print >> sys.stderr, 'Example', test_kind, 'in line', test_begin_line, 'is missing id.'
          test_id = self.test_id(example)
          if test_kind == self.testin_token:
            print >> sys.stderr, 'Use', self.test_id(example)
        elif test_kind == self.testin_token and test_id != self.validate_token and test_id != self.test_id(example):
          print >> sys.stderr, 'Expected test id', test_id, 'for example' \
              , test_kind, 'on line', test_begin_line, 'to be', self.test_id(example)

        if test_id == self.validate_token:
          test_id = "Val-" + str(test_begin_line)
          if test_kind == self.testin_token:
            test_kind = "validate-command"
          elif test_kind == self.testdat_token:
            test_kind = "validate-data"
        try:
          self.examples[test_id]
        except KeyError:
          self.examples[test_id] = dict()

        try:
          example = self.examples[test_id][test_kind][test_kind] + example
        except KeyError:
          pass

        self.examples[test_id][test_kind] = {
            'bpos': test_begin_pos,
            'epos': test_end_pos,
            'blin': test_begin_line,
            'elin': test_end_line,
            'opts': test_options,
            test_kind: example,
            }

  def parse_command(self, test_id, example):
    validate_command = False
    try:
      command = example[self.testin_token][self.testin_token]
    except KeyError:
      if 'validate-data' in example:
        command = '$ ledger bal'
      elif 'validate-command' in example:
        validate_command = True
        command = example['validate-command']['validate-command']
      else:
        return None

    command = command.rstrip().split()
    if command[0] == '$': command.remove('$')
    index = command.index('ledger')
    command[index] = self.ledger
    command.insert(index+1, '--init-file')
    command.insert(index+2, '/dev/null')
    try:
      findex = command.index('-f')
    except ValueError:
      try:
        findex = command.index('--file')
      except ValueError:
        findex = index+1
        command.insert(findex, '--file')
        if validate_command:
          command.insert(findex+1, 'sample.dat')
        else:
          command.insert(findex+1, test_id + '.dat')
    return (command, findex+1)

  def test_examples(self):
    failed = set()
    for test_id in self.examples:
      validation = False
      if "validate-data" in self.examples[test_id] or "validate-command" in self.examples[test_id]:
        validation = True
      example = self.examples[test_id]
      try:
        (command, findex) = self.parse_command(test_id, example)
      except TypeError:
        failed.add(test_id)
        continue

      try:
        output = example[self.testout_token][self.testout_token]
      except KeyError:
        output = None

      try:
        input = example[self.testdat_token][self.testdat_token]
      except KeyError:
        try:
          with_input = example[self.testin_token]['opts'][self.testwithdat_token]
          input = self.examples[with_input][self.testdat_token][self.testdat_token]
        except KeyError:
          try:
            input = example['validate-data']['validate-data']
          except KeyError:
            input = None

      if command and (output or validation):
        test_file_created = False
        if findex:
          scriptpath = os.path.dirname(os.path.realpath(__file__))
          test_input_dir = scriptpath + '/../test/input/'
          test_file = command[findex]
          if not os.path.exists(test_file):
            if input:
              test_file_created = True
              with open(test_file, 'w') as f:
                f.write(input)
            elif os.path.exists(test_input_dir + test_file):
              command[findex] = test_input_dir + test_file
        error = False
        try:
          verify = subprocess.check_output(command)
        except:
          verify = str()
          error = True
        valid = (output == verify) or (not error and validation)
        if valid and test_file_created:
          os.remove(test_file)
        if self.verbose > 0:
          print test_id, ':', 'Passed' if valid else 'FAILED'
        else:
          sys.stdout.write('.' if valid else 'E')

        if not valid:
          failed.add(test_id)
          if self.verbose > 1:
            print ' '.join(command)
            if not validation:
              for line in unified_diff(output.split('\n'), verify.split('\n'), fromfile='generated', tofile='expected'):
                print(line)
              print
    if not self.verbose:
      print
    if len(failed) > 0:
      print "\nThe following examples failed:"
      print " ", "\n  ".join(failed)
    return len(failed)

  def main(self):
    self.file = open(self.sourcepath)
    self.current_line = 0
    self.find_examples()
    failed_examples = self.test_examples()
    self.file.close()
    return failed_examples

if __name__ == "__main__":
  def getargs():
    parser = argparse.ArgumentParser(description='DocTests', prefix_chars='-')
    parser.add_argument('-v', '--verbose',
        dest='verbose',
        action='count',
        help='be verbose. Add -vv for more verbosity')
    parser.add_argument('-l', '--ledger',
        dest='ledger',
        type=str,
        action='store',
        required=True,
        help='the path to the ledger executable to test with')
    parser.add_argument('-f', '--file',
        dest='file',
        type=str,
        action='store',
        required=True,
        help='the texinfo documentation file to run the examples from')
    return parser.parse_args()

  args = getargs()
  script = DocTests(args)
  status = script.main()
  sys.exit(status)
