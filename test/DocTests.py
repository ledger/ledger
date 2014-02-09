#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import re
import sys
import hashlib
import subprocess

from difflib import unified_diff

class DocTests:
  def __init__(self, argv):
    if not os.path.isfile(argv[1]):
        print "Cannot find ledger at '%s'" % argv[1]
        sys.exit(1)
    if not os.path.isfile(argv[2]):
        print "Cannot find source path at '%s'" % argv[2]
        sys.exit(1)

    self.ledger     = os.path.abspath(argv[1])
    self.sourcepath = os.path.abspath(argv[2])
    scriptpath = os.path.dirname(os.path.realpath(__file__))
    self.verbose = False
    self.debug = False
    self.examples = dict()
    self.testin_token = 'command'
    self.testout_token = 'output'
    self.testdat_token = 'input'
    self.test_files = list()

  def read_example(self):
    endexample = re.compile(r'^@end\s+smallexample\s*$')
    example = str()
    while True:
      line = self.file.readline()
      self.current_line += 1
      if len(line) <= 0 or endexample.match(line): break
      example += line
    return example

  def test_id(self, example):
    return hashlib.sha1(example.rstrip()).hexdigest()[0:7].upper()

  def find_examples(self):
    startexample = re.compile(r'^@smallexample\s+@c\s+(%s|%s|%s)(?::([\dA-Fa-f]+))?'
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
        example = self.read_example()
        test_end_pos = self.file.tell()
        test_end_line = self.current_line

        if not test_id:
          print >> sys.stderr, 'Example', test_kind, 'in line', test_begin_line, 'is missing id.'
          test_id = self.test_id(example)
          if test_kind == self.testin_token:
            print >> sys.stderr, 'Use', self.test_id(example)
        elif test_kind == self.testin_token and test_id != self.test_id(example):
          print >> sys.stderr, 'Expected test id', test_id, 'for example' \
              , test_kind, 'on line', test_begin_line, 'to be', self.test_id(example)

        try:
          self.examples[test_id]
        except KeyError:
          self.examples[test_id] = dict()

        self.examples[test_id][test_kind] = {
            'bpos': test_begin_pos,
            'epos': test_end_pos,
            'blin': test_begin_line,
            'elin': test_end_line,
            test_kind: example,
            }

  def test_examples(self):
    failed = set()
    for test_id in self.examples:
      example = self.examples[test_id]
      try:
        command = example[self.testin_token][self.testin_token]
      except KeyError:
        command = None

      try:
        output = example[self.testout_token][self.testout_token]
      except KeyError:
        output = None

      try:
        input = example[self.testdat_token][self.testdat_token]
      except KeyError:
        input = None

      if command and output:
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
            command.insert(findex+1, test_id + '.dat')
            
        if findex:
          scriptpath = os.path.dirname(os.path.realpath(__file__))
          test_input_dir = scriptpath + '/../test/input/'
          test_file = command[findex+1]
          test_file_created = False
          if not os.path.exists(test_file):
            if input:
              test_file_created = True
              with open(test_file, 'w') as f:
                f.write(input)
            elif os.path.exists(test_input_dir + test_file):
              command[findex+1] = test_input_dir + test_file
        try:
          verify = subprocess.check_output(command)
        except:
          verify = str()
        if test_file_created:
          os.remove(test_file)
        valid = (output == verify)
        if self.verbose:
          print test_id, ':', 'Passed' if valid else 'FAILED'
        else:
          sys.stdout.write('.' if valid else 'E')

        if not valid:
          if self.debug:
          failed.add(test_id)
            print ' '.join(command)
            for line in unified_diff(output.split('\n'), verify.split('\n'), fromfile='generated', tofile='expected'):
              print(line)
            print
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
  script = DocTests(sys.argv)
  status = script.main()
  sys.exit(status)
