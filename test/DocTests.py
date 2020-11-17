#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function, unicode_literals
from io import open

import os
import re
import sys
import shlex
import locale
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
    self.tests      = args.examples

    self.examples       = dict()
    self.test_files     = list()
    self.testin_token   = 'command'
    self.testout_token  = 'output'
    self.testdat_token  = 'input'
    self.testfile_token = 'file'
    self.validate_token = 'validate'
    self.validate_cmd_token = 'validate-command'
    self.validate_dat_token = 'validate-data'
    self.testwithdat_token  = 'with_input'
    self.testwithfile_token = 'with_file'

  def read_example(self):
    endexample = re.compile(r'^@end\s+smallexample\s*$')
    example = str()
    while True:
      line = self.file.readline()
      self.current_line += 1
      if len(line) <= 0 or endexample.match(line): break
      # Replace special texinfo character sequences with their ASCII counterpart
      example += re.sub(r'@([@{}])', r'\1', line)
    return example

  def test_id(self, example):
    example_id = example.rstrip().encode('utf-8')
    return hashlib.sha1(example_id).hexdigest()[0:7].upper()

  def find_examples(self):
    startexample = re.compile(r'^@smallexample\s+@c\s+(%s|%s|%s|%s)(?::([\dA-Fa-f]+|validate))?(?:,(.*))?'
        % (self.testin_token, self.testout_token, self.testdat_token, self.testfile_token))
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
          print('Example', test_kind, 'in line', test_begin_line, 'is missing id.', file=sys.stderr)
          test_id = self.test_id(example)
          if test_kind == self.testin_token:
            print('Use', self.test_id(example), file=sys.stderr)
        elif test_kind == self.testin_token and test_id != self.validate_token and test_id != self.test_id(example):
          print('Expected test id', test_id, 'for example' \
              , test_kind, 'on line', test_begin_line, 'to be', self.test_id(example), file=sys.stderr)

        if test_id == self.validate_token:
          test_id = "Val-" + str(test_begin_line)
          if test_kind == self.testin_token:
            test_kind = self.validate_cmd_token
          elif test_kind == self.testdat_token:
            test_kind = self.validate_dat_token
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
      command = re.sub(r'\\\n', '', command)
    except KeyError:
      if self.validate_dat_token in example:
        command = '$ ledger bal'
      elif self.validate_cmd_token in example:
        validate_command = True
        command = example[self.validate_cmd_token][self.validate_cmd_token]
      else:
        return None

    if sys.version_info.major == 2:
      command = command.encode(locale.getpreferredencoding())
    command_parts = shlex.split(command)
    command = filter(lambda x: x != '\n', command_parts)
    if sys.version_info.major > 2:
        command = list(command)
    if command[0] == '$': command.remove('$')
    index = command.index('ledger')
    command[index] = self.ledger
    for i,argument in enumerate(shlex.split('--args-only --columns 80')):
      command.insert(index+i+1, argument)

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
    tests = self.examples.keys()
    if self.tests:
      tests = list(set(self.tests).intersection(tests))
      temp = list(set(self.tests).difference(tests))
      if len(temp) > 0:
        print('Skipping non-existent examples: %s' % ', '.join(temp), file=sys.stderr)

    for test_id in tests:
      validation = False
      if self.validate_dat_token in self.examples[test_id] or self.validate_cmd_token in self.examples[test_id]:
        validation = True
      example = self.examples[test_id]
      try:
        (command, findex) = self.parse_command(test_id, example)
      except TypeError:
        failed.add(test_id)
        continue

      output = example.get(self.testout_token, {}).get(self.testout_token)
      input = example.get(self.testdat_token, {}).get(self.testdat_token)
      if not input:
        with_input = example.get(self.testin_token, {}).get('opts', {}).get(self.testwithdat_token)
        input = self.examples.get(with_input, {}).get(self.testdat_token, {}).get(self.testdat_token)
        if not input:
          input = example.get(self.validate_dat_token, {}).get(self.validate_dat_token)

      if command and (output != None or validation):
        test_file_created = False
        if findex:
          scriptpath = os.path.dirname(os.path.realpath(__file__))
          test_input_dir = os.path.join(scriptpath, '..', 'test', 'input')
          test_file = command[findex]
          if not os.path.exists(test_file):
            if input:
              test_file_created = True
              with open(test_file, 'w', encoding='utf-8') as f:
                f.write(input)
            elif os.path.exists(os.path.join(test_input_dir, test_file)):
              command[findex] = os.path.join(test_input_dir, test_file)
        try:
          convert_idx  = command.index(str('convert'))
          convert_file = command[convert_idx+1]
          convert_data = example[self.testfile_token][self.testfile_token]
          if not os.path.exists(convert_file):
              with open(convert_file, 'w', encoding='utf-8') as f:
                f.write(convert_data)
        except ValueError:
         pass
        error = None
        try:
          verify = subprocess.check_output(command, stderr=subprocess.STDOUT)
          verify = verify.decode('utf-8')
          if sys.platform == 'win32':
            verify = verify.replace('\r\n', '\n')
          valid = (output == verify) or (not error and validation)
        except subprocess.CalledProcessError as e:
          error = e.output
          valid = False
          failed.add(test_id)
        if valid and test_file_created:
          os.remove(test_file)
        if self.verbose > 0:
          print(test_id, ':', 'Passed' if valid else 'FAILED: {}'.format(error) if error else 'FAILED')
        else:
          sys.stdout.write('.' if valid else 'E')
          sys.stdout.flush()

        if not (valid or error):
          failed.add(test_id)
          if self.verbose > 1:
            print(' '.join(command))
            if not validation:
              for line in unified_diff(output.split('\n'), verify.split('\n'), fromfile='generated', tofile='expected'):
                print(line)
              print()
      else:
        if self.verbose > 0:
          print(test_id, ':', 'Skipped')
        else:
          sys.stdout.write('X')

    if self.verbose == 0:
      print()
    if len(failed) > 0:
      print("\nThe following examples failed:")
      print(" ", "\n  ".join(failed))
    return len(failed)

  def main(self):
    self.file = open(self.sourcepath, encoding='utf-8')
    self.current_line = 0
    self.find_examples()
    failed_examples = self.test_examples()
    self.file.close()
    return failed_examples

if __name__ == "__main__":
  def getargs():
    parser = argparse.ArgumentParser(prog='DocTests',
            description='Test and validate ledger examples from the texinfo manual')
    parser.add_argument('-v', '--verbose',
        dest='verbose',
        action='count',
        default=0,
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
    parser.add_argument('examples',
        metavar='EXAMPLE',
        type=str,
        nargs='*',
        help='the examples to test')
    return parser.parse_args()

  args = getargs()
  script = DocTests(args)
  status = script.main()
  sys.exit(status)
