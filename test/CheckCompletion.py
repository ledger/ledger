#!/usr/bin/env python3

import re
import sys
import argparse

from os.path import join

from CheckOptions import CheckOptions

class CheckCompletion (CheckOptions):
  def __init__(self, args):
    CheckOptions.__init__(self, args)
    self.source_file = join(self.source, 'contrib', 'ledger-completion.bash')
    self.source_type = 'bash completion'

  def find_options(self, filename):
    regex = re.compile(r'^\s*options="([^"]*)"')
    with open(filename) as f:
      for line in f:
        match = regex.match(line)
        if match:
          return {option.lstrip('-').rstrip('=') for option in match.group(1).split()}
    return set()

  # CheckOptions.main also checks the fn_ functions of src/report.h, which a
  # completion list does not offer, so this main compares options only.
  def main(self):
    listed = self.find_options(self.source_file)
    options = self.ledger_options()
    self.missing_options = options - listed
    self.unknown_options = listed - options - self.find_alternates()
    if self.missing_options:
      print("Missing %s option entries for:%s%s\n" % (self.source_type, self.sep, self.sep.join(sorted(self.missing_options))))
    if self.unknown_options:
      print("%s entry for unknown options:%s%s\n" % (self.source_type, self.sep, self.sep.join(sorted(self.unknown_options))))
    return len(self.missing_options) + len(self.unknown_options)

if __name__ == "__main__":
  args = argparse.ArgumentParser(prog='CheckCompletion',
                                 description='Check that the bash completion offers every ledger option',
                                 parents=[CheckCompletion.parser()]).parse_args()
  script = CheckCompletion(args)
  status = script.main()
  sys.exit(1 if status else 0)
