#!/usr/bin/env python3

import sys
import re
import os
import argparse

from os.path import *
from subprocess import Popen, PIPE

from CheckOptions import CheckOptions

class CheckManpage (CheckOptions):
  def __init__(self, args):
    CheckOptions.__init__(self, args)
    self.option_pattern = r'^\.It Fl \\-([-A-Za-z]+)'
    self.function_pattern = r'^\.It Fn ([-A-Za-z_]+)'
    self.symbol_pattern = r'^\.It Sy ([-A-Za-z_]+)'
    self.source_file = join(self.source, 'doc', 'ledger.1')
    self.source_type = 'manpage'

if __name__ == "__main__":
  args = argparse.ArgumentParser(prog='CheckManpage',
                                 description='Check that ledger options are documented in the manpage',
                                 parents=[CheckManpage.parser()]).parse_args()
  script = CheckManpage(args)
  status = script.main()
  sys.exit(status)
