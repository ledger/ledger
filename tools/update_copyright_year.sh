#!/bin/sh

# update_copyright_year - Update the year of the Copyright statement in files
#
# This script will replace the last year of Copyright statements with the first
# argument of this script (defaulting to the current year).

# Copyright (c) 2016 Alexis Hildebrandt
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

YEAR=${1:-$(date +%Y)}
# egrep is rather slow, but the much faster ag (the-silver-searcher)
# is not generally installed
GREP=${2:-egrep}

${GREP} -Rl 'Copyright.*Wiegley' $(git ls-files | cut -d / -f1 | uniq) \
  | ${GREP} -v "(test/garbage-input.dat|$(basename $0))" \
  | xargs sed -i '' -e "s/\(Copyright.*\)-20[0-9]\{2\}/\1-${YEAR}/"

