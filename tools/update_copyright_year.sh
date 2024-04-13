#!/bin/sh

# update_copyright_year - Update the year of the Copyright statement in files
#
# This script will replace the last year of Copyright statements with the first
# argument of this script (defaulting to the current year).

# Copyright (c) 2016, 2023 Alexis Hildebrandt
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

# Use ag (the-silver-searcher) when available as it is much faster than
# the venerable egrep
GREP=$(command -v ag || command -v egrep)

git ls-files -z \
  | xargs -0 ${GREP} -Rl 'Copyright.*Wiegley' \
  | uniq \
  | ${GREP} -v "(test/regress/25A099C9.dat|$(basename $0))" \
  | xargs sed -i '' -e "s/\(Copyright.*\)-20[0-9]\{2\}/\1-${YEAR}/" \
  # git ls-files | xargs grep | uniq | grep -v | xargs sed

