#!/usr/bin/python
# csv2ods.py
# Convert example csv file to ods
#
# Copyright (c) 2012 Tom Marble
# Copyright (c) 2012 Bradley M. Kuhn
#
# This program gives you software freedom; you can copy, modify, convey,
# and/or redistribute it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program in a file called 'GPLv3'.  If not, write to the:
#    Free Software Foundation, Inc., 51 Franklin St, Fifth Floor
#                                    Boston, MA 02110-1301, USA.

import sys, os, os.path, optparse
import csv
import ooolib2

file_fields = [ 'Receipt', 'Invoice', 'Statement', 'Contract', 'PurchaseOrder',
                'Approval', 'Check', 'IncomeDistributionAnalysis', 'CurrencyRate' ]

def err(msg):
    print 'error: %s' % msg
    sys.exit(1)

def csv2ods(csvname, odsname, verbose = False):
    if verbose:
        print 'converting from %s to %s' % (csvname, odsname)
    doc = ooolib2.Calc()
    #  add a pagebreak style
    style = 'pagebreak'
    style_pagebreak = doc.styles.get_next_style('row')
    style_data = tuple([style, ('style:row-height', doc.styles.property_row_height)])
    doc.styles.style_config[style_data] = style_pagebreak
    #  add a currency style
    style = 'currency'
    style_currency = doc.styles.get_next_style('cell')
    style_data = tuple([style])
    doc.styles.style_config[style_data] = style_currency
    
    row = 1
    csvdir = os.path.dirname(csvname)
    if len(csvdir) == 0:
        csvdir = '.'
    csvfile = open(csvname, 'rb')
    reader = csv.reader(csvfile, delimiter=',', quotechar='"')
    for fields in reader:
        if len(fields) > 0:
            for col in range(len(fields)):
                val = fields[col]
                if len(val) > 0 and val[0] == '$':
                    doc.set_cell_value(col + 1, row, 'currency', val[1:])
                else:
                    if ((col >= 5) and (not val in file_fields) and len(val) > 0):
                        linkrel = '../' + val # ../ means remove the name of the *.ods
                        linkname = os.path.basename(val) # name is just the last component
                        doc.set_cell_value(col + 1, row, 'link', (linkrel, linkname))
                        linkpath = csvdir + '/' + val
                        if verbose:
                            if os.path.exists(linkpath):
                                print 'relative link %s EXISTS at %s' % (val, linkpath)
                            else:
                                print 'relative link %s DOES NOT EXIST at %s' % (val, linkpath)
                    else:
                        doc.set_cell_value(col + 1, row, 'string', val)
        else:
            # enter an empty string for blank lines
            doc.set_cell_value(1, row, 'string', '')
            # put a pagebreak here
            doc.sheets[doc.sheet_index].set_sheet_config(('row', row), style_pagebreak)
        row += 1
    # save the file
    doc.save(odsname)

def main():
    program = os.path.basename(sys.argv[0])
    version = '0.1'
    parser = optparse.OptionParser(usage='%prog [--help] [--verbose]',
                                   version='%prog ' + version)
    parser.add_option('-v', '--verbose', action='store_true',
                      dest='verbose',
                      help='provide extra information while processing')
    parser.add_option('-c', '--csv', action='store', 
                      help='csv file to process')
    parser.add_option('-o', '--ods', action='store', 
                      help='ods output filename')
    (options, args) = parser.parse_args()
    if len(args) != 0:
        parser.error("not expecting extra args")  
    if not os.path.exists(options.csv):
        err('csv does not exist: %s' % options.csv)
    if not options.ods:
        (root, ext) = os.path.splitext(options.csv)
        options.ods = root + '.ods'
    if options.verbose:
        print '%s: verbose mode on' % program
        print 'csv:', options.csv
        print 'ods:', options.ods
    csv2ods(options.csv, options.ods, options.verbose)

if __name__ == '__main__':
  main()
