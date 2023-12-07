#!/usr/bin/env python3
# csv2ods.py
# Convert example csv file to ods
#
# Copyright (c) 2012       Tom Marble
# Copyright (c) 2012, 2013 Bradley M. Kuhn
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
import shutil
import string
try:
    from Crypto.Hash import SHA256
except ModuleNotFoundError:
    print("Missing pycrypto")
    #sys.exit(-1)


def err(msg):
    print(f'error: {msg}')
    sys.exit(1)

def ReadChecksums(inputFile):
    checksums = {}
    with open(inputFile, "r") as inputFH:
        entries = inputFH.readlines()
    for ee in entries:
        fileName, checksum = ee.split(":")
        fileName = fileName.replace(' ', "")
        checksum = checksum.replace(' ', "")
        checksum = checksum.replace("\n", "")
        checksums[checksum] = fileName
    return checksums

def ChecksumFile(filename):
    sha256 = SHA256.new()
    chunk_size = 8192
    with open(filename, 'rb') as myFile:
        while True:
            chunk = myFile.read(chunk_size)
            if len(chunk) == 0:
                break
            sha256.update(chunk)
    return sha256.hexdigest()

def main():
    program = os.path.basename(sys.argv[0])

    print(get_file_checksum(sys.argv[1]))

def csv2ods(csvname, odsname, encoding='', singleFileDirectory=None, knownChecksums={}, verbose = False):
    filesSavedinManifest = {}

    if knownChecksums:
        checksumCache = {}

    if verbose:
        print(f'converting from {csvname} to {odsname}')

    if singleFileDirectory:
        if not os.path.isdir(os.path.join(os.getcwd(),singleFileDirectory)):
            os.mkdir(singleFileDirectory)

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
    csvfile = open(csvname, 'r')
    reader = csv.reader(csvfile, delimiter=',', quotechar='"')
    for fields in reader:
        if len(fields) > 0:
            for col in range(len(fields)):
                val = fields[col]
                if encoding != '' and val[0:5] != "link:":  # Only utf8 encode if it's not a filename
                    val = val.encode('utf-8')
                if len(val) > 0 and val[0] == '$':
                    doc.set_cell_value(col + 1, row, 'currency', val[1:])
                else:
                    if (len(val) > 0 and val[0:5] == "link:"):
                        val = val[5:]
                        linkname = os.path.basename(val) # name is just the last component
                        newFile = None

                        if not singleFileDirectory:
                            newFile = val

                        if knownChecksums:
                            if not checksumCache.has_key(val):
                                checksum = ChecksumFile(val)
                                checksumCache[val] = checksum
                            else:
                                checksum = checksumCache[val]

                            if knownChecksums.has_key(checksum):
                                newFile = knownChecksums[checksum]
                                print(f'FOUND new file in known: {newFile}')

                        if not newFile:
                            relativeFileWithPath = os.path.basename(val)

                            fileName, fileExtension = os.path.splitext(relativeFileWithPath)
                            newFile = fileName[:15]   # 15 is an arbitrary choice.
                            newFile = newFile + fileExtension
                            # We'll now test to see if we made this file
                            # before, and if it matched the same file we
                            # now want.  If it doesn't, try to make a
                            # short file name for it.
                            if filesSavedinManifest.has_key(newFile) and filesSavedinManifest[newFile] != val:
                                testFile = None
                                for cc in list(string.letters) + list(string.digits):
                                    testFile = cc + newFile
                                    if not filesSavedinManifest.has_key(testFile):
                                        break
                                    testFile = None
                                    if not testFile:
                                        raise Exception("too many similar file names for linkage; giving up")
                                    else:
                                        newFile = testFile
                                        if not os.path.exists(csvdir + '/' + val):
                                            raise Exception("File" + csvdir + '/' + val + " does not exist in single file directory mode; giving up")
                            src = os.path.join(csvdir, val)
                            dest = os.path.join(csvdir, singleFileDirectory, newFile)
                            shutil.copyfile(src, dest)
                            shutil.copystat(src, dest)
                            shutil.copymode(src, dest)

                            newFile = os.path.join(singleFileDirectory, newFile)

                        if knownChecksums:
                            checksumCache[checksum]   = newFile
                            knownChecksums[checksum]  = newFile

                        linkrel = '../' + newFile # ../ means remove the name of the *.ods
                        doc.set_cell_value(col + 1, row, 'link', (linkrel, linkname))
                        linkpath = csvdir + '/' + val

                        if not val in filesSavedinManifest:
                            filesSavedinManifest[newFile] = val

                        if not os.path.exists(linkpath):
                            print(f'WARNING: link {val} DOES NOT EXIST at {linkpath}')
                        if verbose:
                            if os.path.exists(linkpath):
                                print(f'relative link {val} EXISTS at {linkpath}')
                    else:
                        if val == "pagebreak":
                            doc.sheets[doc.sheet_index].set_sheet_config(('row', row), style_pagebreak)
                        else:
                            if val[0:6] == "title:":
                                doc.sheets[doc.sheet_index].set_name(val[6:])
                            else:
                                doc.set_cell_value(col + 1, row, 'string', val)
        else:
            # enter an empty string for blank lines
            doc.set_cell_value(1, row, 'string', '')
        row += 1
    # save manifest file 
    if filesSavedinManifest.keys() != []:
        manifestFH = open("MANIFEST", "a")
        manifestFH.write("# Files from %s\n" % odsname)
        for file in filesSavedinManifest.keys():
            manifestFH.write("%s\n" % file)
            
        manifestFH.close()
    # Save spreadsheet file.
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
    parser.add_option('-e', '--encoding', action='store', 
                      help='unicode character encoding type')
    parser.add_option('-d', '--single-file-directory', action='store',
                      help='directory name to move all files into')
    parser.add_option('-s', '--known-checksum-list', action='store',
                      help='directory name to move all files into')
    (options, args) = parser.parse_args()

    if len(args) != 0:
        parser.error("not expecting extra args")  
    if not options.csv:
      parser.error('Missing required --csv option')
    if not os.path.exists(options.csv):
        err(f'csv does not exist: {options.csv}')
    if not options.ods:
        (root, ext) = os.path.splitext(options.csv)
        options.ods = root + '.ods'
    if options.verbose:
        print(f'''{program}: verbose mode on
        csv: {options.csv}
        ods: {options.ods}
        ods: {options.encoding}
        ''')
    if options.known_checksum_list and not options.single_file_directory:
        err(program + ": --known-checksum-list option is completely useless without --single-file-directory")
    knownChecksums = {}
    if options.known_checksum_list:
        if not os.access(options.known_checksum_list, os.R_OK):
            err(program + ": unable to read file: " + options.known_checksum_list)
        knownChecksums = ReadChecksums(options.known_checksum_list)
    csv2ods(options.csv, options.ods, options.encoding, options.single_file_directory, knownChecksums, options.verbose)

if __name__ == '__main__':
  main()
