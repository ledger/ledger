# -*- coding: utf-8 -*-

import ledger
import cgi
import sys
import types
import posixpath
import urllib
import shutil
import os
import re

from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from os.path import exists, join, isfile

from Cheetah.Template import Template
from Cheetah.Filters import Filter, WebSafe

webroot = join(os.getcwd(), 'python', 'res')

class UnicodeFilter(Filter):
    def filter(self, s, **kargs):
        return Filter.filter(self, s, str=unicode, **kargs)

def strip(value):
    #return re.sub('\n', '<br />', value.strip_annotations().to_string())
    return value.strip_annotations().to_string()

templateDef = '''#encoding utf-8
 <html>
    <head>
      <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
      <title>$title</title>
      <link rel="stylesheet" href="/style.css" type="text/css" media="print, projection, screen" />
      <script type="text/javascript" src="/jquery-latest.js"></script>
      <script type="text/javascript" src="/jquery.tablesorter.min.js"></script>
      <script type="text/javascript" src="/jquery.tablesorter.pager.js"></script>
      <script type="text/javascript" src="/jquery.dimensions.min.js"></script>
      <script type="text/javascript">
        \$(function() { 
          \$("table")
              .tablesorter({textExtraction: 'complex',
                            widthFixed:     true,
                            widgets:        ['zebra']}) 
              .tablesorterPager({size: 100,
                                 container: \$("\#pager")});
        }); 
      </script>
    </head>
    <body>
      <div id="main">
      <h1>Register report</h1>
      <table id="register" cellspacing="1" class="tablesorter">
        <thead>
          <tr>
            <th>Date</th>
            <th>Payee</th>
            <th>Account</th>
            <th>Amount</th>
            <th>Total</th>
          </tr>
        </thead>
        <tfoot>
          <tr>
            <th>Date</th>
            <th>Payee</th>
            <th>Account</th>
            <th>Amount</th>
            <th>Total</th>
          </tr>
        </tfoot>
        <tbody>
          #for $post in $posts
          #set $total = $total + $post.amount
          <tr>
            <!--<td>${$post.xact.date if $post.xact is not $last_xact else $empty}</td>
            <td>${$post.xact.payee if $post.xact is not $last_xact else $empty}</td>-->
            <td>$post.xact.date</td>
            <td>$post.xact.payee</td>
            <td>$post.account</td>
            <td>${strip($post.amount)}</td>
            <td>${strip($total)}</td>
          </tr>
          #set $last_xact = $post.xact
          #end for
        </tbody>
      </table>
      <div id="pager" class="pager">
        <form>
          <img src="/icons/first.png" class="first"/>
          <img src="/icons/prev.png" class="prev"/>
          <input type="text" class="pagedisplay"/>
          <img src="/icons/next.png" class="next"/>
          <img src="/icons/last.png" class="last"/>
          <select class="pagesize">
            <option selected="selected" value="40">40</option>
            <option value="100">100</option>
            <option value="200">200</option>
            <option  value="300">300</option>
          </select>
        </form>
      </div>
    </body>
  </html>
'''

class LedgerHandler(BaseHTTPRequestHandler):
    def __init__(self, *args):
        self.journal = ledger.Journal(sys.argv[1])
        BaseHTTPRequestHandler.__init__(self, *args)

    def do_GET(self):
        path = self.translate_path(self.path)

        if path and exists(path) and isfile(path):
            self.copyfile(open(path), self.wfile)
        else:
            tmpl = Template(templateDef, filter=UnicodeFilter)

            tmpl.title     = 'Ledger Journal'
            tmpl.posts     = self.journal.collect(sys.argv[2])
            tmpl.total     = ledger.Value(0)
            tmpl.strip     = strip
            tmpl.last_xact = None
            tmpl.empty     = ""

            html = unicode(tmpl)
            html = html.encode('utf-8')
            self.wfile.write(html)

    def do_POST(self):
        print "Saw a POST request!"
        try:
            ctype, pdict = cgi.parse_header(self.headers.getheader('content-type'))
            if ctype == 'multipart/form-data':
                query = cgi.parse_multipart(self.rfile, pdict)
            self.send_response(301)
            self.end_headers()
        except Exception:
            print "Saw exception in POST handler"

    # This code is straight from SimpleHTTPServer.py
    def copyfile(self, source, outputfile):
        """Copy all data between two file objects.

        The SOURCE argument is a file object open for reading
        (or anything with a read() method) and the DESTINATION
        argument is a file object open for writing (or
        anything with a write() method).

        The only reason for overriding this would be to change
        the block size or perhaps to replace newlines by CRLF
        -- note however that this the default server uses this
        to copy binary data as well.

        """
        shutil.copyfileobj(source, outputfile)

    def translate_path(self, path):
        """Translate a /-separated PATH to the local filename syntax.

        Components that mean special things to the local file system
        (e.g. drive or directory names) are ignored.  (XXX They should
        probably be diagnosed.)

        """
        # abandon query parameters
        path  = path.split('?',1)[0]
        path  = path.split('#',1)[0]
        path  = posixpath.normpath(urllib.unquote(path))
        words = path.split('/')
        words = filter(None, words)
        path  = webroot
        for word in words:
            drive, word = os.path.splitdrive(word)
            head, word = os.path.split(word)
            if word in (os.curdir, os.pardir): continue
            path = os.path.join(path, word)
        return path

def main(*args):
    try:
        port   = 9000
        server = HTTPServer(('', port), LedgerHandler)
        print "Local HTTP server listening on port %d... (Control-C to exit)" \
            % port
        server.serve_forever()
    except KeyboardInterrupt:
        print "Shutting down server"
        server.socket.close()

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print "usage: server.py <DATA-FILE> <REPORT-QUERY>"
        sys.exit(1)
    main()
