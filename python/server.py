# -*- coding: utf-8 -*-

import ledger
import cgi
import sys
import types

from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

from Cheetah.Template import Template
from Cheetah.Filters import WebSafe

class UnicodeFilter(WebSafe):
    def filter(self, s, **kargs):
        return WebSafe.filter(self, s, str=unicode, **kargs)

templateDef = '''#encoding utf-8
 <html>
    <head>
      <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
      <title>$title</title>
    </head>
    <body>
      <table>
        #for $xact in $journal
        #for $post in $xact
        <tr>
          <td>$post.date</td>
          <td>$post.xact.payee</td>
          <td>$post.account</td>
          <td>$post.amount</td>
        </tr>
        #end for
        #end for
      </table>
    </body>
  </html>
'''

class LedgerHandler(BaseHTTPRequestHandler):
    def __init__(self, *args):
        self.journal = ledger.Journal('/Users/johnw/src/ledger/doc/sample.dat')
        BaseHTTPRequestHandler.__init__(self, *args)

    def do_GET(self):
        tmpl = Template(templateDef, filter=UnicodeFilter)

        tmpl.title   = 'Ledger Journal'
        tmpl.journal = self.journal

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

print __name__
if __name__ == '__main__':
    main()
