import ledger
import cgi
import sys

from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer

class LedgerHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        print "Saw a GET request!"
        sys.exit(0)

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

def cmd_server():
    try:
        port   = 9000
        server = HTTPServer(('', port), LedgerHandler)
        print "Local HTTP server listening on port %d... (Control-C to exit)" \
            % port
        server.serve_forever()
    except KeyboardInterrupt:
        print "Shutting down server"
        server.socket.close()

