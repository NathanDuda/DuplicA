from http.server import SimpleHTTPRequestHandler, HTTPServer

class CORSRequestHandler(SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Access-Control-Allow-Origin', '*')
        super().end_headers()

if __name__ == "__main__":
    server_address = ('', 8002)
    httpd = HTTPServer(server_address, CORSRequestHandler)
    print("Serving on port 8002 with CORS enabled...")
    httpd.serve_forever()
