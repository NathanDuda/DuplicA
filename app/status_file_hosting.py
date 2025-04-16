
# serving status file 
# AND results folder for download 
# AND json file for visualization 
# AND Figure image for rendering on screen

import os
import shutil
import tempfile
from http.server import SimpleHTTPRequestHandler, HTTPServer
from urllib.parse import urlparse

RESULTS_DIR = "Results"
STATUS_FILE_PATH = "status/status.txt"
VISUALIZATION_JSON_PATH = "frontend/duplic-a/public/visualization_options.json"

class CORSRequestHandler(SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Access-Control-Allow-Origin", "*")
        super().end_headers()

    def do_GET(self):
        parsed_path = urlparse(self.path).path

        if parsed_path == "/api/download-results":
            self.serve_results_zip()
        elif parsed_path == "/status.txt":
            self.serve_status_file()
        elif parsed_path == "/visualization_options.json":
            self.serve_json_file(VISUALIZATION_JSON_PATH)
        elif parsed_path == "/Figure.png":
            self.serve_figure()
        else:
            self.send_error(404, "File not found")

    def serve_results_zip(self):
        try:
            temp_zip_path = tempfile.mktemp(suffix=".zip")
            shutil.make_archive(temp_zip_path.replace(".zip", ""), 'zip', RESULTS_DIR)

            self.send_response(200)
            self.send_header("Content-Type", "application/zip")
            self.send_header("Content-Disposition", "attachment; filename=results.zip")
            self.end_headers()

            with open(temp_zip_path, "rb") as f:
                shutil.copyfileobj(f, self.wfile)

            os.remove(temp_zip_path)
        except Exception as e:
            self.send_error(500, f"Failed to zip results: {str(e)}")

    def serve_status_file(self):
        try:
            with open(STATUS_FILE_PATH, "r") as f:
                content = f.read()

            self.send_response(200)
            self.send_header("Content-Type", "text/plain")
            self.end_headers()
            self.wfile.write(content.encode("utf-8"))
        except FileNotFoundError:
            self.send_error(404, "status.txt not found")
        except Exception as e:
            self.send_error(500, f"Failed to read status.txt: {str(e)}")

    def serve_json_file(self, path):
        try:
            with open(path, "r") as f:
                content = f.read()

            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(content.encode("utf-8"))
        except FileNotFoundError:
            self.send_error(404, "visualization_options.json not found")
        except Exception as e:
            self.send_error(500, f"Failed to read JSON: {str(e)}")

    def serve_figure(self):
        figure_path = "status/Figure.png"
        try:
            with open(figure_path, "rb") as f:
                content = f.read()
    
            self.send_response(200)
            self.send_header("Content-Type", "image/png")
            self.end_headers()
            self.wfile.write(content)
        except FileNotFoundError:
            self.send_error(404, "Figure.png not found")
        except Exception as e:
            self.send_error(500, f"Failed to read Figure.png: {str(e)}")


if __name__ == "__main__":
    server_address = ('', 8002)
    httpd = HTTPServer(server_address, CORSRequestHandler)
    print("Serving on port 8002 with CORS enabled...")
    httpd.serve_forever()
