import argparse
import hashlib
import subprocess
import time

from flask import Flask, Response, request, send_from_directory, jsonify
from flask_compress import Compress
from flask_cors import CORS
from functools import wraps
from gevent.pywsgi import WSGIServer

# Decorator to verify the key in a request
# See https://flask.palletsprojects.com/en/1.1.x/patterns/viewdecorators/
#
# To use, annotate the endpoint with @key_required and introduce a boolean
# named argument "key_valid" to receive the result
def key_required(wrapped):
  @wraps(wrapped)
  def validate_key(*args, **kwargs):
    request_key = request.args.get('key')
    time_input = int(time.time()) // 2
    m = hashlib.sha256()
    m.update(bytes(str(time_input), 'utf-8'))
    calculated_key = m.hexdigest()
    key_valid = (request_key == calculated_key)
    return wrapped(*args, **kwargs, key_valid = key_valid)
  return validate_key

static = "static"
app = Flask(__name__,
            static_folder = static,
            static_url_path = f"/{static}")
Compress(app)
CORS(app, resources={r"/api/*": {"origins": "*"}})

def args_parser():
  parser = argparse.ArgumentParser(description='Disable the encryption key')
  parser.add_argument('--lock_script',     type=str, required=True, dest='lock_script')
  parser.add_argument('--verify_script',   type=str, required=True, dest='verify_script')
  parser.add_argument('--retry_max_count', type=str, required=False, default=10, dest='retryMaxCount')
  parser.add_argument('--poll_interval',   type=str, required=False, default=5, dest='retryDelaySec')
  parser.add_argument('--disable_targets', type=str, required=True, dest='disable_targets', nargs='*')
  return parser

args = args_parser().parse_args()

@app.route('/')
def root() -> Response:
  return send_from_directory(static, 'index.html')

@app.route('/api/config', methods=['GET'])
def config() -> Response:
  return jsonify({ 'hosts': args.disable_targets,
                   'retryDelaySec': args.retryDelaySec,
                   'retryMaxCount': args.retryMaxCount
                 })

@app.route('/api/lock', methods=['POST'])
@key_required
def lock(key_valid: bool) -> Response:
  if key_valid:
    if request.args.get('mock') == 'true':
      print("Mock is set to true, ignoring...")
      return return_status(True)
    else:
      print("Mock is set to false, locking...")
      p = subprocess.run(args.lock_script.split())
      return return_status(p.returncode == 0)
  else:
    return return_status(False)

@app.route('/api/verify', methods=['GET'])
@key_required
def verify(key_valid: bool) -> Response:
  if key_valid:
    p = subprocess.run(args.verify_script.split())
    return return_status(p.returncode == 0)
  else:
    return return_status(False)

def return_status(status: bool) -> Response:
  return jsonify({ 'status': ('OK' if status else 'NOK') })

def main():
  http_server = WSGIServer(('', 1234), app)
  http_server.serve_forever()

if __name__ == '__main__':
  main()

