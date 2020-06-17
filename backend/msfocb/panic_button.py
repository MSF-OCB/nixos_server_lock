import argparse
import hashlib
import subprocess
import time

from flask import Flask, Response, request, send_from_directory, jsonify
from flask_compress import Compress
from flask_cors import cross_origin
from functools import wraps
from gevent.pywsgi import WSGIServer
from logging.config import dictConfig

dictConfig({
  'version': 1,
  'formatters': {'default': {
    'format': '[%(asctime)s] %(levelname)s in %(module)s: %(message)s',
  }},
  'handlers': {'wsgi': {
    'class': 'logging.StreamHandler',
    'stream': 'ext://flask.logging.wsgi_errors_stream',
    'formatter': 'default'
  }},
  'root': {
    'level': 'INFO',
    'handlers': ['wsgi']
  }
})

# Decorator to verify the key in a request
# See https://flask.palletsprojects.com/en/1.1.x/patterns/viewdecorators/
#
# To use, annotate the endpoint with @key_required and pass the Response
# to return if the key is invalid.
# Be careful to pass the response as a function object to be called from
# within the decorator and not to call it while defining the decorator,
# otherwise the request context will not be available when the function
# is called.
def key_required(invalid_response: Response):
  seconds_divisor = 2
  def decorator(wrapped):

    def do_validate_key(request_key, time_input):
      def go(shift) -> bool:
        m = hashlib.sha256()
        m.update(bytes(str(time_input - shift), 'utf-8'))
        calculated_key = m.hexdigest()
        return request_key == calculated_key
      return go

    @wraps(wrapped)
    def validate_key(*args, **kwargs):
      request_key = request.args.get('key')
      time_input = int(time.time()) // seconds_divisor
      key_valid = any(map(do_validate_key(request_key, time_input), [0, 1]))
      if key_valid:
        return wrapped(*args, **kwargs)
      else:
        return invalid_response()

    return validate_key
  return decorator

static = "static"
app = Flask(__name__,
            static_folder = static,
            static_url_path = f"/{static}")
Compress(app)

def args_parser():
  parser = argparse.ArgumentParser(description='Disable the encryption key')
  parser.add_argument('--lock_script',     type=str, required=True, dest='lock_script')
  parser.add_argument('--verify_script',   type=str, required=True, dest='verify_script')
  parser.add_argument('--lock_retry_max_count', type=int, required=False, default=10, dest='lockRetryMaxCount')
  parser.add_argument('--verify_retry_max_count', type=int, required=False, default=10, dest='verifyRetryMaxCount')
  parser.add_argument('--poll_interval',   type=int, required=False, default=5, dest='retryDelaySec')
  parser.add_argument('--disable_targets', type=str, required=True, dest='disable_targets', nargs='*')
  return parser

args = args_parser().parse_args()

def return_status(status: bool) -> Response:
  return jsonify({ 'status': ('OK' if status else 'NOK') })

def return_ok() -> Response:
  return return_status(True)

def return_nok() -> Response:
  return return_status(False)

@app.route('/')
def root() -> Response:
  return send_from_directory(static, 'index.html')

@app.route('/api/config', methods=['GET'])
def config() -> Response:
  return jsonify({ 'hosts': args.disable_targets
                 , 'retryDelaySec': args.retryDelaySec
                 , 'lockRetryMaxCount': args.lockRetryMaxCount
                 , 'verifyRetryMaxCount': args.verifyRetryMaxCount
                 })

@app.route('/api/lock', methods=['POST'])
@cross_origin()
@key_required(return_nok)
def lock() -> Response:
  if request.args.get('mock') == 'true':
    print("Mock is set to true, ignoring...")
    return return_ok()
  else:
    print("Mock is set to false, locking...")
    p = subprocess.run(args.lock_script.split())
    return return_status(p.returncode == 0)

@app.route('/api/verify', methods=['GET'])
@cross_origin()
@key_required(return_nok)
def verify() -> Response:
  p = subprocess.run(args.verify_script.split())
  return return_status(p.returncode == 0)

def main():
  http_server = WSGIServer(('', 1234), app)
  http_server.serve_forever()

if __name__ == '__main__':
  main()

