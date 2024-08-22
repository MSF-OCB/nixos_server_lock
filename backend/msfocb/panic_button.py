import argparse
import flask
import hashlib
import random
import subprocess
import time

from flask import Flask, Response, request
from flask_compress import Compress  # type: ignore
from flask_cors import cross_origin  # type: ignore
from functools import wraps
from gevent.pywsgi import WSGIServer  # type: ignore
from logging.config import dictConfig
from typing import Any, Callable


def jsonify(*args: Any, **kwargs: Any) -> Response:
    return flask.jsonify(*args, **kwargs)


def send_from_directory(directory: Any, filename: Any, **options: Any) -> Response:
    return flask.send_from_directory(directory, filename, **options)


dictConfig(
    {
        "version": 1,
        "formatters": {
            "default": {
                "format": "[%(asctime)s] %(levelname)s in %(module)s: %(message)s",
            }
        },
        "handlers": {
            "wsgi": {
                "class": "logging.StreamHandler",
                "stream": "ext://flask.logging.wsgi_errors_stream",
                "formatter": "default",
            }
        },
        "root": {"level": "INFO", "handlers": ["wsgi"]},
    }
)


# Decorator to verify the key in a request
# See https://flask.palletsprojects.com/en/1.1.x/patterns/viewdecorators/
#
# To use, annotate the endpoint with @key_required and pass the Response
# to return if the key is invalid.
# Be careful to pass the response as a function object to be called from
# within the decorator and not to call it while defining the decorator,
# otherwise the request context will not be available when the function
# is called.
def key_required(invalid_response: Callable[[], Response]):
    seconds_divisor: int = 2

    def decorator(wrapped: Callable[..., Response]):
        def do_validate_key(request_key: str, time_input: int) -> Callable[[int], bool]:
            def go(shift: int) -> bool:
                m = hashlib.sha256()
                m.update(bytes(str(time_input - shift), "utf-8"))
                calculated_key = m.hexdigest()
                return request_key == calculated_key

            return go

        @wraps(wrapped)
        def validate_key(*args, **kwargs) -> Response:
            request_key = request.args.get("key") or ""
            time_input = int(time.time()) // seconds_divisor
            key_valid = any(map(do_validate_key(request_key, time_input), [0, 1]))
            if key_valid:
                return wrapped(*args, **kwargs)
            else:
                return invalid_response()

        return validate_key

    return decorator


static = "static"
app = Flask(__name__, static_folder=static, static_url_path=f"/{static}")
Compress(app)


def args_parser():
    parser = argparse.ArgumentParser(description="Disable the encryption key")
    parser.add_argument("--listen_port", type=int, required=True, dest="listen_port")
    parser.add_argument("--lock_script", type=str, required=True, dest="lock_script")
    parser.add_argument(
        "--verify_script", type=str, required=True, dest="verify_script"
    )
    parser.add_argument(
        "--lock_retry_max_count",
        type=int,
        required=False,
        default=10,
        dest="lockRetryMaxCount",
    )
    parser.add_argument(
        "--verify_retry_max_count",
        type=int,
        required=False,
        default=10,
        dest="verifyRetryMaxCount",
    )
    parser.add_argument(
        "--poll_interval", type=int, required=False, default=15, dest="retryDelaySec"
    )
    parser.add_argument(
        "--disable_targets", type=str, required=True, dest="disable_targets", nargs="*"
    )
    return parser


args = args_parser().parse_args()


def return_status(status: bool) -> Response:
    return jsonify({"status": ("OK" if status else "NOK")})


def return_ok() -> Response:
    return return_status(True)


def return_nok() -> Response:
    return return_status(False)


def exec_and_return(cmd: str) -> Response:
    p = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    output, _ = p.communicate()
    app.logger.info(output.decode("utf-8"))
    return return_status(p.returncode == 0)


def random_response() -> Response:
    return return_status(random.random() * 10 < 3)


@app.route("/")
def root() -> Response:
    return send_from_directory(static, "index.html")


@app.route("/api/config", methods=["GET"])
def config() -> Response:
    return jsonify(
        {
            "hosts": args.disable_targets,
            "retryDelaySec": args.retryDelaySec,
            "lockRetryMaxCount": args.lockRetryMaxCount,
            "verifyRetryMaxCount": args.verifyRetryMaxCount,
        }
    )


@app.route("/api/lock", methods=["POST"])
@cross_origin()
@key_required(return_nok)
def lock() -> Response:
    if request.args.get("mock") == "true":
        app.logger.info("Lock: mock is set to true, producing a random response...")
        return random_response()
    else:
        app.logger.info("Lock: mock is set to false, locking...")
        return exec_and_return(args.lock_script)


@app.route("/api/verify", methods=["GET"])
@cross_origin()
@key_required(return_nok)
def verify() -> Response:
    if request.args.get("mock") == "true":
        app.logger.info("Verify: mock is set to true, producing a random response...")
        return random_response()
    else:
        app.logger.info("Verify: mock is set to false, verifying...")
        return exec_and_return(args.verify_script)


def main():
    http_server = WSGIServer(("", args.listen_port), app)
    http_server.serve_forever()


if __name__ == "__main__":
    main()
