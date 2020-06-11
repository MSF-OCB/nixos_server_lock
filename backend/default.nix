{ nixpkgs ? import <nixpkgs> {},
  pythonPkgs ? nixpkgs.pkgs.python3Packages,
  frontend
}:

let
  inherit pythonPkgs;

  package = { buildPythonApplication, flask, gevent, flask-cors, flask-compress }:
    buildPythonApplication {
      pname = "panic_button_backend";
      version = "0.1.0";
      src = ./.;

      # Runtime dependencies
      propagatedBuildInputs = [ flask gevent flask-cors flask-compress ];

      postInstall = ''
        ln -sf ${frontend} $out/lib/python3.7/site-packages/msfocb/static
      '';

      doCheck = false;

      meta = {
        description = ''
          Lock the data partition of a server via a web interface.
        '';
      };
    };
in
  pythonPkgs.callPackage package {}

