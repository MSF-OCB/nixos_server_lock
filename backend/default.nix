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
      propagatedBuildInputs = [ flask flask-compress flask-cors gevent ];

      postInstall = ''
        ln --symbolic ${frontend} $out/${nixpkgs.pkgs.python3.sitePackages}/msfocb/static
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

