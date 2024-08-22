{
  python3,
  ruff,
  version,
  frontend,
}:

python3.pkgs.buildPythonApplication {
  pname = "panic_button_backend";
  src = builtins.path {
    path = ./.;
    name = "backend";
  };
  inherit version;

  pyproject = true;

  nativeCheckInputs = with python3.pkgs; [
    mypy
    ruff
  ];
  nativeBuildInputs = with python3.pkgs; [ flit-core ];
  propagatedBuildInputs = with python3.pkgs; [
    flask
    flask-compress
    flask-cors
    gevent
  ];

  postInstall = ''
    ln --symbolic ${frontend} $out/${python3.sitePackages}/msfocb/static
  '';

  doCheck = true;
  checkPhase = ''
    echo "Running mypy..."
    mypy --warn-redundant-casts \
         --warn-unused-ignores \
         --warn-no-return \
         --warn-return-any \
         --warn-unreachable \
         --check-untyped-defs \
         $src/msfocb/
    echo "Running ruff..."
    ruff check .
  '';

  meta = {
    description = ''
      Lock the data partition of a server via a web interface.
    '';
  };
}
