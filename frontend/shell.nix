{ pkgs ? import <nixpkgs> {} }:

with pkgs;
with elmPackages;

mkShell {
  buildInputs = [ elm elm-format ];
}

