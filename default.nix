{ nixpkgs ? import <nixpkgs> {}
, version ? "dev"
, production ? true
}:

with nixpkgs;
with lib;

let
  frontend = import ./frontend { inherit version production; };
  backend  = import ./backend  { inherit version frontend; };
in
  backend

