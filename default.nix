{ nixpkgs ? import <nixpkgs> {}
, production ? true
}:

with nixpkgs;
with lib;

let
  version  = commitIdFromGitRepo ./.git;
  frontend = import ./frontend { inherit version production; };
  backend  = import ./backend  { inherit version frontend; };
in
  backend

