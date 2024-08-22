{
  nixpkgs ? import <nixpkgs> { },
  version ? "dev",
  production ? true,
}:

let
  frontend = nixpkgs.callPackage ./frontend { inherit version production; };
  backend = nixpkgs.callPackage ./backend { inherit version frontend; };
in
backend
