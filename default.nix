{ nixpkgs ? import <nixpkgs> {} }:

let
  frontend = import ./frontend {};
  backend  = import ./backend  { frontendPath = frontend; };
in
  backend

