{ production ? true }:

let
  frontend = import ./frontend { inherit production; };
  backend  = import ./backend  { inherit frontend; };
in
  backend

