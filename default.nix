let
  frontend = import ./frontend {};
  backend  = import ./backend  { inherit frontend; };
in
  backend

