To run a build directly, run
```
  elm make --optimize src/Main.elm --output generated/Main.js
```
and run a local python webserver with
```
  python -m http.server 8000
```

The production build on NixOS is done with `nix` by running
```
  nix-build
```
and can be tested by running a python server serving the resulting `result` directory (which points to the nix store), using
```
  python -m http.server 8000 --directory result/
```

When changing the Elm dependencies, the Nix build needs to be re-generated using
```
  ./generate-nix-build
```

