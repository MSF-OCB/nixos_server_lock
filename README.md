To run a development build, run
```
  nix-build --arg production "false"
```
and to test the result, run a python server serving the resulting `result` directory (which points to the nix store), using
```
  python -m http.server 8000 --directory result/
```

The production build, which also minifies the resulting JS, is done by running just
```
  nix-build
```
To also copy the generated and minified result to the generated directory, to be deployed to github pages, run
```
  nix-build && ./copy_generated.sh
```
and commit the result.

When changing the Elm dependencies, the Nix build needs to be re-generated using
```
  ./generate-nix-build
```

To run a build directly, without using nix, run
```
  elm make --optimize src/Main.elm --output generated/Main.js
```
and run a local python webserver with
```
  python -m http.server 8000
```

