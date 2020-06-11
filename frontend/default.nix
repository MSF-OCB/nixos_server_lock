{ nixpkgs ? import <nixpkgs> {}, production ? true }:

with nixpkgs;
with lib;

let
  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? []
    , registryDat ? ./registry.dat
    , production
    }:
    stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm nodePackages_10_x.uglify-js ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        elmVersion = "0.19.1";
        inherit registryDat;
      };

      installPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
        extension = "js";
        build_module = module: ''
          echo "compiling ${elmfile module}"
          elm make --optimize ${elmfile module} --output $out/generated/${module}.${extension}
          ${optionalString production ''
            echo "minifying ${module}.${extension}"
            uglifyjs $out/generated/${module}.${extension} \
                     --compress \
                     'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe,passes=2' \
              | uglifyjs --mangle --output=$out/generated/${module}.${extension}
          ''}
        '';
      in ''
        mkdir -p $out/assets
        ${concatMapStrings build_module targets}

        echo "copying assets"
        cp -r index.html assets static $out
      '';
    };
in mkDerivation {
  name = "panic_button_frontend-0.1.0";
  srcs = ./elm-srcs.nix;
  src = ./.;
  targets = ["Main"];
  srcdir = "./src";
  inherit production;
}

