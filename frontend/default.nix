{ nixpkgs ? import <nixpkgs> {}, production ? true }:

with nixpkgs;
with lib;

let
  dist = "dist";
  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , pname
    , version
    , srcdir ? "./src"
    , targets ? []
    , registryDat ? ./registry.dat
    , production
    }:
    stdenv.mkDerivation {
      inherit pname version src;

      buildInputs = [ elmPackages.elm nodePackages_10_x.uglify-js ];

      preBuildPhases = [ "setupElmStuffPhase" ];

      setupElmStuffPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        elmVersion  = "0.19.1";
        inherit registryDat;
      };

      buildPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
        build_module = out: module: let
          out_file = "${out}/generated/${module}.js";
        in ''
          echo "compiling ${elmfile module}"
          elm make --optimize ${elmfile module} --output ${out_file}
          ${optionalString production ''
            echo "minifying ${out_file}"
            uglifyjs ${out_file} \
                     --compress \
                     'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe,passes=2' \
              | uglifyjs --mangle --output=${out_file}
          ''}
        '';
      in ''
        mkdir -p ${dist}/
        ${concatMapStrings (build_module dist) targets}
      '';

      installPhase = ''
        mkdir -p $out
        echo "copying assets"
        cp -r index.html assets ${dist}/generated $out
      '';
    };
in mkDerivation {
  pname = "panic_button_frontend";
  version = "0.1.0";
  srcs = ./elm-srcs.nix;
  src = ./.;
  targets = ["Main"];
  srcdir = "./src";
  inherit production;
}

