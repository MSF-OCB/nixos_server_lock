{
  inputs = {
    systems.url = "github:nix-systems/x86_64-linux";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs =
    inputs:
    let
      eachSystem = inputs.nixpkgs.lib.genAttrs (import inputs.systems);
    in
    {
      overlays.default = final: prev: {
        panic-button = import ./default.nix {
          nixpkgs = final;
          version = inputs.self.sourceInfo.revision or "dev";
        };
      };

      legacyPackages = eachSystem (
        system:
        let
          pkgs = import inputs.nixpkgs {
            overlays = [
              inputs.self.overlays.default
              inputs.devshell.overlays.default
            ];
            inherit system;
          };
        in
        {
          inherit pkgs;
        }
      );

      packages = eachSystem (system: {
        inherit (inputs.self.legacyPackages.${system}.pkgs) panic-button;
      });

      devShells = eachSystem (
        system:
        let
          inherit (inputs.self.legacyPackages.${system}) pkgs;
        in
        {
          default = pkgs.devshell.mkShell {
            packages = with pkgs; [
              elmPackages.elm
              elmPackages.elm-format
            ];
          };
        }
      );

      checks = eachSystem (
        system:
        let
          inherit (inputs.self.legacyPackages.${system}) pkgs;
        in
        {
          inherit (pkgs) panic-button;
        }
      );
    };
}
