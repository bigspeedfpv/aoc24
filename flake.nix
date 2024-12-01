{
  description = "Advent of Code 2024 solutions in OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        inherit (pkgs) ocamlPackages mkShell;
        inherit (ocamlPackages) buildDunePackage;
        version = "0.0.1";
      in {
        devShells = {
          default = mkShell {
            inputsFrom = [
              self'.packages.default
            ];
            packages = builtins.attrValues {
              inherit (ocamlPackages) ocaml-lsp ocamlformat utop;
            };
          };
        };

        packages = {
          default = buildDunePackage {
            inherit version;
            pname = "aoc";
            src = ./.;
          };
        };
      };
    };
}
