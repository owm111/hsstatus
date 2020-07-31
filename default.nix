{ pkgs ? import <nixpkgs> {} }:

let
  ghcVersion = "865"; # There's also 883
  compiler = pkgs.haskell.packages."ghc${ghcVersion}";

  cabal-install = compiler.cabal-install;
in

with pkgs.haskell.lib;

compiler.developPackage {
  root = ./.;
  modifier = drv: addBuildTools drv [ cabal-install ];
}
