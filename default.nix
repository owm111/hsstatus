{ pkgs ? import <nixpkgs> {} }:

with pkgs.haskell.lib;

let
  ghcVersion = "865"; # There's also 883
  compiler = pkgs.haskell.packages."ghc${ghcVersion}".override {
    overrides = self: super: {
      threadscope = markUnbroken (doJailbreak super.threadscope);
    };
  };
in

compiler.developPackage {
  root = ./.;
  modifier = drv: addBuildTools drv (with compiler; [
    cabal-install
    ghc-events
    threadscope
  ]);
  source-overrides = {
    linux-inotify = builtins.fetchTarball "https://hackage.haskell.org/package/linux-inotify-0.3.0.2/linux-inotify-0.3.0.2.tar.gz";
  };
}
