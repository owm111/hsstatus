{
  description = "Experimental status monitor";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";

  outputs = { self, nixpkgs }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = final.lib.composeExtensions
            prev.haskell.packageOverrides
            (hself: hsuper: {
              hsstatus = hself.callCabal2nix "hsstatus" self {};
              # Is this ok?
              linux-inotify = prev.haskell.lib.markUnbroken hsuper.linux-inotify;
            });
        };
      };

      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ overlay ];
      };
      compiler = pkgs.haskell.packages.ghc882;

      pkg = compiler.hsstatus;
      shell = compiler.shellFor {
        packages = p: [ p.hsstatus ];
        buildInputs = [
          compiler.cabal-install
          compiler.ghc-events
        ];
      };
    in {
      inherit overlay;
      packages.x86_64-linux.hsstatus = pkg;
      devShell.x86_64-linux = shell;

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.hsstatus;
    };
}
