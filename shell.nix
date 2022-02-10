let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskell.lib.dontCheck (pkgs.haskell.packages.ghc865.callPackage ./default.nix { })
