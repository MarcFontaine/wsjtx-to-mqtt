{ system ? builtins.currentSystem
, config ? {}
, pkgs ? ( import <nixpkgs> {})
}:
with pkgs;
let
  hsPkgs = haskell.packages.ghc822;
in
  haskell.lib.buildStackProject {
     name = "wsjtx-to-mqtt";
     ghc = hsPkgs.ghc;
     buildInputs = [
       cabal-install stack
       hlint
       hsPkgs.stylish-haskell
     ];
  }
