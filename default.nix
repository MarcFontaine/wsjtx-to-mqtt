{ compiler   ? "ghc863"
, haddock    ? false
, test       ? false
, benchmarks ? false
, nixpkgs ? import <nixpkgs> {}
}:
with builtins;
let

nixpkgs-rev = "0ad9ad40f5aa50abf22489a9e50c32dd82fd3d7e";
nixpkgs-url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgs-rev}.tar.gz";
nixpkgs     = import (builtins.fetchTarball { url=nixpkgs-url; }) { inherit config; };

mqtt-hs-rev = "3cd5cde8306d63f5ebc7a5cef18a54aadb4e247a";
mqtt-hs-url = "https://github.com/MarcFontaine/mqtt-hs/archive/${mqtt-hs-rev}.tar.gz";
mqtt-hs-dir = builtins.fetchTarball {url=mqtt-hs-url;};

config = { packageOverrides = super:
  let self      = super.pkgs;
  lib       = super.haskell.lib;
  overrides = self: super:
  {
    mqtt-hs = super.callPackage "${mqtt-hs-dir}/mqtt-hs.nix" {};
  };
in {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ghc863 = super.haskell.packages.ghc863.override { inherit overrides; };
#        ghc844 = super.haskell.packages.ghc844.override { inherit overrides; };
      };
    };
  };
};

lib         = nixpkgs.haskell.lib;
callPackage = nixpkgs.haskell.packages.${compiler}.callPackage;

doHaddock = if haddock
  then lib.doHaddock
  else lib.dontHaddock;
doTest = if test
  then lib.doCheck
  else lib.dontCheck;
doBench = if benchmarks
  then lib.doBenchmark
  else nixpkgs.lib.id;

myPackage = doHaddock(doTest(doBench(
  callPackage ./wsjtx-to-mqtt.nix {}
)));
in
{ inherit myPackage; }
