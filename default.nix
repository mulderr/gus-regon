{ nixpkgs ? import ./nix/nixpkgs.nix }:

let
  hpkgs = nixpkgs.haskellPackages;
  drv = hpkgs.callCabal2nix "gus-regon" ./. {};
in
  if nixpkgs.lib.inNixShell
  then hpkgs.shellFor {
    packages = ps: [ drv ];
    buildInputs = with nixpkgs;
      [ cabal-install
        hlint
        haskellPackages.haskell-language-server
      ];
    }
  else drv
