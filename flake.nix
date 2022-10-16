{
  description = "Client library for GUS BIR API";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;
    xml-lens-src = {
      url = "https://hackage.haskell.org/package/xml-lens-0.3.1/xml-lens-0.3.1.tar.gz";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
      };
      hlib = pkgs.haskell.lib;
      hpkgs = pkgs.haskell.packages.ghc924.override {
        overrides = hself: hsuper: {
          xml-lens = hlib.dontCheck (hself.callCabal2nix "xml-lens" "${inputs.xml-lens-src}" {});
          gus-regon = hself.callCabal2nix "gus-regon" ./. {};
        };
      };
    in {
      defaultPackage.x86_64-linux = hpkgs.gus-regon;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps: [ ps.gus-regon ];
        buildInputs = [
          hpkgs.cabal-install
          hpkgs.haskell-language-server
        ];
      };
    };
}
