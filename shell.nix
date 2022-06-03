let
  sources = import ./nix/sources.nix;
  niv = import sources.niv { };
  pkgs = import sources.nixpkgs { };
  pkgs-intel = import sources.nixpkgs { localSystem = "x86_64-darwin"; };
in
pkgs.mkShell {
  buildInputs = [
    niv.niv

    pkgs-intel.cabal-install
    pkgs-intel.elmPackages.elm
    pkgs-intel.elmPackages.elm-format
    pkgs-intel.elmPackages.elm-language-server
    pkgs-intel.ghc
    pkgs-intel.haskell-language-server
    pkgs-intel.haskellPackages.fourmolu
    pkgs-intel.zlib
  ];
}
