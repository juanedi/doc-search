let
  sources = import ./nix/sources.nix;
  niv = import sources.niv { };
  pkgs = import sources.nixpkgs { };
  pkgs-darwin = import sources.nixpkgs { localSystem = "x86_64-darwin"; };
in
pkgs.mkShell {
  buildInputs = [
    niv.niv

    pkgs-darwin.cabal-install
    pkgs-darwin.elmPackages.elm
    pkgs-darwin.elmPackages.elm-format
    pkgs-darwin.elmPackages.elm-language-server
    pkgs-darwin.ghc
    pkgs-darwin.haskell-language-server
    pkgs-darwin.haskellPackages.fourmolu
    pkgs-darwin.zlib
  ];
}
