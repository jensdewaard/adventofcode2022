{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  name = "advent2022";
  packages = with pkgs.haskellPackages; [ 
    cabal-install haskell-language-server heaps
  ];
}
