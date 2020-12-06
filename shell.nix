let pkgs = import <nixpkgs> {};
in
pkgs.haskellPackages.developPackage {
  name = "aoc2020";
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
    [ ghcid
      haskell-language-server
    ]);
}
